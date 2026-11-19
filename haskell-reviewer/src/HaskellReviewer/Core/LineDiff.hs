{- The line diff, a port of git's xdiff (xdl_diff, xdl_prepare_env
and xdl_build_script in xdiff/xdiffi.c and xdiff/xprepare.c): git
2.44.1, plus the re-diff of moved groups that git 2.54.0 added (see
Compact.hs).

Copyright (C) 2003 Davide Libenzi
Haskell port: Copyright (C) 2026 Lorenzo Tabacchini

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at
your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, see <http://www.gnu.org/licenses/>.

SPDX-License-Identifier: LGPL-2.1-or-later
-}

-- | Line diff of two texts, as @git diff --histogram@ (SPEC §11.4.7,
-- D-321). Pure. See Note [Line diff].
module HaskellReviewer.Core.LineDiff
  ( -- * Types
    Whitespace (..),
    Op (..),
    Run (..),
    BomChange (..),
    LineDiff (..),

    -- * Diff
    lineDiff,
    warmUpLineDiff,

    -- * For tests
    Stats (..),
    Counts,
    Counter (..),
    countOf,
    GitRule (..),
    lineDiffStats,
    lineDiffStatsAs,
    diffLines,
    warmUpPairs,
  )
where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Array qualified as Text.Array
import Data.Text.Internal qualified as Text.Internal
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Word (Word64)
import HaskellReviewer.Core.LineDiff.Compact (GitRule (..), Side (..), changeCompact)
import HaskellReviewer.Core.LineDiff.Env
  ( Changed,
    Counter (..),
    DiffSide (..),
    Env (..),
    isChanged,
    newEnv,
    readCounter,
  )
import HaskellReviewer.Core.LineDiff.Histogram (histogramDiff)
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Line diff]

What we diff, and how close to git we are.

Lines: as the source view cuts them (D-276): split on \n, a leading BOM
isn't text, one \r before \n or at the very end isn't part of the line,
no line after a final \n, "" has 0 lines. 'diffLines' is that split;
LineDiffTest checks it on fixtures/lines/cases.json.

Equal lines:
- Exact mode ('KeepWhitespace'): same text, and the same "ends with a
  newline". Only the last line can lack one, so a final newline added
  or removed shows as git shows it: the last line deleted and inserted.
  CRLF and LF are equal (G8): this is plain git on both texts with CRLF
  turned into LF.
- 'IgnoreWhitespace' is git's -w: same text once git's whitespace is
  dropped (space, tab, CR, LF; not VT, FF, NBSP or Unicode spaces).
  Like git, it ignores the final newline too.

Notes: 'crlfLines' (equal pairs whose \r was dropped on one side only)
and 'bom' (a BOM added or removed). They change no run.

The algorithm is git's, ported (D-321): histogram diff (Histogram.hs),
the Myers fallback for regions where every common line occurs more than
64 times (Myers.hs: trim, discard pass, split with heuristics and cost
limit), then compaction with the indent heuristic, base first
(Compact.hs). Since git 2.54, compaction also diffs a moved group
again with Myers when the other side's group isn't empty: we do that
too ('Git254'); 'GitBefore254' is the old rule, for the oracle test on
an older git. Lines go to classes (an Int per distinct key) and the
algorithm only compares classes, like git. Byte-equal texts skip all of
it.

Env.hs holds the state the other modules share (git's xdfenv_t).

Known differences from git:
- a BOM (we drop it and add a note; git diffs it as text);
- a lone \r at the very end (D-276 drops it; git keeps it);
- git dies when 64 distinct lines share a hash bucket; we have no
  buckets;
- git -U0 trims the common tail in 1 KiB blocks first: we match any
  context of 1 or more lines;
- the user's diff.algorithm, --minimal, -b, --ignore-blank-lines,
  --anchored: not modelled.

No top-level Text values here: hot code must not build shared values
(Q-RTS-BLACKHOLE). 'warmUpLineDiff' runs the paths once anyway.
-}

-- | Compare lines as they are, or as git's @-w@ does.
data Whitespace = KeepWhitespace | IgnoreWhitespace
  deriving (Eq, Show)

-- | What a run of lines is.
data Op
  = -- | In both texts.
    Equal
  | -- | Only in base.
    Delete
  | -- | Only in head.
    Insert
  deriving (Eq, Show)

-- | @count@ lines with the same op.
data Run = Run {op :: !Op, count :: !Int}
  deriving (Eq, Show)

-- | A BOM at the start of head and not of base, or the other way round.
data BomChange = BomAdded | BomRemoved
  deriving (Eq, Show)

-- | The diff of two texts.
data LineDiff = LineDiff
  { -- | Every count is at least 1, no two neighbours have the same op,
    -- Delete comes before Insert. Equal and Delete add up to base's
    -- lines, Equal and Insert to head's. Equal texts give one Equal run
    -- (none for 0 lines).
    runs :: [Run],
    -- | Equal pairs where only one line ended in CRLF.
    crlfLines :: !Int,
    bom :: !(Maybe BomChange)
  }
  deriving (Eq, Show)

-- | Diff base against head. See Note [Line diff].
lineDiff :: Whitespace -> Pair Text -> LineDiff
lineDiff ws p = (lineDiffStats ws p).diff

-- | A diff and its path counters.
data Stats = Stats {diff :: LineDiff, counts :: Counts}
  deriving (Show)

-- | How often each path ran, so tests can check they all do. See
-- 'Counter'.
newtype Counts = Counts (Map.Map Counter Int)
  deriving (Eq, Show)

-- | How often a path ran.
countOf :: Counter -> Counts -> Int
countOf c (Counts m) = Map.findWithDefault 0 c m

-- | 'lineDiff', with the counters.
lineDiffStats :: Whitespace -> Pair Text -> Stats
lineDiffStats = lineDiffStatsAs Git254

-- | 'lineDiffStats' as a given git does it, for the oracle test on an
-- older git. 'lineDiff' follows 'Git254'.
lineDiffStatsAs :: GitRule -> Whitespace -> Pair Text -> Stats
lineDiffStatsAs rule ws p
  | p.base == p.head =
      Stats
        { diff = LineDiff {runs = [Run Equal n | n > 0], crlfLines = 0, bom = Nothing},
          counts = Counts Map.empty
        }
  | otherwise = runST $ do
      let cl = classify (keys ws baseLs) (keys ws headLs)
          c1 = cl.classes.base
          c2 = cl.classes.head
      env <- newEnv c1 c2 cl.count
      histogramDiff env
      let s1 = Side {diff = env.side1, texts = baseLs.texts}
          s2 = Side {diff = env.side2, texts = headLs.texts}
      changeCompact rule env s1 s2
      changeCompact rule env s2 s1
      rs <- buildRuns (VU.length c1) (VU.length c2) env.side1.changed env.side2.changed
      counts <-
        Counts . Map.fromList
          <$> mapM
            (\c -> (c,) <$> readCounter env.counters c)
            [minBound .. maxBound]
      let !crlf = crlfCount baseLs.crs headLs.crs rs
          !b = bomChange baseLs.bom headLs.bom
      pure Stats {diff = LineDiff {runs = rs, crlfLines = crlf, bom = b}, counts = counts}
  where
    baseLs = splitText p.base
    headLs = splitText p.head
    n = V.length baseLs.texts

-- | The lines of a text, as the diff cuts them. See Note [Line diff].
diffLines :: Text -> [Text]
diffLines t = V.toList (splitText t).texts

-- * Lines

-- | Whether a text starts with a BOM.
data Bom = HasBom | NoBom
  deriving (Eq)

-- | Whether the last line ends with a newline.
data Ending = Terminated | Unterminated
  deriving (Eq, Ord)

-- | A text cut into lines.
data Lines = Lines
  { texts :: V.Vector Text,
    -- | Whether a \r was dropped at the end of each line.
    crs :: VU.Vector Bool,
    ending :: Ending,
    bom :: Bom
  }

-- | Cut a text into lines (D-276). No Text literals here:
-- Q-RTS-BLACKHOLE.
splitText :: Text -> Lines
splitText t0 =
  Lines
    { texts = V.fromListN (length cut) (map fst cut),
      crs = VU.fromListN (length cut) (map snd cut),
      ending = ending,
      bom = bom
    }
  where
    (bom, t) = case Text.uncons t0 of
      Just ('\xFEFF', rest) -> (HasBom, rest)
      _ -> (NoBom, t0)
    ending = case Text.unsnoc t of
      Just (_, '\n') -> Terminated
      _ -> Unterminated
    pieces = Text.split (== '\n') t
    -- After a final \n (or in ""), the last piece is empty and no line.
    whole
      | Text.null t = []
      | ending == Terminated = init pieces
      | otherwise = pieces
    cut = map dropCr whole
    dropCr l = case Text.unsnoc l of
      Just (rest, '\r') -> (rest, True)
      _ -> (l, False)

-- | git's whitespace for -w (its @isspace@ is not C's: no VT or FF).
gitSpace :: Char -> Bool
gitSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | What makes two lines equal. See Note [Line diff].
data Key = Key !Text !Ending
  deriving (Eq, Ord)

keys :: Whitespace -> Lines -> V.Vector Key
keys ws ls = V.imap key ls.texts
  where
    lastLine = V.length ls.texts - 1
    key i t = case ws of
      KeepWhitespace -> Key t (if i == lastLine then ls.ending else Terminated)
      IgnoreWhitespace -> Key (Text.filter (not . gitSpace) t) Terminated

-- | A class per distinct key, in order of first appearance in base
-- then head (as git). Keys go by FNV-1a hash, with a list for the rare
-- collisions: classes never depend on the hash, only on key equality.
classify :: V.Vector Key -> V.Vector Key -> Classified
classify k1 k2 = runST $ do
  out1 <- VUM.new (V.length k1)
  out2 <- VUM.new (V.length k2)
  let step out (Seen m next) i key =
        let h = keyHash key
            same = IntMap.findWithDefault [] h m
         in case lookup key same of
              Just c -> VUM.write out i c >> pure (Seen m next)
              Nothing -> do
                VUM.write out i next
                pure (Seen (IntMap.insert h ((key, next) : same) m) (next + 1))
  s1 <- V.ifoldM' (step out1) (Seen IntMap.empty 0) k1
  Seen _ count <- V.ifoldM' (step out2) s1 k2
  c1 <- VU.unsafeFreeze out1
  c2 <- VU.unsafeFreeze out2
  pure Classified {classes = Pair {head = c2, base = c1}, count = count}

-- | Each line's class, and how many classes there are.
data Classified = Classified
  { classes :: Pair (VU.Vector Int),
    -- | Classes are @0 .. count - 1@.
    count :: Int
  }

-- | Keys seen so far, by hash, and the next class.
data Seen = Seen !(IntMap.IntMap [(Key, Int)]) !Int

-- | FNV-1a, 64 bits, over the key's UTF-8 bytes, then its ending.
keyHash :: Key -> Int
keyHash (Key (Text.Internal.Text arr off len) end) =
  fromIntegral (mix (go off 14695981039346656037) (if end == Terminated then 0 else 1))
  where
    go :: Int -> Word64 -> Word64
    go !i !h
      | i >= off + len = h
      -- Unchecked: i is in [off, off + len), the Text's own bytes.
      | otherwise = go (i + 1) (mix h (fromIntegral (Text.Array.unsafeIndex arr i)))
    mix h b = (h `xor` b) * 1099511628211

-- * Results

-- | Runs from the change flags (git's @xdl_build_script@, walked
-- forward): an unchanged pair is Equal; at a change, base's changed
-- lines are a Delete and head's an Insert.
buildRuns :: Int -> Int -> Changed s -> Changed s -> ST s [Run]
buildRuns n1 n2 ch1 ch2 = go 0 0 []
  where
    go i1 i2 acc
      | i1 >= n1 && i2 >= n2 = pure (reverse acc)
      | otherwise = do
          a <- isChanged ch1 i1
          b <- isChanged ch2 i2
          if a || b
            then do
              d <- runLength ch1 i1
              s <- runLength ch2 i2
              go (i1 + d) (i2 + s) (push Insert s (push Delete d acc))
            else go (i1 + 1) (i2 + 1) (push Equal 1 acc)
    runLength ch i = do
      c <- isChanged ch i
      if c then (+ 1) <$> runLength ch (i + 1) else pure 0
    push _ 0 acc = acc
    push o k (Run o' k' : acc) | o == o' = let !k2 = k + k' in Run o k2 : acc
    push o k acc = Run o k : acc

-- | Equal pairs where only one side dropped a \r.
crlfCount :: VU.Vector Bool -> VU.Vector Bool -> [Run] -> Int
crlfCount cr1 cr2 = go 0 0 0
  where
    go !acc _ _ [] = acc
    go !acc i1 i2 (r : rs) = case r.op of
      Equal ->
        let k = r.count
            differs j = cr1 VU.! (i1 + j) /= cr2 VU.! (i2 + j)
            diffs = length (filter differs [0 .. k - 1])
         in go (acc + diffs) (i1 + k) (i2 + k) rs
      Delete -> go acc (i1 + r.count) i2 rs
      Insert -> go acc i1 (i2 + r.count) rs

bomChange :: Bom -> Bom -> Maybe BomChange
bomChange NoBom HasBom = Just BomAdded
bomChange HasBom NoBom = Just BomRemoved
bomChange _ _ = Nothing

-- * Warm-up

-- | Run the paths once, both modes: the fallback with discards, the
-- Myers cost limit, slides, merges, alignment, the indent heuristic,
-- git 2.54's re-diff, CRLF, BOM, final newline (the test "the
-- warm-up takes its paths" checks the counters). Call before diffs run
-- in parallel (Q-RTS-BLACKHOLE). Not the Myers heuristic split: it
-- needs a fallback region of over 65,000 lines (about 0.1 s), and it
-- builds no shared value the other paths don't.
warmUpLineDiff :: IO ()
warmUpLineDiff =
  forM_ [KeepWhitespace, IgnoreWhitespace] $ \ws ->
    forM_ (warmUpPairs ()) $ \p -> do
      s <- evaluate (lineDiffStats ws p)
      _ <- evaluate (sum (map (.count) s.diff.runs) + s.diff.crlfLines)
      evaluate s.counts

-- | The pairs 'warmUpLineDiff' diffs. GHC may still float the literals
-- out as shared values; that's harmless, as the warm-up runs on one
-- thread first.
warmUpPairs :: () -> [Pair Text]
warmUpPairs () =
  [ pair "a\nb\nc\n" "a\nx\nc\n",
    pair "a\r\nb\r\n" "\xFEFF\&a\nb",
    -- Slides and the indent heuristic, which moves the group.
    pair "\n\n\n\n}\n" "\n\n\n\n}\n\n\n}\n",
    -- git 2.54's re-diff of a merged group.
    pair "  x = 1\n\n}\n-- | Docs.\n\n" "\n}\n  x = 1\n\n-- | Docs.\n}\n\n\n",
    -- A fallback with discards.
    pair
      (Text.unlines (concat (replicate 70 ["x", "}", "u1"])))
      (Text.unlines (concat (replicate 70 ["}", "x", "u2"]))),
    -- Two unrelated texts of 8 letters: a fallback that hits the cost
    -- limit, taking the forward and the backward path.
    pair (noise 7 1500) (noise 8 1500)
  ]
  where
    pair b h = Pair {head = h, base = b}
    -- Lines from an 8-letter alphabet, from a small LCG.
    noise :: Int -> Int -> Text
    noise seed n =
      Text.unlines
        [ Text.singleton (toEnum (fromEnum 'a' + (v `shiftR` 16) `mod` 8))
        | v <- take n (iterate step (seed `xor` 4242))
        ]
    step x = (x * 1103515245 + 12345) `mod` 2147483648
