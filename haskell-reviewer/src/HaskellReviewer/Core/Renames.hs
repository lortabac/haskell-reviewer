{-# LANGUAGE StrictData #-}

{- File pairing for diff mode: a port of the rename detection in git's
diffcore-rename.c (find_exact_renames, find_identical_files,
find_basename_matches, record_if_better, score_compare,
too_many_rename_candidates, find_renames and the set-up of
diffcore_rename_extended), git 2.44.1, as @git diff -M@ runs it (no
copies, no -B, no merge-ort extras). Nothing in it changed in behaviour
up to git 2.56.0. The default limit is diff.c's
diff_rename_limit_default.

Copyright (C) 2005 Junio C Hamano
Haskell port: Copyright (C) 2026 Lorenzo Tabacchini

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; version 2 of the License only.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <http://www.gnu.org/licenses/>.

SPDX-License-Identifier: GPL-2.0-only
-}

-- | Pair the files of base and head: unchanged, modified, added,
-- removed, renamed (SPEC §11.4.7, D-324). Pure. See Note [Renames].
module HaskellReviewer.Core.Renames
  ( -- * Input
    DiffEntry (..),
    Blob (..),
    Settings (..),
    RenameLimit (..),
    defaultSettings,

    -- * Output
    Pairing (..),
    Change (..),
    ModifiedFile (..),
    RenamedFile (..),
    ChangeNote (..),
    ExecChange (..),
    RenameStep (..),
    Limit (..),
    changePath,
    forcePairing,

    -- * Pairing
    Step (..),
    Needed (..),
    unknownBlobs,
    startPairing,
    runPairing,
    pairFiles,

    -- * Warm-up
    warmUpRenames,

    -- * For tests
    gitTableReversed,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.Functor.Identity (runIdentity)
import Data.List (find, sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as V
import HaskellReviewer.Core.BlobId (blobId)
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import HaskellReviewer.Core.RelPath
  ( RelPath,
    relPathFromSegments,
    relPathName,
    relPathText,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity
  ( Score (..),
    Spans (..),
    basenameScore,
    estimateSimilarity,
    hashChars,
    maxScore,
    mergeSteps,
    passesSizeTest,
    renameScore,
  )
import HaskellReviewer.Core.WorkTree (Exec (..))

{- Note [Renames]

D-324. Paths are ROOT-relative; 'RelPath' order is git's (UTF-8 byte)
order, and every step walks in it, so the result is deterministic.

1. Same path on both sides: unchanged if kind, object and exec bit are
   the same, else modified, with 'TypeChanged' (file, symlink,
   submodule) or 'ModeChanged' (the exec bit; the contents may change
   too). An unknown blob counts as modified. Never a rename (git: T or
   M).
2. Sources: paths on base only. Destinations: paths on head only.
   Empty files take part (git's rename_empty).
3. Exact: for each destination in order, the unused sources with the
   same object. Two regular files may differ in the exec bit; other
   kinds must match. Of the first 100 such sources, the first with
   the same basename, else the first. Score 100%.
   git keeps the sources in a hash table, and the candidates come in
   path order, except when the table grows on its last insert (205,
   3,277, 52,429 sources, ...): then in reverse. 'gitTableReversed'
   models that.
4. Basename step: a basename (after the last @/@) found once among the
   sources left and once among the destinations left: the pair is a
   rename if its score is at least 75% ('basenameScore', also the size
   test's minimum). It runs even past the limit, as git does (2.44.1,
   our minimum, and later).
5. Limit: sources × destinations left > L² skips the matrix
   ('OverRenameLimit'). L is 1000 (git's diff.renameLimit default),
   not read from the repo.
6. Matrix: each destination left keeps its best 4 sources (git's
   record_if_better, with score_compare: higher score, then same
   basename, empty slots last). All slots, in destination then slot
   order, are sorted stably by the same order and taken greedily while
   the score is at least 50%, skipping used ends.
7. Destinations left are added, sources left removed.

Only regular files get a score: symlinks and submodules pair exactly
or not at all. Ours, not git's (D-324): files over 'sizeCap' get no
score (nothing else changes); contents come in rounds ('Step'); a work
budget can skip the matrix ('OverWorkBudget'), as the limit does: the
bytes read over all rounds ('readBudget', checked before the matrix
round on the known sizes and again once its contents are in) and the
span-merge steps of the matrix ('mergeBudget'). The basename step is
never skipped: it has at most one pair per source.
-}

-- * Input

-- | One path on one side.
data DiffEntry
  = File Exec Blob
  | Symlink Blob
  | -- | With its commit.
    Submodule ObjectId
  deriving (Eq, Show)

-- | A file's or symlink's blob.
data Blob = Blob
  { -- | 'Nothing': not hashed. It never pairs exactly.
    oid :: Maybe ObjectId,
    -- | Bytes, for the size test. 'Nothing': not known; see 'Needed'.
    size :: Maybe Int
  }
  deriving (Eq, Show)

-- | Limits.
data Settings = Settings
  { renameLimit :: RenameLimit,
    -- | Files over this many bytes get no score.
    sizeCap :: Int,
    -- | Bytes of contents read over all rounds before the matrix is
    -- skipped.
    readBudget :: Int,
    -- | Span-merge steps for the matrix.
    mergeBudget :: Int
  }
  deriving (Eq, Show)

-- | git's rename limit L: no matrix when sources × destinations left
-- are more than L². git reads 0 or less as no limit.
data RenameLimit = NoRenameLimit | RenameLimit Int
  deriving (Eq, Show)

-- | L = 1000, 64 MiB, 256 MiB read, 200 million merge steps (D-324
-- has the numbers and what they cost).
defaultSettings :: Settings
defaultSettings =
  Settings
    { renameLimit = RenameLimit 1000,
      sizeCap = 64 * 1024 * 1024,
      readBudget = 256 * 1024 * 1024,
      mergeBudget = 200_000_000
    }

-- * Output

-- | The changed paths. Unchanged ones are left out.
data Pairing = Pairing
  { -- | By 'changePath'.
    changes :: [Change],
    limit :: Limit
  }
  deriving (Eq, Show)

-- | One changed path or pair of paths.
data Change
  = Added RelPath
  | Removed RelPath
  | Modified ModifiedFile
  | Renamed RenamedFile
  deriving (Eq, Show)

-- | A path on both sides that changed.
data ModifiedFile = ModifiedFile
  { path :: RelPath,
    note :: Maybe ChangeNote
  }
  deriving (Eq, Show)

-- | A base path paired with a head path.
data RenamedFile = RenamedFile
  { from :: RelPath,
    to :: RelPath,
    score :: Score,
    step :: RenameStep,
    exec :: ExecChange
  }
  deriving (Eq, Show)

-- | Besides the contents.
data ChangeNote
  = -- | File, symlink, submodule: git's T.
    TypeChanged
  | -- | The exec bit changed (the contents may too).
    ModeChanged
  deriving (Eq, Show)

-- | Whether a renamed file's exec bit changed. Only regular files can
-- differ in it: other kinds pair with their own kind only.
data ExecChange = SameExec | ExecChanged
  deriving (Eq, Show)

-- | Which step found a rename.
data RenameStep = ExactStep | BasenameStep | MatrixStep
  deriving (Eq, Show)

-- | Whether the matrix ran. On the wire, renameLimitHit.
data Limit
  = WithinLimits
  | OverRenameLimit
  | OverWorkBudget
  deriving (Eq, Show)

-- | The head path, or the base path for 'Removed'.
changePath :: Change -> RelPath
changePath = \case
  Added p -> p
  Removed p -> p
  Modified m -> m.path
  Renamed r -> r.to

-- | Force a pairing fully: it is shared by request threads
-- (Q-RTS-BLACKHOLE).
forcePairing :: Pairing -> ()
forcePairing p = foldl' (\() c -> forceChange c) () p.changes `seq` p.limit `seq` ()
  where
    forceChange = \case
      Added a -> forcePath a
      Removed a -> forcePath a
      Modified m -> forcePath m.path `seq` m.note `seq` ()
      Renamed r ->
        forcePath r.from `seq`
          forcePath r.to `seq`
            r.score `seq`
              r.step `seq`
                r.exec `seq`
                  ()
    forcePath a = Text.length (relPathText a) `seq` ()

-- * Pairing

-- | The contents a round needs, by side (head: destinations, base:
-- sources), as 'HaskellReviewer.Core.Similarity.hashChars' of git's clean bytes. A path
-- left out of the answer (unreadable, too big) gets no score; contents
-- over 'sizeCap' are ignored.
--
-- A file without a known size is asked for whenever a size test needs
-- it, and its bytes count against 'readBudget' only once they are in.
-- So give sizes where you can: then the budget stops before the read.
data Needed = Needed
  { files :: Pair [RelPath],
    -- | The known sizes, summed.
    bytes :: Int
  }
  deriving (Eq, Show)

-- | A pairing in progress: done, or waiting for contents. At most two
-- rounds (the basename step, the matrix).
data Step
  = Done Pairing
  | NeedContent Needed (Pair (Map RelPath Spans) -> Step)

-- | Files or symlinks whose blob is not known but matters: on both
-- sides, or a source or destination while the other set isn't empty.
-- Hashing them first lets them pair exactly.
unknownBlobs :: Pair (Map RelPath DiffEntry) -> Pair [RelPath]
unknownBlobs entries =
  Pair {head = pick entries.head entries.base, base = pick entries.base entries.head}
  where
    pick these others =
      let onlyOther = not (Map.null (Map.difference others these))
       in [ p
          | (p, e) <- Map.toAscList these,
            unknown e,
            Map.member p others || onlyOther
          ]
    unknown = \case
      File _ b -> isNothing b.oid
      Symlink b -> isNothing b.oid
      Submodule _ -> False

-- | Run a pairing, reading contents with the given action.
runPairing :: (Monad m) => (Needed -> m (Pair (Map RelPath Spans))) -> Step -> m Pairing
runPairing load = \case
  Done p -> pure p
  NeedContent n k -> load n >>= runPairing load . k

-- | A pairing with every content known up front (tests, warm-up).
pairFiles ::
  Settings -> Pair (Map RelPath DiffEntry) -> Pair (Map RelPath Spans) -> Pairing
pairFiles settings entries contents =
  runIdentity (runPairing (pure . answer) (startPairing settings entries))
  where
    answer n =
      Pair
        { head = Map.restrictKeys contents.head (Set.fromList n.files.head),
          base = Map.restrictKeys contents.base (Set.fromList n.files.base)
        }

-- | Start a pairing. See Note [Renames].
startPairing :: Settings -> Pair (Map RelPath DiffEntry) -> Step
startPairing settings entries =
  basenameRound (exactStep st0)
  where
    st0 =
      St
        { settings = settings,
          samePath =
            mapMaybe
              (uncurry samePathChange)
              (Map.toAscList (Map.intersectionWith (,) entries.base entries.head)),
          srcs = files (Map.difference entries.base entries.head),
          dsts = files (Map.difference entries.head entries.base),
          usedSrcs = Set.empty,
          matched = Map.empty,
          content = Pair Map.empty Map.empty,
          asked = Pair Set.empty Set.empty,
          bytesRead = 0
        }
    files m =
      V.fromList
        [Path {path = p, entry = e, name = relPathName p} | (p, e) <- Map.toAscList m]

-- * Internal

-- | A source: an index into 'St.srcs'.
newtype SrcIx = SrcIx Int
  deriving (Eq, Ord)

-- | A destination: an index into 'St.dsts'.
newtype DstIx = DstIx Int
  deriving (Eq, Ord)

-- | One path of a side, for the steps.
data Path = Path
  { path :: RelPath,
    entry :: DiffEntry,
    name :: Text
  }

-- | A destination's rename.
data Match = Match
  { src :: SrcIx,
    score :: Score,
    step :: RenameStep
  }

-- | The pairing so far.
data St = St
  { settings :: Settings,
    samePath :: [Change],
    -- | Base only, in path order.
    srcs :: V.Vector Path,
    -- | Head only, in path order.
    dsts :: V.Vector Path,
    usedSrcs :: Set SrcIx,
    matched :: Map DstIx Match,
    -- | Destinations in head, sources in base.
    content :: Pair (Map RelPath Spans),
    asked :: Pair (Set RelPath),
    -- | Known sizes asked for, plus the real size of contents that came
    -- without one.
    bytesRead :: Int
  }

srcAt :: St -> SrcIx -> Path
srcAt s (SrcIx i) = s.srcs V.! i

dstAt :: St -> DstIx -> Path
dstAt s (DstIx j) = s.dsts V.! j

allSrcs :: St -> [SrcIx]
allSrcs s = map SrcIx [0 .. V.length s.srcs - 1]

allDsts :: St -> [DstIx]
allDsts s = map DstIx [0 .. V.length s.dsts - 1]

-- | Step 1.
samePathChange :: RelPath -> (DiffEntry, DiffEntry) -> Maybe Change
samePathChange p (b, h)
  | kindTag b /= kindTag h = modified (Just TypeChanged)
  | execOf b /= execOf h = modified (Just ModeChanged)
  | isJust (objectKey b) && objectKey b == objectKey h = Nothing
  | otherwise = modified Nothing
  where
    modified n = Just (Modified ModifiedFile {path = p, note = n})

data KindTag = TagFile | TagSymlink | TagSubmodule
  deriving (Eq)

kindTag :: DiffEntry -> KindTag
kindTag = \case
  File _ _ -> TagFile
  Symlink _ -> TagSymlink
  Submodule _ -> TagSubmodule

execOf :: DiffEntry -> Maybe Exec
execOf = \case
  File x _ -> Just x
  _ -> Nothing

-- | What exact matching compares: the blob, or a submodule's commit.
objectKey :: DiffEntry -> Maybe ObjectId
objectKey = \case
  File _ b -> b.oid
  Symlink b -> b.oid
  Submodule c -> Just c

-- | Whether git's hashmap of @n@ sources grows on its last insert,
-- which turns its chains (and so the order of exact candidates)
-- around. hashmap_init sizes the table for @n@ (64, then times 4 up to
-- @n * 100 / 80@); hashmap_add grows it past 80% full.
gitTableReversed :: Int -> Bool
gitTableReversed n = n > size * 80 `div` 100
  where
    initial = n * 100 `div` 80
    size = until (>= initial) (* 4) (64 :: Int)

-- | Step 3: git's find_exact_renames.
exactStep :: St -> St
exactStep st = foldl' one st (allDsts st)
  where
    order = if gitTableReversed (V.length st.srcs) then reverse else id
    byKey :: Map ObjectId [SrcIx]
    byKey =
      Map.map
        order
        ( Map.fromListWith
            (flip (<>))
            [(k, [i]) | i <- allSrcs st, Just k <- [objectKey (srcAt st i).entry]]
        )
    one s j = case objectKey d.entry of
      Nothing -> s
      Just k ->
        let cands =
              take
                100
                [ i
                | i <- Map.findWithDefault [] k byKey,
                  Set.notMember i s.usedSrcs,
                  compatible (srcAt st i).entry d.entry
                ]
            best = find (\i -> (srcAt st i).name == d.name) cands
         in case maybe (listToMaybe cands) Just best of
              Nothing -> s
              Just i -> record j Match {src = i, score = maxScore, step = ExactStep} s
      where
        d = dstAt st j
    compatible a b = case (a, b) of
      (File _ _, File _ _) -> True
      _ -> kindTag a == kindTag b

record :: DstIx -> Match -> St -> St
record j m s =
  s {usedSrcs = Set.insert m.src s.usedSrcs, matched = Map.insert j m s.matched}

-- | Sources and destinations not paired yet.
leftSrcs :: St -> [SrcIx]
leftSrcs s = [i | i <- allSrcs s, Set.notMember i s.usedSrcs]

leftDsts :: St -> [DstIx]
leftDsts s = [j | j <- allDsts s, Map.notMember j s.matched]

-- | Whether a file can get a score, and its size if known.
data Scorable = NotScorable | Scorable (Maybe Int)

scorable :: Settings -> DiffEntry -> Scorable
scorable settings = \case
  File _ b | maybe True (<= settings.sizeCap) b.size -> Scorable b.size
  _ -> NotScorable

-- | Whether a pair may pass the size test, from the entries' sizes.
mayPass :: Settings -> Score -> Path -> Path -> Bool
mayPass settings m a b = case (scorable settings a.entry, scorable settings b.entry) of
  (Scorable (Just x), Scorable (Just y)) -> passesSizeTest m x y
  (Scorable _, Scorable _) -> True
  _ -> False

-- | 'mayPass' for a source and a destination.
passes :: Score -> St -> SrcIx -> DstIx -> Bool
passes m s i j = mayPass s.settings m (srcAt s i) (dstAt s j)

-- | Whether a round may be stopped by the read budget.
data BudgetCheck = CheckBudget | NoBudgetCheck

-- | Ask for the contents of these files, then go on. Contents already
-- asked for are not asked again.
ask :: BudgetCheck -> St -> [SrcIx] -> [DstIx] -> (St -> Step) -> Step
ask check s ss ds next
  | null srcNew && null dstNew = next s
  | CheckBudget <- check,
    s.bytesRead + known > s.settings.readBudget =
      Done (finish s OverWorkBudget)
  | otherwise =
      NeedContent
        Needed
          { files = Pair {head = Map.keys dstNew, base = Map.keys srcNew},
            bytes = known
          }
        ( \got ->
            let gotHead = Map.restrictKeys got.head (Map.keysSet dstNew)
                gotBase = Map.restrictKeys got.base (Map.keysSet srcNew)
             in next
                  s
                    { content =
                        Pair
                          { head = Map.union s.content.head gotHead,
                            base = Map.union s.content.base gotBase
                          },
                      asked =
                        Pair
                          { head = Set.union s.asked.head (Map.keysSet dstNew),
                            base = Set.union s.asked.base (Map.keysSet srcNew)
                          },
                      bytesRead =
                        s.bytesRead
                          + known
                          + unsized dstNew gotHead
                          + unsized srcNew gotBase
                    }
        )
  where
    srcNew = newFiles s.asked.base (map (srcAt s) ss)
    dstNew = newFiles s.asked.head (map (dstAt s) ds)
    known = sum (Map.mapMaybe id srcNew) + sum (Map.mapMaybe id dstNew)
    newFiles seen fs =
      Map.fromList [(f.path, sizeOf f.entry) | f <- fs, Set.notMember f.path seen]
    sizeOf = \case
      File _ b -> b.size
      _ -> Nothing
    -- The real bytes of contents that came without a known size.
    unsized asks got =
      sum [sp.size | (p, sp) <- Map.toList got, Map.lookup p asks == Just Nothing]

-- | A pair's score, if both have contents.
scoreOf :: St -> Score -> SrcIx -> DstIx -> Score
scoreOf s m i j =
  case (spansOf s.content.base (srcAt s i), spansOf s.content.head (dstAt s j)) of
    (Just a, Just b) -> estimateSimilarity m a b
    _ -> Score 0
  where
    spansOf c f = case scorable s.settings f.entry of
      NotScorable -> Nothing
      Scorable _ -> case Map.lookup f.path c of
        Just sp | sp.size <= s.settings.sizeCap -> Just sp
        _ -> Nothing

-- | Step 4: git's find_basename_matches. Not budgeted: at most one pair
-- per source.
basenameRound :: St -> Step
basenameRound s =
  ask NoBudgetCheck s (map fst wanted) (map snd wanted) (matrixRound . basenameStep cands)
  where
    srcCount = Map.fromListWith (+) [((srcAt s i).name, 1 :: Int) | i <- leftSrcs s]
    -- Nothing: the name is there more than once.
    dstByName =
      Map.fromListWith (\_ _ -> Nothing) [((dstAt s j).name, Just j) | j <- leftDsts s]
    cands =
      [ (i, j)
      | i <- leftSrcs s,
        let n = (srcAt s i).name,
        Map.lookup n srcCount == Just 1,
        Just (Just j) <- [Map.lookup n dstByName]
      ]
    wanted = [(i, j) | (i, j) <- cands, passes basenameScore s i j]

basenameStep :: [(SrcIx, DstIx)] -> St -> St
basenameStep cands s0 = foldl' one s0 cands
  where
    one s (i, j)
      | sc >= basenameScore = record j Match {src = i, score = sc, step = BasenameStep} s
      | otherwise = s
      where
        sc = scoreOf s basenameScore i j

-- | Steps 5 and 6: git's too_many_rename_candidates, then the matrix.
matrixRound :: St -> Step
matrixRound s
  | null ss || null ds = Done (finish s WithinLimits)
  | RenameLimit lim <- s.settings.renameLimit,
    lim > 0,
    length ss * length ds > lim * lim =
      Done (finish s OverRenameLimit)
  | otherwise = ask CheckBudget s wantedSrcs wantedDsts matrix
  where
    ss = leftSrcs s
    ds = leftDsts s
    -- A file is read if it passes the size test with any partner.
    wantedSrcs = [i | i <- ss, any (passes renameScore s i) ds]
    wantedDsts = [j | j <- ds, any (\i -> passes renameScore s i j) ss]
    matrix s'
      | s'.bytesRead > s'.settings.readBudget = Done (finish s' OverWorkBudget)
      | steps > s'.settings.mergeBudget = Done (finish s' OverWorkBudget)
      | otherwise = Done (finish (matrixStep ss ds s') WithinLimits)
      where
        steps = sum [mergeCost s' i j | j <- ds, i <- ss]

-- | What scoring a pair costs, for the budget: 0 if the size test or
-- missing contents stop it first.
mergeCost :: St -> SrcIx -> DstIx -> Int
mergeCost s i j =
  case (look s.content.base (srcAt s i), look s.content.head (dstAt s j)) of
    (Just a, Just b) | passesSizeTest renameScore a.size b.size -> mergeSteps a b
    _ -> 0
  where
    look c f = Map.lookup f.path c

-- | One slot of a destination: git's struct diff_score.
data Cand = Cand
  { score :: Score,
    sameName :: Bool,
    src :: SrcIx,
    dst :: DstIx
  }

-- | git's score_compare: 'LT' sorts first. Empty slots last.
compareSlots :: Maybe Cand -> Maybe Cand -> Ordering
compareSlots = \cases
  Nothing Nothing -> EQ
  Nothing _ -> GT
  _ Nothing -> LT
  (Just a) (Just b) -> comparing key a b
  where
    key c = (Down c.score, Down c.sameName)

-- | git's record_if_better: replace the first worst slot if the
-- candidate is strictly better.
recordIfBetter :: [Maybe Cand] -> Cand -> [Maybe Cand]
recordIfBetter slots c
  | compareSlots (slots !! worst) (Just c) == GT =
      [if k == worst then Just c else x | (k, x) <- zip [0 ..] slots]
  | otherwise = slots
  where
    worst =
      foldl'
        (\w k -> if compareSlots (slots !! k) (slots !! w) == GT then k else w)
        0
        [1 .. length slots - 1]

matrixStep :: [SrcIx] -> [DstIx] -> St -> St
matrixStep ss ds s0 = foldl' take1 s0 sorted
  where
    slotsOf j =
      foldl'
        ( \slots i ->
            recordIfBetter
              slots
              Cand
                { score = scoreOf s0 renameScore i j,
                  sameName = (srcAt s0 i).name == (dstAt s0 j).name,
                  src = i,
                  dst = j
                }
        )
        (replicate 4 Nothing)
        ss
    sorted = takeWhile usable (sortBy compareSlots (concatMap slotsOf ds))
    usable = maybe False (\c -> c.score >= renameScore)
    take1 s = \case
      Just c
        | Map.notMember c.dst s.matched,
          Set.notMember c.src s.usedSrcs ->
            record c.dst Match {src = c.src, score = c.score, step = MatrixStep} s
      _ -> s

-- | Step 7, and the output.
finish :: St -> Limit -> Pairing
finish s lim =
  Pairing
    { changes = sortOn changePath (s.samePath <> dstChanges <> removed),
      limit = lim
    }
  where
    dstChanges =
      [ case Map.lookup j s.matched of
          Nothing -> Added d.path
          Just m ->
            let f = srcAt s m.src
             in Renamed
                  RenamedFile
                    { from = f.path,
                      to = d.path,
                      score = m.score,
                      step = m.step,
                      exec = execChange f.entry d.entry
                    }
      | j <- allDsts s,
        let d = dstAt s j
      ]
    removed = [Removed (srcAt s i).path | i <- leftSrcs s]
    execChange a b = case (a, b) of
      (File x _, File y _) | x /= y -> ExecChanged
      _ -> SameExec

-- * Warm-up

-- | Run a small pairing through every step, on one thread, before
-- request threads share the code (Q-RTS-BLACKHOLE). The test main
-- calls it too.
warmUpRenames :: IO ()
warmUpRenames = void (evaluate (forcePairing (warmUpPairing ())))

-- | Exact, basename and matrix renames, a binary file, CRLF, a type
-- change and a mode change. The @()@ keeps the pairing from being one
-- shared top-level value.
warmUpPairing :: () -> Pairing
warmUpPairing () =
  pairFiles defaultSettings {renameLimit = RenameLimit 10} entries contents
  where
    path t =
      fromMaybe
        (error "warmUpRenames: bad path")
        (relPathFromSegments (Text.splitOn "/" t))
    text = Text.Encoding.encodeUtf8 . Text.unlines
    body k = ["line " <> Text.pack (show (n :: Int)) | n <- [1 .. 20 + k]]
    base =
      [ ("e/a.txt", text (body 0)),
        ("d1/Same.hs", text (body 1)),
        ("m/old.txt", text (map (<> "\r") (body 2))),
        ("bin/x.bin", "\0\1\2" <> text (body 3)),
        ("t.txt", text (body 4)),
        ("k", text (body 5))
      ]
    headSide =
      [ ("e/b.txt", text (body 0)),
        ("d2/Same.hs", text (body 1 <> ["x"])),
        ("m/new.txt", text (body 2 <> ["y", "z"])),
        ("bin/y.bin", "\0\1\2" <> text (body 3 <> ["w"])),
        ("t.txt", text (body 4)),
        ("k", "e/b.txt")
      ]
    side kindOf xs =
      Map.fromList
        [ (path p, kindOf p Blob {oid = Just (blobId Sha1 b), size = Just (BS.length b)})
        | (p, b) <- xs
        ]
    headKind = \case
      "t.txt" -> File Executable
      "k" -> Symlink
      _ -> File NotExecutable
    entries =
      Pair
        { head = side headKind headSide,
          base = side (const (File NotExecutable)) base
        }
    spansOfSide xs = Map.fromList [(path p, hashChars b) | (p, b) <- xs]
    contents = Pair {head = spansOfSide headSide, base = spansOfSide base}
