-- | A text dump of a line diff, for golden tests: hunks as @git diff@
-- prints them (3 context lines, no function name), then a notes line.
module LineDiffDump (dumpLineDiff) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.LineDiff
  ( BomChange (..),
    LineDiff (..),
    Op (..),
    Run (..),
    Whitespace (..),
    diffLines,
    lineDiff,
  )
import HaskellReviewer.Core.Sides (Pair (..))

-- | Lines of context around a change.
context :: Int
context = 3

-- | One change: where it starts on each side, and how many lines.
data Change = Change
  { baseAt :: Int,
    baseN :: Int,
    headAt :: Int,
    headN :: Int
  }

-- | The hunks, then @notes: ...@.
dumpLineDiff :: Whitespace -> Pair Text -> Text
dumpLineDiff ws p =
  Text.unlines (concatMap hunk (groups (changes d.runs)) <> [notes d])
  where
    d = lineDiff ws p
    baseLs = diffLines p.base
    headLs = diffLines p.head
    nBase = length baseLs
    nHead = length headLs
    baseOpen = unterminated p.base
    headOpen = unterminated p.head
    -- Marker after the last line of a side without a final newline.
    line sign open total i l =
      [sign <> l] <> ["\\ No newline at end of file" | open, i == total - 1]
    hunk cs =
      let first = NE.head cs
          lst = NE.last cs
          s = max 0 (first.baseAt - context)
          hs = first.headAt - (first.baseAt - s)
          e = min nBase (lst.baseAt + lst.baseN + context)
          he = lst.headAt + lst.headN + (e - (lst.baseAt + lst.baseN))
       in [ "@@ -" <> range s (e - s) <> " +" <> range hs (he - hs) <> " @@"
          ]
            <> body (NE.toList cs) s hs e
    body [] b h e = [l | i <- [h .. h + (e - b) - 1], l <- ctxLine i]
    body (c : cs) b h e =
      [l | i <- [h .. h + (c.baseAt - b) - 1], l <- ctxLine i]
        <> [ l
           | i <- [c.baseAt .. c.baseAt + c.baseN - 1],
             l <- line "-" baseOpen nBase i (baseLs !! i)
           ]
        <> [ l
           | i <- [c.headAt .. c.headAt + c.headN - 1],
             l <- line "+" headOpen nHead i (headLs !! i)
           ]
        <> body cs (c.baseAt + c.baseN) (c.headAt + c.headN) e
    ctxLine i = line " " headOpen nHead i (headLs !! i)

-- | git's range: the line before for an empty range, no count for 1.
range :: Int -> Int -> Text
range start n = case n of
  0 -> num start <> ",0"
  1 -> num (start + 1)
  _ -> num (start + 1) <> "," <> num n
  where
    num = Text.pack . show

-- | A non-empty text with no final newline.
unterminated :: Text -> Bool
unterminated t = not (Text.null t) && not ("\n" `Text.isSuffixOf` t)

-- | The changes in the runs: a Delete, an Insert, or a Delete with the
-- Insert after it.
changes :: [Run] -> [Change]
changes = go 0 0
  where
    go _ _ [] = []
    go b h (r : rs) = case r.op of
      Equal -> go (b + r.count) (h + r.count) rs
      Delete -> case rs of
        Run Insert k : rs' ->
          Change b r.count h k : go (b + r.count) (h + k) rs'
        _ -> Change b r.count h 0 : go (b + r.count) h rs
      Insert -> Change b 0 h r.count : go b (h + r.count) rs

-- | Changes that share a hunk: git merges them when at most
-- @2 * context@ lines lie between.
groups :: [Change] -> [NonEmpty Change]
groups [] = []
groups (c : cs) = go (c :| []) cs
  where
    go acc [] = [NE.reverse acc]
    go acc@(prev :| _) (x : xs)
      | x.baseAt - (prev.baseAt + prev.baseN) <= 2 * context =
          go (NE.cons x acc) xs
      | otherwise = NE.reverse acc : go (x :| []) xs

-- | The notes line: what is not in the runs.
notes :: LineDiff -> Text
notes d =
  "notes: " <> case catMaybes [crlf, bom] of
    [] -> "none"
    ns -> Text.intercalate ", " ns
  where
    crlf
      | d.crlfLines > 0 =
          Just (Text.pack (show d.crlfLines) <> " lines with line ending changes")
      | otherwise = Nothing
    bom = case d.bom of
      Just BomAdded -> Just "BOM added"
      Just BomRemoved -> Just "BOM removed"
      Nothing -> Nothing
