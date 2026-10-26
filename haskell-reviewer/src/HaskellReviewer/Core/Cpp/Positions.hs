{-# LANGUAGE StrictData #-}

-- | Where the text after CPP comes from. See Note [Position map].
module HaskellReviewer.Core.Cpp.Positions
  ( -- * Types
    Pos (..),
    PositionMap (..),
    Piece (..),
    Run (..),

    -- * Building
    sameLines,
    insertLine,
    follows,

    -- * Mapping
    mapSpan,
    inExpansion,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))

{- Note [Position map]

CPP never adds or removes lines: output line n always stands for file
line n. Directive lines and excluded lines are blank. When lines are
joined (a backslash at the end, a /* */ comment over several lines, a
macro call over several lines), the joined text goes on the first line
and the other lines are blank, as with gcc.

So most lines map to themselves. Only lines that changed have an entry:
a map from output column to the piece that starts there.

- Copied p: text copied as is, the piece's first char is at p in the
  file. A column k columns into the piece is at p.col + k. A piece can
  come from a later line (joined lines).
- Expanded from to: text made by a macro. Any column in it maps to the
  whole call, from its name to its closing paren (end exclusive).

Columns count code points, like Note [Span convention] in
HaskellReviewer.Core.Span. Tabs are converted first, on the output
text, then the span goes through mapSpan.

A span's start maps through the piece it falls in. Its end is
exclusive, so we map end - 1 and add 1 back (or take the call's end).
The end is never before the start: an empty span at a piece boundary
could map that way, so it's clamped.
-}

-- * Types

-- | A place in the file: 1-based line and column (code points). Plain
-- Ints, not Core.Span's Line and Col: CPP does a lot of arithmetic on
-- them. 'mapSpan' converts.
data Pos = Pos
  { line :: Int,
    col :: Int
  }
  deriving (Eq, Ord, Show)

-- | From output places back to the file. Lines with no entry map to
-- themselves. See Note [Position map].
newtype PositionMap = PositionMap (IntMap (IntMap Piece))
  deriving (Eq, Show)

-- | Where a run of output text comes from.
data Piece
  = -- | Copied as is; its first char is here.
    Copied {-# UNPACK #-} Pos
  | -- | Made by a macro call that spans from here to there (exclusive).
    Expanded {-# UNPACK #-} Pos {-# UNPACK #-} Pos
  deriving (Eq, Show)

-- | Output text and where it comes from.
data Run = Run
  { text :: Text,
    piece :: Piece
  }
  deriving (Eq, Show)

-- * Building

-- | Every line is where it was.
sameLines :: PositionMap
sameLines = PositionMap IntMap.empty

-- | Add an output line, given as its runs in order. A line that is
-- just its own text from column 1 gets no entry.
insertLine :: Int -> [Run] -> PositionMap -> PositionMap
insertLine n rs pm@(PositionMap m) = case filter (not . Text.null . (.text)) rs of
  [] -> pm
  [Run _ (Copied p)] | p == Pos n 1 -> pm
  rs' ->
    let cols = scanl (+) 1 (map (Text.length . (.text)) rs')
     in PositionMap (IntMap.insert n (IntMap.fromList (zip cols (map (.piece) rs'))) m)

-- | Does a run with this piece go on from one with that piece and
-- length? Copied text next to each other in the file, or text from the
-- same call.
follows :: Piece -> Int -> Piece -> Bool
follows p len q = case (p, q) of
  (Copied a, Copied b) -> b == a {col = a.col + len}
  (Expanded f t, Expanded f' t') -> f == f' && t == t'
  _ -> False

-- * Mapping

-- | An output span to a file span. See Note [Position map].
mapSpan :: PositionMap -> Span -> Span
mapSpan pm s =
  Span
    { startLine = Line (fromIntegral start.line),
      startCol = Col (fromIntegral start.col),
      endLine = Line (fromIntegral end.line),
      endCol = Col (fromIntegral end.col)
    }
  where
    start = mapPos pm (Pos (lineOf s.startLine) (colOf s.startCol))
    end = max start (mapEnd pm (Pos (lineOf s.endLine) (colOf s.endCol)))
    lineOf (Line l) = fromIntegral l
    colOf (Col c) = fromIntegral c

-- | Does the output span start in text a macro made?
inExpansion :: PositionMap -> Span -> Bool
inExpansion (PositionMap m) s = case IntMap.lookup line m >>= IntMap.lookupLE col of
  Just (_, Expanded _ _) -> True
  _ -> False
  where
    Line l = s.startLine
    Col c = s.startCol
    line = fromIntegral l
    col = fromIntegral c

-- | Where the char at an output place comes from.
mapPos :: PositionMap -> Pos -> Pos
mapPos (PositionMap m) p = case IntMap.lookup p.line m >>= IntMap.lookupLE p.col of
  Nothing -> p
  Just (k, Copied q) -> q {col = q.col + p.col - k}
  Just (_, Expanded from _) -> from

-- | An exclusive end: the char before it, plus one.
mapEnd :: PositionMap -> Pos -> Pos
mapEnd pm@(PositionMap m) p
  | p.col <= 1 = mapPos pm p
  | otherwise = case IntMap.lookup p.line m >>= IntMap.lookupLE (p.col - 1) of
      Nothing -> p
      Just (k, Copied q) -> q {col = q.col + p.col - k}
      Just (_, Expanded _ to) -> to
