{-# LANGUAGE StrictData #-}

-- | Whole lines that GHC's lexer never sees: CPP directives, lines CPP
-- skipped, and the prose of a literate file. Packed, for highlighting
-- (SPEC §5.6). See Note [Highlight runs] in "HaskellReviewer.Core.Highlight".
module HaskellReviewer.Core.LineRegions
  ( -- * Types
    LineRegions,
    LineRegion (..),
    LineKind (..),

    -- * Building
    lineRegions,
    noLineRegions,

    -- * Reading
    lineRegionCount,
    lineRegionList,
  )
where

import Data.Vector.Unboxed qualified as Vector
import Data.Word (Word32)
import HaskellReviewer.Core.Cpp (Region (..))
import HaskellReviewer.Core.Cpp qualified as Cpp (RegionKind (..))
import HaskellReviewer.Core.Span (Line (..), LineRange (..))

-- | Sorted, not overlapping. 3 words per region: kind, first line, last
-- line. Most modules have none: they all share 'noLineRegions'.
newtype LineRegions = LineRegions (Vector.Vector Word32)
  deriving (Eq, Show)

-- | Lines @firstLine@ to @lastLine@, both included.
data LineRegion = LineRegion
  { kind :: LineKind,
    firstLine :: Line,
    lastLine :: Line
  }
  deriving (Eq, Show)

-- | What the lines are. The 'Enum' numbers are stored.
data LineKind
  = -- | CPP directives that ran (and the @#else@ and @#endif@ of a
    -- skipped part).
    CppDirectives
  | -- | Skipped by CPP.
    CppExcluded
  | -- | Not code, in a literate file.
    Prose
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Words per region.
width :: Int
width = 3

-- | None. Shared.
noLineRegions :: LineRegions
noLineRegions = LineRegions Vector.empty

-- | From CPP's regions and the prose ranges (both sorted). A line that
-- is both is CPP's: unlit keeps @#@ lines, and an excluded branch can
-- hold prose.
lineRegions :: [Region] -> [LineRange] -> LineRegions
lineRegions cpp prose = case merged of
  [] -> noLineRegions
  _ -> LineRegions (Vector.fromList (concatMap pack merged))
  where
    merged = go (map fromCpp cpp) [p | p <- prose, p.firstLine <= p.lastLine]
    fromCpp r =
      LineRegion
        { kind = kindOf r.kind,
          firstLine = Line (fromIntegral r.firstLine),
          lastLine = Line (fromIntegral r.lastLine)
        }
    kindOf = \case
      Cpp.DirectiveLines -> CppDirectives
      Cpp.Excluded -> CppExcluded
    prose' a b = LineRegion {kind = Prose, firstLine = a, lastLine = b}
    -- Prose minus CPP, in one sweep.
    go cs [] = cs
    go [] ps = [prose' p.firstLine p.lastLine | p <- ps]
    go cs@(c : cs') ps@(p : ps')
      | p.lastLine < c.firstLine = prose' p.firstLine p.lastLine : go cs ps'
      | c.lastLine < p.firstLine = c : go cs' ps
      | otherwise =
          -- c.firstLine > p.firstLine >= 1 there, so before c is a line.
          [prose' p.firstLine (before c.firstLine) | p.firstLine < c.firstLine]
            <> go
              cs
              ( [ LineRange {firstLine = after c.lastLine, lastLine = p.lastLine}
                | p.lastLine > c.lastLine
                ]
                  <> ps'
              )
    before (Line l) = Line (l - 1)
    after (Line l) = Line (l + 1)
    pack r =
      let (Line a, Line b) = (r.firstLine, r.lastLine)
       in [fromIntegral (fromEnum r.kind), a, b]

-- | How many.
lineRegionCount :: LineRegions -> Int
lineRegionCount (LineRegions v) = Vector.length v `div` width

-- | All of them, in order.
lineRegionList :: LineRegions -> [LineRegion]
lineRegionList r@(LineRegions v) = map at [0 .. lineRegionCount r - 1]
  where
    at i =
      LineRegion
        { kind = toKind (w i 0),
          firstLine = Line (w i 1),
          lastLine = Line (w i 2)
        }
    w i k = v Vector.! (i * width + k)
    -- Only lineRegions writes the kind word.
    toKind n
      | n <= fromIntegral (fromEnum (maxBound :: LineKind)) = toEnum (fromIntegral n)
      | otherwise = error ("LineRegions: bad kind word " <> show n)
