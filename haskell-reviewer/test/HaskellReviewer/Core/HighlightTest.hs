-- | Highlight runs and line regions: units, properties on random
-- tokens and regions, the warm-up's coverage, and linear time.
module HaskellReviewer.Core.HighlightTest (tests, linearTests) where

import Control.Exception (evaluate)
import Data.List (nub, sort)
import Data.Maybe (listToMaybe)
import Data.Word (Word32)
import HaskellReviewer.Core.Cpp (Region (..), RegionKind (..))
import HaskellReviewer.Core.Highlight
  ( DisplayClass (..),
    Run (..),
    displayClass,
    highlight,
    highlightModule,
    noRuns,
    runCount,
    runList,
  )
import HaskellReviewer.Core.LineRegions
  ( LineKind (..),
    LineRegion (..),
    LineRegions,
    lineRegionList,
    lineRegions,
    noLineRegions,
  )
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Parser (parseSource)
import HaskellReviewer.Core.Span (Col (..), Line (..), LineRange (..), Span (..))
import HaskellReviewer.Core.Tokens (Token (..), TokenClass (..), tokensFromList)
import HaskellReviewer.Effects.Parser (parserWarmUp)
import Hedgehog (Gen, Property, annotateShow, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem (getAllocationCounter)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Highlight"
    [ testCase "class mapping: Special plain, Macro pragma" $ do
        displayClass Special @?= Nothing
        displayClass Macro @?= Just PragmaClass
        displayClass ConSym @?= Just TypeConClass
        displayClass Haddock @?= Just CommentClass
        -- Total, and only Special is plain.
        [c | c <- [minBound .. maxBound], displayClass c == Nothing] @?= [Special],
      testCase "regions: CPP wins over prose" $
        regionList
          [cpp DirectiveLines 3 4, cpp Excluded 6 6]
          [(1, 10)]
          @?= [ (Prose, 1, 2),
                (CppDirectives, 3, 4),
                (Prose, 5, 5),
                (CppExcluded, 6, 6),
                (Prose, 7, 10)
              ],
      testCase "regions: adjacent ones stay apart" $
        regionList [cpp DirectiveLines 1 1, cpp Excluded 2 3] [(4, 4), (5, 6)]
          @?= [ (CppDirectives, 1, 1),
                (CppExcluded, 2, 3),
                (Prose, 4, 4),
                (Prose, 5, 6)
              ],
      testCase "regions: prose all under CPP" $
        regionList [cpp Excluded 1 5] [(2, 3)] @?= [(CppExcluded, 1, 5)],
      testCase "regions: none gives the empty value" $ do
        lineRegions [] [] @?= noLineRegions
        highlight Nothing noLineRegions @?= noRuns,
      testCase "a token across a region is cut around it" $
        spans
          (highlight (Just (tokensFromList [tok 1 5 5 3 Comment])) (regions [(2, 3)]))
          @?= [ ((1, 5, 2, 1), CommentClass),
                ((2, 1, 4, 1), ExcludedClass),
                ((4, 1, 5, 3), CommentClass)
              ],
      testCase "overlapping tokens: clamped" $
        spans
          ( highlight
              (Just (tokensFromList [tok 1 1 1 9 Macro, tok 1 3 1 5 VarId]))
              noLineRegions
          )
          @?= [((1, 1, 1, 9), PragmaClass)],
      testProperty "regions: CPP's kind on CPP lines, else prose" regionsByLine,
      testProperty "runs: sorted, apart, not empty" runsApart,
      testProperty "every position has the class it should" positionsRight,
      testCase "the warm-up covers every class, line kind and text-only path" warmUpCovers
    ]
  where
    cpp k a b = Region {kind = k, firstLine = a, lastLine = b}
    regionList c p =
      [ (r.kind, line r.firstLine, line r.lastLine)
      | r <- lineRegionList (lineRegions c (map range p))
      ]
    line (Line l) = l
    regions rs = lineRegions [cpp Excluded a b | (a, b) <- rs] []
    spans rs =
      [ ((l1, c1, l2, c2), r.display)
      | r <- runList rs,
        let Span (Line l1) (Col c1) (Line l2) (Col c2) = r.span
      ]

-- | Lines a to b.
range :: (Word32, Word32) -> LineRange
range (a, b) = LineRange {firstLine = Line a, lastLine = Line b}

tok :: Word32 -> Word32 -> Word32 -> Word32 -> TokenClass -> Token
tok l1 c1 l2 c2 k = Token {span = Span (Line l1) (Col c1) (Line l2) (Col c2), kind = k}

-- * Properties

-- | Random input: tokens (maybe overlapping), CPP regions and prose.
data Input = Input
  { tokens :: [Token],
    cpp :: [Region],
    prose :: [LineRange]
  }
  deriving (Show)

-- | Tokens that don't overlap: sorted distinct places, paired up.
genTokens :: Gen [Token]
genTokens = do
  places <- Gen.list (Range.linear 0 40) place
  let sorted = nub (sort places)
  pairs sorted
  where
    place = (,) <$> Gen.word32 (Range.linear 1 20) <*> Gen.word32 (Range.linear 1 30)
    pairs = \case
      (l1, c1) : (l2, c2) : rest -> do
        k <- Gen.enumBounded
        (tok l1 c1 l2 c2 k :) <$> pairs rest
      _ -> pure []

-- | Disjoint, sorted line ranges.
genRanges :: Gen [(Int, Int)]
genRanges = do
  steps <- Gen.list (Range.linear 0 6) ((,) <$> small 0 <*> small 1)
  pure (go 1 steps)
  where
    go _ [] = []
    go at ((gap, len) : rest) =
      let a = at + gap in (a, a + len - 1) : go (a + len) rest
    small from = Gen.int (Range.linear from 3)

genInput :: Gen Input
genInput = do
  ts <- genTokens
  cs <- genRanges
  kinds <- Gen.list (Range.singleton (length cs)) (Gen.element [DirectiveLines, Excluded])
  ps <- genRanges
  pure
    Input
      { tokens = ts,
        cpp = [Region k a b | (k, (a, b)) <- zip kinds cs],
        prose = [range (fromIntegral a, fromIntegral b) | (a, b) <- ps]
      }

inputRegions :: Input -> LineRegions
inputRegions i = lineRegions i.cpp i.prose

-- | Checked line by line against the inputs, not against lineRegions'
-- own sweep.
regionsByLine :: Property
regionsByLine = property $ do
  i <- forAll genInput
  let out = lineRegionList (inputRegions i)
      inRange l a b = a <= l && l <= b
      expected l =
        case [c.kind | c <- i.cpp, inRange l c.firstLine c.lastLine] of
          DirectiveLines : _ -> Just CppDirectives
          Excluded : _ -> Just CppExcluded
          []
            | or [inRange (Line l') p.firstLine p.lastLine | p <- i.prose] -> Just Prose
            | otherwise -> Nothing
        where
          l' = fromIntegral l
      got l =
        listToMaybe
          [o.kind | o <- out, inRange (Line (fromIntegral l)) o.firstLine o.lastLine]
  annotateShow out
  sequence_ [(l, got l) === (l, expected l) | l <- [1 .. 30 :: Int]]
  -- Sorted and disjoint, each region not empty.
  assert (and [o.firstLine <= o.lastLine | o <- out])
  assert (and (zipWith (\a b -> a.lastLine < b.firstLine) out (drop 1 out)))

runsApart :: Property
runsApart = property $ do
  i <- forAll genInput
  -- Some overlap too: a token moved into the next one.
  shift <- forAll (Gen.word32 (Range.linear 0 3))
  let moved = [Token {span = longer t.span, kind = t.kind} | t <- i.tokens]
      longer s = let Col c = s.endCol in s {endCol = Col (c + shift)}
      rs = runList (highlight (Just (tokensFromList moved)) (inputRegions i))
      pos s = (s.startLine, s.startCol)
      end s = (s.endLine, s.endCol)
  annotateShow rs
  assert (and [pos r.span < end r.span | r <- rs])
  assert (and (zipWith (\a b -> end a.span <= pos b.span) rs (drop 1 rs)))

positionsRight :: Property
positionsRight = property $ do
  i <- forAll genInput
  let rs = runList (highlight (Just (tokensFromList i.tokens)) (inputRegions i))
      regionAt l =
        listToMaybe
          [ r.kind
          | r <- lineRegionList (inputRegions i),
            Line l >= r.firstLine && Line l <= r.lastLine
          ]
      inside p s = (s.startLine, s.startCol) <= p && p < (s.endLine, s.endCol)
      runAt p = listToMaybe [r.display | r <- rs, inside p r.span]
      expected l c = case regionAt l of
        Just k -> Just $ case k of
          CppDirectives -> DirectiveClass
          CppExcluded -> ExcludedClass
          Prose -> CommentClass
        Nothing ->
          listToMaybe [t.kind | t <- i.tokens, inside (Line l, Col c) t.span]
            >>= displayClass
  annotateShow rs
  sequence_
    [ (l, c, runAt (Line l, Col c)) === (l, c, expected l c)
    | l <- [1 .. 24],
      c <- [1 .. 32]
    ]

-- * Warm-up

warmUpCovers :: Assertion
warmUpCovers = do
  let parses = [parseSource s t | (s, t) <- parserWarmUp]
      runs = concatMap (runList . highlightModule) parses
      kinds = concat [lineRegionList (regionsOf p) | p <- parses]
      regionsOf = \case
        Parsed m -> m.regions
        TextOnly t -> t.regions
  sort (nub (map (.display) runs)) @?= [minBound .. maxBound]
  sort (nub (map (.kind) kinds)) @?= [minBound .. maxBound]
  -- A parse failure with tokens, and one that didn't lex but has CPP
  -- regions.
  assertBool "parse failure with tokens" $
    or [True | TextOnly TextOnlyModule {reason = ParseFailed _ (Just _)} <- parses]
  assertBool "lex failure with regions" $
    or
      [ not (null (lineRegionList t.regions))
      | TextOnly t@TextOnlyModule {reason = ParseFailed _ Nothing} <- parses
      ]

-- * Linear time

-- | 'highlight' allocation grows about linearly with tokens and regions.
-- See Note [Linear time tests] in "HaskellReviewer.Core.DeclsTest".
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: highlight"
      [testCase "tokens and regions, with cuts" (linear 25_000 100_000)]

linear :: Int -> Int -> Assertion
linear small big = do
  a <- allocated small
  b <- allocated big
  let ratio = fromIntegral b / fromIntegral a :: Double
  assertBool ("bytes " <> show (a, b) <> ", ratio " <> show ratio) (ratio < 6)
  where
    -- k lines: a comment from the middle of each line to the next one,
    -- every third line excluded.
    allocated k = do
      let n = fromIntegral k :: Word32
          ts = tokensFromList [tok l 5 (l + 1) 3 Comment | l <- [1 .. n]]
          rs = lineRegions [Region Excluded l l | l <- [3, 6 .. k]] []
      _ <- evaluate ts
      _ <- evaluate rs
      a0 <- getAllocationCounter
      _ <- evaluate (runCount (highlight (Just ts) rs))
      a1 <- getAllocationCounter
      pure (a0 - a1)
