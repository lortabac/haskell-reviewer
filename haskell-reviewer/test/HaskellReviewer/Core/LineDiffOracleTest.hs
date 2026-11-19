-- | The line diff against git (D-321): same runs as
-- @git diff --no-index --histogram@, with and without @-w@.
module HaskellReviewer.Core.LineDiffOracleTest (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.LineDiff
  ( GitRule,
    LineDiff (..),
    Stats (..),
    Whitespace (..),
    diffLines,
    lineDiffStatsAs,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import Hedgehog
  ( Gen,
    Property,
    PropertyT,
    annotate,
    assert,
    evalIO,
    failure,
    forAll,
    forAllWith,
    property,
    withShrinks,
    withTests,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LineDiffSupport
  ( chunks,
    genBigPair,
    genHugePair,
    genPair,
    genRepetitivePair,
    gitRule,
    gitRuns,
    gitRunsMany,
  )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

-- | git 2.54 changed compaction (see 'GitRule'): compare with what the
-- git on PATH does.
tests :: TestTree
tests =
  withResource gitRule (const (pure ())) $ \getRule ->
    testGroup
      "LineDiff oracle"
      [ testGroup "exact" (modeTests getRule KeepWhitespace),
        testGroup "-w" (modeTests getRule IgnoreWhitespace)
      ]

modeTests :: IO GitRule -> Whitespace -> [TestTree]
modeTests getRule ws =
  -- 200 generated and 60 repetitive pairs, 20 and 10 per git run; 10
  -- big noisy and 2 huge pairs, one per git run. In parts that run in
  -- parallel.
  [ testProperty "generated pairs" (withTests 10 (oracleMany getRule ws 20 genPair)),
    testProperty
      "repetitive pairs"
      (withTests 6 (oracleMany getRule ws 10 genRepetitivePair))
  ]
    -- No shrinking: thousands of lines, one git run per step.
    <> chunks 2 "big noisy pairs" (withTests 5 (withShrinks 0 (bigOracle getRule ws)))
    <> chunks 2 "huge pairs" (withTests 1 (withShrinks 0 (hugeOracle getRule ws)))
    <> [testGroup "cases" [testCase name (oracleCase getRule ws p) | (name, p) <- cases]]

-- | @k@ pairs per test, all diffed by one git run. Each pair gets its
-- own size, 0 to 99: with few tests, Hedgehog's size (which grows by
-- one per test from 0) would make every pair tiny and unchanged. At
-- least a quarter of a batch's pairs must have different lines (61% of
-- generated pairs do, 99% of repetitive ones), so the batch tests
-- something.
oracleMany :: IO GitRule -> Whitespace -> Int -> Gen (Pair Text) -> Property
oracleMany getRule ws k gen = property $ do
  let sized = Gen.integral (Range.constant 0 99) >>= \size -> Gen.resize size gen
  ps <- forAll (Gen.list (Range.singleton k) sized)
  rule <- evalIO getRule
  evalIO (gitRunsMany ws ps) >>= \case
    Left err -> annotate err >> failure
    Right gits -> do
      [(lineDiffStatsAs rule ws p).diff.runs | p <- ps] === gits
      let changed = length [() | p <- ps, diffLines p.base /= diffLines p.head]
      annotate (show changed <> " of " <> show k <> " pairs have different lines")
      assert (4 * changed >= k)

-- | Big fallback regions, where Myers' cost limit fires (LineDiffTest's
-- cutoff coverage checks that; a cover over 5 tests here could flake).
bigOracle :: IO GitRule -> Whitespace -> Property
bigOracle getRule ws = property $ do
  p <- forAll genBigPair
  rule <- evalIO getRule
  same rule ws p

-- | Huge fallback regions, where the heuristic split can fire (a unit
-- test in LineDiffTest pins both ways on one pair).
hugeOracle :: IO GitRule -> Whitespace -> Property
hugeOracle getRule ws = property $ do
  p <- forAllWith (const "a huge pair") genHugePair
  rule <- evalIO getRule
  same rule ws p

same :: GitRule -> Whitespace -> Pair Text -> PropertyT IO ()
same rule ws p =
  evalIO (gitRuns ws p) >>= \case
    Left err -> annotate err >> failure
    Right git -> (lineDiffStatsAs rule ws p).diff.runs === git

oracleCase :: IO GitRule -> Whitespace -> Pair Text -> IO ()
oracleCase getRule ws p = do
  rule <- getRule
  gitRuns ws p >>= \case
    Left err -> assertFailure err
    Right git -> (lineDiffStatsAs rule ws p).diff.runs @?= git

-- | Fixed cases: a fallback, sliders, the indent heuristic, group
-- merges, a group lined up with the other side's.
cases :: [(String, Pair Text)]
cases =
  [ ("empty to text", pair "" "a\nb\n"),
    ("text to empty", pair "a\nb\n" ""),
    ("final newline added", pair "a\nb" "a\nb\n"),
    ("final newline removed", pair "a\nb\n" "a\nb"),
    ("CRLF to LF and a change", pair "a\r\nb\r\nc\r\n" "a\nB\nc\n"),
    ( "fallback",
      pair
        (lines' (concat (replicate 70 ["x", "}"]) <> ["end"]))
        (lines' (["new"] <> concat (replicate 70 ["}", "x"])))
    ),
    ("slider", pair "a\nb\n\na\nb\n" "a\nb\n\na\nb\n\na\nb\n"),
    ( "indent heuristic",
      pair
        "f() {\n  a;\n}\n\ng() {\n  b;\n}\n"
        "f() {\n  a;\n}\n\nh() {\n  c;\n}\n\ng() {\n  b;\n}\n"
    ),
    ("merge sliding down", pair "a\nx\nb\nx\nc\n" "a\nb\nx\nc\n"),
    ("merge sliding up", pair "a\nb\nb\nb\nc\n" "a\nc\n"),
    ("lined up with the other side", pair "a\nb\nc\nb\nd\n" "a\nB\nb\nc\nd\n"),
    ("whitespace only", pair "f x =\n  x\n" "f x =\n    x \n"),
    ("blank lines", pair "a\n\n\nb\n" "a\n\n\n\nb\n\n"),
    -- Differs between git 2.53 and 2.54 (see GitRule).
    ( "a merged group diffed again",
      pair "  x = 1\n\n}\n-- | Docs.\n\n" "\n}\n  x = 1\n\n-- | Docs.\n}\n\n\n"
    )
  ]
  where
    pair b h = Pair {head = h, base = b}
    lines' = Text.unlines
