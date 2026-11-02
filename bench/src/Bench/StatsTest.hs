module Bench.StatsTest (tests) where

import Bench.Stats (Latency (..), Summary (..), latency, percentile, summarize)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Stats"
    [ testCase "single value: min = median = max" $
        summarize (5 :| []) @?= Summary {median = 5, min = 5, max = 5},
      testCase "odd count: the middle value" $
        summarize (3 :| [1, 2]) @?= Summary {median = 2, min = 1, max = 3},
      testCase "even count: mean of the middle two" $
        summarize (1 :| [2, 3, 4]) @?= Summary {median = 2.5, min = 1, max = 4},
      testCase "unsorted input doesn't change min/max/median" $
        summarize (4 :| [1, 3, 2]) @?= Summary {median = 2.5, min = 1, max = 4},
      testCase "p95 of 1..20 is 19 (nearest rank)" $
        percentile 95 (NE.fromList [20, 19 .. 1]) @?= 19,
      testCase "p95 of 1..100 is 95" $
        percentile 95 (NE.fromList [1 .. 100]) @?= 95,
      testCase "p95 of 1..10 is the max" $
        percentile 95 (NE.fromList [1 .. 10]) @?= 10,
      testCase "p99 of 1..1000 is 990" $
        percentile 99 (NE.fromList [1 .. 1000]) @?= 990,
      testCase "one sample: every percentile is it" $
        percentile 95 (7 :| []) @?= 7,
      testCase "latency: count, median, p95, max" $
        latency (NE.fromList [1 .. 20])
          @?= Latency {count = 20, median = 10.5, p95 = 19, max = 20}
    ]
