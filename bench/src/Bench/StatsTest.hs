module Bench.StatsTest (tests) where

import Bench.Stats (Summary (..), summarize)
import Data.List.NonEmpty (NonEmpty (..))
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
        summarize (4 :| [1, 3, 2]) @?= Summary {median = 2.5, min = 1, max = 4}
    ]
