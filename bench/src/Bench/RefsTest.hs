module Bench.RefsTest (tests) where

import Bench.Queries (RefsQuery (..))
import Bench.Refs
  ( RefsSummary (..),
    RefsTime (..),
    byQuery,
    refsWarnings,
    slowdownPercent,
    staleQueryMessage,
    startedBefore,
  )
import Bench.Stats (Latency (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Refs"
    [ testCase "by query: list order, last sample's sizes" $
        byQuery [b, a] [time a 10 1, time b 30 5, time a 20 2, time a 40 3]
          @?= [ RefsSummary
                  { query = b,
                    latency = Latency {count = 1, median = 30, p95 = 30, max = 30},
                    total = 5,
                    sent = 5,
                    wireBytes = 500
                  },
                RefsSummary
                  { query = a,
                    latency = Latency {count = 3, median = 20, p95 = 40, max = 40},
                    total = 3,
                    sent = 3,
                    wireBytes = 300
                  }
              ],
      testCase "by query: a query with no sample is left out" $
        map (.query) (byQuery [a, b] [time b 1 1]) @?= [b],
      testCase "during: started before the done line" $ do
        startedBefore 100 (time a 50 1) {startUs = 99} @?= True
        startedBefore 100 (time a 50 1) {startUs = 100} @?= False,
      testCase "slow-down in percent" $ do
        slowdownPercent 200 250 @?= Just 25
        slowdownPercent 200 100 @?= Just (-50)
        slowdownPercent 0 100 @?= Nothing,
      testCase "the stale query message" $ do
        staleQueryMessage a (Just "decl")
          @?= "refs query Data.Id type:UserId: 404 (what: decl); \
              \update refsQueries in bench/src/Bench/Queries.hs"
        staleQueryMessage b Nothing
          @?= "refs query Data.Qualified val:tSplit: 404 (what: ?); \
              \update refsQueries in bench/src/Bench/Queries.hs",
      testCase "warnings: partial or total 0, once per query" $
        refsWarnings
          [ (time a 1 0) {partial = True},
            (time a 2 0) {partial = True},
            time b 3 5
          ]
          @?= [ "refs query Data.Id type:UserId: partial after indexing",
                "refs query Data.Id type:UserId: total 0 after indexing"
              ],
      testCase "warnings: none for good answers" $
        refsWarnings [time a 1 3, time b 2 5] @?= []
    ]
  where
    a = RefsQuery "Data.Id" "type:UserId"
    b = RefsQuery "Data.Qualified" "val:tSplit"
    time q us total =
      RefsTime
        { query = q,
          us = us,
          startUs = 0,
          partial = False,
          total = total,
          sent = total,
          wireBytes = 100 * total
        }
