module Bench.IndexLogTest (tests) where

import Bench.IndexLog (IndexDone (..), parseDoneLine)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.IndexLog"
    [ testCase "the done line" $
        parseDoneLine (doneLine "1972 modules (1970 ready, 2 failed, 11 text only) in 1056 ms")
          @?= Just
            IndexDone {modules = 1972, ready = 1970, failed = 2, textOnly = 11, ms = 1056},
      testCase "with a prefix" $
        fmap (.ms) (parseDoneLine ("[v] " <> doneLine (oneModule <> " in 0 ms"))) @?= Just 0,
      testCase "progress lines are not it" $ do
        parseDoneLine "indexing: 1972 modules, 8 workers" @?= Nothing
        parseDoneLine "indexing: 198/1972" @?= Nothing,
      testCase "anything after ms: not it" $
        parseDoneLine (doneLine (oneModule <> " in 5 ms!")) @?= Nothing,
      testCase "a missing number: not it" $
        parseDoneLine (doneLine "modules (1 ready, 0 failed, 0 text only) in 5 ms") @?= Nothing
    ]
  where
    doneLine rest = "indexing: done, " <> rest
    oneModule = "1 modules (1 ready, 0 failed, 0 text only)"
