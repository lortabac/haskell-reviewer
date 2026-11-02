module Bench.QueriesTest (tests) where

import Bench.Queries (SearchAnswer (..), parseSearchAnswer, queries)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Queries"
    [ testCase "20 queries" $ length queries @?= 20,
      testCase "the search fixtures parse" $ do
        partial <- BS.readFile "../fixtures/api/search-partial.json"
        parseSearchAnswer partial @?= Right SearchAnswer {partial = True, matches = 2}
        full <- BS.readFile "../fixtures/api/search.json"
        fmap (.partial) (parseSearchAnswer full) @?= Right False,
      testCase "not a search answer" $
        assertBool "expected an error" (isLeft (parseSearchAnswer "{\"partial\":true}"))
    ]
