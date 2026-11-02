module Bench.UrlTest (tests) where

import Bench.Url (encodePath, encodeQueryValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Url"
    [ testCase "a plain module name stays" $
        encodePath "Data.Map.Strict" @?= "Data.Map.Strict",
      testCase "a qualified ref: colons encoded" $
        encodePath "pkg:exe:app:Main" @?= "pkg%3Aexe%3Aapp%3AMain",
      testCase "a path ref keeps its slashes" $
        encodePath "scripts/build docs.hs" @?= "scripts/build%20docs.hs",
      testCase "UTF-8 bytes" $
        encodePath "Ünï" @?= "%C3%9Cn%C3%AF",
      testCase "query: operators and plus" $ do
        encodeQueryValue "<$>" @?= "%3C%24%3E"
        encodeQueryValue "+" @?= "%2B"
        encodeQueryValue "a/b" @?= "a%2Fb"
    ]
