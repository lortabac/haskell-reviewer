module Bench.UrlTest (tests) where

import Bench.Queries (RefsQuery (..))
import Bench.Url (encodePath, encodeQueryValue, refsPath, sourcePath, textSearchPath)
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
        encodeQueryValue "a/b" @?= "a%2Fb",
      testCase "a source path keeps its slashes" $
        sourcePath "services/proxy/test/unit/Main.hs"
          @?= "api/source/tree/services/proxy/test/unit/Main.hs",
      testCase "a source path: space and Unicode" $
        sourcePath "a b/Ü.hs" @?= "api/source/tree/a%20b/%C3%9C.hs",
      testCase "a refs path" $
        refsPath (RefsQuery "R.Shape" "val:<+>")
          @?= "api/refs/tree?module=R.Shape&decl=val%3A%3C%2B%3E",
      testCase "a text search path: spaces, quotes, plus, non-ASCII" $ do
        textSearchPath "a \"b+" 8 @?= "api/text-search/tree?q=a%20%22b%2B&limit=8"
        textSearchPath "ü" 1000 @?= "api/text-search/tree?q=%C3%BC&limit=1000"
    ]
