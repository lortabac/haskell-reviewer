module Bench.UrlTest (tests) where

import Bench.Mode (Side (..))
import Bench.Queries (RefsQuery (..))
import Bench.Url
  ( diffFilesPath,
    diffPath,
    encodePath,
    encodeQueryValue,
    filesPath,
    indexingPath,
    modulePath,
    modulesPath,
    refsPath,
    searchPath,
    sessionPath,
    sourcePath,
    textSearchPath,
  )
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
        sourcePath Tree "services/proxy/test/unit/Main.hs"
          @?= "api/source/tree/services/proxy/test/unit/Main.hs",
      testCase "a source path: space and Unicode" $
        sourcePath Tree "a b/Ü.hs" @?= "api/source/tree/a%20b/%C3%9C.hs",
      testCase "a refs path" $
        refsPath Tree (RefsQuery "R.Shape" "val:<+>")
          @?= "api/refs/tree?module=R.Shape&decl=val%3A%3C%2B%3E",
      testCase "a text search path: spaces, quotes, plus, non-ASCII" $ do
        textSearchPath Tree "a \"b+" 8 @?= "api/text-search/tree?q=a%20%22b%2B&limit=8"
        textSearchPath Tree "ü" 1000 @?= "api/text-search/tree?q=%C3%BC&limit=1000",
      testCase "every path builder takes a side" $ do
        modulesPath Head @?= "api/modules/head"
        modulePath Base "pkg:exe:app:Main" @?= "api/module/base/pkg%3Aexe%3Aapp%3AMain"
        sourcePath Base "src/A.hs" @?= "api/source/base/src/A.hs"
        searchPath Head "a b" @?= "api/search/head?q=a%20b"
        refsPath Base (RefsQuery "A" "val:x")
          @?= "api/refs/base?module=A&decl=val%3Ax"
        textSearchPath Base "x" 8 @?= "api/text-search/base?q=x&limit=8"
        indexingPath Base @?= "api/indexing/base"
        filesPath Head @?= "api/files/head"
        indexingPath Tree @?= "api/indexing/tree",
      testCase "side-free paths" $ do
        sessionPath @?= "api/session"
        diffFilesPath @?= "api/diff-files",
      testCase "a diff path keeps its slashes; space and Unicode encoded" $ do
        diffPath "src/A.hs" @?= "api/diff/src/A.hs"
        diffPath "a b/Ü.hs" @?= "api/diff/a%20b/%C3%9C.hs"
    ]
