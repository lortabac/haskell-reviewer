module Bench.QueriesTest (tests) where

import Bench.Projects (Project (..), Role (..), parseProjectsFile, selectProjects)
import Bench.Queries
  ( RefsAnswer (..),
    RefsQuery (..),
    SearchAnswer (..),
    TextAnswer (..),
    parseNotFoundWhat,
    parseRefsAnswer,
    parseSearchAnswer,
    parseTextAnswer,
    queries,
    refsQueries,
    refsQueryText,
    textQueries,
  )
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

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
        assertBool "expected an error" (isLeft (parseSearchAnswer "{\"partial\":true}")),
      testCase "6 refs queries per reference project, none for extras" $ do
        (refs, extras) <- projectNames
        map (length . refsQueries) refs @?= [6, 6, 6]
        assertBool "some extras" (not (null extras))
        map refsQueries extras @?= map (const []) extras,
      testCase "no duplicate refs queries" $ do
        (refs, _) <- projectNames
        mapM_ (\p -> nub (refsQueries p) @?= refsQueries p) refs,
      testCase "every decl id has a known namespace" $ do
        (refs, _) <- projectNames
        mapM_
          ( \q ->
              assertBool
                (Text.unpack (refsQueryText q))
                (any (`Text.isPrefixOf` q.decl) namespaces)
          )
          (concatMap refsQueries refs),
      testCase "the query text" $
        refsQueryText (RefsQuery "Data.Id" "type:UserId") @?= "Data.Id type:UserId",
      testCase "the refs fixtures parse" $ do
        full <- BS.readFile "../fixtures/api/refs.json"
        parseRefsAnswer full @?= Right RefsAnswer {partial = False, total = 10, sent = 10}
        partial <- BS.readFile "../fixtures/api/refs-partial.json"
        parseRefsAnswer partial
          @?= Right RefsAnswer {partial = True, total = 2, sent = 2},
      testCase "not a refs answer" $ do
        notFound <- BS.readFile "../fixtures/api/refs-not-found.json"
        assertBool "not found" (isLeft (parseRefsAnswer notFound))
        let noResults = "{\"partial\":true,\"total\":1}"
        assertBool "no results" (isLeft (parseRefsAnswer noResults)),
      testCase "10 text queries, no duplicates, none with a line break" $ do
        length textQueries @?= 10
        nub (toList textQueries) @?= toList textQueries
        assertBool "a line break" (not (any (Text.any (`elem` ['\n', '\r'])) textQueries))
        assertBool "over 128" (all ((<= 128) . Text.length) textQueries),
      testCase "the text search fixtures parse" $ do
        full <- BS.readFile "../fixtures/api/text-search.json"
        parseTextAnswer full
          @?= Right TextAnswer {stopped = False, total = 7, files = 5, sent = 7}
        capped <- BS.readFile "../fixtures/api/text-search-capped.json"
        parseTextAnswer capped
          @?= Right TextAnswer {stopped = False, total = 7, files = 5, sent = 2},
      testCase "not a text search answer" $ do
        search <- BS.readFile "../fixtures/api/search.json"
        assertBool "a search answer" (isLeft (parseTextAnswer search)),
      testCase "a not-found body" $ do
        notFound <- BS.readFile "../fixtures/api/refs-not-found.json"
        parseNotFoundWhat notFound @?= Just "decl"
        parseNotFoundWhat "{\"partial\":true}" @?= Nothing
        parseNotFoundWhat "not json" @?= Nothing
    ]
  where
    namespaces = ["type:", "con:", "field:", "val:", "class:", "pat:"]

-- | The reference projects and the extra ones in @bench/projects.txt@.
projectNames :: IO ([Text], [Text])
projectNames = do
  raw <- Text.IO.readFile "../bench/projects.txt"
  ps <- either (assertFailure . show) pure (parseProjectsFile raw)
  refs <- either (assertFailure . show) pure (selectProjects [] ps)
  pure (map (.name) refs, [p.name | p <- ps, p.role == Extra])
