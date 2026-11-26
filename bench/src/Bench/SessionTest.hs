module Bench.SessionTest (tests) where

import Bench.Mode (Mode (..))
import Bench.Projects (Project (..), Role (..))
import Bench.Session
  ( DiffSides (..),
    SessionAnswer (..),
    checkSession,
    parseSession,
  )
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Tests run from the package dir.
fixture :: FilePath -> IO SessionAnswer
fixture name = do
  body <- BS.readFile ("../fixtures/api/" <> name)
  either (assertFailure . show) pure (parseSession body)

tests :: TestTree
tests =
  testGroup
    "Bench.Session"
    [ testCase "browse" $ do
        a <- fixture "session-browse.json"
        a @?= BrowseSession
        checkSession Browse project a @?= Right ()
        assertBool "not range" (isLeft (checkSession Range project a)),
      testCase "range: both commits" $ do
        a <- fixture "session-diff.json"
        a @?= DiffSession DiffSides {baseCommit = baseSha, headCommit = Just headSha}
        checkSession Range project a @?= Right ()
        assertBool "not browse" (isLeft (checkSession Browse project a))
        assertBool "not worktree" (isLeft (checkSession WorkTree project a)),
      testCase "worktree: head is null" $ do
        a <- fixture "session-worktree.json"
        a @?= DiffSession DiffSides {baseCommit = otherSha, headCommit = Nothing}
        checkSession WorkTree project {base = Just otherSha} a @?= Right ()
        checkSession WorkTree project a
          @?= Left
            ( "api/session: base is "
                <> otherSha
                <> ", expected "
                <> baseSha
            ),
      testCase "range: another head" $ do
        a <- fixture "session-diff.json"
        checkSession Range project {sha = otherSha} a
          @?= Left
            ("api/session: head is " <> headSha <> ", expected " <> otherSha),
      testCase "three dots parse too" $ do
        a <- fixture "session-three-dot.json"
        a @?= DiffSession DiffSides {baseCommit = baseSha, headCommit = Just headSha},
      testCase "not a session" $
        assertBool "error" (isLeft (parseSession "{\"mode\":\"other\"}"))
    ]
  where
    baseSha, headSha, otherSha :: Text
    baseSha = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    headSha = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    otherSha = "cccccccccccccccccccccccccccccccccccccccc"
    project =
      Project
        { name = "p",
          role = Small,
          url = "https://example.com",
          sha = headSha,
          base = Just baseSha
        }
