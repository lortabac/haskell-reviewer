module Bench.ProjectsTest (tests) where

import Bench.Projects
  ( Project (..),
    ProjectsError (..),
    Role (..),
    parseProjectsFile,
    selectProjects,
  )
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

sample :: Text
sample =
  "# a comment, then a blank line\n\
  \\n\
  \small-one   small   https://example.com/a aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\
  \typical-one typical https://example.com/b bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n\
  \large-one   large   https://example.com/c cccccccccccccccccccccccccccccccccccccccc\n\
  \extra-one   extra   https://example.com/d dddddddddddddddddddddddddddddddddddddddd\n"

parseSample :: IO [Project]
parseSample = either (assertFailure . show) pure (parseProjectsFile sample)

tests :: TestTree
tests =
  testGroup
    "Bench.Projects"
    [ testCase "comments and blank lines are skipped" $ do
        ps <- parseSample
        length ps @?= 4,
      testCase "roles are parsed" $ do
        ps <- parseSample
        map (.role) ps @?= [Small, Typical, Large, Extra],
      testCase "default selection: small, typical, large, not extra" $ do
        ps <- parseSample
        case selectProjects [] ps of
          Right picked -> map (.name) picked @?= ["small-one", "typical-one", "large-one"]
          Left e -> assertFailure (show e),
      testCase "selecting by name can pick an extra project" $ do
        ps <- parseSample
        case selectProjects ["extra-one"] ps of
          Right picked -> map (.name) picked @?= ["extra-one"]
          Left e -> assertFailure (show e),
      testCase "unknown name is an error" $ do
        ps <- parseSample
        selectProjects ["no-such-project"] ps @?= Left (UnknownName "no-such-project"),
      testCase "bad sha is rejected" $
        case parseProjectsFile "p small https://example.com not-a-sha\n" of
          Left (BadLine 1 _) -> pure ()
          other -> assertFailure ("expected a BadLine error, got " <> show other)
    ]
