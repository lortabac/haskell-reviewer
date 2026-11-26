module Bench.ModeTest (tests) where

import Bench.Mode
  ( Mode (..),
    ModeRuns (..),
    Side (..),
    allModes,
    diffSide,
    diffSpec,
    modeNames,
    modeRuns,
    modeSides,
    onSide,
    parseModes,
    projectModes,
  )
import Bench.Projects (Project (..), Role (..))
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Mode"
    [ testCase "--modes: run order, whatever the order given" $ do
        parseModes "worktree,browse" @?= Right [Browse, WorkTree]
        parseModes "range" @?= Right [Range]
        parseModes "browse,range,worktree,range" @?= Right allModes,
      testCase "--modes: an unknown or empty mode is an error" $ do
        assertBool "unknown" (isLeft (parseModes "diff"))
        assertBool "empty" (isLeft (parseModes ""))
        assertBool "trailing comma" (isLeft (parseModes "browse,")),
      testCase "the default names every mode" $
        parseModes (Text.unpack modeNames) @?= Right allModes,
      testCase "no base: browse only" $ do
        projectModes allModes project {base = Nothing} @?= [Browse]
        projectModes [Range] project {base = Nothing} @?= []
        projectModes allModes project @?= allModes,
      testCase "--diff argument" $ do
        diffSpec Browse project @?= Nothing
        diffSpec Range project @?= Just "b..h"
        diffSpec WorkTree project @?= Just "b"
        diffSpec Range project {base = Nothing} @?= Nothing,
      testCase "sides, head first" $ do
        modeSides Browse @?= Tree :| []
        modeSides Range @?= Head :| [Base]
        modeSides WorkTree @?= Head :| [Base],
      testCase "what each mode runs (D-357)" $ do
        map ((.census) . modeRuns) allModes @?= [True, False, False]
        map ((.maxSteadyRuns) . modeRuns) allModes @?= [Nothing, Nothing, Just 1]
        map ((.duringRuns) . modeRuns) allModes @?= [True, True, False]
        map ((.needsBase) . modeRuns) allModes @?= [False, True, True]
        map ((.needsClean) . modeRuns) allModes @?= [False, False, True],
      testCase "browse's side says nothing" $ do
        map diffSide [Tree, Head, Base] @?= [Nothing, Just Head, Just Base]
        map onSide [Tree, Base] @?= ["", " on base"]
    ]
  where
    project =
      Project {name = "p", role = Small, url = "u", sha = "h", base = Just "b"}
