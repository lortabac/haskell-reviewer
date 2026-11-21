-- | The diff route's path rule (Note [Diff target]).
module HaskellReviewer.Core.DiffTargetTest (tests) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import HaskellReviewer.Core.DiffTarget
  ( ChangeIndex (..),
    Target (..),
    changeIndex,
    findTarget,
    targetPaths,
  )
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ChangeNote (..),
    ExecChange (..),
    ModifiedFile (..),
    RenameStep (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (maxScore)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "DiffTarget"
    [ testCase "the index: by head and by base" $ do
        Map.keys idx.byHead @?= map rel ["a", "m", "to"]
        Map.keys idx.byBase @?= map rel ["from", "m", "r"],
      testCase "every status, a rename from both ends" $ do
        find "a" @?= Just (Changed added)
        find "r" @?= Just (Changed removed)
        find "m" @?= Just (Changed modified)
        find "to" @?= Just (Changed renamed)
        find "from" @?= Just (Changed renamed),
      testCase "not in the pairing: same path, added, removed, neither" $ do
        find "same" @?= Just (SamePath (rel "same"))
        find "headonly" @?= Just (Changed (Added (rel "headonly")))
        find "baseonly" @?= Just (Changed (Removed (rel "baseonly")))
        find "nope" @?= Nothing,
      testCase "on both lists but removed (a nested repo on head): removed" $ do
        let on = Pair {head = (== rel "r"), base = (== rel "r")}
        findTarget idx on (rel "r") @?= Just (Changed removed),
      testCase "paths to read on each side" $ do
        targetPaths (Changed renamed)
          @?= Pair {head = Just (rel "to"), base = Just (rel "from")}
        targetPaths (Changed added) @?= Pair {head = Just (rel "a"), base = Nothing}
        targetPaths (Changed removed) @?= Pair {head = Nothing, base = Just (rel "r")}
        targetPaths (SamePath (rel "same"))
          @?= Pair {head = Just (rel "same"), base = Just (rel "same")}
    ]
  where
    added = Added (rel "a")
    removed = Removed (rel "r")
    modified = Modified ModifiedFile {path = rel "m", note = Just TypeChanged}
    renamed =
      Renamed
        RenamedFile
          { from = rel "from",
            to = rel "to",
            score = maxScore,
            step = ExactStep,
            exec = SameExec
          }
    idx = changeIndex [added, modified, removed, renamed]
    onHead = map rel ["a", "m", "to", "same", "headonly"]
    onBase = map rel ["r", "m", "from", "same", "baseonly"]
    find p = findTarget idx Pair {head = (`elem` onHead), base = (`elem` onBase)} (rel p)

rel :: ByteString -> RelPath
rel b = either (error . show) id (parseRelPath b)
