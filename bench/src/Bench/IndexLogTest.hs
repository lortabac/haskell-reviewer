module Bench.IndexLogTest (tests) where

import Bench.IndexLog
  ( ChangedLine (..),
    DiffLog (..),
    IndexDone (..),
    PairingLine (..),
    WorkTreeLine (..),
    addDiffLine,
    noDiffLog,
    parseChangedLine,
    parseDoneLine,
    parsePairingLine,
    parseWorkTreeLine,
  )
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
        parseDoneLine (doneLine "modules (1 ready, 0 failed, 0 text only) in 5 ms")
          @?= Nothing,
      testCase "the changed line" $ do
        parseChangedLine "changed: 56 on head, 55 on base, indexed first"
          @?= Just ChangedLine {head = 56, base = 55}
        parseChangedLine "changed: 56 on head, 55 on base" @?= Nothing,
      testCase "the pairing line" $ do
        parsePairingLine "pairing: 154 changed, 13 renamed in 12 ms"
          @?= Just PairingLine {changed = 154, renamed = 13, limit = Nothing}
        parsePairingLine "pairing: 3 changed, 0 renamed, over the rename limit in 0 ms"
          @?= Just
            PairingLine
              { changed = 3,
                renamed = 0,
                limit = Just "over the rename limit"
              }
        let budget = "pairing: 3 changed, 0 renamed, over the work budget in 9 ms"
        (.limit) <$> parsePairingLine budget @?= Just (Just "over the work budget")
        parsePairingLine "pairing: 3 changed, 0 renamed in ms" @?= Nothing
        parsePairingLine "pairing: 3 changed, 0 renamed, in 4 ms" @?= Nothing,
      testCase "the working tree line" $ do
        parseWorkTreeLine
          "files: working tree: 69 entries, 2 hashed, 1 read from disk"
          @?= Just WorkTreeLine {entries = 69, hashed = 2, fromDisk = 1}
        parseWorkTreeLine "files: working tree: 69 entries, 2 hashed" @?= Nothing
        parseWorkTreeLine "files: outside ROOT: 3 rows, 0 racy not hashed, 1 ms"
          @?= Nothing,
      testCase "the diff log keeps the first line of each kind" $ do
        let d =
              foldl
                (flip addDiffLine)
                noDiffLog
                [ "indexing: 34 modules, 8 workers",
                  "changed: 1 on head, 2 on base, indexed first",
                  "changed: 9 on head, 9 on base, indexed first",
                  "pairing: 3 changed, 0 renamed in 4 ms"
                ]
        d.changed @?= Just ChangedLine {head = 1, base = 2}
        (.changed) <$> d.pairing @?= Just 3
        d.workTree @?= Nothing
    ]
  where
    doneLine rest = "indexing: done, " <> rest
    oneModule = "1 modules (1 ready, 0 failed, 0 text only)"
