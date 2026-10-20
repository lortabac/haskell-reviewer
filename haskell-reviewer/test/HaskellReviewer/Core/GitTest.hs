module HaskellReviewer.Core.GitTest (tests) where

import Data.Either (isLeft)
import HaskellReviewer.Core.Git
  ( GitVersion (..),
    IndexMode (..),
    ObjectId (..),
    OtherEntry (..),
    StageEntry (..),
    minimumGitVersion,
    parseGitVersion,
    parseOthers,
    parseStageEntries,
    renderGitVersion,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Git (pure)"
    [ testCase "version: plain, Apple, Windows" $ do
        parseGitVersion "git version 2.53.0\n" @?= Just (GitVersion 2 53 0)
        parseGitVersion "git version 2.39.3 (Apple Git-146)" @?= Just (GitVersion 2 39 3)
        parseGitVersion "git version 2.45.1.windows.1" @?= Just (GitVersion 2 45 1)
        parseGitVersion "git version 2.46.0-rc1" @?= Just (GitVersion 2 46 0)
        parseGitVersion "git version 3.0" @?= Just (GitVersion 3 0 0),
      testCase "version: garbage" $
        mapM_
          (\t -> (t, parseGitVersion t) @?= (t, Nothing))
          ["", "hello", "git version", "git version x.y.z", "git version 2", "2.44.0"],
      testCase "version: 2.43.x is too old, 2.44.0 is fine" $ do
        assertBool "2.43.9" (GitVersion 2 43 9 < minimumGitVersion)
        assertBool "2.44.0" (GitVersion 2 44 0 >= minimumGitVersion)
        assertBool "3.0.0" (GitVersion 3 0 0 >= minimumGitVersion)
        renderGitVersion (GitVersion 2 43 0) @?= "2.43.0",
      testCase "ls-files --stage: modes" $
        parseStageEntries
          ( "100644 aaaa 0\ta.hs\0\
            \100755 bbbb 0\tbin/run\0\
            \120000 cccc 0\tlink\0\
            \160000 dddd 0\tvendor/dep\0"
          )
          @?= Right
            [ StageEntry "a.hs" ModeFile (ObjectId "aaaa"),
              StageEntry "bin/run" ModeFile (ObjectId "bbbb"),
              StageEntry "link" ModeSymlink (ObjectId "cccc"),
              StageEntry "vendor/dep" ModeGitlink (ObjectId "dddd")
            ],
      testCase "ls-files --stage: odd modes" $
        parseStageEntries "100664 aaaa 0\ta\0\&040000 bbbb 0\tdir\0"
          @?= Right
            [ StageEntry "a" ModeFile (ObjectId "aaaa"),
              StageEntry "dir" (ModeOther "040000") (ObjectId "bbbb")
            ],
      testCase "ls-files --stage: conflict stages collapse, stage 2 wins" $
        parseStageEntries
          ( "100644 base 1\tc.hs\0\
            \100644 ours 2\tc.hs\0\
            \100644 them 3\tc.hs\0\
            \100644 eeee 0\td.hs\0"
          )
          @?= Right
            [ StageEntry "c.hs" ModeFile (ObjectId "ours"),
              StageEntry "d.hs" ModeFile (ObjectId "eeee")
            ],
      testCase "ls-files --stage: tabs and non-UTF-8 bytes in names" $
        parseStageEntries "100644 aaaa 0\ta\tb\0\&100644 bbbb 0\tcaf\xe9\0"
          @?= Right
            [ StageEntry "a\tb" ModeFile (ObjectId "aaaa"),
              StageEntry "caf\xe9" ModeFile (ObjectId "bbbb")
            ],
      testCase "ls-files --stage: bad records" $
        mapM_
          (\b -> assertBool (show b) (isLeft (parseStageEntries b)))
          ["junk\0", "100644 aaaa 0 a.hs\0", "100644 aaaa x\ta\0", "100644 aaaa 0\t\0"],
      testCase "ls-files --stage: empty" $
        parseStageEntries "" @?= Right [],
      testCase "ls-files --others: files and nested repos" $
        parseOthers "new.hs\0nested/\0dir/f\0"
          @?= [OtherFile "new.hs", OtherRepo "nested", OtherFile "dir/f"]
    ]
