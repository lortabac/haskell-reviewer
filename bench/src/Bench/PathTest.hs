module Bench.PathTest (tests) where

import Bench.Path (shortPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Path"
    [ testCase "under cwd" $
        shortPath "/home/u/repo" (Just "/home/u") "/home/u/repo/dist/hr" @?= "dist/hr",
      testCase "under home only" $
        shortPath "/home/u/repo" (Just "/home/u") "/home/u/.local/bin/hr"
          @?= "~/.local/bin/hr",
      testCase "outside both" $
        shortPath "/home/u/repo" (Just "/home/u") "/usr/bin/hr" @?= "/usr/bin/hr",
      testCase "a sibling with the same prefix is not under cwd" $
        shortPath "/home/u/repo" Nothing "/home/u/repo2/hr" @?= "/home/u/repo2/hr",
      testCase "no or empty HOME" $
        shortPath "/w" (Just "") "/home/u/hr" @?= "/home/u/hr",
      testCase "cwd / is ignored" $
        shortPath "/" (Just "/home/u") "/home/u/x/hr" @?= "~/x/hr",
      testCase "home / is ignored" $
        shortPath "/w" (Just "/") "/usr/bin/hr" @?= "/usr/bin/hr",
      testCase "relative stays" $
        shortPath "/w" (Just "/home/u") "dist/hr" @?= "dist/hr"
    ]
