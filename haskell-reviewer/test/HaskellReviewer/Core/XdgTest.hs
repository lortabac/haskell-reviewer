module HaskellReviewer.Core.XdgTest (tests) where

import HaskellReviewer.Core.Xdg
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Xdg"
    [ testCase "the variable, when set and absolute" $ do
        xdgDir XdgConfig (env [("XDG_CONFIG_HOME", "/c"), home]) @?= Right "/c"
        xdgDir XdgState (env [("XDG_STATE_HOME", "/s"), home]) @?= Right "/s",
      testCase "unset, empty or relative: HOME's" $
        mapM_
          ( \vars -> do
              xdgDir XdgConfig (env (home : vars)) @?= Right "/home/u/.config"
              xdgDir XdgState (env (home : vars)) @?= Right "/home/u/.local/state"
          )
          [ [],
            [("XDG_CONFIG_HOME", ""), ("XDG_STATE_HOME", "")],
            [("XDG_CONFIG_HOME", "rel"), ("XDG_STATE_HOME", "./rel")]
          ],
      testCase "one variable doesn't set the other" $
        xdgDir XdgState (env [("XDG_CONFIG_HOME", "/c"), home])
          @?= Right "/home/u/.local/state",
      testCase "no usable HOME" $ do
        xdgDir XdgConfig (env []) @?= Left HomeUnset
        xdgDir XdgConfig (env [("HOME", "")]) @?= Left HomeUnset
        xdgDir XdgState (env [("HOME", "u")]) @?= Left (HomeRelative "u")
        -- The variable still works.
        xdgDir XdgState (env [("XDG_STATE_HOME", "/s")]) @?= Right "/s",
      testCase "the files" $ do
        configFile (env [home]) @?= Right "/home/u/.config/haskell-reviewer/config.toml"
        prefsFile (env [("XDG_STATE_HOME", "/s")]) @?= Right "/s/haskell-reviewer/ui.json"
        prefsFile (env []) @?= Left HomeUnset,
      testCase "messages" $ do
        renderXdgProblem HomeUnset @?= "HOME is not set"
        renderXdgProblem (HomeRelative "u") @?= "HOME is not an absolute path: u"
    ]
  where
    env vars name = lookup name vars
    home = ("HOME", "/home/u")
