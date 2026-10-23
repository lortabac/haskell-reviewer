-- | Checks that don't need a fixture project.
module HaskellReviewer.Core.ProjectTest (tests) where

import Data.Maybe (fromMaybe)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    GhcVersion (..),
    WithCompiler (..),
    detectGhc,
    parserGhcVersion,
    parserVersionText,
    renderGhcVersion,
  )
import HaskellReviewer.Core.RelPath (relPathFromSegments)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Project"
    [ testCase "parserGhcVersion matches ghc-lib-parser's own version (D-83)" $
        renderGhcVersion parserGhcVersion @?= parserVersionText,
      testCase "detectGhc: --ghc wins, then with-compiler, then the parser" $ do
        detectGhc (Just (v [9, 10])) (Just (wc (Just (v [9, 12]))))
          @?= DetectedGhc {version = v [9, 10], source = GhcFromCli}
        detectGhc Nothing (Just (wc (Just (v [9, 12]))))
          @?= DetectedGhc {version = v [9, 12], source = GhcFromWithCompiler}
        detectGhc Nothing Nothing
          @?= DetectedGhc {version = parserGhcVersion, source = GhcFromParser}
        -- Plain "ghc" in with-compiler: no version, falls through.
        detectGhc Nothing (Just (wc Nothing))
          @?= DetectedGhc {version = parserGhcVersion, source = GhcFromParser}
    ]
  where
    v = GhcVersion
    wc version =
      WithCompiler
        { written = "ghc",
          version = version,
          file = fromMaybe (error "bad path") (relPathFromSegments ["cabal.project"])
        }
