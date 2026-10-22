-- | Golden tests for the project model: each dir in
-- fixtures/project is a small project, read from disk like the real
-- thing. The platform and GHC version are always given, so the
-- results don't depend on the machine.
--
-- Tests run from the package dir, so fixtures/project is one level up.
module HaskellReviewer.Core.ProjectGoldenTest (tests) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.Project
  ( GhcVersion (..),
    Platform (..),
    ProjectSettings (..),
  )
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Session.Project (readProject)
import ProjectDump (dumpProject)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Project fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : map golden cases
  where
    golden c =
      let file = fixtureDir <> c.golden <> ".golden"
       in goldenVsString c.golden file (dump c.dir c.settings)

-- | One golden test.
data Case = Case
  { -- | The golden file, without @.golden@.
    golden :: String,
    -- | The fixture project.
    dir :: FilePath,
    settings :: ProjectSettings
  }

cases :: [Case]
cases =
  [ Case "implicit-single" "implicit-single" linux,
    Case "implicit-multi" "implicit-multi" linux,
    Case "globs" "globs" linux,
    Case "conditionals.linux-x86_64" "conditionals" linux,
    Case "conditionals.osx-aarch64" "conditionals" mac,
    Case "conditionals.ghc-9.12" "conditionals" ghc912,
    Case "project-flags" "project-flags" linux,
    Case "imports" "imports" linux,
    Case "project-conditionals.linux-x86_64" "project-conditionals" linux,
    Case "project-conditionals.osx-aarch64" "project-conditionals" mac,
    Case "paths" "paths" linux,
    Case "broken" "broken" linux
  ]
  where
    linux = settings "linux" "x86_64"
    mac = settings "darwin" "aarch64"
    ghc912 = linux {ghcVersion = GhcVersion [9, 12, 2]}
    settings os arch =
      ProjectSettings
        { platform = Platform {os = os, arch = arch},
          ghcVersion = GhcVersion [9, 14, 1]
        }

fixtureDir :: FilePath
fixtureDir = "../fixtures/project/"

-- | Read a fixture from disk and dump it.
dump :: FilePath -> ProjectSettings -> IO LBS.ByteString
dump dir settings = do
  canon <- canonicalRoot (Root (fixtureDir <> dir))
  w <- walkFiles canon (fileListCap + 1)
  project <- readProject canon (mkFileList FromDisk w.skippedNames w.entries) settings
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpProject project)))

-- | Fail if a fixture or golden file has no test.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"]
    @?= sort (nub (map (.dir) cases))
