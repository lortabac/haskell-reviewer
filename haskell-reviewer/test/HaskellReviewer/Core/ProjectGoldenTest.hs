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
import HaskellReviewer.Core.Project (GhcVersion (..), Platform (..))
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
       in goldenVsString c.golden file (dump c.dir c.platform c.cliGhc)

-- | One golden test.
data Case = Case
  { -- | The golden file, without @.golden@.
    golden :: String,
    -- | The fixture project.
    dir :: FilePath,
    platform :: Platform,
    -- | @--ghc@, if given.
    cliGhc :: Maybe GhcVersion
  }

cases :: [Case]
cases =
  [ Case "implicit-single" "implicit-single" linux Nothing,
    Case "implicit-multi" "implicit-multi" linux Nothing,
    Case "globs" "globs" linux Nothing,
    Case "conditionals.linux-x86_64" "conditionals" linux Nothing,
    Case "conditionals.osx-aarch64" "conditionals" mac Nothing,
    Case "conditionals.ghc-9.12" "conditionals" linux (Just (GhcVersion [9, 12, 2])),
    Case "project-flags" "project-flags" linux Nothing,
    Case "imports" "imports" linux Nothing,
    Case "project-conditionals.linux-x86_64" "project-conditionals" linux Nothing,
    Case "project-conditionals.osx-aarch64" "project-conditionals" mac Nothing,
    Case "paths" "paths" linux Nothing,
    Case "broken" "broken" linux Nothing,
    -- with-compiler: ghc-9.12.2 in cabal.project; §7.1 precedence.
    Case "with-compiler" "with-compiler" linux Nothing,
    Case "with-compiler.ghc-9.10" "with-compiler" linux (Just (GhcVersion [9, 10]))
  ]
  where
    linux = Platform {os = "linux", arch = "x86_64"}
    mac = Platform {os = "darwin", arch = "aarch64"}

fixtureDir :: FilePath
fixtureDir = "../fixtures/project/"

-- | Read a fixture from disk and dump it.
dump :: FilePath -> Platform -> Maybe GhcVersion -> IO LBS.ByteString
dump dir platform cliGhc = do
  canon <- canonicalRoot (Root (fixtureDir <> dir))
  w <- walkFiles canon (fileListCap + 1)
  project <-
    readProject canon (mkFileList FromDisk w.skippedNames w.entries) platform cliGhc
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpProject project)))

-- | Fail if a fixture or golden file has no test. Cases can share a
-- dir (e.g. with/without @--ghc@).
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"]
    @?= sort (nub (map (.dir) cases))
