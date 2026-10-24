-- | Golden tests for module discovery: each dir in fixtures/modules is
-- a small project, read from disk like the real thing.
--
-- Tests run from the package dir, so fixtures/modules is one level up.
module HaskellReviewer.Core.ModulesGoldenTest (tests) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.Modules (buildModuleIndex)
import HaskellReviewer.Core.Project (Platform (..))
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Session.Project (readProject)
import ModulesDump (dumpModules)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Module fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [goldenVsString c (fixtureDir <> c <> ".golden") (dump c) | c <- cases]

-- | Each is a dir and its golden file.
cases :: [FilePath]
cases =
  [ -- Every kind of source, and other files.
    "layout",
    -- Only stack.yaml: stack and implicit notices.
    "stack-only",
    "hpack-only",
    -- stack.yaml with a root .cabal: stack notice only.
    "stack-cabal"
  ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/modules/"

-- | Read a fixture from disk and dump its module tree.
dump :: FilePath -> IO LBS.ByteString
dump dir = do
  canon <- canonicalRoot (Root (fixtureDir <> dir))
  w <- walkFiles canon (fileListCap + 1)
  let fl = mkFileList FromDisk w.skippedNames w.entries
  project <- readProject canon fl platform Nothing
  let text = dumpModules (buildModuleIndex project fl)
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 text))
  where
    platform = Platform {os = "linux", arch = "x86_64"}

-- | Fail if a fixture or golden file has no test.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"] @?= sort cases
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort cases
