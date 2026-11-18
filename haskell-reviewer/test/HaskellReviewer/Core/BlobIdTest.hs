-- | Blob ids, with git as the oracle.
module HaskellReviewer.Core.BlobIdTest (tests) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as Text
import HaskellReviewer.Core.BlobId (blobId, emptyBlobId)
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import Hedgehog (forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import TestSupport (GitRun (..), git, gitBytes)

tests :: TestTree
tests =
  -- Two scratch repos, made once (Note [Shared values in tests] in
  -- "HaskellReviewer.Core.PackageGlobTest").
  withResource makeRepos Dir.removeDirectoryRecursive $ \get ->
    testGroup
      "BlobId"
      [ testCase "empty blob" $ do
          emptyBlobId Sha1 @?= ObjectId "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
          emptyBlobId Sha256
            @?= ObjectId
              "473a0f4c3be8a93681a267e3b1e9a7dcda1185436fe141f7749120a303721813",
        testProperty "sha1: same as git hash-object --no-filters" $ oracle get Sha1,
        testProperty "sha256: same as git hash-object --no-filters" $ oracle get Sha256
      ]
  where
    -- One git call per run, for many byte strings: spawning git per
    -- example loads the machine (time-limited tests run next to this).
    oracle get format = withTests 20 . property $ do
      many <-
        forAll . Gen.list (Range.linear 1 20) $
          Gen.choice
            [ Gen.bytes (Range.linear 0 300),
              Gen.utf8 (Range.linear 0 100) Gen.unicode
            ]
      dir <- liftIO get
      let repo = dir </> if format == Sha1 then "sha1" else "sha256"
      files <- liftIO . forM (zip [0 :: Int ..] many) $ \(i, b) -> do
        let f = repo </> ("f" <> show i)
        BS.writeFile f b
        pure f
      run <-
        liftIO $
          gitBytes
            repo
            ["hash-object", "--no-filters", "--stdin-paths"]
            (Char8.pack (unlines files))
      run.code === ExitSuccess
      map (blobId format) many
        === map (ObjectId . Text.pack . Char8.unpack) (Char8.lines run.out)

makeRepos :: IO FilePath
makeRepos = do
  tmp <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmp "blobid" >>= Dir.canonicalizePath
  git dir ["init", "-q", "--object-format=sha1", "sha1"]
  git dir ["init", "-q", "--object-format=sha256", "sha256"]
  pure dir
