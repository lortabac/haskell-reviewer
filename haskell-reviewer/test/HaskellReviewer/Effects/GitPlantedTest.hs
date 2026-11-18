-- | The planted repo (D-311): the commit-side calls and the object
-- reader run none of its traps, start no child and leave .git as it
-- was. Positive controls, with plain git, show the traps work.
module HaskellReviewer.Effects.GitPlantedTest (tests) where

import Control.Monad (forM, forM_, unless)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles (BlobRef (..), BlobSize (..), CommitFiles (..))
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.RelPath (parseRelPath)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.FileSystem (listCommit, listCommitPlain)
import HaskellReviewer.Effects.Git
  ( Git (..),
    GitSetup (..),
    Listing (..),
    TreeListing (..),
    gitProbeTimeLimit,
    lsTree,
    mergeBase,
    probeGitWith,
    repoInfo,
    resolveCommit,
  )
import HaskellReviewer.Effects.GitObjects
  ( BlobBytes (..),
    ObjectError (..),
    defaultReaderSettings,
    objectInfo,
    readBlob,
    withObjectReader,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport (fixtureRepo, gitOut, snapshotDir)

tests :: TestTree
tests =
  testGroup
    "Git planted"
    [ -- Built once (Note [Shared values in tests] in
      -- "HaskellReviewer.Core.PackageGlobTest"). One test: the
      -- controls fire traps, so they must not run next to the safe calls.
      withResource makePlanted Dir.removeDirectoryRecursive $ \get ->
        testCase "safe calls fire nothing; plain git fires every trap" $ do
          t <- get
          safeCalls t
          controls t
    ]

makePlanted :: IO FilePath
makePlanted = do
  tmp <- getCanonicalTemporaryDirectory
  t <- createTempDirectory tmp "planted" >>= Dir.canonicalizePath
  fixtureRepo "planted" [t]
  pure t

-- | Our git, as the program finds it: bin/ (the trace2 wrapper and the
-- fake git-lfs) on PATH, home/ as HOME.
plantedGit :: FilePath -> FilePath -> IO Git
plantedGit t root = do
  let setup =
        GitSetup
          { searchPath = [t </> "bin", "/usr/bin", "/bin"],
            inherited = [("HOME", t </> "home")],
            probeTimeLimit = gitProbeTimeLimit,
            listTimeLimit = gitProbeTimeLimit
          }
  probeGitWith setup quiet (Root root) >>= \case
    Right (ListFromGit g) -> pure g
    Right (ListFromDisk _) -> assertFailure "git not used"
    Left e -> assertFailure ("git error: " <> show e)

safeCalls :: FilePath -> Assertion
safeCalls t = do
  let repo = t </> "repo"
      pc = t </> "pc"
  tree <- filter (/= '\n') <$> gitOut repo ["rev-parse", "HEAD^{tree}"]
  missing <- filter (/= '\n') <$> readFile (t </> "missing-blob")
  gRepo <- plantedGit t repo
  gPc <- plantedGit t pc
  clearMarkers t
  writeFile (t </> "trace.json") ""
  beforeRepo <- snapshotDir (repo </> ".git")
  beforePc <- snapshotDir (pc </> ".git")
  -- The repo: every call, on both commits.
  info <- ok =<< repoInfo gRepo
  Just headId <- ok =<< resolveCommit gRepo "HEAD"
  Just parent <- ok =<< resolveCommit gRepo "HEAD~1"
  mergeBase gRepo parent headId >>= (@?= Right [parent])
  forM_ [WithSizes, WithoutSizes] $ \l -> ok =<< lsTree gRepo l headId
  lists <- forM [headId, parent] $ \c -> do
    a <- ok =<< listCommit quiet gRepo info c
    b <- ok =<< listCommitPlain quiet gRepo info c
    pure [a, b]
  let blobs = concatMap (Map.elems . (.blobs)) (concat lists)
  assertBool "blobs" (length blobs >= 8)
  withObjectReader quiet defaultReaderSettings gRepo $ \r -> do
    forM_ blobs $ \b -> do
      _ <- ok =<< readBlob r 2_000_000 b.oid
      ok =<< objectInfo r b.oid
    let gone = ObjectId (Text.replicate 39 "0" <> "1")
    readBlob r 10 gone >>= (@?= Left (ObjectMissing gone))
    readBlob r 10 (ObjectId (Text.pack tree)) >>= \case
      Left (ObjectNotABlob _) -> pure ()
      other -> assertFailure ("tree: " <> show other)
  -- The partial clone: the old blob is missing. git 2.56 lists it as
  -- BAD; on 2.44.1 ls-tree --long dies and we list without sizes.
  pcInfo <- ok =<< repoInfo gPc
  Just pcParent <- ok =<< resolveCommit gPc "HEAD~1"
  Just pcHead <- ok =<< resolveCommit gPc "HEAD"
  old <- ok =<< listCommit quiet gPc pcInfo pcParent
  plain <- ok =<< listCommitPlain quiet gPc pcInfo pcParent
  new <- ok =<< listCommit quiet gPc pcInfo pcHead
  let f = either (error . show) id (parseRelPath "f.txt")
  case Map.lookup f old.blobs of
    Just b -> do
      b.oid @?= ObjectId (Text.pack missing)
      assertBool (show b.size) (b.size `elem` [BlobMissing, BlobSizeUnknown])
    Nothing -> assertFailure "no f.txt"
  fmap (.size) (Map.lookup f plain.blobs) @?= Just BlobSizeUnknown
  withObjectReader quiet defaultReaderSettings gPc $ \r -> do
    let m = ObjectId (Text.pack missing)
    readBlob r 10 m >>= (@?= Left (ObjectMissing m))
    objectInfo r m >>= (@?= Left (ObjectMissing m))
    case Map.lookup f new.blobs of
      Just b -> fmap (.bytes) <$> readBlob r 10 b.oid >>= (@?= Right "v2\n")
      Nothing -> assertFailure "no f.txt in HEAD"
  -- Nothing ran, nothing changed.
  markers <- Dir.listDirectory (t </> "markers")
  markers @?= []
  trace <- readFile (t </> "trace.json")
  assertBool "git ran: no trace2 events" (not (null trace))
  assertBool "a child was started" (not ("\"child_start\"" `isInfixOf` trace))
  afterRepo <- snapshotDir (repo </> ".git")
  afterPc <- snapshotDir (pc </> ".git")
  afterRepo @?= beforeRepo
  afterPc @?= beforePc
  where
    ok :: (Show e) => Either e a -> IO a
    ok = either (assertFailure . show) pure

-- | Each trap fires with plain git and the same HOME and PATH.
controls :: FilePath -> Assertion
controls t = do
  let repo = t </> "repo"
  missing <- filter (/= '\n') <$> readFile (t </> "missing-blob")
  fires repo ["cat-file", "--filters", "HEAD:a.x"] "x-"
  fires repo ["cat-file", "--filters", "HEAD:a.bin"] "lfs-"
  fires repo ["cat-file", "--textconv", "HEAD:a.txt"] "textconv"
  fires (t </> "pc") ["cat-file", "-p", missing] "lazy-fetch"
  writeFile (t </> "trace.json") ""
  fires repo ["status", "--porcelain"] "fsmonitor-hook"
  -- The trace check works: status started the hook as a child.
  trace <- readFile (t </> "trace.json")
  assertBool "no child_start seen" ("\"child_start\"" `isInfixOf` trace)
  where
    fires dir args prefix = do
      clearMarkers t
      let cp =
            (proc (t </> "bin" </> "git") args)
              { cwd = Just dir,
                env =
                  Just
                    [ ("HOME", t </> "home"),
                      ("PATH", t </> "bin" <> ":/usr/bin:/bin"),
                      ("LC_ALL", "C")
                    ],
                close_fds = True
              }
      (code, _, _) <- readCreateProcessWithExitCode cp ""
      markers <- Dir.listDirectory (t </> "markers")
      unless (any (prefix `isPrefixOf`) markers) $
        assertFailure
          ( "control "
              <> unwords args
              <> " ("
              <> show code
              <> ") fired "
              <> show markers
              <> ", expected "
              <> prefix
          )

clearMarkers :: FilePath -> IO ()
clearMarkers t = do
  let m = t </> "markers"
  Dir.listDirectory m >>= mapM_ (Dir.removeFile . (m </>))

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())
