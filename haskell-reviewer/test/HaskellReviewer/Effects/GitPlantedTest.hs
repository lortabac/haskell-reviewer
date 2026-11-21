-- | The planted repo (D-311): the commit-side calls, the object
-- reader and the working-tree lister run none of its traps, start no
-- child and leave .git as it was (but for the mtime of a split index's
-- shared file). Positive controls, with plain git, show the traps work.
module HaskellReviewer.Effects.GitPlantedTest (tests) where

import Control.Monad (forM, forM_, unless, when)
import Data.ByteString.Char8 qualified as Char8
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles
  ( BlobRef (..),
    BlobSize (..),
    CommitFiles (..),
    OutsideRows (..),
  )
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileList (FileList (..))
import HaskellReviewer.Core.Git (ObjectId (..), RepoInfo (..))
import HaskellReviewer.Core.RelPath (parseRelPath, relPathText)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.WorkTree (DiskFile (..), WorkFile (..))
import HaskellReviewer.Effects.FileSystem (canonicalRoot, listCommit, listCommitPlain)
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
import HaskellReviewer.Effects.Source
  ( EntrySource (..),
    WorkTreeSource (..),
    readSourceEntry,
  )
import HaskellReviewer.Effects.WorkTree (WorkTreeFiles (..), listWorkTree)
import System.Directory qualified as Dir
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport (FileState (..), fixtureRepo, gitOut, snapshotDir)

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
  workTreeCalls t
  where
    ok :: (Show e) => Either e a -> IO a
    ok = either (assertFailure . show) pure

-- | The working-tree lister, and a read of every file it lists, in
-- each racy state, a submodule, the fsmonitor-daemon repo and the
-- partial clone.
workTreeCalls :: FilePath -> Assertion
workTreeCalls t = do
  let roots = ["r1", "r2", "r3", "r4", "r2/sub", "fsd", "pc"] <> inLib
      -- ROOT with a prefix: the calls from the top run too.
      inLib = ["r2/lib", "r4/lib", "fsd/lib"]
      gitDirOf r = t </> takeWhile (/= '/') r </> ".git"
  clearMarkers t
  writeFile (t </> "trace.json") ""
  dirsBefore <- mapM (snapshotDir . gitDirOf) roots
  -- One probe: every spawn forks the test process, and that slows the
  -- time-limited tests that run next to this one.
  g0 <- plantedGit t (t </> "r1")
  forM_ roots $ \r -> do
    let g = atRoot g0 (t </> r)
    info <- ok =<< repoInfo g
    canon <- canonicalRoot (Root (t </> r))
    withObjectReader quiet defaultReaderSettings g $ \reader -> do
      wt <- ok =<< listWorkTree quiet g info reader canon
      assertBool (r <> ": files") (Map.size wt.files.entries >= 1)
      when (r `elem` inLib) $ do
        o <- maybe (assertFailure (r <> ": outside not known")) pure wt.outside
        -- Hashed outside ROOT: clean ones are rows; filtered (we can't
        -- run the filter) and edited ones are dirty.
        let inRows f = Char8.pack f `Map.member` o.rows
            isDirty f = Char8.pack f `Set.member` o.dirty
        forM_ ["a.txt", "plain.hs"] $ \f ->
          assertBool (r <> ": " <> f <> " in rows") (inRows f)
        forM_ ["a.x", "a.bin", "b.txt", "untracked.hs"] $ \f ->
          assertBool (r <> ": " <> f <> " dirty") (isDirty f)
      let source =
            WorkTreeEntries
              WorkTreeSource
                { root = canon,
                  reader = reader,
                  format = info.format,
                  entries = wt.diffEntries,
                  hashable = wt.hashable,
                  outside = wt.outside,
                  files = wt.work
                }
      -- Reads through the index blobs (cat-file) and from disk.
      when (r `elem` ["r2", "r4", "r2/sub"]) $
        forM_ (Map.toList wt.files.entries) $ \(p, k) -> do
          _ <- readSourceEntry source p k
          pure ()
      -- The racy and touched files are clean, unless filtered: we
      -- can't run the filter, so those are read from disk. b.txt is
      -- dirty.
      unless (r `elem` ["r2/sub", "pc"] <> inLib) $ do
        let clean = [relPathText p | (p, FromIndex _) <- Map.toList wt.work]
            filtered =
              [ relPathText p
              | (p, FromDisk d) <- Map.toList wt.work,
                d.filtered == Filtered
              ]
        forM_ ["a.txt", "plain.hs"] $ \f ->
          assertBool (r <> ": " <> Text.unpack f <> " clean") (f `elem` clean)
        -- In r3 they may be clean by stat (only plain.hs was touched).
        unless (r == "r3") $
          forM_ ["a.x", "a.cl", "a.info", "a.ca", "a.bin"] $ \f ->
            assertBool (r <> ": " <> Text.unpack f <> " filtered") (f `elem` filtered)
        assertBool (r <> ": b.txt dirty") ("b.txt" `notElem` clean)
  markers <- Dir.listDirectory (t </> "markers")
  markers @?= []
  trace <- readFile (t </> "trace.json")
  assertBool "git ran: no trace2 events" (not (null trace))
  assertBool "a child was started" (not ("\"child_start\"" `isInfixOf` trace))
  dirsAfter <- mapM (snapshotDir . gitDirOf) roots
  forM_ (zip3 roots dirsBefore dirsAfter) $ \(r, b, a) ->
    (r, map sharedMtime a) @?= (r, map sharedMtime b)
  -- The daemon was never asked for.
  ipc <- Dir.doesPathExist (t </> "fsd" </> ".git" </> "fsmonitor--daemon.ipc")
  assertBool "fsmonitor daemon socket" (not ipc)
  where
    ok :: (Show e) => Either e a -> IO a
    ok = either (assertFailure . show) pure
    -- Reading a split index bumps the mtime of its shared file (git
    -- does it on every read, D-311).
    sharedMtime f
      | "sharedindex." `isPrefixOf` takeFileName f.path = f {mtime = ""}
      | otherwise = f

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
  -- The racy states make git run clean filters. Filters that fail
  -- (process, required) are off, so git gets to the clean commands.
  -- (r1's zero stat entries are listed dirty with no content check.)
  let noReq =
        concat
          [ ["-c", k]
          | k <-
              [ "filter.x.process=",
                "filter.x.required=false",
                "filter.lfs.process=",
                "filter.lfs.required=false"
              ]
          ]
  forM_ ["r2", "r4", "fsd"] $ \r ->
    firesAny
      (t </> r)
      (noReq <> ["diff-files", "--name-only"])
      ["x-clean", "cl-clean", "lfs-clean"]
  fires (t </> "r2") ["status", "--porcelain"] "fsmonitor-hook"
  -- The fsmonitor daemon: plain git status starts it. Not in every
  -- build (2.44.x has none).
  (_, opts, _) <-
    readCreateProcessWithExitCode (plainGit t t ["version", "--build-options"]) ""
  when ("fsmonitor--daemon" `isInfixOf` opts) $ do
    let fsd = t </> "fsd"
        ipc = fsd </> ".git" </> "fsmonitor--daemon.ipc"
    _ <-
      readCreateProcessWithExitCode
        (plainGit t fsd (noReq <> ["status", "--porcelain"]))
        ""
    started <- Dir.doesPathExist ipc
    _ <- readCreateProcessWithExitCode (plainGit t fsd ["fsmonitor--daemon", "stop"]) ""
    assertBool "control: git status started no fsmonitor daemon" started
  where
    fires dir args prefix = firesAny dir args [prefix]
    firesAny dir args prefixes = do
      clearMarkers t
      let cp = plainGit t dir args
      (code, _, _) <- readCreateProcessWithExitCode cp ""
      markers <- Dir.listDirectory (t </> "markers")
      unless (or [p `isPrefixOf` m | p <- prefixes, m <- markers]) $
        assertFailure
          ( "control "
              <> unwords args
              <> " ("
              <> show code
              <> ") fired "
              <> show markers
              <> ", expected one of "
              <> show prefixes
          )

-- | The same git, run in another ROOT.
atRoot :: Git -> FilePath -> Git
atRoot g root =
  Git
    { exe = g.exe,
      cwd = root,
      env = g.env,
      probeTimeLimit = g.probeTimeLimit,
      listTimeLimit = g.listTimeLimit
    }

-- | git with bin/ on PATH (the trace2 wrapper) and home/ as HOME.
plainGit :: FilePath -> FilePath -> [String] -> CreateProcess
plainGit t dir args =
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

clearMarkers :: FilePath -> IO ()
clearMarkers t = do
  let m = t </> "markers"
  Dir.listDirectory m >>= mapM_ (Dir.removeFile . (m </>))

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())
