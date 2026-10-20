-- | Listing and reading files, on real temp dirs, with the real git.
module HaskellReviewer.Effects.FileSystemTest (tests) where

import Control.Exception (IOException, finally, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromBytes, relPathText)
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    ReadError (..),
    WalkResult (..),
    canonicalRoot,
    listFiles,
    readEntry,
    walkFiles,
  )
import HaskellReviewer.Effects.Git
  ( GitError (..),
    GitSetup (..),
    Listing (..),
    gitProbeTimeLimit,
    probeGitWith,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.Posix.Files (createNamedPipe)
import System.Posix.IO.ByteString (closeFd, createFile)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport (git, gitOut, gitSetup, isRootUser, withCanonicalTemp, writeScript)

tests :: TestTree
tests =
  testGroup
    "FileSystem"
    [ gitTests,
      walkTests,
      readTests
    ]

-- * With git

gitTests :: TestTree
gitTests =
  testGroup
    "list with git"
    [ testCase "tracked and untracked listed, ignored not" $
        withRepo $ \dir -> do
          writeFile (dir </> ".gitignore") "ignored.txt\n/build/\n"
          writeFile (dir </> "a.hs") ""
          writeFile (dir </> "new.hs") ""
          writeFile (dir </> "ignored.txt") ""
          Dir.createDirectory (dir </> "build")
          writeFile (dir </> "build" </> "out") ""
          git dir ["add", ".gitignore", "a.hs"]
          fl <- listWithGit dir
          fl.source @?= FromGit
          names fl
            @?= [(".gitignore", EntryFile), ("a.hs", EntryFile), ("new.hs", EntryFile)],
      testCase "deleted tracked file dropped" $
        withRepo $ \dir -> do
          writeFile (dir </> "a.hs") ""
          writeFile (dir </> "b.hs") ""
          git dir ["add", "a.hs", "b.hs"]
          Dir.removeFile (dir </> "b.hs")
          fl <- listWithGit dir
          names fl @?= [("a.hs", EntryFile)],
      testCase "submodule and nested repo" $
        withRepo $ \dir -> do
          let sha = "0123456789abcdef0123456789abcdef01234567"
          Dir.createDirectoryIfMissing True (dir </> "vendor" </> "dep")
          git
            dir
            ["update-index", "--add", "--cacheinfo", "160000," <> sha <> ",vendor/dep"]
          Dir.createDirectory (dir </> "nested")
          git (dir </> "nested") ["init", "-q"]
          writeFile (dir </> "nested" </> "x") ""
          fl <- listWithGit dir
          names fl
            @?= [ ("nested", EntryNestedRepo),
                  ("vendor/dep", EntrySubmodule (CommitId (Text.pack sha)))
                ],
      testCase "tracked symlink, and a file now a symlink" $
        withRepo $ \dir -> do
          writeFile (dir </> "a.hs") ""
          writeFile (dir </> "b.hs") ""
          Dir.createFileLink "a.hs" (dir </> "link")
          git dir ["add", "a.hs", "b.hs", "link"]
          Dir.removeFile (dir </> "b.hs")
          Dir.createFileLink "a.hs" (dir </> "b.hs")
          fl <- listWithGit dir
          names fl
            @?= [("a.hs", EntryFile), ("b.hs", EntrySymlink), ("link", EntrySymlink)],
      testCase "ROOT in a subdir: only paths under it, relative" $
        withRepo $ \dir -> do
          Dir.createDirectory (dir </> "sub")
          writeFile (dir </> "top.hs") ""
          writeFile (dir </> "sub" </> "x.hs") ""
          writeFile (dir </> "sub" </> "new.hs") ""
          git dir ["add", "top.hs", "sub/x.hs"]
          fl <- listWithGit (dir </> "sub")
          names fl @?= [("new.hs", EntryFile), ("x.hs", EntryFile)],
      testCase "tracked file under a dir now a symlink to outside: dropped" $
        withCanonicalTemp $ \tmp -> do
          let dir = tmp </> "repo"
              outside = tmp </> "outside"
          Dir.createDirectory dir
          Dir.createDirectory outside
          git dir ["init", "-q"]
          Dir.createDirectory (dir </> "d")
          writeFile (dir </> "d" </> "f.hs") ""
          writeFile (outside </> "f.hs") "secret"
          git dir ["add", "d/f.hs"]
          Dir.removeDirectoryRecursive (dir </> "d")
          Dir.createDirectoryLink outside (dir </> "d")
          fl <- listWithGit dir
          names fl @?= [("d", EntrySymlink)],
      testCase "not a repo: from disk" $
        withCanonicalTemp $ \dir -> do
          r <- probe gitProbeTimeLimit [] (Root dir)
          case r of
            Right ListFromDisk -> pure ()
            Right (ListFromGit _) -> assertFailure "expected from disk"
            Left e -> assertFailure ("git error: " <> show e),
      testCase "ROOT given as a symlink" $
        withCanonicalTemp $ \tmp -> do
          let dir = tmp </> "repo"
              link = tmp </> "link"
          Dir.createDirectory dir
          git dir ["init", "-q"]
          writeFile (dir </> "a.hs") "a\n"
          Dir.createDirectoryLink dir link
          fl <- listWithGit link
          names fl @?= [("a.hs", EntryFile)]
          r <- readAt link "a.hs" EntryFile
          r @?= Right (TextContent "a\n"),
      testCase "crafted index naming .git/config: not listed" $
        withRepo $ \dir -> do
          -- An index entry for xgit/config, renamed to .git/config by
          -- hand. With index.skipHash git doesn't check the index sum.
          writeFile (dir </> "blob") "x\n"
          oid <- takeWhile (/= '\n') <$> gitOut dir ["hash-object", "-w", "blob"]
          git dir ["config", "index.skipHash", "true"]
          git
            dir
            ["update-index", "--add", "--cacheinfo", "100644," <> oid <> ",xgit/config"]
          let indexFile = dir </> ".git" </> "index"
          index <- BS.readFile indexFile
          let (before, after) = BS.breakSubstring "xgit/config" index
          BS.writeFile indexFile (before <> ".git/config" <> BS.drop 11 after)
          listed <- gitOut dir ["ls-files"]
          -- git itself lists it: the test means something.
          assertBool "git doesn't list .git/config" (".git/config" `elem` lines listed)
          fl <- listWithGit dir
          names fl @?= [("blob", EntryFile)]
          fl.skippedNames @?= 0,
      testCase "a FIFO .gitignore: git killed at the time limit" $
        withRepo $ \dir -> do
          let bin = takeDirectory dir </> "bin"
          Dir.createDirectory bin
          real <- Dir.findExecutable "git" >>= maybe (assertFailure "no git") pure
          let pids = bin </> "pids"
          _ <-
            writeScript
              bin
              "git"
              ["echo $$ >> '" <> pids <> "'", "exec '" <> real <> "' \"$@\""]
          writeFile (dir </> "a.hs") ""
          createNamedPipe (dir </> ".gitignore") 0o600
          listing <- probe (Microseconds 5_000_000) [bin] (Root dir)
          canon <- canonicalRoot (Root dir)
          case listing of
            Right l@(ListFromGit _) -> do
              r <- timeout 20_000_000 (listFiles quiet canon l)
              case r of
                Just (Left (ListGitFailed (GitTimedOut _))) -> pure ()
                other -> assertFailure ("expected a time out, got " <> show other)
            _ -> assertFailure "git not used"
          ps <- map read . lines <$> readFile pids
          mapM_
            ( \pid -> do
                alive <- try @IOException (signalProcess nullSignal pid)
                assertBool ("git " <> show pid <> " still running") (isLeft alive)
            )
            ps
    ]

-- * Without git

walkTests :: TestTree
walkTests =
  testGroup
    "disk walk"
    [ testCase ".git skipped at every level" $
        withCanonicalTemp $ \dir -> do
          Dir.createDirectoryIfMissing True (dir </> ".git" </> "objects")
          writeFile (dir </> ".git" </> "config") ""
          Dir.createDirectoryIfMissing True (dir </> "sub")
          writeFile (dir </> "sub" </> ".git") "gitdir: ../.git"
          writeFile (dir </> "sub" </> "a.hs") ""
          writeFile (dir </> ".gitignore") ""
          -- Same dir as .git on a case-insensitive file system.
          Dir.createDirectoryIfMissing True (dir </> "other" </> ".GIT")
          writeFile (dir </> "other" </> ".GIT" </> "config") ""
          w <- walk dir
          walkNames w @?= [(".gitignore", EntryFile), ("sub/a.hs", EntryFile)],
      testCase "symlinked dir listed as a symlink, not entered" $
        withCanonicalTemp $ \tmp -> do
          let dir = tmp </> "root"
          Dir.createDirectory dir
          Dir.createDirectory (tmp </> "outside")
          writeFile (tmp </> "outside" </> "secret") ""
          Dir.createDirectoryLink (tmp </> "outside") (dir </> "out")
          w <- walk dir
          walkNames w @?= [("out", EntrySymlink)],
      testCase "FIFO skipped, no hang" $
        withCanonicalTemp $ \dir -> do
          createNamedPipe (dir </> "fifo") 0o600
          writeFile (dir </> "a") ""
          w <- walk dir
          walkNames w @?= [("a", EntryFile)],
      testCase "unreadable dir skipped with a warning" $
        withCanonicalTemp $ \dir -> do
          root <- isRootUser
          unless root $ do
            let locked = dir </> "locked"
            Dir.createDirectory locked
            writeFile (locked </> "x") ""
            writeFile (dir </> "a") ""
            Dir.setPermissions locked Dir.emptyPermissions
            flip finally (unlock locked) $ do
              w <- walk dir
              walkNames w @?= [("a", EntryFile)]
              length w.warnings @?= 1,
      testCase "stops at the limit" $
        withCanonicalTemp $ \dir -> do
          mapM_ (\i -> writeFile (dir </> ("f" <> show i)) "") [1 .. 10 :: Int]
          canon <- canonicalRoot (Root dir)
          w <- walkFiles canon 4
          length w.entries @?= 4,
      testCase "non-UTF-8 name counted (Linux)" $
        -- macOS file systems refuse such names.
        when (os == "linux") $
          withCanonicalTemp $ \dir -> do
            canon <- canonicalRoot (Root dir)
            fd <- createFile (canon.bytes <> "/caf\xe9") 0o600
            closeFd fd
            writeFile (dir </> "a") ""
            w <- walkFiles canon 100
            walkNames w @?= [("a", EntryFile)]
            w.skippedNames @?= 1
    ]

-- * Reading

readTests :: TestTree
readTests =
  testGroup
    "readEntry"
    [ testCase "text" $
        withCanonicalTemp $ \dir -> do
          BS.writeFile (dir </> "a.hs") "main = pure ()\n"
          r <- readAt dir "a.hs" EntryFile
          r @?= Right (TextContent "main = pure ()\n"),
      testCase "binary" $
        withCanonicalTemp $ \dir -> do
          BS.writeFile (dir </> "b.bin") "\x89PNG\0\0"
          r <- readAt dir "b.bin" EntryFile
          r @?= Right (Binary (FileSize 6)),
      testCase "over the limit" $
        withCanonicalTemp $ \dir -> do
          let FileSize lim = textLimit
          BS.writeFile (dir </> "big.txt") (BS.replicate (lim + 10) 97)
          r <- readAt dir "big.txt" EntryFile
          r
            @?= Right
              (TooLarge TooLargeInfo {size = FileSize (lim + 10), limit = textLimit}),
      testCase "symlink to /etc/passwd: the target only" $
        withCanonicalTemp $ \dir -> do
          Dir.createFileLink "/etc/passwd" (dir </> "pw")
          r <- readAt dir "pw" EntrySymlink
          r @?= Right (Link LinkInfo {target = "/etc/passwd", resolved = Nothing}),
      testCase "symlink inside ROOT: resolved" $
        withCanonicalTemp $ \dir -> do
          Dir.createDirectory (dir </> "docs")
          Dir.createFileLink "../shared/x" (dir </> "docs" </> "x")
          r <- readAt dir "docs/x" EntrySymlink
          r @?= Right (Link LinkInfo {target = "../shared/x", resolved = rel "shared/x"}),
      testCase "a file listed as a file but now a symlink: not followed" $
        withCanonicalTemp $ \dir -> do
          Dir.createFileLink "/etc/passwd" (dir </> "a.hs")
          r <- readAt dir "a.hs" EntryFile
          case r of
            Right (Link _) -> pure ()
            other -> assertFailure ("expected a link, got " <> show other),
      testCase "parent dir is a symlink: gone" $
        withCanonicalTemp $ \tmp -> do
          let dir = tmp </> "root"
          Dir.createDirectory dir
          Dir.createDirectory (tmp </> "outside")
          writeFile (tmp </> "outside" </> "f.hs") "secret"
          Dir.createDirectoryLink (tmp </> "outside") (dir </> "d")
          r <- readAt dir "d/f.hs" EntryFile
          r @?= Left ReadGone,
      testCase "FIFO: gone, no hang" $
        withCanonicalTemp $ \dir -> do
          createNamedPipe (dir </> "fifo") 0o600
          r <- timeout 5_000_000 (readAt dir "fifo" EntryFile)
          r @?= Just (Left ReadGone),
      testCase "missing file, dir: gone" $
        withCanonicalTemp $ \dir -> do
          Dir.createDirectory (dir </> "sub")
          readAt dir "nope" EntryFile >>= (@?= Left ReadGone)
          readAt dir "sub" EntryFile >>= (@?= Left ReadGone)
          readAt dir "nope/x" EntryFile >>= (@?= Left ReadGone),
      testCase "submodule: no disk access" $
        withCanonicalTemp $ \dir -> do
          let c = CommitId "0123"
          readAt dir "vendor/dep" (EntrySubmodule c) >>= (@?= Right (Submodule c))
          readAt dir "nested" EntryNestedRepo >>= (@?= Right NestedRepo)
    ]

-- * Helpers

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | A new repo in tmp/repo.
withRepo :: (FilePath -> IO a) -> IO a
withRepo action =
  withCanonicalTemp $ \tmp -> do
    let dir = tmp </> "repo"
    Dir.createDirectory dir
    git dir ["init", "-q"]
    action dir

-- | Probe with a clean HOME, with the given PATH entries (none: ours).
probe :: Microseconds -> [FilePath] -> Root -> IO (Either GitError Listing)
probe limit dirs root =
  withCanonicalTemp $ \home -> do
    setup <- gitSetup home limit
    let setup' = if null dirs then setup else setup {searchPath = dirs}
    probeGitWith setup' quiet root

-- | Probe and list, like the program does. Fails unless git is used.
listWithGit :: FilePath -> IO FileList
listWithGit dir = do
  r <- probe gitProbeTimeLimit [] (Root dir)
  case r of
    Left e -> assertFailure ("git error (git 2.44 or newer needed): " <> show e)
    Right ListFromDisk -> assertFailure "git not used"
    Right listing -> do
      canon <- canonicalRoot (Root dir)
      listFiles quiet canon listing >>= \case
        Left e -> assertFailure ("listing failed: " <> show e)
        Right fl -> pure fl

names :: FileList -> [(Text, EntryKind)]
names fl = [(relPathText p, k) | (p, k) <- Map.toList fl.entries]

walk :: FilePath -> IO WalkResult
walk dir = do
  canon <- canonicalRoot (Root dir)
  r <- timeout 5_000_000 (walkFiles canon 1000)
  maybe (assertFailure "walk hung") pure r

walkNames :: WalkResult -> [(Text, EntryKind)]
walkNames w = Map.toList (Map.fromList [(relPathText p, k) | (p, k) <- w.entries])

readAt :: FilePath -> BS.ByteString -> EntryKind -> IO (Either ReadError FileContent)
readAt dir path kind = do
  canon <- canonicalRoot (Root dir)
  case relPathFromBytes path of
    Nothing -> assertFailure "bad path"
    Just p -> readEntry canon p kind

rel :: BS.ByteString -> Maybe RelPath
rel = relPathFromBytes

unlock :: FilePath -> Assertion
unlock p =
  Dir.setPermissions p $
    Dir.setOwnerReadable True $
      Dir.setOwnerWritable True $
        Dir.setOwnerSearchable True Dir.emptyPermissions
