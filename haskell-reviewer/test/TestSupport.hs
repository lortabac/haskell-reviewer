-- | Helpers shared by several test modules: temp dirs, git repos,
-- scripts.
module TestSupport
  ( -- * Temp dirs
    withCanonicalTemp,
    warmUpTempDirs,
    isRootUser,

    -- * git
    git,
    gitOut,
    gitSetup,

    -- * Scripts
    writeScript,
    writeExecutable,

    -- * Fds
    withFdLock,
    listFdsTo,
    baselineFds,
    assertFds,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Effects.Clock (Microseconds)
import HaskellReviewer.Effects.Git (GitSetup (..))
import System.Directory qualified as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, (</>))
import System.IO (IOMode (ReadMode), hGetContents', readFile', withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
  ( OpenFileFlags (..),
    OpenMode (WriteOnly),
    closeFd,
    defaultFileFlags,
    openFd,
  )
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.User (getEffectiveUserID)
import System.Process
  ( CreateProcess (..),
    StdStream (CreatePipe, UseHandle),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
  )
import Test.Tasty.HUnit (assertFailure)

-- * Temp dirs

-- | A fresh temp dir, with symlinks resolved (macOS has /tmp ->
-- /private/tmp).
withCanonicalTemp :: (FilePath -> IO a) -> IO a
withCanonicalTemp action =
  withSystemTempDirectory "haskell-reviewer-test" $ \tmp ->
    Dir.canonicalizePath tmp >>= action

-- | Make and remove one temp dir. Temp dir names go through
-- 'Text.Printf.printf' (in temporary), and printf has shared values:
-- build them once, before tests make temp dirs in parallel. See Note
-- [Shared values in tests] in "HaskellReviewer.Core.PackageGlobTest".
warmUpTempDirs :: IO ()
warmUpTempDirs = withCanonicalTemp (\_ -> pure ())

-- | Root can read anything: tests of unreadable files make no sense.
isRootUser :: IO Bool
isRootUser = (== 0) <$> getEffectiveUserID

-- * git

-- | Run the git on PATH in a dir, for test set-up. Fails the test if
-- git fails. Author and dates are fixed, and the user's config is not
-- read, so set-up is the same everywhere.
git :: FilePath -> [String] -> IO ()
git dir args = void (gitOut dir args)

-- | Like 'git', and gives back stdout.
gitOut :: FilePath -> [String] -> IO String
gitOut dir args = do
  path <- fromMaybe "/usr/bin:/bin" <$> lookupEnv "PATH"
  let cp =
        (proc "git" args)
          { cwd = Just dir,
            close_fds = True,
            env =
              Just
                [ ("PATH", path),
                  ("HOME", dir),
                  ("GIT_CONFIG_NOSYSTEM", "1"),
                  ("GIT_CONFIG_GLOBAL", "/dev/null"),
                  ("GIT_AUTHOR_NAME", "Test"),
                  ("GIT_AUTHOR_EMAIL", "test@example.com"),
                  ("GIT_AUTHOR_DATE", "2026-01-01T00:00:00Z"),
                  ("GIT_COMMITTER_NAME", "Test"),
                  ("GIT_COMMITTER_EMAIL", "test@example.com"),
                  ("GIT_COMMITTER_DATE", "2026-01-01T00:00:00Z")
                ]
          }
  (code, out, err) <- readCreateProcessWithExitCode cp ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> assertFailure ("git " <> unwords args <> " failed: " <> err)

-- | How the program would find git, but with HOME and XDG_CONFIG_HOME
-- in the given (empty) dir: the developer's own git config can't change
-- the tests.
gitSetup :: FilePath -> Microseconds -> IO GitSetup
gitSetup home limit = do
  path <- fromMaybe "/usr/bin:/bin" <$> lookupEnv "PATH"
  pure
    GitSetup
      { searchPath = splitSearchPath path,
        inherited =
          [ ("PATH", path),
            ("HOME", home),
            ("XDG_CONFIG_HOME", home </> ".config")
          ],
        probeTimeLimit = limit,
        listTimeLimit = limit
      }

-- * Scripts

-- | Write an executable shell script. Gives back its path.
writeScript :: FilePath -> String -> [String] -> IO FilePath
writeScript dir name body = do
  let file = dir </> name
  writeExecutable file (unlines ("#!/bin/sh" : body))
  pure file

-- | Write an executable file through a close-on-exec fd. A plain
-- write fd could leak into a child forked meanwhile by another test;
-- the kernel then refuses to run the file (ETXTBSY) while it's open.
writeExecutable :: FilePath -> String -> IO ()
writeExecutable file text =
  bracket open closeFd $ \fd -> writeAll fd (Text.Encoding.encodeUtf8 (Text.pack text))
  where
    open =
      openFd
        file
        WriteOnly
        defaultFileFlags {creat = Just 0o755, trunc = True, cloexec = True}
    writeAll fd bytes =
      unless (BS.null bytes) $ do
        n <- fdWrite fd bytes
        writeAll fd (BS.drop (fromIntegral n) bytes)

-- * Fds

-- | Tests run in parallel. The Spawn tests change the fd limit and
-- open a plain fd above the cap for a moment; the tests that check
-- what a child inherits must not run meanwhile. Both take this lock.
withFdLock :: IO a -> IO a
withFdLock action = withMVar fdLock (const action)

fdLock :: MVar ()
fdLock = unsafePerformIO (newMVar ())
{-# NOINLINE fdLock #-}

-- | A shell line that writes @ls -l /dev/fd/@ to the file (a shell
-- word, quoted by the caller). One command, its own redirect: the shell
-- holds no extra fd while ls runs.
listFdsTo :: String -> String
listFdsTo word = "/bin/ls -l /dev/fd/ > " <> word

-- | The fds a clean child sees with 'listFdsTo', run from a /bin/sh
-- script in the given dir: what a fake git should see too. ls itself
-- holds some (Linux: /dev/fd; macOS: also the cwd, to get back to it).
-- The script and its output go in the first dir.
baselineFds :: FilePath -> FilePath -> IO [Int]
baselineFds tmp dir = do
  let out = tmp </> "baseline.fds"
  script <- writeScript tmp "baseline" [listFdsTo ("'" <> out <> "'")]
  -- createProcess closes devNull for us.
  withBinaryFile "/dev/null" ReadMode $ \devNull -> do
    let cp =
          (proc script [])
            { cwd = Just dir,
              std_in = UseHandle devNull,
              std_out = CreatePipe,
              std_err = CreatePipe,
              close_fds = True
            }
    (_, hOut, hErr, ph) <- createProcess cp
    mapM_ hGetContents' hOut
    mapM_ hGetContents' hErr
    void (waitForProcess ph)
  fst <$> readFds out

-- | The fds in a 'listFdsTo' file must be the baseline. On failure, show
-- the listing, and @<file without .fds>.lsof@ if there.
assertFds :: [Int] -> FilePath -> IO ()
assertFds baseline file = do
  (fds, listing) <- readFds file
  let lsofFile = take (length file - length (".fds" :: String)) file <> ".lsof"
  lsof <- either (const "(none)") id <$> try @IOException (readFile' lsofFile)
  unless (fds == baseline) $
    assertFailure $
      "fds seen: "
        <> show fds
        <> ", clean child: "
        <> show baseline
        <> "\nls -l /dev/fd/:\n"
        <> listing
        <> "lsof of the shell:\n"
        <> lsof

-- | Sorted fd numbers, and the raw listing.
readFds :: FilePath -> IO ([Int], String)
readFds file = do
  listing <- readFile' file
  let entries = filter (not . ("total" `isPrefixOf`)) (lines listing)
  pure (sort (map fdOf entries), listing)
  where
    -- The name is before "->" if it's a link (Linux), else last (macOS).
    fdOf line = case break (== "->") (words line) of
      (before@(_ : _), _ : _) -> read (last before)
      (ws, _) -> read (last ws)
