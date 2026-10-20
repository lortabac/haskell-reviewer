-- | Helpers shared by several test modules: temp dirs, git repos,
-- scripts.
module TestSupport
  ( -- * Temp dirs
    withCanonicalTemp,
    isRootUser,

    -- * git
    git,
    gitOut,
    gitSetup,

    -- * Scripts
    writeScript,
    writeExecutable,
  )
where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Effects.Clock (Microseconds)
import HaskellReviewer.Effects.Git (GitSetup (..))
import System.Directory qualified as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO
  ( OpenFileFlags (..),
    OpenMode (WriteOnly),
    closeFd,
    defaultFileFlags,
    openFd,
  )
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.User (getEffectiveUserID)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty.HUnit (assertFailure)

-- * Temp dirs

-- | A fresh temp dir, with symlinks resolved (macOS has /tmp ->
-- /private/tmp).
withCanonicalTemp :: (FilePath -> IO a) -> IO a
withCanonicalTemp action =
  withSystemTempDirectory "haskell-reviewer-test" $ \tmp ->
    Dir.canonicalizePath tmp >>= action

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
