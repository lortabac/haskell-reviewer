-- | Running git, hardened (SPEC §11.4.1, D-53, D-75).
module HaskellReviewer.Effects.Git
  ( -- * Types
    Git (..),
    GitSetup (..),
    Listing (..),
    GitError (..),
    TooOld (..),
    Unreadable (..),
    CannotRun (..),
    Call (..),
    Failure (..),
    LsFilesOutput (..),

    -- * Operations
    probeGit,
    probeGitWith,
    lsFiles,
    renderGitError,
    gitProbeTimeLimit,
    gitListTimeLimit,

    -- * Internal (for tests)
    hardenedArgs,
    hardenedEnv,
  )
where

import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.IO.Exception (IOException (..))
import HaskellReviewer.Core.Git
  ( GitVersion (..),
    minimumGitVersion,
    parseGitVersion,
    renderGitVersion,
  )
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.Helper (findHelperIn, safePathDirs)
import HaskellReviewer.Effects.Log (Logger, logVerbose)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath)
import System.IO (Handle, IOMode (ReadMode), hClose, withBinaryFile)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe, UseHandle),
    createProcess,
    getPid,
    proc,
    waitForProcess,
  )
import System.Timeout (timeout)

{- Note [Git hardening]

The repo under review is untrusted (SPEC §9.1): its config, hooks and
index come from someone else. Git must only read. Every call:

- gets an environment built from scratch:
  - HOME and XDG_CONFIG_HOME, if set, so the user's config (and its
    safe.directory) still applies;
  - PATH with only absolute entries outside ROOT (same check as
    Note [Helper lookup] in "HaskellReviewer.Effects.Helper");
  - LC_ALL=C, so messages are in English and we can match them;
  - GIT_OPTIONAL_LOCKS=0: no index refresh, so no write to .git;
  - GIT_NO_LAZY_FETCH=1: a partial clone never goes to the network;
  - GIT_NO_REPLACE_OBJECTS=1: refs/replace can't swap content;
  - GIT_TERMINAL_PROMPT=0: never prompt.
  Everything else is dropped: GIT_DIR, GIT_CONFIG_PARAMETERS, LANG,
  PAGER, EDITOR, ...
- gets these flags first:
  - --no-pager;
  - -c core.fsmonitor=false: repo config can't run a command;
  - -c core.untrackedCache=false: no cache to write;
  - -c core.hooksPath=/dev/null: no hooks, ever;
  - -c safe.bareRepository=explicit: a bare repo planted in the tree
    is not picked up.
- has stdin on /dev/null and no inherited fds.
- has a time limit: 30 s at start-up ('gitProbeTimeLimit'), 120 s for
  ls-files ('gitListTimeLimit'). A FIFO in the tree (named
  .gitignore, say, or set as core.excludesFile) makes git block in
  open() for ever. Past the limit we kill git (SIGKILL) and wait for it.
  We do the same when the call is cancelled (shutdown), so no git is
  left behind.
- has its output capped: 256 MiB of stdout, 4 KiB of stderr kept.

git itself is found with findHelper: never a binary inside ROOT. We need
2.44 or newer (for GIT_NO_LAZY_FETCH). An older one, or one whose
version we can't read, is exit 2, even if ROOT is not a repo: better to
say so at once than when diff mode needs it. On macOS, /usr/bin/git
without the Command Line Tools is a shim that fails --version: also
exit 2.

We only run --version, rev-parse --is-inside-work-tree and ls-files.
None of them writes.
-}

-- | How to run git.
data Git = Git
  { -- | Found by findHelper.
    exe :: FilePath,
    -- | ROOT.
    cwd :: FilePath,
    -- | See Note [Git hardening].
    env :: [(String, String)],
    -- | For --version and rev-parse, at start-up.
    probeTimeLimit :: Microseconds,
    -- | For ls-files: a big tree takes longer.
    listTimeLimit :: Microseconds
  }

-- | Where 'probeGitWith' looks for git and what it starts from.
data GitSetup = GitSetup
  { -- | The PATH entries.
    searchPath :: [FilePath],
    -- | Our environment. Only a few variables are passed on.
    inherited :: [(String, String)],
    -- | See 'Git'.
    probeTimeLimit :: Microseconds,
    -- | See 'Git'.
    listTimeLimit :: Microseconds
  }

-- | How to list the files of ROOT.
data Listing
  = -- | ROOT is in a git work tree.
    ListFromGit Git
  | -- | No git, or ROOT is not in a work tree.
    ListFromDisk

-- | git can't be used. At start-up, these are exit 2.
data GitError
  = GitTooOld TooOld
  | GitVersionUnreadable Unreadable
  | GitCannotRun CannotRun
  | GitFailed Failure
  | GitTimedOut Call
  | -- | More stdout than we take.
    GitOutputTooLarge Call
  deriving (Eq, Show)

-- | git is older than 'minimumGitVersion'.
data TooOld = TooOld
  { exe :: FilePath,
    version :: GitVersion
  }
  deriving (Eq, Show)

-- | @git --version@ printed something we can't read.
data Unreadable = Unreadable
  { exe :: FilePath,
    -- | The first line it printed.
    firstLine :: Text
  }
  deriving (Eq, Show)

-- | git could not be started.
data CannotRun = CannotRun
  { exe :: FilePath,
    -- | From the OS.
    reason :: Text
  }
  deriving (Eq, Show)

-- | One git call.
data Call = Call
  { exe :: FilePath,
    args :: [String]
  }
  deriving (Eq, Show)

-- | git ran, but failed.
data Failure = Failure
  { exe :: FilePath,
    args :: [String],
    code :: Int,
    -- | As git printed it, cut at 4 KiB.
    stderr :: Text
  }
  deriving (Eq, Show)

-- | The raw output of the two ls-files calls.
data LsFilesOutput = LsFilesOutput
  { -- | @ls-files -z --stage@.
    staged :: ByteString,
    -- | @ls-files -z --others --exclude-standard@.
    others :: ByteString
  }

-- | 30 s per call at start-up. See Note [Git hardening].
gitProbeTimeLimit :: Microseconds
gitProbeTimeLimit = Microseconds 30_000_000

-- | 120 s per ls-files call.
gitListTimeLimit :: Microseconds
gitListTimeLimit = Microseconds 120_000_000

-- | Message for the user, without the program name.
renderGitError :: GitError -> Text
renderGitError = \case
  GitTooOld e ->
    "git at "
      <> Text.pack e.exe
      <> " is version "
      <> renderGitVersion e.version
      <> "; need "
      <> needed
      <> " or newer"
  GitVersionUnreadable e ->
    "cannot read the version of git at " <> Text.pack e.exe <> ": " <> e.firstLine
  GitCannotRun e ->
    "cannot run git at " <> Text.pack e.exe <> ": " <> e.reason
  GitFailed f -> "git failed in ROOT: " <> f.stderr
  GitTimedOut c -> "git took too long in ROOT: " <> callText c
  GitOutputTooLarge c -> "git printed too much in ROOT: " <> callText c
  where
    needed =
      Text.pack (show minimumGitVersion.major)
        <> "."
        <> Text.pack (show minimumGitVersion.minor)
    callText c = Text.pack (unwords ("git" : c.args))

-- | Find git, check its version, and see if ROOT is in a work tree.
-- See Note [Git hardening].
--
-- No git on PATH, or ROOT not in a work tree: 'ListFromDisk'. git
-- refusing ROOT (dubious ownership, broken repo, ...) is an error: we
-- don't silently show something else.
probeGit :: Logger -> Root -> IO (Either GitError Listing)
probeGit logger root = do
  env <- getEnvironment
  let path = fromMaybe "" (lookup "PATH" env)
  let setup =
        GitSetup
          { searchPath = splitSearchPath path,
            inherited = env,
            probeTimeLimit = gitProbeTimeLimit,
            listTimeLimit = gitListTimeLimit
          }
  probeGitWith setup logger root

-- | Like 'probeGit', with PATH, environment and time limit given.
probeGitWith :: GitSetup -> Logger -> Root -> IO (Either GitError Listing)
probeGitWith setup logger root = do
  found <- findHelperIn setup.searchPath root "git"
  case found of
    Nothing -> do
      logVerbose logger "git: not found on PATH, listing files from disk"
      pure (Right ListFromDisk)
    Just exe -> do
      dirs <- safePathDirs root setup.searchPath
      let git =
            Git
              { exe = exe,
                cwd = root.path,
                env = hardenedEnv setup.inherited dirs,
                probeTimeLimit = setup.probeTimeLimit,
                listTimeLimit = setup.listTimeLimit
              }
      versionResult <- checkVersion git
      case versionResult of
        Left e -> pure (Left e)
        Right v -> do
          logVerbose logger ("git: " <> Text.pack exe <> " " <> renderGitVersion v)
          inWorkTree logger git

-- | List the files under ROOT with git. No pathspec: git runs in ROOT
-- and gives paths relative to it, only those under it.
lsFiles :: Git -> IO (Either GitError LsFilesOutput)
lsFiles git = do
  (staged, others) <-
    concurrently
      (runOk git ["ls-files", "-z", "--stage"])
      (runOk git ["ls-files", "-z", "--others", "--exclude-standard"])
  pure (LsFilesOutput <$> staged <*> others)
  where
    runOk = runOkWithin git.listTimeLimit

-- | The flags in front of every call. See Note [Git hardening].
hardenedArgs :: [String] -> [String]
hardenedArgs args =
  [ "--no-pager",
    "-c",
    "core.fsmonitor=false",
    "-c",
    "core.untrackedCache=false",
    "-c",
    "core.hooksPath=/dev/null",
    "-c",
    "safe.bareRepository=explicit"
  ]
    <> args

-- | The environment for git, from ours and the safe PATH entries. See
-- Note [Git hardening].
hardenedEnv :: [(String, String)] -> [FilePath] -> [(String, String)]
hardenedEnv inherited pathDirs =
  [(k, v) | k <- ["HOME", "XDG_CONFIG_HOME"], Just v <- [lookup k inherited]]
    <> [ ("PATH", intercalate ":" pathDirs),
         ("LC_ALL", "C"),
         ("GIT_OPTIONAL_LOCKS", "0"),
         ("GIT_NO_LAZY_FETCH", "1"),
         ("GIT_NO_REPLACE_OBJECTS", "1"),
         ("GIT_TERMINAL_PROMPT", "0")
       ]

-- * Internal

-- | What one git call gave.
data GitResult = GitResult
  { exitCode :: ExitCode,
    out :: ByteString,
    err :: ByteString
  }

checkVersion :: Git -> IO (Either GitError GitVersion)
checkVersion git = do
  result <- runGit git.probeTimeLimit git ["--version"]
  pure $ case result of
    Left e -> Left e
    Right r -> case (r.exitCode, parseGitVersion (decode r.out)) of
      (ExitSuccess, Just v)
        | v >= minimumGitVersion -> Right v
        | otherwise -> Left (GitTooOld TooOld {exe = git.exe, version = v})
      _ ->
        Left (GitVersionUnreadable Unreadable {exe = git.exe, firstLine = firstLine r})
  where
    firstLine r =
      let printed = if BS.null r.out then r.err else r.out
       in Text.strip (Text.takeWhile (/= '\n') (decode printed))

inWorkTree :: Logger -> Git -> IO (Either GitError Listing)
inWorkTree logger git = do
  let args = ["rev-parse", "--is-inside-work-tree"]
  result <- runGit git.probeTimeLimit git args
  case result of
    Left e -> pure (Left e)
    Right r -> case r.exitCode of
      ExitSuccess
        | Text.strip (decode r.out) == "true" -> pure (Right (ListFromGit git))
        | otherwise -> disk "ROOT is not in a work tree"
      ExitFailure _
        | notARepo r.err -> disk "ROOT is not in a git repository"
      ExitFailure c -> pure (Left (GitFailed (failure git args c r)))
  where
    disk why = do
      logVerbose logger ("git: " <> why <> ", listing files from disk")
      pure (Right ListFromDisk)

-- | git found no repo at all, from ROOT up. Not "fatal: not a git
-- repository: <path>", which is a .git file pointing nowhere: a broken
-- repo, so an error.
notARepo :: ByteString -> Bool
notARepo err =
  any
    ("fatal: not a git repository (or any" `Text.isPrefixOf`)
    (Text.lines (decode err))

-- | Run, and fail on a non-zero exit.
runOkWithin :: Microseconds -> Git -> [String] -> IO (Either GitError ByteString)
runOkWithin limit git args = do
  result <- runGit limit git args
  pure $ case result of
    Left e -> Left e
    Right r -> case r.exitCode of
      ExitSuccess -> Right r.out
      ExitFailure c -> Left (GitFailed (failure git args c r))

failure :: Git -> [String] -> Int -> GitResult -> Failure
failure git args c r =
  Failure
    { exe = git.exe,
      args = args,
      code = c,
      stderr = Text.stripEnd (decode r.err)
    }

-- | Most stdout we take from one call.
maxStdout :: Int
maxStdout = 256 * 1024 * 1024

-- | Most stderr we keep. The rest is read and dropped.
maxStderr :: Int
maxStderr = 4096

-- | Run git with the hardened flags and env. stdout and stderr are
-- read at the same time, so a full pipe can't block git. On time out,
-- too much output, an exception or cancel, git is killed and waited
-- for. See Note [Git hardening].
runGit :: Microseconds -> Git -> [String] -> IO (Either GitError GitResult)
runGit (Microseconds limit) git args = do
  result <- try @IOException $
    withBinaryFile "/dev/null" ReadMode $ \devNull -> do
      let cp =
            (proc git.exe (hardenedArgs args))
              { cwd = Just git.cwd,
                env = Just git.env,
                std_in = UseHandle devNull,
                std_out = CreatePipe,
                std_err = CreatePipe,
                close_fds = True
              }
      bracket (createProcess cp) cleanUp $ \case
        (_, Just hOut, Just hErr, ph) -> do
          -- The wait is in the limit too: git can close its pipes and
          -- go on running.
          outcome <-
            timeout limit $
              withAsync (readOutput DropRest maxStderr hErr) $ \errReader -> do
                out <- readOutput FailOnMore maxStdout hOut
                case out of
                  -- Stop here: cleanUp kills git.
                  Nothing -> pure Nothing
                  Just o -> do
                    err <- fromMaybe "" <$> wait errReader
                    code <- waitForProcess ph
                    pure (Just GitResult {exitCode = code, out = o, err = err})
          pure $ case outcome of
            Nothing -> Left (GitTimedOut call)
            Just Nothing -> Left (GitOutputTooLarge call)
            Just (Just r) -> Right r
        _ -> ioError (userError "no pipes")
  pure $ case result of
    Left e -> Left (GitCannotRun CannotRun {exe = git.exe, reason = reasonOf e})
    Right r -> r
  where
    call = Call {exe = git.exe, args = args}
    reasonOf e
      | null e.ioe_description = Text.pack (displayException e)
      | otherwise = Text.pack e.ioe_description

-- | Kill git if still there, close the pipes, reap it.
cleanUp :: (a, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanUp (_, hOut, hErr, ph) = do
  pid <- getPid ph
  mapM_ (\p -> void (try @IOException (signalProcess sigKILL p))) pid
  mapM_ hClose hOut
  mapM_ hClose hErr
  void (try @IOException (waitForProcess ph))

-- | What 'readOutput' does past its size.
data Overflow
  = -- | Give up: 'Nothing'.
    FailOnMore
  | -- | Keep the first bytes, read and drop the rest.
    DropRest

-- | Read to the end, at most the given size.
readOutput :: Overflow -> Int -> Handle -> IO (Maybe ByteString)
readOutput overflow cap h = go 0 []
  where
    go n acc = do
      chunk <- BS.hGetSome h 65536
      let n' = n + BS.length chunk
          done = BS.take cap (BS.concat (reverse acc))
      case overflow of
        _ | BS.null chunk -> pure (Just done)
        FailOnMore | n' > cap -> pure Nothing
        DropRest | n >= cap -> go n acc
        _ -> go n' (chunk : acc)

decode :: ByteString -> Text
decode = Text.Encoding.decodeUtf8Lenient
