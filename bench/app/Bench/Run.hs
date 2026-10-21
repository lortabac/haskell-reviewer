-- | Runs the product binary once against one project and measures the
-- SPEC §10 numbers.
module Bench.Run
  ( -- * Types
    RunConfig (..),
    RunResult (..),
    FileSource (..),
    RunError (..),
    renderRunError,

    -- * Running
    runOnce,
  )
where

import Bench.Http (HttpResponse (..), rawHttp, requestBytes)
import Bench.RtsStats (RtsStats (..), parseRtsStats, renderRtsStatsError)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception
  ( Exception,
    IOException,
    SomeAsyncException,
    SomeException,
    bracket,
    displayException,
    fromException,
    throwIO,
    try,
    uninterruptibleMask_,
  )
import Control.Monad (unless, void, when)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO
  ( Handle,
    IOMode (ReadMode),
    hClose,
    hGetContents',
    hGetLine,
    hIsEOF,
    hPutStrLn,
    openFile,
    readFile',
    stderr,
  )
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe, UseHandle),
    createProcess,
    getPid,
    getProcessExitCode,
    proc,
  )
import System.Timeout (timeout)

-- * Types

-- | What one run needs.
data RunConfig = RunConfig
  { -- | Path to the product binary (linked with @-rtsopts@).
    exe :: FilePath,
    projectDir :: FilePath,
    -- | Where to write this run's trace file and RTS stats file.
    tmpDir :: FilePath,
    -- | Index inside its project (used only for file names).
    index :: Int
  }

-- | Everything one run measures.
--
-- Most timings are microseconds since right before 'createProcess', so
-- they include exec and RTS init (D-85). 'stopUs' is its own interval:
-- from the stop signal to exit.
data RunResult = RunResult
  { urlUs :: Integer,
    serveUs :: Integer,
    filesUs :: Integer,
    -- | From the trace file's @startup@\/@files@ spans. 'Nothing' if the
    -- span didn't show up within the poll window.
    startupSpanUs :: Maybe Integer,
    filesSpanUs :: Maybe Integer,
    stopUs :: Integer,
    -- | Always 0: a non-zero code fails the run below, before a
    -- 'RunResult' is built. Kept as a field anyway: the JSON schema
    -- wants it, and a reader shouldn't have to know that.
    exitCode :: Int,
    peakMemBytes :: Integer,
    maxLiveBytes :: Maybe Integer,
    allocatedBytes :: Maybe Integer,
    gcCpuUs :: Maybe Integer,
    mutatorCpuUs :: Maybe Integer,
    -- | Entries in the project's file tree, and where it came from.
    -- Same for every run of a project (the project doesn't change),
    -- read once from the @api\/files\/tree@ check.
    filesCount :: Int,
    source :: FileSource
  }
  deriving (Eq, Show)

-- | Where the product read the file list from (@api\/files\/tree@'s
-- @source@ field).
data FileSource = FromGit | FromDisk
  deriving (Eq, Show)

-- | Why a run didn't produce a result. Unlike missing a §10 target,
-- this always fails the harness (SPEC §10.3: timings never do).
newtype RunError = RunError String
  deriving (Eq, Show)

-- | A message for the user.
renderRunError :: RunError -> String
renderRunError (RunError s) = s

-- * Running

-- | Run the product binary once, measuring it. Leaves no process
-- behind, on success or failure.
runOnce :: RunConfig -> IO (Either RunError RunResult)
runOnce cfg = bracket (openFile "/dev/null" ReadMode) hClose $ \devNull -> do
  t0 <- getMonotonicTimeNSec
  -- A stale file from an earlier run at the same index (the warm-up,
  -- say) must not look like this run's: if the product dies before
  -- writing a fresh one, we want that to fail the run, not silently
  -- read someone else's memory numbers.
  _ <- try @IOException (removeFile (rtsFile cfg)) :: IO (Either IOException ())
  -- Inherited, minus GHCRTS: -rtsopts (needed for +RTS -t) also means
  -- the product now reads GHCRTS, and a developer's own GHCRTS (-N,
  -- -A, ...) would silently change every number.
  childEnv <- filter ((/= "GHCRTS") . fst) <$> getEnvironment
  let cp =
        (proc cfg.exe (runArgs cfg))
          { std_in = UseHandle devNull,
            std_out = CreatePipe,
            std_err = CreatePipe,
            -- True would close every fd up to our (uncapped) fd limit in
            -- the child: slow. Our fds aren't secret and we spawn from one
            -- thread. See Note [No inherited fds] in HaskellReviewer.Effects.Spawn.
            close_fds = False,
            env = Just childEnv
          }
  bracket (createProcess cp) killAndReap $ \(_, mout, merr, ph) ->
    case (mout, merr) of
      (Just out, Just err) -> do
        result <- try @SomeException (steps t0 cfg out err ph)
        either toRunError (pure . Right) result
      _ -> pure (Left (RunError "no stdout/stderr pipes"))
  where
    -- Any failure in 'steps' becomes a 'RunError': not just our own
    -- 'RunFailure', but also a stray 'IOException' (a closed pipe, a
    -- missing RTS stats file, ...). An async exception (Ctrl-C, the
    -- harness's own timeout) is not a run failure, so it's rethrown:
    -- 'bracket' still cleans up the process either way.
    toRunError e
      | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
      | Just (RunFailure msg) <- fromException e = pure (Left (RunError msg))
      | otherwise = pure (Left (RunError (displayException e)))

runArgs :: RunConfig -> [String]
runArgs cfg =
  [ "--no-open",
    "--idle-exit",
    "0",
    "--trace-file",
    traceFile cfg,
    "+RTS",
    "-t" <> rtsFile cfg,
    "--machine-readable",
    "-RTS",
    cfg.projectDir
  ]

rtsFile :: RunConfig -> FilePath
rtsFile cfg = cfg.tmpDir <> "/rts-" <> show cfg.index <> ".txt"

traceFile :: RunConfig -> FilePath
traceFile cfg = cfg.tmpDir <> "/run-" <> show cfg.index <> ".jsonl"

-- | Same process cleanup as "ExeTest"'s @kill@ (works whether the
-- process is still running or was already reaped by a normal stop),
-- plus closing the stdout\/stderr pipes: left to the GC, they'd stay
-- open for the whole benchmark run.
--
-- Reaping polls, with a bound, rather than blocking in
-- 'waitForProcess': see Note [Polling for exit, not waitForProcess]
-- below. SIGKILL can't be ignored, so this should only ever take one
-- or two polls. If it somehow doesn't, we give up on reaping but
-- leave the pipes open rather than close them under a reader that's
-- still using them (see the warning this prints): a 'Handle' close
-- while another thread is mid-read on it can itself block, which is
-- exactly the kind of hang this whole function exists to avoid.
--
-- Wrapped in 'uninterruptibleMask_' (not just the ordinary masking
-- 'bracket' already gives its cleanup action): this is bounded to a
-- few seconds, so a second Ctrl-C arriving mid-cleanup must not skip
-- the SIGKILL and leave the product running.
killAndReap :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
killAndReap (_, mout, merr, ph) = uninterruptibleMask_ $ do
  pid <- getPid ph
  mapM_ (\p -> try @IOException (signalProcess sigKILL p)) pid
  reaped <- reapPolling (1000 :: Int)
  if reaped
    then do
      mapM_ (\h -> try @IOException (hClose h)) mout
      mapM_ (\h -> try @IOException (hClose h)) merr
    else
      hPutStrLn
        stderr
        "haskell-reviewer-bench: warning: a process did not exit after SIGKILL; \
        \leaving its pipes open"
  where
    reapPolling 0 = pure False
    reapPolling n = do
      found <- getProcessExitCode ph
      case found of
        Just _ -> pure True
        Nothing -> threadDelay 5000 >> reapPolling (n - 1)

-- * Internal

-- | Used only inside 'steps', to short-circuit on the first failure.
-- Caught once, in 'runOnce', and turned into a 'RunError'. Simpler than
-- threading @Either RunError@ through every step by hand, and it never
-- escapes this module.
newtype RunFailure = RunFailure String
  deriving (Show)

instance Exception RunFailure

failWith :: String -> IO a
failWith = throwIO . RunFailure

steps :: Word64 -> RunConfig -> Handle -> Handle -> ProcessHandle -> IO RunResult
steps t0 cfg out err ph = do
  seen <- newIORef []
  urlVar <- newEmptyMVar
  stdoutVar <- newEmptyMVar
  -- Both pipes are drained for the whole run, not just up to the URL
  -- line: a chatty product could otherwise fill the OS pipe buffer and
  -- block on a later write, long after we've stopped reading. These
  -- threads are plain 'forkIO', never joined: if we give up on them
  -- (the process never exits, say), they're simply abandoned, and die
  -- on their own once 'killAndReap' (in 'runOnce') kills the process
  -- and closes the handles. A structured-concurrency join here (e.g.
  -- 'Control.Concurrent.Async.withAsync') would be wrong: it would
  -- block *us* waiting for a reader that's stuck on a pipe whose
  -- writer is still alive, which is exactly the deadlock this is
  -- avoiding.
  _ <- forkIO (drainStderr err seen urlVar)
  _ <- forkIO (drainStdout out stdoutVar)

  urlLine <- readUrlLine seen urlVar
  t1 <- getMonotonicTimeNSec
  (port, token) <- maybe (failWith ("bad URL line: " <> urlLine)) pure (parseUrl urlLine)

  checkRoot port token
  t2 <- getMonotonicTimeNSec

  (filesN, src) <- checkFilesResponse port token
  t3 <- getMonotonicTimeNSec

  (startupSpan, filesSpan) <- pollSpans (traceFile cfg)

  tSignal <- getMonotonicTimeNSec
  pid <- maybe (failWith "process already gone before stop") pure =<< getPid ph
  signalProcess sigTERM pid
  exitC <- waitForExit ph
  tExit <- getMonotonicTimeNSec

  unless (exitC == 0) $ failWith ("exit code " <> show exitC)

  -- The process has exited, so drainStdout's hGetContents' should
  -- already be done or finishing right now; the timeout is only a
  -- safety net, not expected to fire.
  leftoverOut <-
    maybe (failWith "reading stdout timed out") pure
      =<< timeout 5000000 (takeMVar stdoutVar)
  unless (null leftoverOut) $ failWith ("stdout not empty: " <> show leftoverOut)

  rtsRaw <- readFile' (rtsFile cfg)
  rts <- either (failWith . renderRtsStatsError) pure (parseRtsStats rtsRaw)

  pure
    RunResult
      { urlUs = microsSince t0 t1,
        serveUs = microsSince t0 t2,
        filesUs = microsSince t0 t3,
        startupSpanUs = startupSpan,
        filesSpanUs = filesSpan,
        stopUs = microsSince tSignal tExit,
        exitCode = exitC,
        peakMemBytes = rts.peakMemBytes,
        maxLiveBytes = rts.maxLiveBytes,
        allocatedBytes = rts.allocatedBytes,
        gcCpuUs = rts.gcCpuUs,
        mutatorCpuUs = rts.mutatorCpuUs,
        filesCount = filesN,
        source = src
      }
  where
    checkRoot port token = do
      r <- httpGetOrFail port token "" "/"
      unless (r.status == 200) $ failWith ("/: expected 200, got " <> show r.status)

    checkFilesResponse port token = do
      r <- httpGetOrFail port token "api/files/tree" "files"
      unless (r.status == 200) $ failWith ("files: expected 200, got " <> show r.status)
      checkFilesBody r.body

    -- @path@ is the URL path under the token (no leading slash);
    -- @what@ names the check, for the error message only.
    httpGetOrFail port token path what = do
      let host = "127.0.0.1:" <> show port
          req = requestBytes "GET" (Just host) ("/" <> token <> "/" <> path)
      res <- rawHttp port req
      either (failWith . ((what <> ": HTTP: ") <>)) pure res

checkFilesBody :: BS.ByteString -> IO (Int, FileSource)
checkFilesBody body = case Aeson.eitherDecodeStrict body of
  Left e -> failWith ("files: cannot decode JSON: " <> e)
  Right v -> case Aeson.Types.parseEither parser v of
    Left e -> failWith ("files: " <> e)
    Right r -> pure r
  where
    parser = Aeson.withObject "files" $ \o -> do
      entries <- o .: "entries" :: Aeson.Types.Parser [Aeson.Value]
      srcText <- o .: "source" :: Aeson.Types.Parser Text
      truncated <- o .: "truncated"
      src <- case srcText of
        "git" -> pure FromGit
        "disk" -> pure FromDisk
        other -> fail ("unknown source: " <> Text.unpack other)
      if null entries
        then fail "entries is empty"
        else
          if truncated
            then fail "truncated is true"
            else pure (length entries, src)

-- | Reads stderr, line by line, for as long as the handle stays open.
-- Never joined (see the comment in 'steps'): on an 'IOException' (the
-- handle was closed under it, by 'killAndReap') it just stops, quietly.
--
-- Every line goes into @seen@, for error messages. The first line
-- that starts with 'urlPrefix' also goes into @urlVar@ (once: a
-- 'tryPutMVar' that a later EOF can't overwrite); at EOF with no such
-- line, @urlVar@ gets 'Nothing'. Reading on, past the URL line, keeps
-- the pipe from filling up and blocking the product later in the run.
drainStderr :: Handle -> IORef [String] -> MVar (Maybe String) -> IO ()
drainStderr h seen urlVar = do
  result <- try @IOException go
  case result of
    Right () -> pure ()
    Left _ -> void (tryPutMVar urlVar Nothing)
  where
    go = do
      eof <- hIsEOF h
      if eof
        then void (tryPutMVar urlVar Nothing)
        else do
          l <- hGetLine h
          atomicModifyIORef' seen (\ls -> (ls <> [l], ()))
          when (urlPrefix `isPrefixOf` l) $ void (tryPutMVar urlVar (Just l))
          go

-- | Reads all of stdout in the background (it's supposed to stay
-- empty the whole run), putting the result in @doneVar@ at EOF. Same
-- "never joined" shape as 'drainStderr': on an 'IOException', it puts
-- @""@ so a caller that's still waiting (it shouldn't be, by then) at
-- least doesn't hang.
drainStdout :: Handle -> MVar String -> IO ()
drainStdout h doneVar = do
  result <- try @IOException (hGetContents' h)
  void (tryPutMVar doneVar (either (const "") id result))

-- | Wait for 'drainStderr' to find the URL line, up to 30 s.
readUrlLine :: IORef [String] -> MVar (Maybe String) -> IO String
readUrlLine seen urlVar = do
  result <- timeout 30000000 (takeMVar urlVar)
  case result of
    Just (Just l) -> pure l
    Just Nothing ->
      seenSoFar >>= failWith . ("process exited before the URL line; stderr: " <>)
    Nothing -> seenSoFar >>= failWith . ("no URL line within 30 s; stderr so far: " <>)
  where
    seenSoFar = show <$> readIORef seen

urlPrefix :: String
urlPrefix = "haskell-reviewer: serving at "

{- Note [Polling for exit, not waitForProcess]

'waitForExit' polls 'getProcessExitCode' (non-blocking: it never calls
the OS's blocking wait, just checks what the RTS's SIGCHLD reaper
already recorded) instead of blocking in 'waitForProcess' under a
'System.Timeout.timeout'.

We first tried 'timeout' around 'waitForProcess'. In isolation (a
short script that just sleeps, no pipes), that combination does time
out correctly: 'waitForProcess' is a safe FFI call, interruptible by
an async exception on the @-threaded@ RTS. But with the stderr/stdout
readers of 'steps' running alongside it (reading from a child that's
still alive, pipes still open), a run against a product that ignores
SIGTERM hung for hours instead of failing after 10 s. The 'timeout'
around 'waitForProcess' wasn't the culprit: giving up there still
throws our 'RunFailure' straight away. The real deadlock was
upstream, in an earlier version of this module that used
'Control.Concurrent.Async.withAsync' for the readers: its automatic
cleanup, run while that exception is propagating out of 'steps',
waits for the reader threads to finish — and a reader blocked on a
pipe whose writer (the child) is still alive, because we haven't
killed it yet, never will. Plain, unjoined 'forkIO' threads (see
'drainStderr' and 'drainStdout') don't have that problem: nothing
waits on them, so there's nothing to deadlock on.

Polling here removes any remaining doubt about 'waitForProcess'
itself, which is the one thing we couldn't fully explain.

Verified by hand, in isolation (a script that ignores SIGTERM, with
concurrent 'forkIO' readers on its stdout\/stderr pipes, same shape as
'drainStderr'\/'drainStdout' here): sending the stop signal, polling
for exit, giving up after 10 s, SIGKILLing and reaping all complete
promptly, and nothing is left running. We could not get a clean
same-sandbox repro all the way through the real harness (a port that
refuses connections, or a stand-in HTTP server, both made
'Network.Socket.connect' itself take minutes in this one sandboxed
environment, unrelated to anything above); the isolated test is the
same control flow this module uses, so we're treating it as
sufficient rather than chasing an unrelated sandbox quirk further.
-}

-- | Wait up to 10 s for the process to exit after the stop signal. See
-- Note [Polling for exit, not waitForProcess].
--
-- Polls every 0.5 ms for the first 50 ms, then every 5 ms: most
-- products exit within a few ms of SIGTERM, and 'stopUs' is a number
-- we report, so its resolution matters while it's small. No point
-- keeping that resolution for the rest of the 10 s bound, which only
-- ever gets used by a product that's stuck.
waitForExit :: ProcessHandle -> IO Int
waitForExit ph = fastPoll (100 :: Int)
  where
    fastPoll 0 = slowPoll (1900 :: Int)
    fastPoll n = do
      found <- getProcessExitCode ph
      case found of
        Just code -> pure (exitInt code)
        Nothing -> threadDelay 500 >> fastPoll (n - 1)
    slowPoll 0 = failWith "no exit within 10 s of SIGTERM"
    slowPoll n = do
      found <- getProcessExitCode ph
      case found of
        Just code -> pure (exitInt code)
        Nothing -> threadDelay 5000 >> slowPoll (n - 1)
    exitInt ExitSuccess = 0
    exitInt (ExitFailure c) = c

-- | Split @haskell-reviewer: serving at http:\/\/127.0.0.1:PORT\/TOKEN\/@
-- into port and token.
parseUrl :: String -> Maybe (Int, String)
parseUrl line = do
  afterPrefix <- stripPrefix' urlPrefix line
  rest <- stripPrefix' "http://127.0.0.1:" afterPrefix
  let (digits, afterPort) = span isDigit rest
  path <- stripPrefix' "/" afterPort
  let (token, end) = break (== '/') path
  if null digits || end /= "/" then Nothing else Just (read digits, token)

stripPrefix' :: String -> String -> Maybe String
stripPrefix' prefix s
  | prefix `isPrefixOf` s = Just (drop (length prefix) s)
  | otherwise = Nothing

-- | Poll the trace file every 10 ms, up to 5 s, for the @startup@ and
-- @files@ span durations.
pollSpans :: FilePath -> IO (Maybe Integer, Maybe Integer)
pollSpans file = go (500 :: Int)
  where
    go n = do
      ls <- tryReadLines
      let spans' = (durationOf "startup" ls, durationOf "files" ls)
      if (n <= 0) || bothFound spans'
        then pure spans'
        else threadDelay 10000 >> go (n - 1)
    bothFound (Just _, Just _) = True
    bothFound _ = False
    tryReadLines = do
      result <- try @IOException (lines <$> readFile' file)
      pure (either (const []) id result)
    durationOf name ls = firstJust (map (spanDuration name) ls)
    firstJust = foldr (\x acc -> maybe acc Just x) Nothing

spanDuration :: String -> String -> Maybe Integer
spanDuration wantName line = do
  v <- Aeson.decodeStrict (Char8.pack line)
  Aeson.Types.parseMaybe parser v
  where
    parser = Aeson.withObject "span" $ \o -> do
      n <- o .: "name" :: Aeson.Types.Parser Text
      if n == Text.pack wantName
        then o .: "duration_us"
        else fail "a different span"

microsSince :: Word64 -> Word64 -> Integer
microsSince t0 t1 = toInteger (t1 - t0) `div` 1000
