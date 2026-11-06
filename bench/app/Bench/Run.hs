-- | One run of the product binary against one project: a session. Spawns
-- it, waits for the URL line, hands the running server to the caller,
-- then stops it and reads its RTS stats. "Bench.Startup",
-- "Bench.Steady" and "Bench.SearchDuring" say what to do in between.
module Bench.Run
  ( -- * Types
    SessionConfig (..),
    StatsFile (..),
    Session (..),
    DoneAt (..),
    Ended (..),
    RunError (..),
    renderRunError,
    requireRts,
    Timed (..),
    ProcMemory (..),

    -- * Sessions
    withSession,

    -- * Inside a session
    failWith,
    elapsedUs,
    awaitDone,
    getApi,
    getPath,
    procMemory,
    traceSpans,
  )
where

import Bench.GcLog (GcLog, GcLogError, parseGcLog)
import Bench.Http (Gzip, HttpResponse (..), rawHttp, requestBytes)
import Bench.IndexLog (IndexDone, parseDoneLine)
import Bench.RtsStats (RtsStats, parseRtsStats, parseRtsSummary, renderRtsStatsError)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
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
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
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
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
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
import Text.Read (readMaybe)

-- * Types

-- | What one session needs.
data SessionConfig = SessionConfig
  { -- | Path to the product binary (linked with @-rtsopts@).
    exe :: FilePath,
    projectDir :: FilePath,
    -- | Where to write this session's trace, @-t@ and @-S@ files.
    tmpDir :: FilePath,
    -- | Unique per session in 'tmpDir' (used only for file names).
    tag :: String,
    -- | Extra @+RTS@ options (@--rts@), after ours.
    rtsOpts :: [String],
    stats :: StatsFile
  }

-- | Which RTS stats file to ask for. Only one: see "Bench.RtsStats".
data StatsFile
  = -- | @-t --machine-readable@: memory and GC totals.
    TStats
  | -- | @-S@: every GC pause, then the same totals as text.
    GcStats

-- | A running server.
data Session = Session
  { -- | Right before the spawn: every @...Us@ "since start" is from here.
    t0 :: Word64,
    pid :: ProcessID,
    port :: Int,
    token :: String,
    -- | When the URL line came.
    urlUs :: Integer,
    -- | The done line and when it came; 'Left' if stderr ended first.
    done :: MVar (Either String DoneAt),
    -- | The last stderr lines, for error messages.
    seen :: IORef [String],
    traceFile :: FilePath
  }

-- | What we learn once the server stopped.
data Ended = Ended
  { -- | From the first stop signal to exit. With 'resent', that's at
    -- least 3 s: the wait before the second signal.
    stopUs :: Integer,
    -- | The stop needed a second SIGTERM. See Note [Second SIGTERM].
    resent :: Bool,
    -- | 'Left' (the parse error) only if 'resent': the product then
    -- exits at once, without writing them. Runs that report them use
    -- 'requireRts'.
    rts :: Either String RtsStats,
    -- | 'Nothing' without 'GcStats' (or without 'rts'). A file with no
    -- GC line is kept as its error: a warning in the report, not a
    -- broken run.
    gc :: Maybe (Either GcLogError GcLog)
  }

-- | The RTS stats, or why the run has none.
requireRts :: Ended -> Either RunError RtsStats
requireRts ended = case ended.rts of
  Right r -> Right r
  Left e ->
    Left
      ( RunError
          ( "no RTS stats after a second SIGTERM, which exits without them \
            \(Note [Second SIGTERM] in Bench.Run, D-139): "
              <> e
          )
      )

-- | The done line, and when it came.
data DoneAt = DoneAt
  { -- | Since start.
    atUs :: Integer,
    line :: IndexDone
  }

-- | Why a run didn't produce a result. Unlike missing a §10 target,
-- this always fails the harness (SPEC §10.3: timings never do).
newtype RunError = RunError String
  deriving (Eq, Show)

-- | A message for the user.
renderRunError :: RunError -> String
renderRunError (RunError s) = s

-- | One timed request.
data Timed = Timed
  { response :: HttpResponse,
    -- | From just before the connect to the last byte read.
    us :: Integer,
    -- | When it ended, since start.
    endUs :: Integer
  }

-- | From @\/proc\/\<pid\>\/status@: Linux only, 'Nothing' elsewhere.
-- Unlike the RTS numbers (Note [What this memory excludes] in
-- "Bench.RtsStats"), this is the whole process.
data ProcMemory = ProcMemory
  { rssBytes :: Maybe Integer,
    -- | Peak RSS so far.
    hwmBytes :: Maybe Integer
  }

-- * Sessions

-- | Run the product, give the session to the action, stop the product.
-- Leaves no process behind, on success or failure. Any failure inside
-- (ours, from 'failWith', or an 'IOException') becomes a 'RunError'.
withSession :: SessionConfig -> (Session -> IO a) -> IO (Either RunError (a, Ended))
withSession cfg action =
  bracket (openFile "/dev/null" ReadMode) hClose $ \devNull ->
    withSystemTempDirectory "haskell-reviewer-bench-xdg" $ \xdg ->
      runIn cfg action devNull xdg

-- | 'withSession' with fresh XDG dirs: the user's config.toml must not
-- change the numbers, and a run must not touch their ui.json.
runIn ::
  SessionConfig -> (Session -> IO a) -> Handle -> FilePath -> IO (Either RunError (a, Ended))
runIn cfg action devNull xdg = do
  -- A stale file from an earlier session with the same tag must not
  -- look like this one's: if the product dies before writing a fresh
  -- one, we want that to fail the run, not silently read someone
  -- else's numbers.
  mapM_ (\f -> try @IOException (removeFile f)) [rtsFile cfg, gcFile cfg, traceFileOf cfg]
  -- Inherited, minus GHCRTS: -rtsopts (needed for +RTS -t) also means
  -- the product now reads GHCRTS, and a developer's own GHCRTS (-N,
  -- -A, ...) would silently change every number. Our own XDG dirs
  -- (D-202).
  let ours =
        [ ("XDG_CONFIG_HOME", xdg </> "config"),
          ("XDG_STATE_HOME", xdg </> "state")
        ]
      dropped = "GHCRTS" : map fst ours
  childEnv <- (ours <>) . filter ((`notElem` dropped) . fst) <$> getEnvironment
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
  t0 <- getMonotonicTimeNSec
  bracket (createProcess cp) killAndReap $ \(_, mout, merr, ph) ->
    case (mout, merr) of
      (Just out, Just err) -> do
        result <- try @SomeException (steps t0 cfg action out err ph)
        either toRunError (pure . Right) result
      _ -> pure (Left (RunError "no stdout/stderr pipes"))
  where
    -- An async exception (Ctrl-C, the harness's own timeout) is not a
    -- run failure, so it's rethrown: 'bracket' still cleans up the
    -- process either way.
    toRunError e
      | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
      | Just (RunFailure msg) <- fromException e = pure (Left (RunError msg))
      | otherwise = pure (Left (RunError (displayException e)))

-- | @--verbose@ for the done line (D-164); @-t@, or @-S@ if asked.
runArgs :: SessionConfig -> [String]
runArgs cfg =
  [ "--no-open",
    "--verbose",
    "--idle-exit",
    "0",
    "--trace-file",
    traceFileOf cfg,
    "+RTS"
  ]
    <> statsArgs
    <> cfg.rtsOpts
    <> ["-RTS", cfg.projectDir]
  where
    -- One stats file per RTS: see "Bench.RtsStats".
    statsArgs =
      case cfg.stats of
        GcStats -> ["-S" <> gcFile cfg]
        TStats -> ["-t" <> rtsFile cfg, "--machine-readable"]

rtsFile, gcFile, traceFileOf :: SessionConfig -> FilePath
rtsFile cfg = cfg.tmpDir </> ("rts-" <> cfg.tag <> ".txt")
gcFile cfg = cfg.tmpDir </> ("gc-" <> cfg.tag <> ".txt")
traceFileOf cfg = cfg.tmpDir </> ("trace-" <> cfg.tag <> ".jsonl")

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

-- * Inside a session

-- | Used only inside a session, to short-circuit on the first failure.
-- Caught once, in 'withSession', and turned into a 'RunError'. Simpler
-- than threading @Either RunError@ through every step by hand.
newtype RunFailure = RunFailure String
  deriving (Show)

instance Exception RunFailure

-- | Fail the session (and so the run) with this message.
failWith :: String -> IO a
failWith = throwIO . RunFailure

-- | Microseconds since start.
elapsedUs :: Session -> IO Integer
elapsedUs s = microsSince s.t0 <$> getMonotonicTimeNSec

-- | Wait for the done line, up to 300 s: when it came (since start) and
-- what it says.
awaitDone :: Session -> IO DoneAt
awaitDone s = do
  r <- timeout 300_000_000 (readMVar s.done)
  case r of
    Just (Right d) -> pure d
    Just (Left why) -> lastLines s >>= failWith . ((why <> "; stderr: ") <>)
    Nothing -> lastLines s >>= failWith . ("no 'indexing: done' within 300 s; stderr: " <>)

-- | GET a path under the token (no leading slash, already encoded).
getApi :: Session -> Gzip -> String -> IO Timed
getApi s gzip path = getPath s gzip ("/" <> s.token <> "/" <> path)

-- | GET a full path. Fails the run if there's no HTTP answer; any
-- status is fine here, callers check it.
getPath :: Session -> Gzip -> String -> IO Timed
getPath s gzip path = do
  let req = requestBytes "GET" (Just ("127.0.0.1:" <> show s.port)) gzip path
  a <- getMonotonicTimeNSec
  res <- try @IOException (rawHttp s.port req)
  b <- getMonotonicTimeNSec
  case res of
    Left e -> failWith (path <> ": " <> displayException e)
    Right (Left e) -> failWith (path <> ": HTTP: " <> e)
    Right (Right r) ->
      pure Timed {response = r, us = microsSince a b, endUs = microsSince s.t0 b}

-- | RSS now and its peak so far.
procMemory :: Session -> IO ProcMemory
procMemory s = do
  r <- try @IOException (readFile' ("/proc/" <> show s.pid <> "/status"))
  pure $ case r of
    Left _ -> ProcMemory Nothing Nothing
    Right txt -> ProcMemory (field "VmRSS:" txt) (field "VmHWM:" txt)
  where
    -- "VmRSS:	  123456 kB"
    field name txt = case [ws | l <- lines txt, ws@(n : _) <- [words l], n == name] of
      ((_ : kb : "kB" : _) : _) -> (* 1024) <$> readMaybe kb
      _ -> Nothing

-- | The durations of these trace spans, by name. Polls the trace file
-- every 10 ms, up to 5 s, until all are there: a span is written when
-- it ends, which can be a bit after the log line.
traceSpans :: Session -> [Text] -> IO [Maybe Integer]
traceSpans s names = go (500 :: Int)
  where
    go n = do
      ls <- tryReadLines
      let found = map (\nm -> firstJust (map (spanDuration nm) ls)) names
      if n <= 0 || all (/= Nothing) found
        then pure found
        else threadDelay 10000 >> go (n - 1)
    tryReadLines = do
      result <- try @IOException (lines <$> readFile' s.traceFile)
      pure (either (const []) id result)
    firstJust = foldr (\x acc -> maybe acc Just x) Nothing

-- * Internal

spanDuration :: Text -> String -> Maybe Integer
spanDuration wantName line = do
  v <- Aeson.decodeStrict (Char8.pack line)
  Aeson.Types.parseMaybe parser v
  where
    parser = Aeson.withObject "span" $ \o -> do
      n <- o .: "name" :: Aeson.Types.Parser Text
      if n == wantName
        then o .: "duration_us"
        else fail "a different span"

steps ::
  Word64 ->
  SessionConfig ->
  (Session -> IO a) ->
  Handle ->
  Handle ->
  ProcessHandle ->
  IO (a, Ended)
steps t0 cfg action out err ph = do
  seen <- newIORef []
  urlVar <- newEmptyMVar
  doneVar <- newEmptyMVar
  stdoutVar <- newEmptyMVar
  -- Both pipes are drained for the whole run, not just up to the URL
  -- line: a chatty product could otherwise fill the OS pipe buffer and
  -- block on a later write, long after we've stopped reading. These
  -- threads are plain 'forkIO', never joined: if we give up on them
  -- (the process never exits, say), they're simply abandoned, and die
  -- on their own once 'killAndReap' kills the process and closes the
  -- handles. A structured-concurrency join here (e.g.
  -- 'Control.Concurrent.Async.withAsync') would be wrong: it would
  -- block *us* waiting for a reader that's stuck on a pipe whose
  -- writer is still alive, which is exactly the deadlock this is
  -- avoiding.
  _ <- forkIO (drainStderr t0 err seen urlVar doneVar)
  _ <- forkIO (drainStdout out stdoutVar)

  urlLine <- readUrlLine seen urlVar
  (port, token) <-
    maybe (failWith ("bad URL line: " <> urlLine.text)) pure (parseUrl urlLine.text)
  pid <- maybe (failWith "process gone right after the URL line") pure =<< getPid ph

  let session =
        Session
          { t0 = t0,
            pid = pid,
            port = port,
            token = token,
            urlUs = microsSince t0 urlLine.at,
            done = doneVar,
            seen = seen,
            traceFile = traceFileOf cfg
          }
  result <- action session

  tSignal <- getMonotonicTimeNSec
  stillThere <- getPid ph
  maybe (failWith "process already gone before stop") (signalProcess sigTERM) stillThere
  (exitC, resent) <- waitForExit tSignal ph
  tExit <- getMonotonicTimeNSec

  when resent $
    hPutStrLn
      stderr
      ( "haskell-reviewer-bench: warning: "
          <> cfg.tag
          <> ": no exit within 3 s of SIGTERM, sent a second one (D-139)"
      )
  unless (exitC == 0) $ failWith ("exit code " <> show exitC)

  -- The process has exited, so drainStdout's hGetContents' should
  -- already be done or finishing right now; the timeout is only a
  -- safety net, not expected to fire.
  leftoverOut <-
    maybe (failWith "reading stdout timed out") pure
      =<< timeout 5000000 (takeMVar stdoutVar)
  unless (null leftoverOut) $ failWith ("stdout not empty: " <> show leftoverOut)

  -- After a second signal the file has the command line only.
  let readStats parse raw = case parse raw of
        Right r -> pure (Right r)
        Left e | resent -> pure (Left (renderRtsStatsError e))
        Left e -> failWith (renderRtsStatsError e)
  (rts, gc) <- case cfg.stats of
    GcStats -> do
      raw <- readFile' (gcFile cfg)
      r <- readStats parseRtsSummary raw
      pure (r, if isRight r then Just (parseGcLog raw) else Nothing)
    TStats -> do
      raw <- readFile' (rtsFile cfg)
      r <- readStats parseRtsStats raw
      pure (r, Nothing)

  pure
    ( result,
      Ended
        { stopUs = microsSince tSignal tExit,
          resent = resent,
          rts = rts,
          gc = gc
        }
    )

-- | Reads stderr, line by line, for as long as the handle stays open.
-- Never joined (see the comment in 'steps'): on an 'IOException' (the
-- handle was closed under it, by 'killAndReap') it just stops, quietly.
--
-- The last lines go into @seen@, for error messages. The first line
-- that starts with 'urlPrefix' also goes into @urlVar@, and the done
-- line into @doneVar@ with the time it came (once each: a
-- 'tryPutMVar' that a later EOF can't overwrite). At EOF both get
-- their "no such line" value.
drainStderr ::
  Word64 ->
  Handle ->
  IORef [String] ->
  MVar (Maybe UrlLine) ->
  MVar (Either String DoneAt) ->
  IO ()
drainStderr t0 h seen urlVar doneVar = do
  _ <- try @IOException go
  void (tryPutMVar urlVar Nothing)
  void (tryPutMVar doneVar (Left "stderr ended before 'indexing: done'"))
  where
    go = do
      eof <- hIsEOF h
      unless eof $ do
        l <- hGetLine h
        now <- getMonotonicTimeNSec
        atomicModifyIORef' seen (\ls -> (take 40 (l : ls), ()))
        when (urlPrefix `isPrefixOf` l) $
          void (tryPutMVar urlVar (Just UrlLine {text = l, at = now}))
        case parseDoneLine l of
          Just d ->
            void (tryPutMVar doneVar (Right DoneAt {atUs = microsSince t0 now, line = d}))
          Nothing -> pure ()
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

-- | The URL line, and when we read it (monotonic ns): the URL time is
-- that, not when the waiting thread woke up.
data UrlLine = UrlLine
  { text :: String,
    at :: Word64
  }

-- | Wait for 'drainStderr' to find the URL line, up to 30 s.
readUrlLine :: IORef [String] -> MVar (Maybe UrlLine) -> IO UrlLine
readUrlLine seen urlVar = do
  result <- timeout 30000000 (readMVar urlVar)
  case result of
    Just (Just l) -> pure l
    Just Nothing ->
      seenSoFar >>= failWith . ("process exited before the URL line; stderr: " <>)
    Nothing -> seenSoFar >>= failWith . ("no URL line within 30 s; stderr so far: " <>)
  where
    seenSoFar = show . reverse <$> readIORef seen

lastLines :: Session -> IO String
lastLines s = show . reverse <$> readIORef s.seen

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

{- Note [Second SIGTERM]

On macOS a signal that comes while the product is busy can sit in the
RTS's pipe until the timer manager wakes up for something else (D-139,
a GHC bug, not worked around in the product). A busy server can then
miss our stop for longer than the 10 s bound.

So sessions stop an idle server: they all wait for the done line
('awaitDone') first. And as a safety net, 'waitForExit' sends SIGTERM
again if there's no exit after 3 s.
Another signal wakes the timer manager, and the product exits at once
on a second signal (Note [Shutdown] in "HaskellReviewer.App.Run").

That exit skips the RTS's own exit, so no RTS stats get written: runs
that report them fail ('requireRts'), others go on. We print a warning
either way, so a stuck stop stays visible. 'stopUs' is still from the
first signal, so it's at least 3 s then.
-}

-- | Wait up to 10 s for the process to exit after the stop signal sent
-- at @t0@ (monotonic ns): its exit code, and whether we sent a second
-- SIGTERM (after 3 s; not if the process was already gone). See Note
-- [Polling for exit, not waitForProcess] and Note [Second SIGTERM].
--
-- Polls every 0.5 ms for the first 50 ms, then every 5 ms: most
-- products exit within a few ms of SIGTERM, and 'stopUs' is a number
-- we report, so its resolution matters while it's small. No point
-- keeping that resolution for the rest of the 10 s bound, which only
-- ever gets used by a product that's stuck.
waitForExit :: Word64 -> ProcessHandle -> IO (Int, Bool)
waitForExit t0 ph = do
  first <- pollUntil 3
  case first of
    Just code -> pure (code, False)
    Nothing -> do
      -- Gone in between: then the poll below finds its exit code.
      sent <-
        getPid ph >>= \case
          Nothing -> pure False
          Just pid -> isRight <$> try @IOException (signalProcess sigTERM pid)
      second <- pollUntil 10
      case second of
        Just code -> pure (code, sent)
        Nothing ->
          failWith
            ( "no exit within 10 s of SIGTERM"
                <> (if sent then " (sent twice)" else " (second one not sent)")
            )
  where
    pollUntil :: Word64 -> IO (Maybe Int)
    pollUntil secs = do
      found <- getProcessExitCode ph
      now <- getMonotonicTimeNSec
      let since = now - t0
      case found of
        Just code -> pure (Just (exitInt code))
        Nothing
          | since >= secs * 1_000_000_000 -> pure Nothing
          | otherwise -> do
              threadDelay (if since < 50_000_000 then 500 else 5000)
              pollUntil secs
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

microsSince :: Word64 -> Word64 -> Integer
microsSince a b = toInteger (b - a) `div` 1000
