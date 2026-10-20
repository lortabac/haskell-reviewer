-- | Program entry point: parse the command line, check inputs, start up.
module HaskellReviewer.App.Run
  ( -- * Types
    Deps (..),
    StartupError (..),

    -- * Running
    main,
    run,
    newRealDeps,
    renderStartupError,
  )
where

import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
  ( newEmptyMVar,
    newMVar,
    takeMVar,
    tryPutMVar,
    withMVar,
  )
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    catch,
    finally,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (unless, void, when)
import Crypto.Random (getRandomBytes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock (UTCTime)
import HaskellReviewer.App.Cli
  ( Browser (..),
    CliResult (..),
    Command (..),
    IdleExit (..),
    Minutes (..),
    Options (..),
    parseArgs,
  )
import HaskellReviewer.App.Version (versionText)
import HaskellReviewer.Core.Address
  ( Port,
    Token,
    sessionUrl,
    tokenByteCount,
    tokenFromBytes,
  )
import HaskellReviewer.Core.FileList (FileList (..), ListSource (..), fileListCap)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Browser qualified as Browser
import HaskellReviewer.Effects.Clock
  ( Clock (..),
    Microseconds (..),
    MonotonicTime,
    Nanoseconds (..),
    elapsed,
    systemClock,
  )
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    Lister (..),
    RootError,
    renderListError,
    renderRootError,
  )
import HaskellReviewer.Effects.FileSystem qualified as FileSystem
import HaskellReviewer.Effects.Git (GitError, renderGitError)
import HaskellReviewer.Effects.Log
  ( Logger,
    exceptionMessage,
    logDebug,
    logInfo,
    logVerbose,
    newLogger,
  )
import HaskellReviewer.Effects.Shutdown (Shutdown (..), installShutdownHandlers)
import HaskellReviewer.Effects.Trace
  ( Span (..),
    TraceFileError,
    Tracer (..),
    noTracer,
    openFileTracer,
    renderTraceFileError,
  )
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot)
import HaskellReviewer.Server.Events (EventStream (..), keepAliveInterval)
import HaskellReviewer.Server.Listen
  ( ListenError,
    Listener (..),
    openListener,
    renderListenError,
  )
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Session.Clients (newClients, waitIdle)
import HaskellReviewer.Session.Snapshot
  ( Snapshot (..),
    SnapshotSlot,
    awaitSnapshot,
    fillSnapshot,
    newSnapshotSlot,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, utf8)

{- Note [Layers]

The library has layers. Lower layers never import upper ones:

1. Core (HaskellReviewer.Core.*): pure code.
2. Effects (HaskellReviewer.Effects.*): handles for IO (clock, files, ...).
3. Session (HaskellReviewer.Session.*): state shared while the server
   runs, in STM (SPEC §11.5).
4. Server (HaskellReviewer.Server.*): HTTP.
5. App (HaskellReviewer.App.*): CLI parsing and wiring. Imports all
   layers; none imports it.

The executable is just `main = Run.main`. See SPEC §11.2.
-}

{- Note [Exit codes]

SPEC §2.1:

- 0: normal exit, --help, --version, SIGINT (Ctrl-C), SIGTERM, idle
  exit. See Note [Shutdown].
- 1: internal error, i.e. a bug. Any exception we didn't expect ends up
  here. We print "haskell-reviewer: internal error: " and the exception,
  instead of GHC's default output. Warp stopping on its own counts too.
- 2: bad usage or bad input (unknown flag, bad ROOT, bad trace file, port
  we can't listen on, git too old, that we can't run or that hangs, git
  refusing ROOT). These are errors as data, never exceptions. They win
  over a signal that came during start-up.

A failed file listing is not an exit: it happens after the URL line
(Note [Listing after the URL]). We print a line and the file routes
answer 500.

The catch-all in 'run' lets async exceptions (killThread, ...) and
ExitCode through untouched: they are not bugs. Ctrl-C isn't one: our
signal handler replaces GHC's UserInterrupt (Note [Shutdown]).
-}

{- Note [Shutdown]

Right after parsing the command line, we catch SIGINT and SIGTERM
(installShutdownHandlers), so Ctrl-C during start-up also exits 0. Then
the main thread races "wait for a stop" against warp. A stop is a signal,
or the idle timeout (Note [Idle exit] in "HaskellReviewer.Session.Clients";
off with --idle-exit 0). When it wins, race kills warp's accept loop, we
close the socket and the trace file, and exit 0. An idle exit also prints
a line on stderr. The idle timer starts with the race, so a browser that
never connects ends the process too.

If a signal came during start-up, we don't listen at all (or, if it came
while listening, skip the URL line, the startup span and the browser) and
exit 0. A start-up error (exit 2) still wins over such a signal: we report
it as usual.

A second signal exits at once (exitImmediately, still code 0), with no
clean-up. So a stuck shutdown can't trap the user.

There is no graceful drain: requests still in flight are cut. Fine for a
local tool, and the event stream never ends, so a drain would wait on it
forever anyway.

The browser opener is never waited for or killed (see
HaskellReviewer.Effects.Browser).
-}

{- Note [Listing after the URL]

The URL must come fast (SPEC §10.1), and listing a big tree takes a
while. So at start-up we only check git (version, is ROOT in a work
tree): a bad git is exit 2, before the URL. The listing itself runs in
a thread that starts once the URL is printed. The file routes wait for
it (Session.Snapshot), so an early request is just slower.

The thread lives as long as the server (withAsync): on exit it is
cancelled, and a git it runs is killed (Note [Git hardening] in
"HaskellReviewer.Effects.Git").
-}

-- | Everything 'run' needs from the outside world. Tests plug in fakes.
data Deps = Deps
  { -- | Write one line on stderr. Called from several threads.
    console :: Text -> IO (),
    clock :: Clock,
    checkRoot :: FilePath -> IO (Either RootError Root),
    -- | Open a trace file. The logger is for warnings while writing.
    openTracer :: Logger -> FilePath -> IO (Either TraceFileError Tracer),
    -- | A fresh random session token.
    newToken :: IO Token,
    -- | Open the listening socket, on the given port or a free one.
    listen :: Logger -> Maybe Port -> IO (Either ListenError Listener),
    -- | Check git and get ready to list the files of ROOT. See
    -- Note [Listing after the URL].
    probeFiles :: Logger -> Root -> IO (Either GitError Lister),
    -- | Open the URL in the browser. Must not block or throw.
    openBrowser :: Logger -> Root -> Text -> IO (),
    -- | Catch the stop signals.
    installShutdown :: IO Shutdown,
    -- | How long a minute is, for --idle-exit. Tests make it short.
    idleMinute :: Microseconds
  }

-- | The real world.
newRealDeps :: IO Deps
newRealDeps = do
  -- Lines from warp's threads and the browser opener must not mix.
  lock <- newMVar ()
  pure
    Deps
      { console = \line -> withMVar lock (\_ -> Text.IO.hPutStrLn stderr line),
        clock = systemClock,
        checkRoot = FileSystem.checkRoot,
        openTracer = openFileTracer,
        newToken = tokenFromBytes <$> getRandomBytes tokenByteCount,
        listen = openListener,
        probeFiles = FileSystem.probeFiles,
        openBrowser = Browser.openBrowser,
        installShutdown = installShutdownHandlers,
        idleMinute = Microseconds 60_000_000
      }

-- | Bad input found at start-up. All exit with code 2.
data StartupError
  = BadRoot RootError
  | BadTraceFile TraceFileError
  | BadListen ListenError
  | BadGit GitError
  deriving (Eq, Show)

-- | Message for the user, with the program name in front.
renderStartupError :: StartupError -> Text
renderStartupError err =
  "haskell-reviewer: " <> case err of
    BadRoot e -> renderRootError e
    BadTraceFile e -> renderTraceFileError e
    BadListen e -> renderListenError e
    BadGit e -> renderGitError e

-- | The real program.
main :: IO ()
main = do
  -- Always UTF-8: with LC_ALL=C, printing a non-ASCII path would throw
  -- (exit 1 instead of 2). Bytes that aren't valid in the locale show up
  -- as U+FFFD, since Text can't hold them.
  hSetEncoding stderr utf8
  deps <- newRealDeps
  getArgs >>= run deps >>= exitWith

-- | Run with the given arguments and return the exit code.
-- See Note [Exit codes].
run :: Deps -> [String] -> IO ExitCode
run deps args = startUp deps args `catch` internalError deps

-- * Internal

internalError :: Deps -> SomeException -> IO ExitCode
internalError deps e
  | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
  | Just (_ :: ExitCode) <- fromException e = throwIO e
  | otherwise = do
      deps.console
        ("haskell-reviewer: internal error: " <> exceptionMessage e)
      pure (ExitFailure 1)

startUp :: Deps -> [String] -> IO ExitCode
startUp deps args = do
  t0 <- deps.clock.monotonic
  w0 <- deps.clock.now
  case parseArgs args of
    CliExit code msg -> do
      deps.console msg
      pure code
    CliCommand ShowVersion -> do
      deps.console versionText
      pure ExitSuccess
    CliCommand (Run opts) -> do
      shutdown <- deps.installShutdown
      let logger = newLogger opts.logLevel deps.console
      rootResult <- deps.checkRoot opts.root
      case rootResult of
        Left e -> startupFailed deps (BadRoot e)
        Right root -> do
          tracerResult <- openTracerFor deps logger opts
          case tracerResult of
            Left e -> startupFailed deps (BadTraceFile e)
            Right tracer -> do
              logVerbose logger ("root: " <> Text.pack root.path)
              logDebug logger ("options: " <> Text.pack (show opts))
              probed <- deps.probeFiles logger root
              case probed of
                Left e -> startupFailed deps (BadGit e) `finally` tracer.close
                Right lister ->
                  serve deps logger opts root lister tracer shutdown t0 w0
                    `finally` tracer.close

-- | Listen, print the URL, serve until a signal. See Note [Shutdown].
serve ::
  Deps ->
  Logger ->
  Options ->
  Root ->
  Lister ->
  Tracer ->
  Shutdown ->
  MonotonicTime ->
  UTCTime ->
  IO ExitCode
serve deps logger opts root lister tracer shutdown t0 w0 = do
  stoppedEarly <- shutdown.stopped
  if stoppedEarly
    then pure ExitSuccess
    else do
      token <- deps.newToken
      listenResult <- deps.listen logger opts.port
      case listenResult of
        Left e -> startupFailed deps (BadListen e)
        Right listener -> do
          slot <- newSnapshotSlot
          -- Filled by ready: the listing starts after the URL line.
          start <- newEmptyMVar
          let url = sessionUrl listener.port token
              -- Runs once warp accepts connections. Skipped if a signal
              -- came meanwhile: we are about to stop.
              ready = do
                stopping <- shutdown.stopped
                unless stopping $ do
                  deps.console ("haskell-reviewer: serving at " <> url)
                  t1 <- deps.clock.monotonic
                  tracer.record
                    Span {name = "startup", start = w0, duration = elapsed t0 t1}
                  when (opts.browser == OpenBrowser) $
                    deps.openBrowser logger root url
                  void (tryPutMVar start ())
          clients <- newClients
          let events =
                EventStream
                  { clients = clients,
                    keepAlive = keepAliveInterval,
                    logger = logger
                  }
              files =
                FilesApi
                  { snapshot = awaitSnapshot slot,
                    readEntry = \snap -> FileSystem.readEntry snap.root,
                    logger = logger
                  }
              routes = Routes {events = events, files = files}
              app = application listener.port token routes
              waitStop = case opts.idleExit of
                IdleExitOff -> StopSignal <$ shutdown.wait
                IdleExitAfter m ->
                  either (const StopSignal) (const (StopIdle m))
                    <$> race
                      shutdown.wait
                      (waitIdle clients (idleTimeout deps.idleMinute m))
              -- Scoped: on the way out it's cancelled, and git with it.
              listing = takeMVar start >> runListing deps logger tracer lister slot
          result <-
            withAsync listing (\_ -> race waitStop (listener.serve ready app))
              `finally` listener.close
          case result of
            Left StopSignal -> pure ExitSuccess
            Left (StopIdle m) -> do
              deps.console (idleExitMessage m)
              pure ExitSuccess
            Right () -> throwIO (userError "the HTTP server stopped")

-- | Make the file list, fill the slot, report. See
-- Note [Listing after the URL].
--
-- The slot is always filled, or requests would wait for ever: with the
-- list, or with an error if anything goes wrong (even a cancel).
runListing :: Deps -> Logger -> Tracer -> Lister -> SnapshotSlot -> IO ()
runListing deps logger tracer lister slot = do
  result <- try @SomeException $ do
    t0 <- deps.clock.monotonic
    w0 <- deps.clock.now
    listed <- lister.list
    t1 <- deps.clock.monotonic
    _ <- fill listed
    -- After the fill: a slow trace file can't hold up requests.
    tracer.record Span {name = "files", start = w0, duration = elapsed t0 t1}
    report listed (elapsed t0 t1)
  case result of
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> do
          _ <- fill (Left (ListCrashed "the listing was stopped"))
          throwIO e
      | otherwise -> do
          let err = ListCrashed (exceptionMessage e)
          filled <- fill (Left err)
          when filled (report (Left err) (Nanoseconds 0))
  where
    fill listed = fillSnapshot slot (newSnapshot lister.root <$> listed)
    report listed (Nanoseconds ns) = case listed of
      Left e ->
        logInfo logger ("haskell-reviewer: cannot list files: " <> renderListError e)
      Right fl -> do
        when (fl.skippedNames > 0) $
          logInfo logger $
            "haskell-reviewer: skipped "
              <> showText fl.skippedNames
              <> " file name(s) that are not UTF-8"
        when fl.truncated $
          logInfo logger $
            "haskell-reviewer: more than "
              <> showText fileListCap
              <> " files under ROOT; listing the first "
              <> showText fileListCap
        let from = case fl.source of
              FromGit -> "git"
              FromDisk -> "disk"
        logVerbose logger $
          "files: "
            <> showText (length fl.entries)
            <> " from "
            <> from
            <> " in "
            <> showText (ns `div` 1_000_000)
            <> " ms"

showText :: (Show a) => a -> Text
showText = Text.pack . show

-- | Why the server stopped.
data StopReason
  = StopSignal
  | -- | No tab for this long.
    StopIdle Minutes

idleTimeout :: Microseconds -> Minutes -> Microseconds
idleTimeout (Microseconds minute) (Minutes m) = Microseconds (minute * m)

idleExitMessage :: Minutes -> Text
idleExitMessage (Minutes m) =
  "haskell-reviewer: no browser tab for " <> minutes <> ", exiting"
  where
    minutes
      | m == 1 = "1 minute"
      | otherwise = Text.pack (show m) <> " minutes"

openTracerFor :: Deps -> Logger -> Options -> IO (Either TraceFileError Tracer)
openTracerFor deps logger opts = case opts.traceFile of
  Nothing -> pure (Right noTracer)
  Just file -> deps.openTracer logger file

startupFailed :: Deps -> StartupError -> IO ExitCode
startupFailed deps err = do
  deps.console (renderStartupError err)
  pure (ExitFailure 2)
