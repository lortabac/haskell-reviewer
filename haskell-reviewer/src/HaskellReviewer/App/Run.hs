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

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.MVar
  ( newEmptyMVar,
    newMVar,
    takeMVar,
    tryPutMVar,
    withMVar,
  )
import Control.Concurrent.STM (atomically)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    catch,
    evaluate,
    finally,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (unless, void, when)
import Crypto.Random (getRandomBytes)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
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
import HaskellReviewer.Core.Config (UiDefaults)
import HaskellReviewer.Core.FileContent (FileContent)
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
  )
import HaskellReviewer.Core.ModuleModel (ModuleParse (..))
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ModuleIndex (..),
    PackageModules (..),
  )
import HaskellReviewer.Core.Parser (ParseSettings)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcVersion,
    Platform,
    Project (..),
    ProjectSource (..),
    ProjectWarning (..),
    detectGhc,
    hostPlatform,
    renderGhcSource,
    renderGhcVersion,
  )
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Root (CanonicalRoot, Root (..))
import HaskellReviewer.Core.Xdg (XdgProblem, prefsFile, renderXdgProblem)
import HaskellReviewer.Effects.Browser qualified as Browser
import HaskellReviewer.Effects.Clock
  ( Clock (..),
    Microseconds (..),
    MonotonicTime,
    Nanoseconds (..),
    elapsed,
    systemClock,
  )
import HaskellReviewer.Effects.Config qualified as Config
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    Lister (..),
    ReadError (..),
    RootError,
    renderListError,
    renderRootError,
  )
import HaskellReviewer.Effects.FileSystem qualified as FileSystem
import HaskellReviewer.Effects.Git (GitError, renderGitError)
import HaskellReviewer.Effects.Log
  ( LogLevel,
    Logger,
    exceptionMessage,
    logDebug,
    logInfo,
    logVerbose,
    newLogger,
  )
import HaskellReviewer.Effects.Parser qualified as Parser
import HaskellReviewer.Effects.Shutdown (Shutdown (..), installShutdownHandlers)
import HaskellReviewer.Effects.Spawn
  ( SpawnSetup,
    prepareSpawning,
    renderSpawnSetup,
  )
import HaskellReviewer.Effects.Trace
  ( Span (..),
    TraceFileError,
    Tracer (..),
    noTracer,
    openFileTracer,
    renderTraceFileError,
  )
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot)
import HaskellReviewer.Server.Events
  ( EventStream (..),
    indexingGap,
    keepAliveInterval,
  )
import HaskellReviewer.Server.Listen
  ( ListenError,
    Listener (..),
    openListener,
    renderListenError,
  )
import HaskellReviewer.Server.Prefs (warmUpPrefs)
import HaskellReviewer.Server.Router (Routes (..), application, warmUpGzip)
import HaskellReviewer.Server.Wire.Page (warmUpPages)
import HaskellReviewer.Session.Clients (newClients, waitIdle)
import HaskellReviewer.Session.Index
  ( IndexDeps (..),
    IndexFailure (..),
    Indexed (..),
    Progress (..),
    SlotState (..),
    failUnfinished,
    readProgress,
    readStates,
    runWorkers,
  )
import HaskellReviewer.Session.Prefs (newPrefsStore)
import HaskellReviewer.Session.Project qualified as Session.Project
import HaskellReviewer.Session.Snapshot
  ( Snapshot (..),
    SnapshotSlot,
    awaitSnapshot,
    fillSnapshot,
    indexingState,
    newSnapshotSlot,
    snapshotSTM,
  )
import System.Environment (getArgs, getEnvironment)
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

With --verbose, each step of the way out logs a "stop: ..." line, so a
hang shows where it is. The signal handler writes its own line straight
to fd 2, past the logger.
-}

{- Note [Startup work]

Before the URL line we only do quick things (SPEC §10.1): check ROOT,
open the trace file, read config.toml (one small file), check git,
listen, warm up the gzip and prefs code. ui.json is not read: the first
GET api/prefs does that. The rest comes after: Note [Listing after the
URL].

The warm-ups and the config run on the main thread, before warp
accepts: request threads then find their shared values built
(Q-RTS-BLACKHOLE).
-}

{- Note [Listing after the URL]

The URL must come fast (SPEC §10.1), and listing a big tree takes a
while. So at start-up we only check git (version, is ROOT in a work
tree): a bad git is exit 2, before the URL. The listing itself runs in
a thread that starts once the URL is printed. Then the same thread
reads the project and finds the modules. The snapshot routes wait for
both (Session.Snapshot), so an early request is just slower. The
snapshot is fully built before the routes see it (Note [Forced
snapshot] in "HaskellReviewer.Server.Api").

Once the slot is filled, the same thread runs the parse workers, one
per capability (Note [Indexing] in "HaskellReviewer.Session.Index").
So parsing never delays the URL either. Not when the listing failed:
there is nothing to index. However the thread ends after the fill (a
crash, or a cancel at shutdown), the modules not done are set Failed,
so no request waits for ever.

If reading the project crashes (a bug), the snapshot gets no packages
and a warning, logged and shown in the module tree. The file routes
still work.

The thread lives as long as the server (withAsync): on exit it is
cancelled, with the workers, and a git it runs is killed (Note [Git
hardening] in "HaskellReviewer.Effects.Git").
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
    -- | Read config.toml, log its problems, give back the UI defaults.
    -- Never fails. See Note [Config file] in "HaskellReviewer.Core.Config".
    loadConfig :: Logger -> IO UiDefaults,
    -- | Where ui.json is. See Note [Saving preferences] in
    -- "HaskellReviewer.Session.Prefs".
    prefsPath :: IO (Either XdgProblem FilePath),
    -- | Check git and get ready to list the files of ROOT. See
    -- Note [Listing after the URL].
    probeFiles :: Logger -> Root -> IO (Either GitError Lister),
    -- | Read the project of a snapshot. See Note [Listing after the URL].
    readProject ::
      CanonicalRoot ->
      FileList ->
      Platform ->
      Maybe GhcVersion ->
      IO Project,
    -- | Build the shared values of the parser and of the page encoder.
    -- See Note [Forced snapshot] in "HaskellReviewer.Server.Api".
    warmUp :: IO (),
    -- | Read one entry of the file list from disk.
    readEntry ::
      CanonicalRoot ->
      RelPath ->
      EntryKind ->
      IO (Either ReadError FileContent),
    -- | Parse one module, fully forced.
    parse :: ParseSettings -> Text -> IO ModuleParse,
    -- | How many parse workers to run.
    indexWorkers :: IO Int,
    -- | Open the URL in the browser. Must not block or throw.
    openBrowser :: Logger -> Root -> Text -> IO (),
    -- | Catch the stop signals. The level says if the handler logs.
    installShutdown :: LogLevel -> IO Shutdown,
    -- | How long a minute is, for --idle-exit. Tests make it short.
    idleMinute :: Microseconds,
    -- | What 'prepareSpawning' did, for the debug log.
    spawnSetup :: SpawnSetup
  }

-- | The real world.
newRealDeps :: SpawnSetup -> IO Deps
newRealDeps spawnSetup = do
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
        loadConfig = \logger -> do
          env <- getEnvironment
          Config.loadConfig logger (`lookup` env),
        prefsPath = (\env -> prefsFile (`lookup` env)) <$> getEnvironment,
        probeFiles = FileSystem.probeFiles,
        readProject = Session.Project.readProject,
        warmUp = Parser.warmUpParser >> warmUpPages,
        readEntry = FileSystem.readEntry,
        parse = Parser.parseGuarded,
        indexWorkers = getNumCapabilities,
        openBrowser = Browser.openBrowser,
        installShutdown = installShutdownHandlers,
        idleMinute = Microseconds 60_000_000,
        spawnSetup = spawnSetup
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
  -- First, before anything spawns: see Note [No inherited fds] in
  -- "HaskellReviewer.Effects.Spawn".
  spawnSetup <- prepareSpawning
  -- Always UTF-8: with LC_ALL=C, printing a non-ASCII path would throw
  -- (exit 1 instead of 2). Bytes that aren't valid in the locale show up
  -- as U+FFFD, since Text can't hold them.
  hSetEncoding stderr utf8
  deps <- newRealDeps spawnSetup
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
      shutdown <- deps.installShutdown opts.logLevel
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
              logDebug logger (renderSpawnSetup deps.spawnSetup)
              -- On this thread, before warp: see Note [Startup work]. A
              -- bad config is never an exit.
              defaults <- evaluate =<< deps.loadConfig logger
              probed <- deps.probeFiles logger root
              case probed of
                Left e -> startupFailed deps (BadGit e) `finally` tracer.close
                Right lister ->
                  serve deps logger opts root defaults lister tracer shutdown t0 w0
                    `finally` tracer.close

-- | Listen, print the URL, serve until a signal. See Note [Shutdown].
serve ::
  Deps ->
  Logger ->
  Options ->
  Root ->
  UiDefaults ->
  Lister ->
  Tracer ->
  Shutdown ->
  MonotonicTime ->
  UTCTime ->
  IO ExitCode
serve deps logger opts root defaults lister tracer shutdown t0 w0 = do
  stoppedEarly <- shutdown.stopped
  if stoppedEarly
    then pure ExitSuccess
    else do
      token <- deps.newToken
      listenResult <- deps.listen logger opts.port
      case listenResult of
        Left e -> startupFailed deps (BadListen e)
        Right listener -> do
          -- Before warp accepts: see Note [Compression] in
          -- "HaskellReviewer.Server.Router".
          warmUpGzip
          -- The store doesn't read ui.json yet: the first GET does.
          location <- deps.prefsPath
          logVerbose logger $
            "prefs: " <> case location of
              Right path -> Text.pack path
              Left problem -> "in memory (" <> renderXdgProblem problem <> ")"
          prefs <- newPrefsStore logger defaults location
          warmUpPrefs
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
                    indexing = indexingState slot,
                    minGap = indexingGap,
                    logger = logger
                  }
              files =
                FilesApi
                  { snapshot = awaitSnapshot slot,
                    readEntry = \snap -> deps.readEntry snap.root,
                    logger = logger
                  }
              routes = Routes {events = events, files = files, prefs = prefs}
              app = application listener.port token routes
              waitStop = do
                why <- case opts.idleExit of
                  IdleExitOff -> StopSignal <$ shutdown.wait
                  IdleExitAfter m ->
                    either (const StopSignal) (const (StopIdle m))
                      <$> race
                        shutdown.wait
                        (waitIdle clients (idleTimeout deps.idleMinute m))
                step "stopping warp"
                pure why
              -- Shutdown steps, for --verbose: a hang shows where it is.
              step msg = logVerbose logger ("stop: " <> msg)
              -- Scoped: on the way out it's cancelled, and git with it.
              -- The finally covers a cancel anywhere after the fill.
              listing = do
                takeMVar start
                ( runListing deps logger tracer lister slot opts.ghc
                    >>= mapM_ (runIndexing deps logger tracer)
                  )
                  `finally` stopIndexing slot
              serving _ = do
                r <- race waitStop (listener.serve ready app)
                step "warp stopped, stopping the listing"
                pure r
              closing = step "closing the socket" >> listener.close
          result <- withAsync listing serving `finally` closing
          step "socket closed, exiting"
          case result of
            Left StopSignal -> pure ExitSuccess
            Left (StopIdle m) -> do
              deps.console (idleExitMessage m)
              pure ExitSuccess
            Right () -> throwIO (userError "the HTTP server stopped")

-- | Make the file list, read the project, fill the slot, report. Gives
-- back the snapshot in the slot, if there is one. See Note [Listing
-- after the URL].
--
-- The slot is always filled, or requests would wait for ever: with the
-- list, or with an error if anything goes wrong (even a cancel).
runListing ::
  Deps ->
  Logger ->
  Tracer ->
  Lister ->
  SnapshotSlot ->
  Maybe GhcVersion ->
  IO (Maybe Snapshot)
runListing deps logger tracer lister slot cliGhc = do
  result <- try @SomeException $ do
    t0 <- deps.clock.monotonic
    w0 <- deps.clock.now
    listed <- lister.list
    t1 <- deps.clock.monotonic
    w1 <- deps.clock.now
    made <- traverse (\fl -> snapshotOf deps logger lister.root fl cliGhc) listed
    t2 <- deps.clock.monotonic
    w2 <- deps.clock.now
    -- Warm up before the fill: requests parse. See Note [Forced
    -- snapshot] in "HaskellReviewer.Server.Api".
    when (isRight made) (runWarmUp deps logger)
    t3 <- deps.clock.monotonic
    _ <- fill made
    -- After the fill: a slow trace file can't hold up requests.
    tracer.record Span {name = "files", start = w0, duration = elapsed t0 t1}
    case made of
      Right snap -> do
        tracer.record Span {name = "project", start = w1, duration = elapsed t1 t2}
        tracer.record
          Span {name = "parser-warm-up", start = w2, duration = elapsed t2 t3}
        reportFiles snap.files (elapsed t0 t1)
        reportProject snap.modules (elapsed t1 t2)
        reportWarmUp (elapsed t2 t3)
        pure (Just snap)
      Left e -> Nothing <$ reportError e
  case result of
    Right listed -> pure listed
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> do
          _ <- fill (Left (ListCrashed "the listing was stopped"))
          throwIO e
      | otherwise -> do
          let err = ListCrashed (exceptionMessage e)
          filled <- fill (Left err)
          when filled (reportError err)
          -- It may have crashed after the fill: then the snapshot is
          -- served, and its modules still need the workers.
          either (const Nothing) Just <$> awaitSnapshot slot
  where
    fill = fillSnapshot slot
    reportError e =
      logInfo logger ("haskell-reviewer: cannot list files: " <> renderListError e)
    reportFiles fl (Nanoseconds ns) = do
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
    reportProject idx (Nanoseconds ns) =
      logVerbose logger $
        "project: "
          <> showText (length idx.packages)
          <> " packages, "
          <> showText (sum [length c.modules | p <- idx.packages, c <- p.components])
          <> " modules, "
          <> showText (length idx.other)
          <> " other files in "
          <> showText (ns `div` 1_000_000)
          <> " ms"
    reportWarmUp (Nanoseconds ns) =
      logVerbose logger ("parser warm-up: " <> showText (ns `div` 1_000) <> " us")

-- | The deps' 'warmUp', but a crash only logs a line: the snapshot is
-- fine without it.
runWarmUp :: Deps -> Logger -> IO ()
runWarmUp deps logger =
  try @SomeException deps.warmUp >>= \case
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise ->
          logInfo logger $
            "haskell-reviewer: parser warm-up failed: " <> exceptionMessage e

-- | Read the project and build the snapshot, all of it. If that
-- crashes, the snapshot has no project, only a warning. See
-- Note [Listing after the URL].
snapshotOf ::
  Deps -> Logger -> CanonicalRoot -> FileList -> Maybe GhcVersion -> IO Snapshot
snapshotOf deps logger root fl cliGhc = do
  result <- try @SomeException $ do
    project <- deps.readProject root fl hostPlatform cliGhc
    logVerbose logger $
      "ghc: "
        <> renderGhcVersion project.ghc.version
        <> " ("
        <> renderGhcSource project.ghc.source
        <> ")"
    newSnapshot root fl project
  case result of
    Right snap -> pure snap
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let msg = exceptionMessage e
          logInfo logger ("haskell-reviewer: cannot read the project: " <> msg)
          newSnapshot root fl (crashed msg)
  where
    -- Not the implicit source: there may be a project we failed to read.
    -- Never got to with-compiler: --ghc, else the parser's version.
    crashed msg =
      Project
        { source = ProjectFile [],
          withCompiler = Nothing,
          ghc = detectGhc cliGhc Nothing,
          packages = [],
          brokenPackages = [],
          warnings = [ProjectCrashed msg]
        }

-- | Run the parse workers until every module is done, then report.
-- A crash (even before the workers start) fails the modules not done;
-- a cancel goes on to 'stopIndexing'. See Note [Listing after the URL].
runIndexing :: Deps -> Logger -> Tracer -> Snapshot -> IO ()
runIndexing deps logger tracer snap =
  indexAndReport deps logger tracer snap `catch` \e -> case fromException e of
    Just (_ :: SomeAsyncException) -> throwIO e
    Nothing -> do
      let msg = exceptionMessage e
      failUnfinished snap.index (IndexCrashed msg)
      logInfo logger ("haskell-reviewer: indexing crashed: " <> msg)

-- | However the listing thread ends, no module is left Pending or
-- Parsing: requests would wait for ever. For a cancel (shutdown), or a
-- crash after the fill.
stopIndexing :: SnapshotSlot -> IO ()
stopIndexing slot =
  atomically (snapshotSTM slot) >>= \case
    Just (Right snap) ->
      failUnfinished snap.index (IndexCrashed "indexing was stopped")
    _ -> pure ()

indexAndReport :: Deps -> Logger -> Tracer -> Snapshot -> IO ()
indexAndReport deps logger tracer snap = do
  workers <- deps.indexWorkers
  total <- (.total) <$> atomically (readProgress snap.index)
  logVerbose logger $
    "indexing: "
      <> showText total
      <> " modules, "
      <> showText workers
      <> " workers"
  t0 <- deps.clock.monotonic
  w0 <- deps.clock.now
  result <- try @SomeException (runWorkers indexDeps workers snap.index)
  t1 <- deps.clock.monotonic
  case result of
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise ->
          logInfo logger ("haskell-reviewer: indexing crashed: " <> exceptionMessage e)
  states <- Map.elems <$> atomically (readStates snap.index)
  tracer.record Span {name = "indexing", start = w0, duration = elapsed t0 t1}
  let Nanoseconds ns = elapsed t0 t1
  logVerbose logger $
    "indexing: done, "
      <> showText total
      <> " modules ("
      <> showText (length [() | Ready _ <- states])
      <> " ready, "
      <> showText (length [() | Failed _ <- states])
      <> " failed, "
      <> showText (length (filter textOnly states))
      <> " text only) in "
      <> showText (ns `div` 1_000_000)
      <> " ms"
  where
    indexDeps =
      IndexDeps
        { readModule = \p -> case Map.lookup p snap.files.entries of
            -- Slots only have files of the list.
            Just EntryFile -> deps.readEntry snap.root p EntryFile
            _ -> pure (Left ReadGone),
          parse = deps.parse,
          logger = logger
        }
    textOnly = \case
      Ready (IndexedTextOnly _) -> True
      Ready (IndexedNotText _) -> True
      Ready (IndexedTooLarge _) -> True
      _ -> False

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
