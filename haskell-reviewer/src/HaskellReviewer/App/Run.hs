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
import Control.Concurrent.Async (forConcurrently, race, withAsync)
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
import Control.Monad (forM_, unless, void, when, (<$!>))
import Crypto.Random (getRandomBytes)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import HaskellReviewer.Core.Changed (ModuleKey, changedModules, moduleKeys)
import HaskellReviewer.Core.CommitFiles (outsideChanges)
import HaskellReviewer.Core.Config (UiDefaults)
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent (FileContent, FileNotes (..))
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
  )
import HaskellReviewer.Core.Lang (Langs)
import HaskellReviewer.Core.Markers (Marker (..), moduleFacts, moduleMarkers)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..))
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ModuleId,
    ModuleIndex (..),
    PackageModules (..),
    buildModuleIndex,
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
import HaskellReviewer.Core.Renames
  ( Change (..),
    Limit (..),
    ModifiedFile (..),
    Pairing (..),
    defaultSettings,
    forcePairing,
    warmUpRenames,
  )
import HaskellReviewer.Core.Revspec
  ( CommitsInfo (..),
    DiffInfo (..),
    Range (..),
    Revspec,
    WorkTreeInfo (..),
    diffInfoLines,
  )
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.Sides
  ( Pair (..),
    Side (..),
    Sides (..),
    lookupSide,
    sideName,
    sideTag,
  )
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
    ReadError (..),
    RootError,
    renderListError,
    renderRootError,
  )
import HaskellReviewer.Effects.FileSystem qualified as FileSystem
import HaskellReviewer.Effects.Lang qualified as Lang
import HaskellReviewer.Effects.Log
  ( LogLevel,
    Logger,
    exceptionMessage,
    logDebug,
    logInfo,
    logVerbose,
    newLogger,
  )
import HaskellReviewer.Effects.Pairing (FilePairing (..), pairListed)
import HaskellReviewer.Effects.Parser qualified as Parser
import HaskellReviewer.Effects.Shutdown (Shutdown (..), installShutdownHandlers)
import HaskellReviewer.Effects.Source
  ( EntrySource (..),
    Listed (..),
    Lister (..),
    Probe (..),
    ProbeError,
    fileNotes,
    readSourceEntry,
    renderProbeError,
    sourceBlob,
    warmUpListing,
    warmUpObjects,
    warmUpWorkTree,
  )
import HaskellReviewer.Effects.Source qualified as Source
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
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot, warmUpSideNames)
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
import HaskellReviewer.Server.Refs (warmUpRefs)
import HaskellReviewer.Server.Router (Routes (..), application, warmUpGzip)
import HaskellReviewer.Server.Search (warmUpSearch)
import HaskellReviewer.Server.TextSearch (warmUpTextSearch)
import HaskellReviewer.Server.Wire
  ( DiffEnd (..),
    DiffFiles (..),
    encodeDiffFiles,
  )
import HaskellReviewer.Server.Wire.Page (warmUpPages)
import HaskellReviewer.Server.Wire.Source (warmUpSource)
import HaskellReviewer.Session.Clients (newClients, waitIdle)
import HaskellReviewer.Session.Diff (DiffProblem (..), DiffSlot, fillDiff, newDiffSlot)
import HaskellReviewer.Session.Index
  ( IndexDeps (..),
    IndexFailure (..),
    IndexJob (..),
    Indexed (..),
    Progress (..),
    ShareCounts (..),
    SlotState (..),
    failUnfinished,
    prioritise,
    readProgress,
    readStates,
    runWorkers,
    shareCounts,
  )
import HaskellReviewer.Session.Prefs (newPrefsStore)
import HaskellReviewer.Session.Project qualified as Session.Project
import HaskellReviewer.Session.Resolve (warmUpResolve)
import HaskellReviewer.Session.Snapshot
  ( SessionShare,
    Snapshot (..),
    SnapshotSlot,
    awaitSnapshot,
    fillSnapshot,
    firstGeneration,
    indexingStates,
    newSessionShare,
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
listen, warm up the gzip, prefs and search code, the side names
(warmUpSideNames in "HaskellReviewer.Server.Api"), commit listing,
commit reads and the working-tree rules (warmUpListing, warmUpObjects,
warmUpWorkTree in "HaskellReviewer.Effects.Source"). In diff mode, the revisions are
resolved too (D-316): a bad one is exit 2. ui.json is not read: the
first GET api/prefs does that. The rest comes after: Note [Listing
after the URL].

The warm-ups and the config run on the main thread, before warp
accepts: request threads then find their shared values built
(Q-RTS-BLACKHOLE).
-}

{- Note [Listing after the URL]

The URL must come fast (SPEC §10.1), and listing a big tree takes a
while. So at start-up we only check git (version, is ROOT in a work
tree): a bad git is exit 2, before the URL. The listing itself runs in
a thread that starts once the URL is printed. Then it reads the
project and finds the modules. The sides of the session (Note [Sides]
in "HaskellReviewer.Core.Sides") are listed at the same time, each in
its own thread, then their projects are read and their snapshots built
one after the other, head first: this forces shared values, which
threads must not do at once (Q-RTS-BLACKHOLE). The listing step's
shared values are built before warp (warmUpListing in
"HaskellReviewer.Effects.Source"). A crash fails that side only.

In diff mode, once both projects are read, the files are paired
(D-326): the api/diff-files body and the module markers, which go into
each snapshot's module tree. Once both sides are built, their changed
modules go to the front of the queues (D-317). All before any fill;
the diff slot is filled with the snapshot slots. The object reader of
diff mode wraps the whole server, so it stops after the listing
thread.

The snapshot routes wait for their side (Session.Snapshot), so an
early request is just slower. Each snapshot is fully built before the
routes see it (Note [Forced snapshot] in "HaskellReviewer.Server.Api"),
and the warm-ups run once, before any slot is filled.

Once every slot is filled, the same thread runs the parse workers, one
per capability, in one pool over every listed side's index, head first
(Note [Indexing] in "HaskellReviewer.Session.Index"). A request
computing parks one of them, never the last. So parsing never delays
the URL either. No workers if no side was listed: there is nothing to
index. However the thread ends (a crash, or a cancel at shutdown),
every slot (the diff slot too) is filled and the modules not done are
set Failed, so no request waits for ever.

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
    -- | Read the highlighting rules, log their problems. Never fails,
    -- and the result is fully built. See Note [Language rules] in
    -- "HaskellReviewer.Core.Lang".
    loadLangs :: Logger -> IO Langs,
    -- | Where ui.json is. See Note [Saving preferences] in
    -- "HaskellReviewer.Session.Prefs".
    prefsPath :: IO (Either XdgProblem FilePath),
    -- | Check git, resolve @--diff@ if given, and get ready to list the
    -- files of ROOT: one lister per side of the session (Note [Sides] in
    -- "HaskellReviewer.Core.Sides"). See Note [Listing after the URL].
    probeFiles :: Logger -> Root -> Maybe Revspec -> IO (Either ProbeError Probe),
    -- | Read the project of a snapshot. See Note [Listing after the URL].
    readProject ::
      EntrySource ->
      FileList ->
      Platform ->
      Maybe GhcVersion ->
      IO Project,
    -- | Build the shared values of the parser, of resolution and of the
    -- page encoder. Each one runs even if one before it throws.
    -- See Note [Forced snapshot] in "HaskellReviewer.Server.Api".
    warmUp :: [IO ()],
    -- | Read one entry of the file list from its source.
    readEntry ::
      EntrySource ->
      RelPath ->
      EntryKind ->
      IO (Either ReadError FileContent),
    -- | Parse one module, fully forced.
    parse :: ParseSettings -> Text -> IO ModuleParse,
    -- | Pair the files of two listed sides, in diff mode (D-326).
    -- 'Nothing' if a side can't be paired. Tests set a small rename
    -- limit, or make it crash.
    pairFiles :: Pair Listed -> IO (Maybe FilePairing),
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
        loadLangs = \logger -> do
          env <- getEnvironment
          Lang.loadLangs logger (`lookup` env),
        prefsPath = (\env -> prefsFile (`lookup` env)) <$> getEnvironment,
        probeFiles = Source.probeFiles,
        readProject = Session.Project.readProject,
        warmUp =
          [ Parser.warmUpParser,
            warmUpResolve,
            warmUpPages,
            warmUpSource,
            warmUpRefs,
            Lang.warmUpLangs
          ],
        readEntry = readSourceEntry,
        parse = Parser.parseGuarded,
        pairFiles = pairListed defaultSettings,
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
  | -- | git, or @--diff@.
    BadProbe ProbeError
  deriving (Eq, Show)

-- | Message for the user, with the program name in front.
renderStartupError :: StartupError -> Text
renderStartupError err =
  "haskell-reviewer: " <> case err of
    BadRoot e -> renderRootError e
    BadTraceFile e -> renderTraceFileError e
    BadListen e -> renderListenError e
    BadProbe e -> renderProbeError e

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
              langs <- evaluate =<< deps.loadLangs logger
              probed <- deps.probeFiles logger root opts.diff
              case probed of
                Left e -> startupFailed deps (BadProbe e) `finally` tracer.close
                Right probe ->
                  serve deps logger opts root defaults langs probe tracer shutdown t0 w0
                    `finally` tracer.close

-- | Listen, print the URL, serve until a signal. In diff mode the
-- object reader wraps it all, so it stops after the listing thread.
-- See Note [Shutdown].
serve ::
  Deps ->
  Logger ->
  Options ->
  Root ->
  UiDefaults ->
  Langs ->
  Probe ->
  Tracer ->
  Shutdown ->
  MonotonicTime ->
  UTCTime ->
  IO ExitCode
serve deps logger opts root defaults langs probe tracer shutdown t0 w0 = case probe of
  BrowseProbe l -> go (BrowseListers l)
  DiffProbe info with -> do
    -- Built here, on this thread, before warp (Q-RTS-BLACKHOLE).
    let ls = diffInfoLines info
    _ <- evaluate (sum (map Text.length ls))
    mapM_ (logVerbose logger) ls
    with (go . DiffListers info)
  where
    go listers =
      serveSides deps logger opts root defaults langs listers tracer shutdown t0 w0

-- | The listers of a session, by mode.
data Listers
  = BrowseListers Lister
  | -- | With what @--diff@ resolved to.
    DiffListers DiffInfo (Pair Lister)

-- | 'serve', with the listers.
serveSides ::
  Deps ->
  Logger ->
  Options ->
  Root ->
  UiDefaults ->
  Langs ->
  Listers ->
  Tracer ->
  Shutdown ->
  MonotonicTime ->
  UTCTime ->
  IO ExitCode
serveSides deps logger opts root defaults langs listers tracer shutdown t0 w0 = do
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
          warmUpSearch
          warmUpTextSearch
          warmUpSideNames
          warmUpListing
          warmUpObjects
          warmUpWorkTree
          warmUpRenames
          -- On this thread, before warp: see Note [Forced snapshot] in
          -- "HaskellReviewer.Server.Api".
          session <- newSessionWork listers
          let sides = sessionSides session
          slots <- strictSides ((.slot) <$> sides)
          diffSlot <- forcedMaybe (sessionDiffSlot session)
          share <- newSessionShare
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
                    indexing = indexingStates slots,
                    minGap = indexingGap,
                    logger = logger
                  }
              files =
                FilesApi
                  { snapshotOn = \s -> awaitSnapshot . (.slot) <$> lookupSide s sides,
                    readEntry = \snap -> deps.readEntry snap.source,
                    fileNotes = \snap -> fileNotes snap.source,
                    langs = langs,
                    logger = logger
                  }
              routes =
                Routes {events = events, files = files, prefs = prefs, diff = diffSlot}
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
              -- The finally covers a cancel anywhere once the thread
              -- runs: every slot is filled.
              listing =
                ( takeMVar start
                    >> runListing deps logger tracer share session opts.ghc
                    >>= runIndexing deps logger tracer
                )
                  `finally` stopIndexing session
              serving _ = do
                r <- race waitStop (listener.serve ready app)
                step "warp stopped, stopping the listing"
                pure r
              closing = step "closing the socket" >> listener.close
          -- stopIndexing again: a cancel can come before the listing
          -- thread runs at all, and then its finally never does.
          result <- withAsync listing serving `finally` (stopIndexing session >> closing)
          step "socket closed, exiting"
          case result of
            Left StopSignal -> pure ExitSuccess
            Left (StopIdle m) -> do
              deps.console (idleExitMessage m)
              pure ExitSuccess
            Right () -> throwIO (userError "the HTTP server stopped")

-- | One side of the session: how to list it, and its slot.
data SideWork = SideWork
  { side :: Side,
    lister :: Lister,
    slot :: SnapshotSlot
  }

-- | The session's sides, by mode. Diff mode also pairs the files.
data SessionWork
  = BrowseWork SideWork
  | DiffModeWork DiffWork (Pair SideWork)

-- | Diff mode: what @--diff@ resolved to, and the pairing's slot.
data DiffWork = DiffWork
  { info :: DiffInfo,
    slot :: DiffSlot
  }

-- | The slots of a session, with no thunk left in the structure:
-- request threads read it (Q-RTS-BLACKHOLE).
newSessionWork :: Listers -> IO SessionWork
newSessionWork = \case
  BrowseListers l -> do
    w <- sideWork TreeSide l
    pure $! BrowseWork w
  DiffListers info p -> do
    h <- sideWork HeadSide p.head
    b <- sideWork BaseSide p.base
    slot <- newDiffSlot
    d <- evaluate DiffWork {info = info, slot = slot}
    pair <- evaluate Pair {head = h, base = b}
    pure $! DiffModeWork d pair
  where
    sideWork s l = newSnapshotSlot >>= \slot -> evaluate (SideWork s l slot)

-- | The sides of a session.
sessionSides :: SessionWork -> Sides SideWork
sessionSides = \case
  BrowseWork w -> BrowseSides w
  DiffModeWork _ p -> DiffSides p

-- | The pairing's slot, in diff mode.
sessionDiffSlot :: SessionWork -> Maybe DiffSlot
sessionDiffSlot = \case
  BrowseWork _ -> Nothing
  DiffModeWork d _ -> Just d.slot

-- | The same sides, with no thunk left in the structure: request
-- threads read it (Q-RTS-BLACKHOLE). Not the values' insides.
strictSides :: Sides a -> IO (Sides a)
strictSides = \case
  BrowseSides a -> do
    a' <- evaluate a
    pure $! BrowseSides a'
  DiffSides p -> do
    h <- evaluate p.head
    b <- evaluate p.base
    pair <- evaluate Pair {head = h, base = b}
    pure $! DiffSides pair

-- | The value and what is in it, to WHNF: no thunk goes in a slot
-- (Q-RTS-BLACKHOLE).
forcedEither :: Either a b -> IO (Either a b)
forcedEither x =
  evaluate x >>= \case
    Left a -> Left <$!> evaluate a
    Right b -> Right <$!> evaluate b

-- | Like 'forcedEither'.
forcedMaybe :: Maybe a -> IO (Maybe a)
forcedMaybe x =
  evaluate x >>= \case
    Nothing -> pure Nothing
    Just a -> Just <$!> evaluate a

-- | When a step started, and how long it took.
data Timing = Timing
  { start :: UTCTime,
    duration :: Nanoseconds
  }

-- | How one side's listing ended.
data Listing
  = -- | Listed, after this long.
    ListingOk Listed Timing
  | -- | The listing gave an error, after this long.
    ListingFailed ListError Timing
  | -- | The listing threw, with the message.
    ListingThrew Text

-- | How one side ended so far: with an @a@, or not.
data SideResult a
  = SideOk a
  | -- | The listing gave an error, after this long.
    SideNotListed ListError Timing
  | -- | The listing or the build threw, with the message.
    SideCrashed Text

-- | A side listed, with its modules: what pairing and markers need.
data Prepared = Prepared
  { listed :: Listed,
    modules :: ModuleIndex,
    -- | Its modules' keys, for diff mode: the changed modules and the
    -- markers. Lazy: made once, only if used, on the listing thread.
    keys :: [(ModuleId, ModuleKey)],
    files :: Timing,
    -- | Reading the project and finding the modules.
    project :: Timing
  }

-- | A side built.
data Built = Built
  { prepared :: Prepared,
    snapshot :: Snapshot,
    -- | From reading the project to the snapshot built.
    project :: Timing
  }

-- | A side and how it ended so far.
data SideDone a = SideDone
  { work :: SideWork,
    result :: SideResult a
  }

-- | Make every side's file list, read its project, pair the files in
-- diff mode, fill the slots, report. Gives back the snapshots in the
-- slots, head first. See Note [Listing after the URL].
--
-- Every slot is filled here, or by 'stopIndexing' on a cancel: else
-- requests would wait for ever.
runListing ::
  Deps ->
  Logger ->
  Tracer ->
  SessionShare ->
  SessionWork ->
  Maybe GhcVersion ->
  IO [Snapshot]
runListing deps logger tracer share session cliGhc = do
  result <- try @SomeException $ do
    -- Listed at once, built one at a time, head first: building reads
    -- the project and forces shared values (Q-RTS-BLACKHOLE).
    listings <- forConcurrently work (listSide deps)
    prepared <- mapM (uncurry (prepareSide deps logger cliGhc)) (zip work listings)
    -- Before the snapshots: they encode the module trees with the
    -- markers.
    paired <- case session of
      DiffModeWork d _ -> pairSides deps logger prepared d
      BrowseWork _ -> pure PairNotRun
    done <- mapM (buildSide deps share (markersOf paired)) prepared
    -- Before the warm-ups and the fill: no worker or request runs yet.
    changedFirst logger done
    t2 <- deps.clock.monotonic
    w2 <- deps.clock.now
    let built = [b.snapshot | SideDone {result = SideOk b} <- done]
    -- Warm up once, before any fill: requests parse. See Note [Forced
    -- snapshot] in "HaskellReviewer.Server.Api".
    unless (null built) (runWarmUp deps logger)
    t3 <- deps.clock.monotonic
    forM_ done $ \d -> do
      -- No thunk goes in a slot (Q-RTS-BLACKHOLE).
      v <- forcedEither (slotValue d.result)
      fillSnapshot d.work.slot v
    case session of
      DiffModeWork d _ -> do
        v <- forcedEither (diffBody paired)
        void (fillDiff d.slot v)
      BrowseWork _ -> pure ()
    -- After the fill: a slow trace file can't hold up requests.
    forM_ done $ \d -> case d.result of
      SideOk b -> do
        tracer.record (timed "files" d.work.side b.prepared.files)
        tracer.record (timed "project" d.work.side b.project)
      SideNotListed _ files -> tracer.record (timed "files" d.work.side files)
      SideCrashed _ -> pure ()
    unless (null built) $
      tracer.record
        Span {name = "parser-warm-up", start = w2, duration = elapsed t2 t3}
    forM_ done $ \d -> case d.result of
      SideOk b -> do
        reportFiles d.work.side b.snapshot.files b.prepared.files.duration
        reportProject d.work.side b.snapshot.modules b.project.duration
      SideNotListed e _ -> reportError d.work.side e
      SideCrashed msg -> reportError d.work.side (ListCrashed msg)
    unless (null built) (reportWarmUp (elapsed t2 t3))
    case paired of
      PairedOk o -> do
        tracer.record (pairingSpan o.timing)
        logVerbose logger o.report
      PairFailed _ t -> tracer.record (pairingSpan t)
      PairNotRun -> pure ()
    pure built
  case result of
    Right built -> pure built
    Left e
      -- A cancel: 'stopIndexing' fills the slots.
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let err = ListCrashed (exceptionMessage e)
          forM_ work $ \w -> do
            filled <- fillSnapshot w.slot (Left err)
            when filled (reportError w.side err)
          -- A crash in pairing is caught there: this one is in the
          -- listing (or after the fill, and then this does nothing).
          forM_ (sessionDiffSlot session) $ \slot -> fillDiff slot (Left DiffNotListed)
          -- It may have crashed after the fill: then the snapshots are
          -- served, and their modules still need the workers.
          slots <- mapM (awaitSnapshot . (.slot)) work
          pure [snap | Right snap <- slots]
  where
    work = toList (sessionSides session)
    slotValue = \case
      SideOk b -> Right b.snapshot
      SideNotListed e _ -> Left e
      SideCrashed msg -> Left (ListCrashed msg)
    diffBody = \case
      PairedOk o -> Right o.body
      PairFailed problem _ -> Left problem
      PairNotRun -> Left DiffNotListed
    markersOf paired side = case paired of
      PairedOk o | Just m <- onSide side o.markers -> m
      _ -> Map.empty
    pairingSpan t = Span {name = "pairing", start = t.start, duration = t.duration}
    timed what side t =
      Span {name = spanName what side, start = t.start, duration = t.duration}
    reportError side e =
      logInfo logger $
        "haskell-reviewer: " <> sideTag side <> "cannot list files: " <> renderListError e
    reportFiles side fl (Nanoseconds ns) = do
      when (fl.skippedNames > 0) $
        logInfo logger $
          "haskell-reviewer: "
            <> sideTag side
            <> "skipped "
            <> showText fl.skippedNames
            <> " file name(s) that are not UTF-8"
      when fl.truncated $
        logInfo logger $
          "haskell-reviewer: "
            <> sideTag side
            <> "more than "
            <> showText fileListCap
            <> " files under ROOT; listing the first "
            <> showText fileListCap
      let from = case fl.source of
            FromGit -> "git"
            FromDisk -> "disk"
      logVerbose logger $
        sideTag side
          <> "files: "
          <> showText (length fl.entries)
          <> " from "
          <> from
          <> " in "
          <> showText (ns `div` 1_000_000)
          <> " ms"
    reportProject side idx (Nanoseconds ns) =
      logVerbose logger $
        sideTag side
          <> "project: "
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

-- | A diff side's value of a pair. 'Nothing' for the browse side, which
-- has no pair.
onSide :: Side -> Pair a -> Maybe a
onSide side p = case side of
  HeadSide -> Just p.head
  BaseSide -> Just p.base
  TreeSide -> Nothing

-- | In diff mode, push each side's changed modules to the front of its
-- queue (D-317). Only when both sides are built. A crash only logs a
-- line: indexing works without it.
changedFirst :: Logger -> [SideDone Built] -> IO ()
changedFirst logger done = case (lookup HeadSide built, lookup BaseSide built) of
  (Just h, Just b) ->
    try @SomeException (go h b) >>= \case
      Right () -> pure ()
      Left e
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise ->
            logInfo logger $
              "haskell-reviewer: cannot find the changed modules: "
                <> exceptionMessage e
  _ -> pure ()
  where
    built = [(w.side, b) | SideDone {work = w, result = SideOk b} <- done]
    go hb bb = do
      let changed =
            changedModules Pair {head = hb.prepared.keys, base = bb.prepared.keys}
      nh <- evaluate (length changed.head)
      nb <- evaluate (length changed.base)
      atomically $ do
        prioritise hb.snapshot.index changed.head
        prioritise bb.snapshot.index changed.base
      logVerbose logger $
        "changed: "
          <> showText nh
          <> " on head, "
          <> showText nb
          <> " on base, indexed first"

-- | How pairing ended, before the fill.
data Paired
  = PairedOk PairOutput
  | -- | Pairing threw, or a side can't be paired; after this long.
    PairFailed DiffProblem Timing
  | -- | A side was not listed.
    PairNotRun

-- | What a pairing gave, all forced.
data PairOutput = PairOutput
  { -- | The body of api/diff-files.
    body :: LBS.ByteString,
    markers :: Pair (Map ModuleId Marker),
    timing :: Timing,
    -- | For --verbose, after the fill.
    report :: Text
  }

-- | In diff mode, once both sides are listed and their projects read:
-- pair their files, make the api/diff-files body and the module markers
-- (D-325, D-326). All forced: request threads share them
-- (Q-RTS-BLACKHOLE). A crash logs a line; then api/diff-files is a 500
-- and the markers are null. A cancel goes through.
pairSides :: Deps -> Logger -> [SideDone Prepared] -> DiffWork -> IO Paired
pairSides deps logger done diff = case (lookup HeadSide ready, lookup BaseSide ready) of
  (Just h, Just b) -> do
    t0 <- deps.clock.monotonic
    w0 <- deps.clock.now
    result <- try @SomeException (pairPrepared Pair {head = h, base = b})
    t1 <- deps.clock.monotonic
    let timing = Timing {start = w0, duration = elapsed t0 t1}
        Nanoseconds ns = timing.duration
        ms = showText (ns `div` 1_000_000) <> " ms"
    case result of
      Right (Just o) ->
        pure . PairedOk $
          PairOutput
            { body = o.body,
              markers = o.markers,
              timing = timing,
              report = pairingLine o.pairing <> " in " <> ms
            }
      -- Only tests have diff sessions on disk.
      Right Nothing -> pure (PairFailed (DiffNotPaired "a side is read from disk") timing)
      Left e
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise -> do
            let msg = exceptionMessage e
            logInfo logger ("haskell-reviewer: cannot pair files: " <> msg)
            pure (PairFailed (DiffNotPaired msg) timing)
  _ -> pure PairNotRun
  where
    ready = [(w.side, p) | SideDone {work = w, result = SideOk p} <- done]
    pairPrepared sides =
      deps.pairFiles ((.listed) <$> sides) >>= \case
        Nothing -> pure Nothing
        Just fp -> Just <$> encodePairing sides fp
    encodePairing sides fp = do
      let p = fp.pairing
      _ <- evaluate (forcePairing p)
      let sources = (.listed.source) <$> sides
          filtered =
            Set.fromList
              [ m.path
              | Modified m <- p.changes,
                (fileNotes sources.head m.path).filtered == Filtered
              ]
          files =
            DiffFiles
              { base = ends.base,
                head = ends.head,
                pairing = p,
                filtered = filtered,
                outside = outsideChanges <$> traverse sideOutside sources,
                truncated = fp.cut
              }
          body = encodeDiffFiles firstGeneration files
      _ <- evaluate (LBS.length body)
      let facts s = moduleFacts s.keys s.modules
          markers = moduleMarkers p (facts <$> sides)
      _ <- evaluate (forceMarkers markers.head)
      _ <- evaluate (forceMarkers markers.base)
      pure Encoded {pairing = p, body = body, markers = markers}
    sideOutside = \case
      CommitEntries c -> Just c.outside
      WorkTreeEntries w -> w.outside
      _ -> Nothing
    ends = case diff.info of
      CommitsDiff ci ->
        Pair
          { head = DiffEnd {label = ci.range.head, commit = Just ci.sides.head},
            base = DiffEnd {label = ci.range.base, commit = Just ci.sides.base}
          }
      WorkTreeDiff wi ->
        Pair
          { head = DiffEnd {label = "working tree", commit = Nothing},
            base = DiffEnd {label = wi.rev, commit = Just wi.base}
          }
    forceMarkers :: Map ModuleId Marker -> ()
    forceMarkers = Map.foldl' (\() m -> m.change `seq` m.settingsChange `seq` ()) ()
    pairingLine p =
      "pairing: "
        <> showText (length p.changes)
        <> " changed, "
        <> showText (length [() | Renamed _ <- p.changes])
        <> " renamed"
        <> case p.limit of
          WithinLimits -> ""
          OverRenameLimit -> ", over the rename limit"
          OverWorkBudget -> ", over the work budget"

-- | A pairing, encoded.
data Encoded = Encoded
  { pairing :: Pairing,
    body :: LBS.ByteString,
    markers :: Pair (Map ModuleId Marker)
  }

-- | List one side. A crash fails this side only; a cancel goes through.
listSide :: Deps -> SideWork -> IO Listing
listSide deps w =
  catchCrash ListingThrew $ do
    t0 <- deps.clock.monotonic
    w0 <- deps.clock.now
    listed <- w.lister.list
    t1 <- deps.clock.monotonic
    let files = Timing {start = w0, duration = elapsed t0 t1}
    pure $ case listed of
      Left e -> ListingFailed e files
      Right l -> ListingOk l files

-- | Read one listed side's project and find its modules. If reading the
-- project crashes, the side has no project, only a warning. A crash
-- past that fails this side only; a cancel goes through. See Note
-- [Listing after the URL].
prepareSide ::
  Deps -> Logger -> Maybe GhcVersion -> SideWork -> Listing -> IO (SideDone Prepared)
prepareSide deps logger cliGhc w listing =
  SideDone w <$> case listing of
    ListingFailed e files -> pure (SideNotListed e files)
    ListingThrew msg -> pure (SideCrashed msg)
    ListingOk l files -> catchCrash SideCrashed $ do
      t1 <- deps.clock.monotonic
      w1 <- deps.clock.now
      modules <- modulesOf deps logger w.side l cliGhc
      t2 <- deps.clock.monotonic
      pure . SideOk $
        Prepared
          { listed = l,
            modules = modules,
            keys = moduleKeys (sourceBlob l.source) modules,
            files = files,
            project = Timing {start = w1, duration = elapsed t1 t2}
          }

-- | Build one side's snapshot, with its markers. A crash fails this
-- side only; a cancel goes through.
buildSide ::
  Deps ->
  SessionShare ->
  (Side -> Map ModuleId Marker) ->
  SideDone Prepared ->
  IO (SideDone Built)
buildSide deps share markersOn d =
  SideDone d.work <$> case d.result of
    SideNotListed e files -> pure (SideNotListed e files)
    SideCrashed msg -> pure (SideCrashed msg)
    SideOk p -> catchCrash SideCrashed $ do
      t1 <- deps.clock.monotonic
      let side = d.work.side
      snap <-
        newSnapshot side share p.listed.source p.listed.files p.modules (markersOn side)
      t2 <- deps.clock.monotonic
      let Nanoseconds before = p.project.duration
          Nanoseconds after = elapsed t1 t2
          project =
            Timing {start = p.project.start, duration = Nanoseconds (before + after)}
      pure (SideOk Built {prepared = p, snapshot = snap, project = project})

-- | Run it; a crash gives the message to the handler, a cancel goes
-- through.
catchCrash :: (Text -> a) -> IO a -> IO a
catchCrash onCrash act =
  try @SomeException act >>= \case
    Right r -> pure r
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (onCrash (exceptionMessage e))

-- | A trace span's name: as is in browse mode, with the side in diff
-- mode (@files:head@).
spanName :: Text -> Side -> Text
spanName what = \case
  TreeSide -> what
  s -> what <> ":" <> sideName s

-- | The deps' warm-ups, each in its own try: a crash only logs a line,
-- and the next one still runs. The snapshot is fine without them.
runWarmUp :: Deps -> Logger -> IO ()
runWarmUp deps logger =
  forM_ deps.warmUp $ \w ->
    try @SomeException w >>= \case
      Right () -> pure ()
      Left e
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise ->
            logInfo logger $
              "haskell-reviewer: parser warm-up failed: " <> exceptionMessage e

-- | Read the project and find its modules. If that crashes, the side
-- has no project, only a warning. See Note [Listing after the URL].
modulesOf :: Deps -> Logger -> Side -> Listed -> Maybe GhcVersion -> IO ModuleIndex
modulesOf deps logger side listed cliGhc = do
  result <- try @SomeException $ do
    project <- deps.readProject listed.source listed.files hostPlatform cliGhc
    logVerbose logger $
      sideTag side
        <> "ghc: "
        <> renderGhcVersion project.ghc.version
        <> " ("
        <> renderGhcSource project.ghc.source
        <> ")"
    forced (buildModuleIndex project listed.files)
  case result of
    Right idx -> pure idx
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let msg = exceptionMessage e
          logInfo logger $
            "haskell-reviewer: " <> sideTag side <> "cannot read the project: " <> msg
          forced (buildModuleIndex (crashed msg) listed.files)
  where
    -- Enough to hit a crash here; the snapshot forces the rest.
    forced idx = do
      _ <- evaluate idx
      _ <- evaluate (Set.size idx.ids + Map.size idx.paths + length idx.packages)
      pure idx
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

-- | Run the parse workers until every module of every side is done,
-- then report. A crash (even before the workers start) fails the
-- modules not done; a cancel goes on to 'stopIndexing'. Nothing to do
-- if no side was listed. See Note [Listing after the URL].
runIndexing :: Deps -> Logger -> Tracer -> [Snapshot] -> IO ()
runIndexing deps logger tracer = \case
  [] -> pure ()
  snaps ->
    indexAndReport deps logger tracer snaps `catch` \e -> case fromException e of
      Just (_ :: SomeAsyncException) -> throwIO e
      Nothing -> do
        let msg = exceptionMessage e
        forM_ snaps $ \snap -> failUnfinished snap.index (IndexCrashed msg)
        logInfo logger ("haskell-reviewer: indexing crashed: " <> msg)

-- | However the listing thread ends, every slot is filled (the diff
-- slot too) and no module is left Pending or Parsing: requests would
-- wait for ever. For a cancel (shutdown) anywhere, or a crash after
-- the fill. A slot filled already keeps what it has.
stopIndexing :: SessionWork -> IO ()
stopIndexing session = do
  forM_ (sessionDiffSlot session) $ \slot -> fillDiff slot (Left DiffNotListed)
  forM_ (sessionSides session) $ \w -> do
    _ <- fillSnapshot w.slot (Left (ListCrashed "the listing was stopped"))
    atomically (snapshotSTM w.slot) >>= \case
      Just (Right snap) ->
        failUnfinished snap.index (IndexCrashed "indexing was stopped")
      _ -> pure ()

-- | One pool of workers over every side's index, head first (G19), and
-- one line for all of them.
indexAndReport :: Deps -> Logger -> Tracer -> [Snapshot] -> IO ()
indexAndReport deps logger tracer snaps = do
  workers <- deps.indexWorkers
  total <- sum <$> mapM (\snap -> (.total) <$> atomically (readProgress snap.index)) snaps
  logVerbose logger $
    "indexing: "
      <> showText total
      <> " modules, "
      <> showText workers
      <> " workers"
  t0 <- deps.clock.monotonic
  w0 <- deps.clock.now
  let jobs = [IndexJob {deps = indexDeps snap, index = snap.index} | snap <- snaps]
  result <- try @SomeException (runWorkers workers jobs)
  t1 <- deps.clock.monotonic
  case result of
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise ->
          logInfo logger ("haskell-reviewer: indexing crashed: " <> exceptionMessage e)
  states <- concatMap Map.elems <$> mapM (atomically . readStates . (.index)) snaps
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
  -- Diff mode: how much the sides shared (D-320). All indexes are in
  -- one pool, so any of them has the counts.
  case snaps of
    snap : _ | any (\s -> s.side /= TreeSide) snaps -> do
      k <- atomically (shareCounts snap.index)
      logVerbose logger $
        "indexing: " <> showText k.parses <> " parsed, " <> showText k.shares <> " shared"
    _ -> pure ()
  where
    indexDeps snap =
      IndexDeps
        { readModule = \p -> case Map.lookup p snap.files.entries of
            -- Slots only have files of the list.
            Just EntryFile -> deps.readEntry snap.source p EntryFile
            _ -> pure (Left ReadGone),
          parse = deps.parse,
          logger = logger,
          label = sideTag snap.side
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
