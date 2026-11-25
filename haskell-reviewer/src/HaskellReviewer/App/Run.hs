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
import Control.Concurrent.Async
  ( Async,
    AsyncCancelled (..),
    cancelWith,
    forConcurrently,
    race,
    withAsync,
  )
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
    onException,
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
import HaskellReviewer.Core.DiffTarget (ChangeIndex, changeIndex, forceChangeIndex)
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
  ( BrokenPackage (..),
    CabalProblem (..),
    DetectedGhc (..),
    FileProblem (..),
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
  ( DiffInfo (..),
    Revspec,
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
    sharedBlob,
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
import HaskellReviewer.Server.Diff
  ( DiffRoutes (..),
    diffTimeLimit,
    guardedLineDiff,
    warmUpDiff,
  )
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
import HaskellReviewer.Server.Reload (warmUpReload)
import HaskellReviewer.Server.Router (Routes (..), application, warmUpGzip)
import HaskellReviewer.Server.Search (warmUpSearch)
import HaskellReviewer.Server.TextSearch (warmUpTextSearch)
import HaskellReviewer.Server.Wire
  ( DiffFiles (..),
    SessionInfo (..),
    diffEnds,
    encodeDiffFiles,
    encodeSession,
  )
import HaskellReviewer.Server.Wire.Page (warmUpPages)
import HaskellReviewer.Server.Wire.Source (warmUpSource)
import HaskellReviewer.Session.Clients (newClients, waitIdle)
import HaskellReviewer.Session.Diff
  ( DiffProblem (..),
    DiffReady (..),
    DiffSlot,
    fillDiff,
    newDiffSlot,
  )
import HaskellReviewer.Session.Index
  ( IndexDeps (..),
    IndexFailure (..),
    IndexJob (..),
    Indexed (..),
    Progress (..),
    ShareCounts (..),
    SlotState (..),
    StopWorkers (..),
    failUnfinished,
    prioritise,
    pruneCache,
    readProgress,
    readStates,
    runWorkers,
    shareCounts,
  )
import HaskellReviewer.Session.Live
  ( Current (..),
    DiffLive (..),
    Live,
    LiveSlots (..),
    awaitSideSTM,
    indexingNow,
    newLive,
    readCurrent,
    swapLive,
  )
import HaskellReviewer.Session.Prefs (newPrefsStore)
import HaskellReviewer.Session.Project qualified as Session.Project
import HaskellReviewer.Session.Reload
  ( Batch,
    ReloadOutcome (..),
    ReloadQueue,
    answerReloads,
    closeReloads,
    newReloadQueue,
    requestReload,
    takeReloads,
  )
import HaskellReviewer.Session.Resolve (warmUpResolve)
import HaskellReviewer.Session.Snapshot
  ( Generation (..),
    SessionShare (..),
    Snapshot (..),
    SnapshotSlot,
    awaitSnapshot,
    fillSnapshot,
    firstGeneration,
    newSessionShare,
    newSnapshotSlot,
    nextGeneration,
    snapshotSTM,
  )
import HaskellReviewer.Session.TextStore (pruneBlobTexts)
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

A POST api/reload still waiting, or one that comes later, gets 503:
the listing thread answers every waiter on its way out (Note [Reload
queue] in "HaskellReviewer.Session.Reload").

With --verbose, each step of the way out logs a "stop: ..." line, so a
hang shows where it is. The signal handler writes its own line straight
to fd 2, past the logger.
-}

{- Note [Startup work]

Before the URL line we only do quick things (SPEC §10.1): check ROOT,
open the trace file, read config.toml (one small file), check git,
listen, warm up the gzip, prefs, reload and search code, the side names
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

Once every slot is filled, the parse workers run in a child of this
thread, one per capability, in one pool over every listed side's
index, head first (Note [Indexing] in "HaskellReviewer.Session.Index").
A request computing parks one of them, never the last. So parsing
never delays the URL either. No workers if no side was listed: there
is nothing to index. However the thread ends (a crash, or a cancel at
shutdown), every slot of the current generation (the diff slot too) is
filled and the modules not done are set Failed, so no request waits
for ever.

If reading the project crashes (a bug), the snapshot gets no packages
and a warning, logged and shown in the module tree. The file routes
still work.

The thread lives as long as the server (withAsync): on exit it is
cancelled, with the workers, and a git it runs is killed (Note [Git
hardening] in "HaskellReviewer.Effects.Git").

Reload (D-351, D-352): browse mode and --diff REV, not a commit
range (nothing there can change). Once the first generation is built,
the same thread waits for POST api/reload (Note [Reload queue] in
"HaskellReviewer.Session.Reload"). For each batch it lists again, with
the same listers, and builds the next generation into new slots that
no request sees, while the old workers go on. With --diff REV, base
keeps its listing and project from the last generation (its commit
can't change), unless it wasn't built then, or its project read
crashed or could not read a file; its snapshot is still built again,
with the new generation and markers. The kept module index is shared
by the old snapshot and the new build: fine for Q-RTS-BLACKHOLE, as
the first build forced it the same way, so this build finds nothing
left to force. Pairing runs again, so the diff slot, the markers and
the outside count are new. No warm-ups: they ran at start-up. If a
side can't be listed or built, the old generation stays and the batch
gets a 500. Else:

1. the changed modules go first, as above, and every slot is filled;
2. the new generation is swapped in (Note [Live] in
   "HaskellReviewer.Session.Live"): new requests see it, old ones keep
   their snapshot;
3. the old workers are stopped and we wait for them to end (one
   runWorkers per pool, D-314); their modules not done fail with
   "replaced by a reload";
4. the old modules not done are failed again: a stop that came before
   the old runWorkers ran failed nothing;
5. the parse cache and the shared texts drop what the new snapshots
   don't use (Note [Indexing] in "HaskellReviewer.Session.Index",
   Note [Text store] in "HaskellReviewer.Session.TextStore"); no
   worker runs now;
6. the new workers are started, then the batch gets 200 with the new
   generation.

A build that is cancelled fills its own slots too (D-313). A build
that crashes (a bug) gets 500 "cannot reload".

If the thread itself crashes (a bug outside the steps above), it logs
a line and the server goes on without indexing or reloads: POSTs get
503 "reload is not available: <why>".

A browse reload parses every module again: browse mode has no blob
ids to share parses by (Note [Indexing] in
"HaskellReviewer.Session.Index"). With --diff REV, only the files read
from disk (dirty or untracked) are parsed again: the rest hit the
cache.
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
    -- | Build the shared values of the diff route ('warmUpDiff'), in
    -- diff mode, before its slot is filled. About 50 ms: tests skip it.
    diffWarmUp :: IO (),
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
        diffWarmUp = warmUpDiff,
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
          warmUpReload
          -- On this thread, before warp: see Note [Forced snapshot] in
          -- "HaskellReviewer.Server.Api".
          session <- newSessionWork listers
          live <- newLive =<< currentOf firstGeneration session
          let sessionBody = encodeSession (sessionInfo session)
          _ <- evaluate (LBS.length sessionBody)
          share <- newSessionShare
          reloads <- newReloadQueue
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
                    current = indexingNow live,
                    minGap = indexingGap,
                    logger = logger
                  }
              files =
                FilesApi
                  { snapshotOn = \s -> case lookupSide s (sessionSides session) of
                      Nothing -> Nothing
                      -- The sides never change: every generation has it.
                      Just _ -> Just (awaitSide live s),
                    readEntry = \snap -> deps.readEntry snap.source,
                    fileNotes = \snap -> fileNotes snap.source,
                    langs = langs,
                    logger = logger
                  }
              diffRoutes = case session of
                BrowseWork _ -> Nothing
                DiffModeWork _ _ ->
                  Just
                    DiffRoutes
                      { current = diffCurrent live,
                        lineDiff = guardedLineDiff diffTimeLimit
                      }
              routes =
                Routes
                  { events = events,
                    files = files,
                    prefs = prefs,
                    diff = diffRoutes,
                    session = sessionBody,
                    reload =
                      if canReload listers
                        then Just (requestReload reloads queued)
                        else Nothing
                  }
              queued = logDebug logger "reload: queued"
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
              driver =
                Driver
                  { builder =
                      Builder
                        { deps = deps,
                          logger = logger,
                          tracer = tracer,
                          share = share,
                          cliGhc = opts.ghc
                        },
                    live = live,
                    reloads = reloads,
                    listers = listers
                  }
              -- Scoped: on the way out it's cancelled, and git with it.
              -- The finally covers a cancel anywhere once the thread
              -- runs: every slot is filled, every reload answered.
              listing =
                ((takeMVar start >> runDriver driver session) `catch` driverCrashed)
                  `finally` stopAll
              -- A bug: the server goes on, without indexing or reloads.
              driverCrashed e
                | Just AsyncCancelled <- fromException e = throwIO e
                | otherwise = do
                    let msg = exceptionMessage e
                    logInfo logger $
                      "haskell-reviewer: the listing thread crashed: " <> msg
                    closeReloads reloads (ReloadUnavailable msg)
              stopAll = stopIndexing live >> closeReloads reloads ReloadStopping
              serving _ = do
                r <- race waitStop (listener.serve ready app)
                step "warp stopped, stopping the listing"
                pure r
              closing = step "closing the socket" >> listener.close
          -- stopAll again: a cancel can come before the listing thread
          -- runs at all, and then its finally never does.
          result <- withAsync listing serving `finally` (stopAll >> closing)
          step "socket closed, exiting"
          case result of
            Left StopSignal -> pure ExitSuccess
            Left (StopIdle m) -> do
              deps.console (idleExitMessage m)
              pure ExitSuccess
            Right () -> throwIO (userError "the HTTP server stopped")

-- | What building a generation needs.
data Builder = Builder
  { deps :: Deps,
    logger :: Logger,
    tracer :: Tracer,
    share :: SessionShare,
    cliGhc :: Maybe GhcVersion
  }

-- | What the listing thread needs. See Note [Listing after the URL].
data Driver = Driver
  { builder :: Builder,
    live :: Live,
    reloads :: ReloadQueue,
    -- | To make each generation's sides.
    listers :: Listers
  }

-- | A generation swapped in by a reload, to answer once its workers
-- are started.
data Swapped = Swapped
  { generation :: Generation,
    made :: Made,
    batch :: Batch,
    -- | When the reload started, wall clock (the trace span).
    startWall :: UTCTime,
    -- | When the reload started, monotonic (its duration).
    startMono :: MonotonicTime
  }

-- | The listing thread: build the first generation and index it, then
-- serve reloads one at a time. See Note [Listing after the URL].
runDriver :: Driver -> SessionWork -> IO ()
runDriver d session = do
  made <- firstListing d.builder session
  loop firstGeneration made Nothing
  where
    b = d.builder
    -- One runWorkers at a time: 'waitReload' stops the old one before
    -- it returns, and only then the next one starts.
    loop gen made swapped = do
      let indexAll = runIndexing b.deps b.logger b.tracer made.snapshots
      next <- withAsync indexAll $ \indexing -> do
        forM_ swapped (answerSwapped d)
        waitReload d gen made indexing
      loop next.generation next.made (Just next)

-- | Wait for reload requests, build the next generation off to the
-- side, swap it in, stop the old workers. If the build fails, answer
-- at once, keep the old generation and wait again. See Note [Reload
-- queue] in "HaskellReviewer.Session.Reload".
waitReload :: Driver -> Generation -> Made -> Async () -> IO Swapped
waitReload d gen old indexing = do
  batch <- atomically (takeReloads d.reloads)
  t0 <- b.deps.clock.monotonic
  w0 <- b.deps.clock.now
  let next = nextGeneration gen
  work <- newSessionWork d.listers
  built <- reloadListing b next (keptSides d.listers old) work `onException` stopWork work
  case built of
    Left failure -> do
      answerReloads d.reloads batch $ case failure of
        SideFailed -> ReloadFailed
        BuildCrashed msg -> ReloadCrashed msg
      catchCrash (const ()) . logVerbose b.logger $
        "reload: failed, generation " <> generationText gen <> " kept"
      waitReload d gen old indexing
    Right made -> do
      c <- currentOf next work
      swapLive d.live c
      -- Its modules not done fail; then the pool is free for the new
      -- workers (D-313, D-314). Then again: a stop that came before
      -- runWorkers ran failed nothing. In a finally: a cancel here
      -- (shutdown) must not leave them Pending either.
      cancelWith indexing (StopWorkers replaced)
        `finally` forM_
          old.snapshots
          (\snap -> failUnfinished snap.index (IndexCrashed replaced))
      -- No worker runs now: nobody writes the caches.
      pruneShared b made.snapshots
      pure
        Swapped
          { generation = next,
            made = made,
            batch = batch,
            startWall = w0,
            startMono = t0
          }
  where
    b = d.builder
    replaced = "replaced by a reload"

-- | Tell the waiters, then log and trace the reload.
answerSwapped :: Driver -> Swapped -> IO ()
answerSwapped d sw = do
  answerReloads d.reloads sw.batch (Reloaded sw.generation)
  -- A trace or log write that throws must not end the listing thread.
  catchCrash (const ()) $ do
    t1 <- b.deps.clock.monotonic
    let took = elapsed sw.startMono t1
        Nanoseconds ns = took
    b.tracer.record Span {name = "reload", start = sw.startWall, duration = took}
    logVerbose b.logger $
      "reload: generation "
        <> generationText sw.generation
        <> " in "
        <> showText (ns `div` 1_000_000)
        <> " ms"
  where
    b = d.builder

-- | For log lines.
generationText :: Generation -> Text
generationText (Generation g) = showText g

-- | Whether the mode has POST api/reload: browse and @--diff REV@. Not
-- a commit range: nothing there can change (D-351, D-352).
canReload :: Listers -> Bool
canReload = \case
  BrowseListers _ -> True
  DiffListers (WorkTreeDiff _) _ -> True
  DiffListers (CommitsDiff _) _ -> False

-- | What a reload keeps from the last generation: with @--diff REV@,
-- base's listing and project, if it was built and its project read
-- worked ('ProjectReadOk'; its commit can't change). Else every side is listed again.
-- See D-352.
keptSides :: Listers -> Made -> [(Side, Prepared)]
keptSides listers made = case listers of
  DiffListers (WorkTreeDiff _) _ -> filter ((== BaseSide) . fst) made.prepared
  DiffListers (CommitsDiff _) _ -> []
  BrowseListers _ -> []

-- | Drop the parse results and the texts the new snapshots don't use
-- (D-352). Only between the old workers' end and the new ones' start:
-- then nobody writes them. A crash only logs a line.
pruneShared :: Builder -> [Snapshot] -> IO ()
pruneShared b snaps = do
  r <- catchCrash Left $ do
    parses <- pruneCache b.share.pool [snap.index | snap <- snaps]
    let blobs =
          Set.fromList
            [ o
            | snap <- snaps,
              (p, EntryFile) <- Map.toList snap.files.entries,
              Just o <- [sharedBlob snap.source p]
            ]
    texts <- pruneBlobTexts b.share.blobTexts blobs
    pure (Right (parses, texts))
  -- A log write that throws must not end the listing thread.
  catchCrash (const ()) $ case r of
    Right (parses, texts) ->
      logVerbose b.logger $
        "reload: dropped "
          <> showText parses
          <> " parse results and "
          <> showText texts
          <> " texts"
    Left msg -> logInfo b.logger ("haskell-reviewer: cannot prune the caches: " <> msg)

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

-- | What api/session says.
sessionInfo :: SessionWork -> SessionInfo
sessionInfo = \case
  BrowseWork _ -> BrowseSession
  DiffModeWork d _ -> DiffSession d.info

-- | A generation's slots, with no thunk left in the structure: request
-- threads read it (Q-RTS-BLACKHOLE).
currentOf :: Generation -> SessionWork -> IO Current
currentOf gen work = do
  slots <- case work of
    BrowseWork w -> BrowseSlots <$!> evaluate w.slot
    DiffModeWork d p -> do
      h <- evaluate p.head.slot
      b <- evaluate p.base.slot
      sides <- evaluate Pair {head = h, base = b}
      DiffSlots <$!> evaluate DiffLive {pairing = d.slot, sides = sides}
  evaluate Current {generation = gen, slots = slots}

-- | The current generation's snapshot of a side the session has. Waits
-- until it's listed.
awaitSide :: Live -> Side -> IO (Either ListError Snapshot)
awaitSide live s =
  atomically (awaitSideSTM live s) >>= \case
    Just r -> pure r
    -- The sides never change, and the route checked this one.
    Nothing -> throwIO (userError "awaitSide: not a side of the session")

-- | The current generation's pairing and sides, in diff mode.
diffCurrent :: Live -> IO DiffLive
diffCurrent live = do
  c <- readCurrent live
  case c.slots of
    DiffSlots d -> pure d
    -- The mode never changes: only diff mode has the route.
    BrowseSlots _ -> throwIO (userError "diffCurrent: not a diff session")

-- | The value and what is in it, to WHNF: no thunk goes in a slot
-- (Q-RTS-BLACKHOLE).
forcedEither :: Either a b -> IO (Either a b)
forcedEither x =
  evaluate x >>= \case
    Left a -> Left <$!> evaluate a
    Right b -> Right <$!> evaluate b

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
  | -- | Not listed: kept from the last generation (D-352).
    ListingKept Prepared

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
    -- | A side whose project read crashed, or could not read a file,
    -- is not kept for the next reload (D-352).
    projectRead :: ProjectRead,
    files :: FilesFrom,
    -- | Reading the project and finding the modules.
    project :: Timing
  }

-- | Whether reading a side's project worked.
data ProjectRead
  = ProjectReadOk
  | -- | It crashed, or a project file could not be read (a git read
    -- error can be a one-off): read it again on the next reload.
    ProjectReadAgain

-- | Where a side's listing came from, in this generation.
data FilesFrom
  = -- | Listed now, at this time.
    ListedNow Timing
  | -- | Kept from the last generation (D-352).
    KeptFiles

-- | A generation as built: its snapshots, head first, and its sides
-- that were built with their project read, for the next reload.
data Made = Made
  { snapshots :: [Snapshot],
    prepared :: [(Side, Prepared)]
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

-- | Whether a build runs the warm-ups: once, at start-up.
data WarmUps = WithWarmUps | NoWarmUps

-- | Why a reload built no generation. Logged already.
data ReloadFailure
  = -- | A side was not listed, or not built.
    SideFailed
  | -- | A crash outside the steps of one side (a bug), with the
    -- message.
    BuildCrashed Text

-- | List every side, read its project, pair the files in diff mode,
-- build the snapshots. Nothing is filled yet. Listed at once, built one
-- at a time, head first: building reads the project and forces shared
-- values (Q-RTS-BLACKHOLE). A side's crash fails that side only. A
-- side in the list given is not listed: it keeps that listing and
-- project.
buildSides ::
  Builder ->
  Generation ->
  [(Side, Prepared)] ->
  SessionWork ->
  IO (Paired, [SideDone Built])
buildSides b gen kept session = do
  let work = toList (sessionSides session)
      listOne w = case lookup w.side kept of
        Just p -> pure (ListingKept p)
        Nothing -> listSide b.deps w
  listings <- forConcurrently work listOne
  prepared <- mapM (uncurry (prepareSide b.deps b.logger b.cliGhc)) (zip work listings)
  -- Before the snapshots: they encode the module trees with the
  -- markers.
  paired <- case session of
    DiffModeWork d _ -> pairSides b.deps b.logger gen prepared d
    BrowseWork _ -> pure PairNotRun
  done <- mapM (buildSide b.deps b.share gen (markersOf paired)) prepared
  pure (paired, done)

-- | The first generation: build it, fill the slots, report. Gives back
-- the snapshots in the slots, head first. See Note [Listing after the
-- URL].
--
-- Every slot is filled here, or by 'stopIndexing' on a cancel: else
-- requests would wait for ever.
firstListing :: Builder -> SessionWork -> IO Made
firstListing b session = do
  result <- try @SomeException $ do
    (paired, done) <- buildSides b firstGeneration [] session
    fillAndReport b WithWarmUps session paired done
  case result of
    Right built -> pure built
    Left e
      -- A cancel: 'stopIndexing' fills the slots.
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let err = ListCrashed (exceptionMessage e)
          forM_ work $ \w -> do
            filled <- fillSnapshot w.slot (Left err)
            when filled (reportError b.logger w.side err)
          -- A crash in pairing is caught there: this one is in the
          -- listing (or after the fill, and then this does nothing).
          forM_ (sessionDiffSlot session) $ \slot -> fillDiff slot (Left DiffNotListed)
          -- It may have crashed after the fill: then the snapshots are
          -- served, and their modules still need the workers.
          slots <- mapM (awaitSnapshot . (.slot)) work
          -- Nothing kept: a reload lists every side again.
          pure Made {snapshots = [snap | Right snap <- slots], prepared = []}
  where
    work = toList (sessionSides session)

-- | A reload's generation, into slots no request sees yet: build it,
-- fill the slots, report. The sides given keep their listing. No
-- warm-ups: they ran at start-up. If a side is not built, or the build
-- crashes, the caller keeps the old generation. The slots are filled
-- anyway (D-313). A cancel goes through.
reloadListing ::
  Builder ->
  Generation ->
  [(Side, Prepared)] ->
  SessionWork ->
  IO (Either ReloadFailure Made)
reloadListing b gen kept work = do
  result <- try @SomeException $ do
    (paired, done) <- buildSides b gen kept work
    if all (sideOk . (.result)) done
      then Right <$> fillAndReport b NoWarmUps work paired done
      else do
        forM_ done $ \d -> case d.result of
          SideOk _ -> pure ()
          SideNotListed e _ -> reportError b.logger d.work.side e
          SideCrashed msg -> reportError b.logger d.work.side (ListCrashed msg)
        pure (Left SideFailed)
  case result of
    Right (Right made) -> pure (Right made)
    Right (Left failure) -> stopWork work >> pure (Left failure)
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let msg = exceptionMessage e
          logInfo b.logger ("haskell-reviewer: reload crashed: " <> msg)
          stopWork work
          pure (Left (BuildCrashed msg))
  where
    sideOk = \case
      SideOk _ -> True
      _ -> False

-- | Fill the slots with the built sides, then report: trace spans and
-- log lines. Gives back the snapshots, head first, and the sides
-- built. With 'WithWarmUps', the warm-ups run before the fill.
fillAndReport ::
  Builder -> WarmUps -> SessionWork -> Paired -> [SideDone Built] -> IO Made
fillAndReport b warmUps session paired done = do
  -- Before the warm-ups and the fill: no worker or request runs yet.
  changedFirst b.logger done
  t2 <- b.deps.clock.monotonic
  w2 <- b.deps.clock.now
  let built = [x.snapshot | SideDone {result = SideOk x} <- done]
      warm = case warmUps of
        WithWarmUps -> not (null built)
        NoWarmUps -> False
  -- Warm up once, before any fill: requests parse. See Note [Forced
  -- snapshot] in "HaskellReviewer.Server.Api".
  when warm (runWarmUp b.deps b.logger)
  t3 <- b.deps.clock.monotonic
  -- The diff route's shared values, before its slot is filled.
  diffWarmUp <- case (session, warmUps) of
    (DiffModeWork _ _, WithWarmUps) -> Just <$> timedDiffWarmUp b
    _ -> pure Nothing
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
    SideOk x -> do
      case x.prepared.files of
        ListedNow t -> b.tracer.record (timed "files" d.work.side t)
        KeptFiles -> pure ()
      b.tracer.record (timed "project" d.work.side x.project)
    SideNotListed _ files -> b.tracer.record (timed "files" d.work.side files)
    SideCrashed _ -> pure ()
  when warm $
    b.tracer.record
      Span {name = "parser-warm-up", start = w2, duration = elapsed t2 t3}
  forM_ done $ \d -> case d.result of
    SideOk x -> do
      case x.prepared.files of
        ListedNow t -> reportFiles b.logger d.work.side x.snapshot.files t.duration
        KeptFiles ->
          logVerbose b.logger $
            sideTag d.work.side
              <> "files: "
              <> showText (length x.snapshot.files.entries)
              <> " kept"
      reportProject b.logger d.work.side x.snapshot.modules x.project.duration
    SideNotListed e _ -> reportError b.logger d.work.side e
    SideCrashed msg -> reportError b.logger d.work.side (ListCrashed msg)
  when warm $ do
    let Nanoseconds ns = elapsed t2 t3
    logVerbose b.logger ("parser warm-up: " <> showText (ns `div` 1_000) <> " us")
  forM_ diffWarmUp $ \(Nanoseconds ns) ->
    logVerbose b.logger ("diff warm-up: " <> showText (ns `div` 1_000) <> " us")
  case paired of
    PairedOk o -> do
      b.tracer.record (pairingSpan o.timing)
      logVerbose b.logger o.report
    PairFailed _ t -> b.tracer.record (pairingSpan t)
    PairNotRun -> pure ()
  pure
    Made
      { snapshots = built,
        prepared =
          [ (w.side, x.prepared)
          | SideDone {work = w, result = SideOk x} <- done,
            ProjectReadOk <- [x.prepared.projectRead]
          ]
      }
  where
    slotValue = \case
      SideOk x -> Right x.snapshot
      SideNotListed e _ -> Left e
      SideCrashed msg -> Left (ListCrashed msg)
    diffBody = \case
      PairedOk o -> Right DiffReady {body = o.body, changes = o.changes}
      PairFailed problem _ -> Left problem
      PairNotRun -> Left DiffNotListed
    pairingSpan t = Span {name = "pairing", start = t.start, duration = t.duration}
    timed what side t =
      Span {name = spanName what side, start = t.start, duration = t.duration}

-- | One side's markers, from the pairing. None if it didn't run.
markersOf :: Paired -> Side -> Map ModuleId Marker
markersOf paired side = case paired of
  PairedOk o | Just m <- onSide side o.markers -> m
  _ -> Map.empty

-- | The diff route's warm-up, timed. A crash only logs a line, as
-- 'runWarmUp'.
timedDiffWarmUp :: Builder -> IO Nanoseconds
timedDiffWarmUp b = do
  t0 <- b.deps.clock.monotonic
  try @SomeException b.deps.diffWarmUp >>= \case
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise ->
          logInfo b.logger $
            "haskell-reviewer: diff warm-up failed: " <> exceptionMessage e
  t1 <- b.deps.clock.monotonic
  pure (elapsed t0 t1)

-- | A side that could not be listed.
reportError :: Logger -> Side -> ListError -> IO ()
reportError logger side e =
  logInfo logger $
    "haskell-reviewer: " <> sideTag side <> "cannot list files: " <> renderListError e

-- | A side's file list: warnings, and a verbose line.
reportFiles :: Logger -> Side -> FileList -> Nanoseconds -> IO ()
reportFiles logger side fl (Nanoseconds ns) = do
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

-- | A side's project, a verbose line.
reportProject :: Logger -> Side -> ModuleIndex -> Nanoseconds -> IO ()
reportProject logger side idx (Nanoseconds ns) =
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
    -- | For api/diff/<path…>.
    changes :: ChangeIndex,
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
pairSides :: Deps -> Logger -> Generation -> [SideDone Prepared] -> DiffWork -> IO Paired
pairSides deps logger g done diff = case (lookup HeadSide ready, lookup BaseSide ready) of
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
              changes = o.changes,
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
          body = encodeDiffFiles g files
      _ <- evaluate (LBS.length body)
      changes <- evaluate (changeIndex p.changes)
      _ <- evaluate (forceChangeIndex changes)
      let facts s = moduleFacts s.keys s.modules
          markers = moduleMarkers p (facts <$> sides)
      _ <- evaluate (forceMarkers markers.head)
      _ <- evaluate (forceMarkers markers.base)
      pure Encoded {pairing = p, body = body, changes = changes, markers = markers}
    sideOutside = \case
      CommitEntries c -> Just c.outside
      WorkTreeEntries w -> w.outside
      _ -> Nothing
    ends = diffEnds diff.info
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
    changes :: ChangeIndex,
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
    -- Nothing read now: 0 here, so its project span is the build only.
    ListingKept p -> do
      w1 <- deps.clock.now
      let none = Timing {start = w1, duration = Nanoseconds 0}
      pure (SideOk p {files = KeptFiles, project = none})
    ListingOk l files -> catchCrash SideCrashed $ do
      t1 <- deps.clock.monotonic
      w1 <- deps.clock.now
      (modules, projectRead) <- modulesOf deps logger w.side l cliGhc
      t2 <- deps.clock.monotonic
      pure . SideOk $
        Prepared
          { listed = l,
            modules = modules,
            projectRead = projectRead,
            keys = moduleKeys (sourceBlob l.source) modules,
            files = ListedNow files,
            project = Timing {start = w1, duration = elapsed t1 t2}
          }

-- | Build one side's snapshot, with its markers. A crash fails this
-- side only; a cancel goes through.
buildSide ::
  Deps ->
  SessionShare ->
  Generation ->
  (Side -> Map ModuleId Marker) ->
  SideDone Prepared ->
  IO (SideDone Built)
buildSide deps share gen markersOn d =
  SideDone d.work <$> case d.result of
    SideNotListed e files -> pure (SideNotListed e files)
    SideCrashed msg -> pure (SideCrashed msg)
    SideOk p -> catchCrash SideCrashed $ do
      t1 <- deps.clock.monotonic
      let side = d.work.side
      snap <-
        newSnapshot
          gen
          side
          share
          p.listed.source
          p.listed.files
          p.modules
          (markersOn side)
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
-- has no project, only a warning. Says whether to read it again on the
-- next reload. See Note [Listing after the URL].
modulesOf ::
  Deps -> Logger -> Side -> Listed -> Maybe GhcVersion -> IO (ModuleIndex, ProjectRead)
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
    idx <- forced (buildModuleIndex project listed.files)
    pure (idx, if failedRead project then ProjectReadAgain else ProjectReadOk)
  case result of
    Right r -> pure r
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          let msg = exceptionMessage e
          logInfo logger $
            "haskell-reviewer: " <> sideTag side <> "cannot read the project: " <> msg
          (,ProjectReadAgain) <$> forced (buildModuleIndex (crashed msg) listed.files)
  where
    -- A file we could not read. Other warnings are about the files'
    -- content: reading again gives the same.
    failedRead project =
      or [True | ProjectUnreadable _ (CantRead _) <- project.warnings]
        || or [True | CabalUnreadable (CantRead _) <- problems project]
    problems project = map (.problem) project.brokenPackages
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
-- modules not done; an async exception (a cancel, 'StopWorkers') goes
-- on to the caller. Nothing to do if no side was listed. See Note
-- [Listing after the URL].
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

-- | However the listing thread ends, every slot of the current
-- generation is filled (the diff slot too) and no module is left
-- Pending or Parsing: requests would wait for ever. For a cancel
-- (shutdown) anywhere, or a crash after the fill. A slot filled already
-- keeps what it has. Older generations were stopped by the reload
-- that replaced them.
stopIndexing :: Live -> IO ()
stopIndexing live =
  (.slots) <$> readCurrent live >>= \case
    BrowseSlots s -> stopSlots (BrowseSides s) Nothing
    DiffSlots d -> stopSlots (DiffSides d.sides) (Just d.pairing)

-- | 'stopSlots' for a generation not swapped in (D-313).
stopWork :: SessionWork -> IO ()
stopWork work = stopSlots ((.slot) <$> sessionSides work) (sessionDiffSlot work)

-- | 'stopIndexing' for these slots.
stopSlots :: Sides SnapshotSlot -> Maybe DiffSlot -> IO ()
stopSlots sides diff = do
  forM_ diff $ \slot -> fillDiff slot (Left DiffNotListed)
  forM_ sides $ \slot -> do
    _ <- fillSnapshot slot (Left (ListCrashed "the listing was stopped"))
    atomically (snapshotSTM slot) >>= \case
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
