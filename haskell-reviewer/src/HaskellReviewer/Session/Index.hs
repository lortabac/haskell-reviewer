{-# LANGUAGE StrictData #-}

-- StrictData: what goes in a slot must hold no thunk. See Note [Indexing].

-- | Progressive indexing: one slot per module, parse workers fed by a
-- priority queue, progress and the name table (SPEC §11.5).
module HaskellReviewer.Session.Index
  ( -- * Types
    Pool,
    Index,
    SlotState (..),
    Indexed (..),
    ParsedSource (..),
    newParsedSource,
    IndexFailure (..),
    Progress (..),
    IndexDeps (..),
    IndexJob (..),

    -- * Building
    newPool,
    newIndex,

    -- * Requests
    awaitModule,
    requestModule,

    -- * Workers
    runWorkers,
    withRequest,
    failUnfinished,
    bigFileSize,

    -- * Reading
    readProgress,
    readNames,
    readStates,
    slotState,

    -- * Internal, for tests
    requestCount,
    isRequested,
  )
where

import Control.Concurrent.Async (AsyncCancelled (..), replicateConcurrently_)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Exception
  ( ErrorCall (..),
    SomeAsyncException,
    SomeException,
    bracket_,
    catch,
    evaluate,
    finally,
    fromException,
    mask,
    throwIO,
    try,
  )
import Control.Monad (forM, forM_, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Foreign qualified as Text.Foreign
import Data.Unique (Unique, newUnique)
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    TooLargeInfo,
  )
import HaskellReviewer.Core.IndexQueue (Job (..), Priority (..), Ticket)
import HaskellReviewer.Core.IndexQueue qualified as Queue
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule,
  )
import HaskellReviewer.Core.Modules
  ( ComponentSettings,
    HaskellFile (..),
    ModuleId,
    ModuleIndex (..),
  )
import HaskellReviewer.Core.NameTable (NameEntry, forceNames, moduleNames)
import HaskellReviewer.Core.Parser (ParseSettings, moduleParseSettings)
import HaskellReviewer.Core.Project (GhcVersion)
import HaskellReviewer.Core.References (OccKeys, occKeys)
import HaskellReviewer.Core.RelPath (RelPath, relPathText)
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (Logger, exceptionMessage, logInfo, logVerbose)

{- Note [Indexing]

SPEC §11.5. Each snapshot has an 'Index': one slot per module that
needs a parse (Haskell sources of components and loose modules). The
others (generated, preprocessed, no source) are answered from the
module index and have no slot.

A slot goes Pending -> Parsing -> Ready or Failed, and stays there:
no retry within a snapshot. Ready also covers modules we show as text
(parse error, not text, too large, too slow). Failed means we have no
answer: the file is gone, can't be read, or indexing it crashed or was
stopped. (A big file can go back from Parsing to Pending once, see
below.) A module that parsed keeps its file's text in the slot, for
the docs page (D-171); text-only ones don't.

The queue has the module ids by priority (Core.IndexQueue). At first
every module is in it, at Background, in module tree order. A request
for a Pending module pushes it again at Requested and waits on its
slot. So an id can be in the queue twice: 'claimNext' drops ids whose
slot is no longer Pending. Every Pending slot has at least one entry
in the queue, so an empty queue means nothing is Pending.

Pool: a session can have several indexes (diff mode: head and base).
They share one 'Pool': the big-file lock and the request counts below,
so the memory rule and the request gate hold for the whole session
(D-314). One 'runWorkers' runs one set of workers over all of them.
Run only one 'runWorkers' per pool at a time: the gate takes the
worker count from the call, but the in-flight count is the pool's.
Indexes from different pools in one call are a bug: it throws.

Order: for each priority (Requested, Changed, Background), the claim
goes through the indexes in list order and takes the first module it
can, all in one transaction ('claimNext'). So a request on any index
goes before background work, and head's background work goes before
base's if head comes first. Within a priority the order is by index,
not by arrival: a head request goes before an older base request.

Workers ('runWorkers', one per capability in the server) take the next
Pending module, set it Parsing in the same transaction, read and parse
it outside STM, then in one transaction fill the slot, count it in the
progress and add its names. Each module runs in its own try: a crash
fails that module only. A log line that can't be written is dropped.
If the workers die anyway, or are stopped, every slot not done yet, in
every index, is set Failed ('failUnfinished'), so no request waits for
ever. The server also calls it when indexing ends for any reason.

Memory: a parse of a big hostile file can take about 1 GB (D-127). So
files over 'bigFileSize' parse one at a time; smaller ones in parallel
(D-159). We only learn a file is big when we read it. Then, unless the
worker holds the big-file lock, the slot goes back to Pending, marked
big, at the same place in the queue. 'claimNext' takes a big module
only if it can take the lock in the same transaction; else it skips it
and takes the next one. So no worker ever waits for the lock while
other modules wait: it only waits (retry) when big modules are all
that is left. A request marks the slot requested whatever its state;
a big module put back also goes in at Requested if it's marked, so a
request that came during the first read keeps its priority. A big file
is read twice; there are few of them. The lock is the pool's.

Requests: a request's compute (resolve, links, encode) needs a core
too. 'withRequest' counts the requests computing now, on any index of
the pool, and 'claimNext' lets at most @max 1 (workers - requests)@
workers hold a module. So each request parks a worker when its parse
ends, never the last one. A parked worker waits in 'retry', so its
core is free. Don't put 'withRequest' around a wait for a slot: the
slot may need the parked worker. The in-flight count goes down in the
same 'finally' as the big-file lock, so a big file put back gives its
place back. The counts use modifyTVar'. Trade-off (accepted): under
steady requests with 2 workers, one big file can delay a Requested
module.

Q-RTS-BLACKHOLE: request threads read what workers write, and workers
share the queue. So nothing shared holds a thunk. The two counts are
strict Ints, written with modifyTVar'. The parse result is fully
forced by parseGuarded (the text is strict, read already), the
names by 'forceNames', the keys of find references by 'evaluate' (an
unboxed vector), all before the transaction that publishes them. The
queue is a strict map with strict fields, written with '$!'. The rest
is StrictData values written already evaluated. The slots and the queue
are built and forced in 'newIndex', before any worker or request sees
them. 'runWorkers' forces each job's deps before the workers start:
every worker's first claim would force them in STM.
-}

-- * Types

-- | What a session's indexes share: the big-file lock and the request
-- gate. See Note [Indexing].
data Pool = Pool
  { -- | Tells pools apart.
    identity :: Unique,
    -- | A big file is being parsed.
    bigBusy :: TVar Bool,
    -- | Requests computing now ('withRequest').
    requests :: TVar Int,
    -- | Workers holding a module now.
    inFlight :: TVar Int
  }

-- | The indexing state of one snapshot. See Note [Indexing].
data Index = Index
  { ghc :: GhcVersion,
    slots :: Map ModuleId Slot,
    queue :: TVar Queue.Queue,
    progress :: TVar Progress,
    names :: TVar (Map ModuleId [NameEntry]),
    pool :: Pool
  }

-- | One module to index.
data Slot = Slot
  { file :: HaskellFile,
    settings :: ComponentSettings,
    state :: TVar SlotState,
    -- | Found over 'bigFileSize' when read.
    big :: TVar Bool,
    -- | A request asked for it.
    requested :: TVar Bool
  }

-- | Where a module is. See Note [Indexing].
data SlotState
  = Pending
  | Parsing
  | Ready Indexed
  | Failed IndexFailure
  deriving (Eq, Show)

-- | What we have for a module: a parse, or why it's not text.
data Indexed
  = -- | It parsed: the model and the file's text.
    IndexedParsed ParsedSource
  | -- | We ran the parser, and show it as text, with the reason.
    IndexedTextOnly TextOnlyModule
  | -- | Binary, or not UTF-8.
    IndexedNotText FileSize
  | -- | Over the text limit.
    IndexedTooLarge TooLargeInfo
  deriving (Eq, Show)

-- | A module that parsed, with its file's text as read: the docs page
-- cuts declarations' heads and source out of it (D-171). One strict
-- text, shared with nothing else.
data ParsedSource = ParsedSource
  { text :: Text,
    parsed :: ParsedModule,
    -- | For find references. See Note [Find references] in
    -- "HaskellReviewer.Core.References".
    occKeys :: OccKeys
  }
  deriving (Eq, Show)

-- | A parsed module and its text, with its keys.
newParsedSource :: Text -> ParsedModule -> ParsedSource
newParsedSource t m = ParsedSource {text = t, parsed = m, occKeys = occKeys t m}

-- | Why there is no answer for a module.
data IndexFailure
  = -- | Not there any more, or no longer a file.
    SourceGone
  | -- | A read error, as the OS says it.
    SourceUnreadable Text
  | -- | A bug, or indexing was stopped, with a message.
    IndexCrashed Text
  deriving (Eq, Show)

-- | How far indexing is. Done when @ready + failed == total@.
data Progress = Progress
  { total :: Int,
    ready :: Int,
    failed :: Int
  }
  deriving (Eq, Show)

-- | One index to work on, and how to read and parse its modules.
data IndexJob = IndexJob
  { deps :: IndexDeps,
    index :: Index
  }

-- | What the workers need from outside. Tests plug in fakes.
data IndexDeps = IndexDeps
  { -- | Read a module's file. 'ReadGone' if it's no longer a file of
    -- the list.
    readModule :: RelPath -> IO (Either ReadError FileContent),
    -- | Parse one module, fully forced: 'parseGuarded' in
    -- "HaskellReviewer.Effects.Parser".
    parse :: ParseSettings -> Text -> IO ModuleParse,
    logger :: Logger
  }

-- * Building

-- | A pool for a session's indexes.
newPool :: IO Pool
newPool = do
  identity <- newUnique
  bigBusy <- newTVarIO False
  requests <- newTVarIO 0
  inFlight <- newTVarIO 0
  pure
    Pool
      { identity = identity,
        bigBusy = bigBusy,
        requests = requests,
        inFlight = inFlight
      }

-- | Every module of the index that needs a parse, Pending. Fully built
-- before it returns. See Note [Indexing].
newIndex :: Pool -> ModuleIndex -> IO Index
newIndex pool idx = do
  jobs <- forM (Queue.indexJobs idx) $ \job -> do
    i <- evaluate job.id
    st <- newTVarIO Pending
    big <- newTVarIO False
    requested <- newTVarIO False
    slot <-
      evaluate
        Slot
          { file = job.file,
            settings = job.settings,
            state = st,
            big = big,
            requested = requested
          }
    pure (i, slot)
  slots <- evaluate (Map.fromList jobs)
  q <- evaluate (Queue.fromList (map fst jobs))
  queue <- newTVarIO q
  progress <- newTVarIO Progress {total = Map.size slots, ready = 0, failed = 0}
  names <- newTVarIO Map.empty
  ghc <- evaluate idx.ghc
  pure
    Index
      { ghc = ghc,
        slots = slots,
        queue = queue,
        progress = progress,
        names = names,
        pool = pool
      }

-- * Requests

-- | Wait until a module is done, and move it to the front of the queue
-- if it hasn't started. 'Nothing' if the module has no slot.
awaitModule :: Index -> ModuleId -> IO (Maybe (Either IndexFailure Indexed))
awaitModule index i = case Map.lookup i index.slots of
  Nothing -> pure Nothing
  Just slot -> do
    atomically (requestModule index i)
    atomically $
      readTVar slot.state >>= \case
        Ready r -> pure (Just (Right r))
        Failed f -> pure (Just (Left f))
        _ -> retry

-- | Move a module to the front of the queue, if it hasn't started.
-- Marks it requested in any case: a big file put back keeps the
-- priority (Note [Indexing]).
requestModule :: Index -> ModuleId -> STM ()
requestModule index i = case Map.lookup i index.slots of
  Nothing -> pure ()
  Just slot -> do
    writeTVar slot.requested True
    readTVar slot.state >>= \case
      Pending -> modifyTVar' index.queue (Queue.push Requested i)
      _ -> pure ()

-- * Workers

-- | Files over this size (256 KiB of UTF-8) parse one at a time. See
-- Note [Indexing].
bigFileSize :: FileSize
bigFileSize = FileSize 262_144

-- | Run this many workers over these indexes until every module is
-- done. Earlier indexes go first within a priority. All of them must
-- share one pool, else it throws; one call per pool at a time. If the
-- workers die (a bug, or a heap overflow) or are stopped, every module
-- not done yet, in every index, fails, then the exception goes on. See
-- Note [Indexing].
runWorkers :: Int -> [IndexJob] -> IO ()
runWorkers n0 work = case work of
  [] -> pure ()
  first : _ ->
    run first.index.pool `catch` \e -> do
      why <- case fromException e of
        Just AsyncCancelled -> pure "indexing was stopped"
        Nothing -> safeMessage e
      forM_ work $ \job -> failUnfinished job.index (IndexCrashed why)
      throwIO (e :: SomeException)
  where
    n = max 1 n0
    run pool = do
      -- Checked at run time: making it a type would need much more
      -- machinery than this one line.
      when (any (\job -> job.index.pool.identity /= pool.identity) work) $
        throwIO (ErrorCall "runWorkers: indexes from different pools")
      -- Before the workers share them. See Note [Indexing].
      forM_ work $ \job -> evaluate job.deps
      replicateConcurrently_ n (worker n pool work)

-- | Run a request's compute. A worker parks meanwhile. Not around a wait
-- for a slot. See Note [Indexing].
withRequest :: Index -> IO a -> IO a
withRequest index =
  bracket_
    (atomically (modifyTVar' index.pool.requests (+ 1)))
    (atomically (modifyTVar' index.pool.requests (subtract 1)))

-- | Every slot not done yet fails with this. For when the workers are
-- gone: no request may wait for ever.
failUnfinished :: Index -> IndexFailure -> IO ()
failUnfinished index why = do
  why' <- evaluate why
  atomically $
    forM_ (Map.elems index.slots) $ \slot ->
      readTVar slot.state >>= \case
        Pending -> failOne slot why'
        Parsing -> failOne slot why'
        _ -> pure ()
  where
    failOne slot why' = do
      writeTVar slot.state (Failed why')
      modifyTVar' index.progress (\p -> p {failed = p.failed + 1})

-- * Reading

-- | The progress now.
readProgress :: Index -> STM Progress
readProgress index = readTVar index.progress

-- | The names of the modules parsed so far.
readNames :: Index -> STM (Map ModuleId [NameEntry])
readNames index = readTVar index.names

-- | Every slot's state.
readStates :: Index -> STM (Map ModuleId SlotState)
readStates index = traverse (\slot -> readTVar slot.state) index.slots

-- | One module's state. 'Nothing' if it has no slot.
slotState :: Index -> ModuleId -> STM (Maybe SlotState)
slotState index i = traverse (\slot -> readTVar slot.state) (Map.lookup i index.slots)

-- * Internal, for tests

-- | Requests in 'withRequest' now, on any index of the pool.
requestCount :: Index -> STM Int
requestCount index = readTVar index.pool.requests

-- | A request asked for this module. False if it has no slot.
isRequested :: Index -> ModuleId -> STM Bool
isRequested index i =
  maybe (pure False) (\slot -> readTVar slot.requested) (Map.lookup i index.slots)

-- * Internal

-- | A module a worker took.
data Claim = Claim
  { deps :: IndexDeps,
    index :: Index,
    id :: ModuleId,
    ticket :: Ticket,
    slot :: Slot,
    -- | It took the big-file lock with it.
    holdsBig :: Bool
  }

-- | Take modules until none is left. The big-file lock and the
-- in-flight count are given back however the module ends.
worker :: Int -> Pool -> [IndexJob] -> IO ()
worker n pool work = loop
  where
    loop = do
      more <- mask $ \restore ->
        atomically (claimNext n pool work) >>= \case
          Nothing -> pure False
          Just c -> do
            restore (indexClaim c) `finally` release c
            pure True
      when more loop
    release c = atomically $ do
      modifyTVar' pool.inFlight (subtract 1)
      when c.holdsBig (writeTVar pool.bigBusy False)

-- | How the scan of one index at one priority ended. The claim takes
-- the first Claimed; it is done when every scan is Empty, else it waits.
data Scan
  = Claimed Claim
  | -- | Only big modules are left, and the lock is busy.
    Blocked
  | -- | No Pending module.
    Empty

-- | The next Pending module of any index, set to Parsing. By priority,
-- then in list order. Waits if only big modules are left while the
-- lock is busy, or if this worker must park for a request. 'Nothing'
-- when every queue is empty. There are @n@ workers. See Note
-- [Indexing].
claimNext :: Int -> Pool -> [IndexJob] -> STM (Maybe Claim)
claimNext n pool work = do
  busy <- readTVar pool.bigBusy
  let go blocked = \case
        [] -> if blocked then retry else pure Nothing
        (p, job) : rest ->
          scanAt n busy p job >>= \case
            Claimed c -> pure (Just c)
            Blocked -> go True rest
            Empty -> go blocked rest
  go False [(p, job) | p <- [minBound .. maxBound], job <- work]

-- | Scan one index's queue at one priority. Drops ids that are taken
-- already; skips big modules while the lock is busy.
scanAt :: Int -> Bool -> Priority -> IndexJob -> STM Scan
scanAt n busy p job = do
  q0 <- readTVar index.queue
  let go q dropped blocked = \case
        [] -> do
          when dropped (writeTVar index.queue $! q)
          pure (if blocked then Blocked else Empty)
        (t, i) : rest -> case Map.lookup i index.slots of
          Nothing -> go (Queue.remove t q) True blocked rest
          Just slot ->
            readTVar slot.state >>= \case
              Pending -> do
                big <- readTVar slot.big
                if big && busy
                  then go q dropped True rest
                  else do
                    waitIfParked
                    writeTVar index.queue $! Queue.remove t q
                    modifyTVar' index.pool.inFlight (+ 1)
                    writeTVar slot.state Parsing
                    when big (writeTVar index.pool.bigBusy True)
                    pure . Claimed $
                      Claim
                        { deps = job.deps,
                          index = index,
                          id = i,
                          ticket = t,
                          slot = slot,
                          holdsBig = big
                        }
              _ -> go (Queue.remove t q) True blocked rest
  go q0 False False (Queue.entriesAt p q0)
  where
    index = job.index
    -- Each request parks a worker, never the last.
    waitIfParked = do
      holding <- readTVar index.pool.inFlight
      r <- readTVar index.pool.requests
      when (holding >= max 1 (n - r)) retry

-- | How reading and parsing one module ended.
data Outcome
  = -- | With the names, for a parsed module.
    Done Indexed (Maybe [NameEntry])
  | Broke IndexFailure
  | -- | Over 'bigFileSize', and we don't hold the lock.
    FoundBig

-- | Index one module, or put it back if it's big. Never throws, except
-- async exceptions.
indexClaim :: Claim -> IO ()
indexClaim c =
  indexOne c >>= \case
    FoundBig ->
      atomically $ do
        writeTVar c.slot.big True
        writeTVar c.slot.state Pending
        modifyTVar' c.index.queue (Queue.putBack c.ticket c.id)
        -- Asked for while we were reading it.
        requested <- readTVar c.slot.requested
        when requested (modifyTVar' c.index.queue (Queue.push Requested c.id))
    Done r ns -> finish c (Ready r) ns
    Broke f -> finish c (Failed f) Nothing

-- | Read and parse one module.
indexOne :: Claim -> IO Outcome
indexOne c =
  try @SomeException work >>= \case
    Right outcome -> pure outcome
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          msg <- safeMessage e
          quietly . logInfo deps.logger $
            "haskell-reviewer: cannot index " <> relPathText path <> ": " <> msg
          pure (Broke (IndexCrashed msg))
  where
    deps = c.deps
    path = c.slot.file.path
    work =
      deps.readModule path >>= \case
        Left ReadGone -> pure (Broke SourceGone)
        Left (ReadFailed why) -> do
          quietly . logInfo deps.logger $
            "haskell-reviewer: cannot read " <> relPathText path <> ": " <> why
          pure (Broke (SourceUnreadable why))
        Right (TextContent t)
          | FileSize (Text.Foreign.lengthWord8 t) > bigFileSize,
            not c.holdsBig ->
              pure FoundBig
          | otherwise -> do
              let settings = moduleParseSettings c.index.ghc c.slot.settings c.slot.file
              r <- evaluate =<< deps.parse settings t
              -- Built and forced before the transaction: see Note
              -- [Indexing]. Here, so a throw fails only this module.
              case r of
                Parsed m -> do
                  let ns = moduleNames m.declarations
                  _ <- evaluate (forceNames ns)
                  source <- evaluate (newParsedSource t m)
                  pure (Done (IndexedParsed source) (Just ns))
                TextOnly o -> pure (Done (IndexedTextOnly o) Nothing)
        Right (Binary size) -> pure (Done (IndexedNotText size) Nothing)
        Right (TooLarge info) -> pure (Done (IndexedTooLarge info) Nothing)
        -- A link, a submodule: no longer a file.
        Right _ -> pure (Broke SourceGone)

-- | Fill the slot, count it, add its names, all at once. Logs the
-- progress at each tenth.
finish :: Claim -> SlotState -> Maybe [NameEntry] -> IO ()
finish c st ns = do
  let index = c.index
  (before, after) <- atomically $ do
    writeTVar c.slot.state st
    before <- readTVar index.progress
    let after = case st of
          Ready _ -> before {ready = before.ready + 1}
          _ -> before {failed = before.failed + 1}
    writeTVar index.progress $! after
    forM_ ns $ \names -> modifyTVar' index.names (Map.insert c.id names)
    pure (before, after)
  when (tenth before < tenth after) $
    quietly . logVerbose c.deps.logger $
      "indexing: " <> showText (doneCount after) <> "/" <> showText after.total
  where
    tenth p = (doneCount p * 10) `div` max 1 p.total
    doneCount p = p.ready + p.failed

-- | Run a log write; if it fails (say stderr is gone), go on without
-- it. Async exceptions go through.
quietly :: IO () -> IO ()
quietly act =
  try @SomeException act >>= \case
    Right () -> pure ()
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure ()

-- | The exception's message, built here: showing it could throw too.
safeMessage :: SomeException -> IO Text
safeMessage e =
  try @SomeException (evaluate (exceptionMessage e)) >>= \case
    Right t -> pure t
    Left e2
      | Just (_ :: SomeAsyncException) <- fromException e2 -> throwIO e2
      | otherwise -> pure "crashed"

showText :: (Show a) => a -> Text
showText = Text.pack . show
