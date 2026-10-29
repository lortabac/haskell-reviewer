{-# LANGUAGE StrictData #-}

-- StrictData: what goes in a slot must hold no thunk. See Note [Indexing].

-- | Progressive indexing: one slot per module, parse workers fed by a
-- priority queue, progress and the name table (SPEC §11.5).
module HaskellReviewer.Session.Index
  ( -- * Types
    Index,
    SlotState (..),
    Indexed (..),
    IndexFailure (..),
    Progress (..),
    IndexDeps (..),

    -- * Building
    newIndex,

    -- * Requests
    awaitModule,
    requestModule,

    -- * Workers
    runWorkers,
    failUnfinished,
    bigFileSize,

    -- * Reading
    readProgress,
    readNames,
    readStates,
    slotState,
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
  ( SomeAsyncException,
    SomeException,
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
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    TooLargeInfo,
  )
import HaskellReviewer.Core.IndexQueue (Job (..), Priority (..), Queue, Ticket)
import HaskellReviewer.Core.IndexQueue qualified as Queue
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.Modules
  ( ComponentSettings,
    HaskellFile (..),
    ModuleId,
    ModuleIndex (..),
  )
import HaskellReviewer.Core.NameTable (NameEntry, forceNames, moduleNames)
import HaskellReviewer.Core.Parser (ParseSettings, moduleParseSettings)
import HaskellReviewer.Core.Project (GhcVersion)
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
below.)

The queue has the module ids by priority (Core.IndexQueue). At first
every module is in it, at Background, in module tree order. A request
for a Pending module pushes it again at Requested and waits on its
slot. So an id can be in the queue twice: 'claimNext' drops ids whose
slot is no longer Pending. Every Pending slot has at least one entry
in the queue, so an empty queue means nothing is Pending, and a worker
that finds it empty is done.

Workers ('runWorkers', one per capability in the server) take the next
Pending module, set it Parsing in the same transaction, read and parse
it outside STM, then in one transaction fill the slot, count it in the
progress and add its names. Each module runs in its own try: a crash
fails that module only. A log line that can't be written is dropped.
If the workers die anyway, or are stopped, every slot not done yet is
set Failed ('failUnfinished'), so no request waits for ever. The server
also calls it when indexing ends for any reason.

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
is read twice; there are few of them.

Q-RTS-BLACKHOLE: request threads read what workers write, and workers
share the queue. So nothing shared holds a thunk. The parse result is
fully forced by parseGuarded, the names by 'forceNames', both before
the transaction that publishes them. The queue is a strict map with
strict fields, written with '$!'. The rest is StrictData values written
already evaluated. The slots and the queue are built and forced in
'newIndex', before any worker or request sees them.
-}

-- * Types

-- | The indexing state of one snapshot. See Note [Indexing].
data Index = Index
  { ghc :: GhcVersion,
    slots :: Map ModuleId Slot,
    queue :: TVar Queue,
    progress :: TVar Progress,
    names :: TVar (Map ModuleId [NameEntry]),
    -- | A big file is being parsed. See Note [Indexing].
    bigBusy :: TVar Bool
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
  = IndexedParse ModuleParse
  | IndexedNotText FileSize
  | IndexedTooLarge TooLargeInfo
  deriving (Eq, Show)

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

-- | Every module of the index that needs a parse, Pending. Fully built
-- before it returns. See Note [Indexing].
newIndex :: ModuleIndex -> IO Index
newIndex idx = do
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
  bigBusy <- newTVarIO False
  ghc <- evaluate idx.ghc
  pure
    Index
      { ghc = ghc,
        slots = slots,
        queue = queue,
        progress = progress,
        names = names,
        bigBusy = bigBusy
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

-- | Run this many workers until every module is done. If they die (a
-- bug, or a heap overflow) or are stopped, every module not done yet
-- fails, then the exception goes on. See Note [Indexing].
runWorkers :: IndexDeps -> Int -> Index -> IO ()
runWorkers deps n index =
  replicateConcurrently_ (max 1 n) (worker deps index) `catch` \e -> do
    why <- case fromException e of
      Just AsyncCancelled -> pure "indexing was stopped"
      Nothing -> safeMessage e
    failUnfinished index (IndexCrashed why)
    throwIO (e :: SomeException)

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

-- * Internal

-- | A module a worker took.
data Claim = Claim
  { id :: ModuleId,
    ticket :: Ticket,
    slot :: Slot,
    -- | It took the big-file lock with it.
    holdsBig :: Bool
  }

-- | Take modules until none is left. The big-file lock is given back
-- however the module ends.
worker :: IndexDeps -> Index -> IO ()
worker deps index = loop
  where
    loop = do
      more <- mask $ \restore ->
        atomically (claimNext index) >>= \case
          Nothing -> pure False
          Just c -> do
            restore (indexClaim deps index c) `finally` release c
            pure True
      when more loop
    release c = when c.holdsBig (atomically (writeTVar index.bigBusy False))

-- | The next Pending module, set to Parsing. Drops ids that are taken
-- already; skips big modules while the lock is busy; waits if only
-- those are left. See Note [Indexing].
claimNext :: Index -> STM (Maybe Claim)
claimNext index = do
  q <- readTVar index.queue
  busy <- readTVar index.bigBusy
  go busy q (Queue.entries q)
  where
    go busy q = \case
      []
        | Queue.isEmpty q -> do
            writeTVar index.queue $! q
            pure Nothing
        | otherwise -> retry
      (t, i) : rest -> case Map.lookup i index.slots of
        Nothing -> go busy (Queue.remove t q) rest
        Just slot ->
          readTVar slot.state >>= \case
            Pending -> do
              big <- readTVar slot.big
              if big && busy
                then go busy q rest
                else do
                  writeTVar index.queue $! Queue.remove t q
                  writeTVar slot.state Parsing
                  when big (writeTVar index.bigBusy True)
                  pure (Just Claim {id = i, ticket = t, slot = slot, holdsBig = big})
            _ -> go busy (Queue.remove t q) rest

-- | How reading and parsing one module ended.
data Outcome
  = -- | With the names, for a parsed module.
    Done Indexed (Maybe [NameEntry])
  | Broke IndexFailure
  | -- | Over 'bigFileSize', and we don't hold the lock.
    FoundBig

-- | Index one module, or put it back if it's big. Never throws, except
-- async exceptions.
indexClaim :: IndexDeps -> Index -> Claim -> IO ()
indexClaim deps index c =
  indexOne deps index c >>= \case
    FoundBig ->
      atomically $ do
        writeTVar c.slot.big True
        writeTVar c.slot.state Pending
        modifyTVar' index.queue (Queue.putBack c.ticket c.id)
        -- Asked for while we were reading it.
        requested <- readTVar c.slot.requested
        when requested (modifyTVar' index.queue (Queue.push Requested c.id))
    Done r ns -> finish deps index c.id c.slot (Ready r) ns
    Broke f -> finish deps index c.id c.slot (Failed f) Nothing

-- | Read and parse one module.
indexOne :: IndexDeps -> Index -> Claim -> IO Outcome
indexOne deps index c =
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
              let settings = moduleParseSettings index.ghc c.slot.settings c.slot.file
              r <- evaluate =<< deps.parse settings t
              -- Built and forced before the transaction: see Note
              -- [Indexing]. Here, so a throw fails only this module.
              ns <- case r of
                Parsed m -> do
                  let ns = moduleNames m.declarations
                  _ <- evaluate (forceNames ns)
                  pure (Just ns)
                TextOnly _ -> pure Nothing
              pure (Done (IndexedParse r) ns)
        Right (Binary size) -> pure (Done (IndexedNotText size) Nothing)
        Right (TooLarge info) -> pure (Done (IndexedTooLarge info) Nothing)
        -- A link, a submodule: no longer a file.
        Right _ -> pure (Broke SourceGone)

-- | Fill the slot, count it, add its names, all at once. Logs the
-- progress at each tenth.
finish ::
  IndexDeps -> Index -> ModuleId -> Slot -> SlotState -> Maybe [NameEntry] -> IO ()
finish deps index i slot st ns = do
  (before, after) <- atomically $ do
    writeTVar slot.state st
    before <- readTVar index.progress
    let after = case st of
          Ready _ -> before {ready = before.ready + 1}
          _ -> before {failed = before.failed + 1}
    writeTVar index.progress $! after
    forM_ ns $ \names -> modifyTVar' index.names (Map.insert i names)
    pure (before, after)
  when (tenth before < tenth after) $
    quietly . logVerbose deps.logger $
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
