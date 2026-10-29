-- | The snapshot of the working tree (SPEC §5.2, §11.5).
module HaskellReviewer.Session.Snapshot
  ( -- * Types
    Generation (..),
    Snapshot (..),
    SnapshotSlot,
    IndexingState (..),

    -- * Operations
    firstGeneration,
    newSnapshotSlot,
    fillSnapshot,
    awaitSnapshot,
    snapshotSTM,
    indexingState,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
    retry,
    tryPutTMVar,
    tryReadTMVar,
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word64)
import HaskellReviewer.Core.FileList (FileList)
import HaskellReviewer.Core.Modules (ModuleIndex)
import HaskellReviewer.Core.Root (CanonicalRoot)
import HaskellReviewer.Effects.FileSystem (ListError)
import HaskellReviewer.Session.Index (Index, Progress, readProgress)

-- | Which snapshot a response comes from. Goes up on reload (later); the
-- client caches by it (SPEC §11.6.2).
newtype Generation = Generation Word64
  deriving (Eq, Show)

-- | The generation of the first snapshot. The only one for now: there
-- is no reload yet.
firstGeneration :: Generation
firstGeneration = Generation 1

-- | One snapshot. Never changes once made, apart from the slots in its
-- index. Forced before requests see it: see Note [Forced snapshot] in
-- "HaskellReviewer.Server.Api".
data Snapshot = Snapshot
  { generation :: Generation,
    root :: CanonicalRoot,
    files :: FileList,
    modules :: ModuleIndex,
    -- | The body of GET api/files/tree, made once.
    fileListJson :: LBS.ByteString,
    -- | The body of GET api/modules/tree, made once.
    moduleTreeJson :: LBS.ByteString,
    -- | The modules' slots. See Note [Indexing] in
    -- "HaskellReviewer.Session.Index".
    index :: Index
  }

-- | Where the snapshot goes once listed. Requests wait on it: the list
-- is made after the URL is printed.
newtype SnapshotSlot = SnapshotSlot (TMVar (Either ListError Snapshot))

-- | An empty slot.
newSnapshotSlot :: IO SnapshotSlot
newSnapshotSlot = SnapshotSlot <$> newEmptyTMVarIO

-- | Fill the slot. Only the first fill counts: 'False' if it was full.
fillSnapshot :: SnapshotSlot -> Either ListError Snapshot -> IO Bool
fillSnapshot (SnapshotSlot v) s = atomically (tryPutTMVar v s)

-- | Wait until the slot is filled.
awaitSnapshot :: SnapshotSlot -> IO (Either ListError Snapshot)
awaitSnapshot (SnapshotSlot v) = atomically (readTMVar v)

-- | What the slot holds now, if it's filled.
snapshotSTM :: SnapshotSlot -> STM (Maybe (Either ListError Snapshot))
snapshotSTM (SnapshotSlot v) = tryReadTMVar v

-- | How far indexing is, in which snapshot.
data IndexingState = IndexingState
  { generation :: Generation,
    progress :: Progress
  }
  deriving (Eq, Show)

-- | The indexing state for the event stream. Waits (retries) until the
-- slot is filled; 'Nothing' if the listing failed: there is nothing to
-- index.
indexingState :: SnapshotSlot -> STM (Maybe IndexingState)
indexingState slot =
  snapshotSTM slot >>= \case
    Nothing -> retry
    Just (Left _) -> pure Nothing
    Just (Right snap) ->
      Just . IndexingState snap.generation <$> readProgress snap.index
