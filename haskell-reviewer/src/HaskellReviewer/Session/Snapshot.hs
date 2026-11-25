-- | The snapshot of one side (SPEC §5.2, §11.5).
module HaskellReviewer.Session.Snapshot
  ( -- * Types
    Generation (..),
    Snapshot (..),
    SnapshotSlot,
    IndexingState (..),
    SessionShare (..),

    -- * Operations
    newSessionShare,
    firstGeneration,
    nextGeneration,
    newSnapshotSlot,
    fillSnapshot,
    awaitSnapshot,
    awaitSnapshotSTM,
    snapshotSTM,
    indexingStates,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
    tryPutTMVar,
    tryReadTMVar,
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Word (Word64)
import HaskellReviewer.Core.FileList (FileList)
import HaskellReviewer.Core.Modules (ModuleId, ModuleIndex)
import HaskellReviewer.Core.Search (ModuleCandidate)
import HaskellReviewer.Core.Sides (Side, Sides)
import HaskellReviewer.Effects.FileSystem (ListError)
import HaskellReviewer.Effects.Source (EntrySource)
import HaskellReviewer.Session.Index (Index, Pool, Progress, newPool, readProgress)
import HaskellReviewer.Session.Resolve (Resolver)
import HaskellReviewer.Session.TextStore (BlobTexts, TextStore, newBlobTexts)

-- | Which snapshot a response comes from. One counter for the session:
-- each reload makes the next one, for every side (D-351). The client
-- caches by it (SPEC §11.6.2).
newtype Generation = Generation Word64
  deriving (Eq, Show)

-- | What a session's snapshots share: the workers' pool (with its
-- parse cache) and the text stores' bytes by blob (D-320).
data SessionShare = SessionShare
  { pool :: Pool,
    blobTexts :: BlobTexts
  }

-- | A new one, for one session.
newSessionShare :: IO SessionShare
newSessionShare = do
  pool <- newPool
  blobTexts <- newBlobTexts
  pure SessionShare {pool = pool, blobTexts = blobTexts}

-- | The generation of the first snapshots.
firstGeneration :: Generation
firstGeneration = Generation 1

-- | The generation after this one, for a reload.
nextGeneration :: Generation -> Generation
nextGeneration (Generation g) = Generation (g + 1)

-- | One snapshot. Never changes once made, apart from the slots in its
-- index. Forced before requests see it: see Note [Forced snapshot] in
-- "HaskellReviewer.Server.Api".
data Snapshot = Snapshot
  { generation :: Generation,
    -- | Must match its slot in the session's 'Sides'.
    side :: Side,
    -- | Where its files are read from.
    source :: EntrySource,
    files :: FileList,
    modules :: ModuleIndex,
    -- | The body of GET api/files/tree, made once.
    fileListJson :: LBS.ByteString,
    -- | The body of GET api/modules/tree, made once.
    moduleTreeJson :: LBS.ByteString,
    -- | Every module, for search. See Note [Search] in
    -- "HaskellReviewer.Core.Search".
    searchModules :: Map ModuleId ModuleCandidate,
    -- | The modules' slots. See Note [Indexing] in
    -- "HaskellReviewer.Session.Index".
    index :: Index,
    -- | Resolution over the index. See Note [Session resolution] in
    -- "HaskellReviewer.Session.Resolve".
    resolver :: Resolver,
    -- | The texts for text search, loaded on the first one. See Note
    -- [Text store] in "HaskellReviewer.Session.TextStore".
    textStore :: TextStore
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
awaitSnapshot = atomically . awaitSnapshotSTM

-- | Like 'awaitSnapshot', in STM: retries until the slot is filled.
awaitSnapshotSTM :: SnapshotSlot -> STM (Either ListError Snapshot)
awaitSnapshotSTM (SnapshotSlot v) = readTMVar v

-- | What the slot holds now, if it's filled.
snapshotSTM :: SnapshotSlot -> STM (Maybe (Either ListError Snapshot))
snapshotSTM (SnapshotSlot v) = tryReadTMVar v

-- | How far indexing is, in which snapshot, on which side.
data IndexingState = IndexingState
  { generation :: Generation,
    side :: Side,
    progress :: Progress
  }
  deriving (Eq, Show)

-- | The indexing states for the event stream: one per side that is
-- listed, head first. A side not listed yet, or whose listing failed,
-- has none. Never waits.
indexingStates :: Sides SnapshotSlot -> STM [IndexingState]
indexingStates slots = concat <$> mapM stateOf (toList slots)
  where
    stateOf slot =
      snapshotSTM slot >>= \case
        Just (Right snap) -> do
          p <- readProgress snap.index
          pure
            [ IndexingState
                { generation = snap.generation,
                  side = snap.side,
                  progress = p
                }
            ]
        _ -> pure []
