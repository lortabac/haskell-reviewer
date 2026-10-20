-- | The snapshot of the working tree (SPEC §5.2, §11.5).
module HaskellReviewer.Session.Snapshot
  ( -- * Types
    Generation (..),
    Snapshot (..),
    SnapshotSlot,

    -- * Operations
    firstGeneration,
    newSnapshotSlot,
    fillSnapshot,
    awaitSnapshot,
  )
where

import Control.Concurrent.STM
  ( TMVar,
    atomically,
    newEmptyTMVarIO,
    readTMVar,
    tryPutTMVar,
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word64)
import HaskellReviewer.Core.FileList (FileList)
import HaskellReviewer.Core.Root (CanonicalRoot)
import HaskellReviewer.Effects.FileSystem (ListError)

-- | Which snapshot a response comes from. Goes up on reload (later); the
-- client caches by it (SPEC §11.6.2).
newtype Generation = Generation Word64
  deriving (Eq, Show)

-- | The generation of the first snapshot. The only one for now: there
-- is no reload yet.
firstGeneration :: Generation
firstGeneration = Generation 1

-- | One snapshot. Never changes once made.
data Snapshot = Snapshot
  { generation :: Generation,
    root :: CanonicalRoot,
    files :: FileList,
    -- | The body of GET api/files/tree, made once. Lazy: made on the
    -- first request.
    fileListJson :: LBS.ByteString
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
