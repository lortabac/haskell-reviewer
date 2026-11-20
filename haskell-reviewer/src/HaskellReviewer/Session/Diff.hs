-- | The session's file pairing (diff mode): the body of GET
-- api\/diff-files, made once after both sides are built (D-326).
module HaskellReviewer.Session.Diff
  ( -- * Types
    DiffSlot,
    DiffProblem (..),

    -- * Operations
    newDiffSlot,
    fillDiff,
    awaitDiff,
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
import Data.Text (Text)

-- | Why there is no pairing.
data DiffProblem
  = -- | A side was not listed (or the listing was stopped).
    DiffNotListed
  | -- | Pairing threw, or a side can't be paired (a disk side). The
    -- message.
    DiffNotPaired Text
  deriving (Eq, Show)

-- | Where the body goes. Requests wait on it.
newtype DiffSlot = DiffSlot (TMVar (Either DiffProblem LBS.ByteString))

-- | An empty slot.
newDiffSlot :: IO DiffSlot
newDiffSlot = DiffSlot <$> newEmptyTMVarIO

-- | Fill the slot, with a forced body. Only the first fill counts.
fillDiff :: DiffSlot -> Either DiffProblem LBS.ByteString -> IO Bool
fillDiff (DiffSlot v) x = atomically (tryPutTMVar v x)

-- | Wait until the slot is filled.
awaitDiff :: DiffSlot -> IO (Either DiffProblem LBS.ByteString)
awaitDiff (DiffSlot v) = atomically (readTMVar v)
