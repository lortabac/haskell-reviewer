-- | The session's file pairing (diff mode): the body of GET
-- api\/diff-files and the changes by path, made once after both sides
-- are built (D-326, D-331).
module HaskellReviewer.Session.Diff
  ( -- * Types
    DiffSlot,
    DiffProblem (..),
    DiffReady (..),

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
import HaskellReviewer.Core.DiffTarget (ChangeIndex)

-- | Why there is no pairing.
data DiffProblem
  = -- | A side was not listed (or the listing was stopped).
    DiffNotListed
  | -- | Pairing threw, or a side can't be paired (a disk side). The
    -- message.
    DiffNotPaired Text
  deriving (Eq, Show)

-- | What the diff routes read. Forced before the fill: request threads
-- share it (Q-RTS-BLACKHOLE).
data DiffReady = DiffReady
  { -- | The body of api\/diff-files.
    body :: !LBS.ByteString,
    -- | For api\/diff\/\<path…>.
    changes :: !ChangeIndex
  }

-- | Where the pairing goes. Requests wait on it.
newtype DiffSlot = DiffSlot (TMVar (Either DiffProblem DiffReady))

-- | An empty slot.
newDiffSlot :: IO DiffSlot
newDiffSlot = DiffSlot <$> newEmptyTMVarIO

-- | Fill the slot, with a forced value. Only the first fill counts.
fillDiff :: DiffSlot -> Either DiffProblem DiffReady -> IO Bool
fillDiff (DiffSlot v) x = atomically (tryPutTMVar v x)

-- | Wait until the slot is filled.
awaitDiff :: DiffSlot -> IO (Either DiffProblem DiffReady)
awaitDiff (DiffSlot v) = atomically (readTMVar v)
