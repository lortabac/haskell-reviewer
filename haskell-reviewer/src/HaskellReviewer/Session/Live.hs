-- | The session's current generation: its slots, swapped at once on
-- reload (SPEC §11.5, D-351). See Note [Live].
module HaskellReviewer.Session.Live
  ( -- * Types
    Live,
    Current (..),
    LiveSlots (..),
    DiffLive (..),
    LiveIndexing (..),

    -- * Operations
    newLive,
    currentSTM,
    readCurrent,
    swapLive,
    liveSides,
    awaitSideSTM,
    indexingNow,
  )
where

import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Exception (evaluate)
import HaskellReviewer.Core.Sides (Pair, Side, Sides (..), lookupSide)
import HaskellReviewer.Effects.FileSystem (ListError)
import HaskellReviewer.Session.Diff (DiffSlot)
import HaskellReviewer.Session.Snapshot
  ( Generation,
    IndexingState,
    Snapshot,
    SnapshotSlot,
    awaitSnapshotSTM,
    indexingStates,
  )

{- Note [Live]

One TVar holds the current generation: its number and its slots, a
slot per side and, in diff mode, the pairing's slot. A reload builds
the next generation on the side, fills its slots, then writes the TVar
once ('swapLive'). So a request reads the TVar once and gets slots of
one generation. It keeps its snapshot to the end, even if a swap comes
meanwhile.

The first generation goes in at start-up with empty slots: requests
wait on them until the listing fills them. A later one is filled
before the swap, so nobody waits on its slots.

The record and its slots are built and forced before they go in:
request threads read them (Q-RTS-BLACKHOLE).
-}

-- | One generation's slots.
data Current = Current
  { generation :: !Generation,
    slots :: !LiveSlots
  }

-- | The slots, by mode.
data LiveSlots
  = BrowseSlots !SnapshotSlot
  | DiffSlots !DiffLive

-- | A diff session's slots: the pairing and both sides.
data DiffLive = DiffLive
  { pairing :: !DiffSlot,
    sides :: !(Pair SnapshotSlot)
  }

-- | The generation and the indexing states of its sides, head first.
data LiveIndexing = LiveIndexing
  { generation :: !Generation,
    states :: [IndexingState]
  }

-- | Where the current generation is.
newtype Live = Live (TVar Current)

-- | Start with this generation. The caller forces what is in it.
newLive :: Current -> IO Live
newLive c = Live <$> (newTVarIO =<< evaluate c)

-- | The current generation.
currentSTM :: Live -> STM Current
currentSTM (Live v) = readTVar v

-- | Like 'currentSTM', in IO.
readCurrent :: Live -> IO Current
readCurrent (Live v) = readTVarIO v

-- | Put in a new generation. Its slots should be filled already, and
-- its fields forced (Note [Live]).
swapLive :: Live -> Current -> IO ()
swapLive (Live v) c = do
  c' <- evaluate c
  atomically (writeTVar v c')

-- | The slots by side.
liveSides :: LiveSlots -> Sides SnapshotSlot
liveSides = \case
  BrowseSlots s -> BrowseSides s
  DiffSlots d -> DiffSides d.sides

-- | Read the current generation and wait for one side's slot, in one
-- transaction. 'Nothing' if the session has no such side.
awaitSideSTM :: Live -> Side -> STM (Maybe (Either ListError Snapshot))
awaitSideSTM live side = do
  c <- currentSTM live
  traverse awaitSnapshotSTM (lookupSide side (liveSides c.slots))

-- | The generation and its sides' indexing states, read at once. Never
-- waits.
indexingNow :: Live -> STM LiveIndexing
indexingNow live = do
  c <- currentSTM live
  states <- indexingStates (liveSides c.slots)
  pure LiveIndexing {generation = c.generation, states = states}
