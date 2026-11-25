-- | The reload requests waiting for the listing thread (D-351). See
-- Note [Reload queue].
module HaskellReviewer.Session.Reload
  ( -- * Types
    ReloadQueue,
    ReloadOutcome (..),

    -- * Requests
    newReloadQueue,
    requestReload,

    -- * The listing thread
    Batch,
    takeReloads,
    answerReloads,
    closeReloads,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    TVar,
    atomically,
    modifyTVar',
    newEmptyTMVar,
    newTVarIO,
    readTMVar,
    readTVar,
    retry,
    tryPutTMVar,
    writeTVar,
  )
import Control.Monad (forM_)
import Data.Text (Text)
import HaskellReviewer.Session.Snapshot (Generation)

{- Note [Reload queue]

A POST api/reload puts a fresh TMVar in the queue and waits on it. The
listing thread takes every waiter at once ('takeReloads'), builds one
new generation and answers them all ('answerReloads'). A POST that
comes during a build stays in the queue for the next one. So many
POSTs cost one reload, and no POST gets the result of a listing that
started before it. Never a 409.

The listing thread takes the first batch only once the first listing
is done: a POST before that waits for it.

Each batch is kept in the queue until it's answered, so the queue
can't lose a waiter, even if the listing thread takes a second batch
first.

On the way out, 'closeReloads' answers every waiter, taken or not, and
later POSTs get the same answer at once: 'ReloadStopping' when the
server stops, 'ReloadUnavailable' if the listing thread crashed while
the server goes on. A TMVar is filled at most once ('tryPutTMVar'),
so a late answer does nothing.
-}

-- | How a reload ended, for the POST that asked for it.
data ReloadOutcome
  = -- | Swapped in: the new generation.
    Reloaded Generation
  | -- | A side could not be listed or built. The old generation stays.
    ReloadFailed
  | -- | The build crashed (a bug), with the message. The old
    -- generation stays.
    ReloadCrashed Text
  | -- | The server is stopping.
    ReloadStopping
  | -- | The listing thread is gone (a bug), with the message. The
    -- server goes on without reloads.
    ReloadUnavailable Text
  deriving (Eq, Show)

-- | One request's answer, to come.
type Waiter = TMVar ReloadOutcome

-- | The waiters one build answers ('takeReloads').
newtype Batch = Batch [Waiter]

-- | The waiters. See Note [Reload queue].
data ReloadQueue = ReloadQueue
  { -- | Not taken yet.
    waiting :: TVar [Waiter],
    -- | Taken by the listing thread, not answered yet.
    taken :: TVar [Waiter],
    -- | Closed, with the answer for everyone left and later.
    closed :: TVar (Maybe ReloadOutcome)
  }

-- | An empty queue.
newReloadQueue :: IO ReloadQueue
newReloadQueue = do
  waiting <- newTVarIO []
  taken <- newTVarIO []
  closed <- newTVarIO Nothing
  pure ReloadQueue {waiting = waiting, taken = taken, closed = closed}

-- | Ask for a reload and wait for its outcome. The action runs once
-- the request is in the queue (a log line).
requestReload :: ReloadQueue -> IO () -> IO ReloadOutcome
requestReload q queued = do
  mine <- atomically $ do
    readTVar q.closed >>= \case
      Just outcome -> pure (Left outcome)
      Nothing -> do
        v <- newEmptyTMVar
        modifyTVar' q.waiting (v :)
        pure (Right v)
  case mine of
    Left outcome -> pure outcome
    Right v -> do
      queued
      atomically (readTMVar v)

-- | Wait for at least one waiter, then take them all.
takeReloads :: ReloadQueue -> STM Batch
takeReloads q = do
  ws <- readTVar q.waiting
  case ws of
    [] -> retry
    _ -> do
      writeTVar q.waiting []
      modifyTVar' q.taken (ws <>)
      pure (Batch ws)

-- | Answer a batch.
answerReloads :: ReloadQueue -> Batch -> ReloadOutcome -> IO ()
answerReloads q (Batch ws) outcome = atomically $ do
  modifyTVar' q.taken (filter (`notElem` ws))
  answer ws outcome

-- | Answer every waiter with this, and every later request too. Only
-- the first close counts.
closeReloads :: ReloadQueue -> ReloadOutcome -> IO ()
closeReloads q outcome = atomically $ do
  final <-
    readTVar q.closed >>= \case
      Just first -> pure first
      Nothing -> outcome <$ writeTVar q.closed (Just outcome)
  ws <- readTVar q.waiting
  ts <- readTVar q.taken
  writeTVar q.waiting []
  writeTVar q.taken []
  answer (ws <> ts) final

-- | Fill each waiter's TMVar, unless filled already.
answer :: [Waiter] -> ReloadOutcome -> STM ()
answer ws outcome = forM_ ws (\v -> tryPutTMVar v outcome)
