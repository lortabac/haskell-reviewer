-- | Open event streams, and the idle timer (SPEC §2.1, §11.5).
module HaskellReviewer.Session.Clients
  ( -- * Types
    Clients,
    EventId (..),

    -- * Operations
    newClients,
    withClient,
    openCount,
    nextEventId,
    waitIdle,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    registerDelay,
    stateTVar,
  )
import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.Word (Word64)
import HaskellReviewer.Effects.Clock (Microseconds (..))

{- Note [Idle exit]

SPEC §2.1: we exit when no browser tab has been connected for the idle
timeout. A browser is connected while its event stream is open, so we
count open streams ('withClient').

'waitIdle' waits until the count is 0, then starts a timer. If a stream
opens before the timer fires (a tab reloaded, or reopened), it starts
over: wait for 0 again, then a fresh timer. It returns only after a full
timeout with no stream open.

The timer starts when 'waitIdle' is called, i.e. at server start. So a
browser that never connects (--no-open, or the opener failed) also ends
the process after the timeout.

A stream that opens and closes again between two STM wake-ups may not
reset the timer. Fine: no tab was really there.

registerDelay needs the threaded RTS. The executable and the tests are
built with -threaded.
-}

-- | Open event streams and the event id counter. Lives as long as the
-- server.
data Clients = Clients
  { -- | Number of open streams.
    open :: TVar Int,
    -- | The id of the next event.
    nextId :: TVar Word64
  }

-- | The id of an event on the stream (the SSE @id:@ field). Counts up
-- from 1, global to the session, never reused.
newtype EventId = EventId Word64
  deriving (Eq, Show)

-- | No streams open yet.
newClients :: IO Clients
newClients = Clients <$> newTVarIO 0 <*> newTVarIO 1

-- | Count a stream as open while the action runs, even if it throws.
withClient :: Clients -> IO a -> IO a
withClient clients =
  bracket_
    (atomically (modifyTVar' clients.open (+ 1)))
    (atomically (modifyTVar' clients.open (subtract 1)))

-- | How many streams are open now.
openCount :: Clients -> IO Int
openCount clients = readTVarIO clients.open

-- | A fresh event id.
nextEventId :: Clients -> IO EventId
nextEventId clients =
  atomically (stateTVar clients.nextId (\n -> (EventId n, n + 1)))

-- | Return once no stream has been open for the given time. See
-- Note [Idle exit].
waitIdle :: Clients -> Microseconds -> IO ()
waitIdle clients (Microseconds d) = loop
  where
    loop = do
      atomically (readTVar clients.open >>= check . (== 0))
      timer <- registerDelay d
      -- A stream that opens on the same wake-up wins over the timer.
      expired <-
        atomically $
          (False <$ (readTVar clients.open >>= check . (> 0)))
            <|> (True <$ (readTVar timer >>= check))
      unless expired loop
