-- | The event stream: server-sent events to the browser (SPEC §11.6.2).
module HaskellReviewer.Server.Events
  ( -- * Types
    EventStream (..),

    -- * Application
    eventsApp,
    keepAliveInterval,
    indexingGap,

    -- * Framing
    renderEvent,
    keepAliveFrame,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, check, readTVar, registerDelay, retry)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, word64Dec)
import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as Text
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.Log (Logger, logVerbose)
import HaskellReviewer.Server.Security (methodNotAllowed)
import HaskellReviewer.Server.Wire (encodeIndexing, helloPayload)
import HaskellReviewer.Session.Clients
  ( Clients,
    EventId (..),
    nextEventId,
    openCount,
    withClient,
  )
import HaskellReviewer.Session.Snapshot (IndexingState (..))
import Network.HTTP.Types (hContentType, methodGet, status200)
import Network.Wai (Application, Request (..), responseStream)
import Network.Wai.Handler.Warp qualified as Warp

{- Note [Event stream]

GET /<token>/api/events is a text/event-stream that never ends. One per
browser, held by a SharedWorker (D-52); a tab without SharedWorker opens
its own.

Framing:

- Each event has an id (global counter, see Clients), a name and data.
  Data with newlines becomes several data: lines.
- On open we send `hello` with data {}.
- Then `indexing`, with the indexing progress (encodeIndexing), as soon
  as the files are listed. Again each time it changes, at most once per
  minGap (100 ms): we send the state, not each step, so the last state
  always gets there and a slow client never piles up a backlog. Never
  if the listing failed.
- Every keepAliveInterval (15 s) we send the comment ": keep-alive".
- Last-Event-ID is ignored. There is no replay: after a reconnect the
  client fetches what it shows again (SPEC §11.6.2). The indexing
  state comes by itself on each new stream.

No Content-Length, so warp sends it chunked. No Cache-Control here:
Security adds no-store. Never gzipped (no gzip middleware covers it).

Warp's timeout (30 s by default) would kill the stream. We turn it off
with pauseTimeout. It must be called inside the streaming body: warp
resumes the timer when the app calls 'respond', so a call before that
would be undone. (Warp 3.4 also pauses it between writes of a streaming
body, but that's an internal detail we don't rely on.) Outside warp
(tests) pauseTimeout does nothing.

Closed tabs: we only learn that the browser went away when a write
fails. Writes happen every 15 s, and the OS may take one more write to
report the reset, so a closed tab is noticed in about 15 to 30 s. The
failed write throws, which ends the body and 'withClient' counts the
stream as closed. That's what drives the idle exit (Note [Idle exit] in
"HaskellReviewer.Session.Clients").
-}

-- | What the event stream needs.
data EventStream = EventStream
  { clients :: Clients,
    -- | Time between two keep-alives. 'keepAliveInterval' in real use.
    keepAlive :: Microseconds,
    -- | The indexing progress. Retries until the files are listed;
    -- 'Nothing' if the listing failed.
    indexing :: STM (Maybe IndexingState),
    -- | Least time between two @indexing@ events. 'indexingGap' in real
    -- use.
    minGap :: Microseconds,
    logger :: Logger
  }

-- | 15 s. See Note [Event stream].
keepAliveInterval :: Microseconds
keepAliveInterval = Microseconds 15_000_000

-- | 100 ms. See Note [Event stream].
indexingGap :: Microseconds
indexingGap = Microseconds 100_000

-- | The stream route. GET only: anything else (HEAD too) gets 405.
eventsApp :: EventStream -> Application
eventsApp es req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | otherwise =
      respond $
        responseStream status200 [(hContentType, "text/event-stream")] $
          \write flush -> do
            Warp.pauseTimeout req
            let stream = withClient es.clients $ do
                  logCount "event stream opened"
                  hello <- nextEventId es.clients
                  write (renderEvent hello "hello" helloPayload)
                  flush
                  timer <- registerDelay delay
                  loop write flush timer Nothing
            stream `finally` logCount "event stream closed"
  where
    Microseconds delay = es.keepAlive
    Microseconds gap = es.minGap
    -- Waits for a new indexing state or the keep-alive timer, whichever
    -- comes first. Never returns: a failed write ends it.
    loop write flush timer sent = do
      next <-
        atomically $
          (Left <$> changed sent) <|> (Right () <$ (readTVar timer >>= check))
      case next of
        Left now -> do
          i <- nextEventId es.clients
          write (renderEvent i "indexing" (encodeIndexing now.generation now.progress))
          flush
          threadDelay gap
          loop write flush timer (Just now)
        Right () -> do
          write keepAliveFrame
          flush
          timer' <- registerDelay delay
          loop write flush timer' sent
    changed sent =
      es.indexing >>= \case
        Just now | Just now /= sent -> pure now
        _ -> retry
    logCount what = do
      n <- openCount es.clients
      logVerbose es.logger (what <> " (" <> Text.pack (show n) <> " open)")

-- | One event. The name must not hold a newline, and the data no @\\r@.
-- Data with @\\n@ is split over several @data:@ lines; the browser joins
-- them back.
renderEvent :: EventId -> ByteString -> ByteString -> Builder
renderEvent (EventId n) name payload =
  "id: "
    <> word64Dec n
    <> "\nevent: "
    <> byteString name
    <> "\n"
    <> foldMap (\l -> "data: " <> byteString l <> "\n") dataLines
    <> "\n"
  where
    -- Empty data still needs one data: line, or the browser drops the
    -- event.
    dataLines
      | BS.null payload = [""]
      | otherwise = Char8.split '\n' payload

-- | The keep-alive comment. Browsers ignore it.
keepAliveFrame :: Builder
keepAliveFrame = ": keep-alive\n\n"
