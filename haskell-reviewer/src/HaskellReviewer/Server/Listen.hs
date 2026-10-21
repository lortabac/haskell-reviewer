-- | The listening socket and warp.
module HaskellReviewer.Server.Listen
  ( -- * Types
    Listener (..),
    ListenError (..),

    -- * Operations
    openListener,
    renderListenError,

    -- * Internal (for tests)
    acceptWithMss,
  )
where

import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Exception (IOException (..))
import HaskellReviewer.Core.Address (Port (..))
import HaskellReviewer.Effects.Log
  ( Logger,
    exceptionMessage,
    logDebug,
    logVerbose,
  )
import HaskellReviewer.Server.Security (addSecurityHeaders)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import System.IO.Error (ioeGetErrorString)

{- Note [Loopback MSS]

On loopback the MSS is about 32 KB (MTU 65536). A client's receive
window can shrink below that while it's busy. Then our kernel won't
send a segment smaller than the MSS (silly window avoidance): it holds
the rest of the data until the TCP persist timer fires, about 200 ms
later. A 160 KB response waits three such steps, with no packet lost.

So we set TCP_MAXSEG to 1460 (a normal Ethernet MSS) on the listening
socket, and again on each accepted socket. Linux copies it from the
listener; macOS may not, hence the second set (harmless on Linux). Small segments fit in a small window, so data keeps flowing.
The extra segments cost nothing we can measure on loopback.

Best effort: if the OS refuses, we log at debug level and go on.

Side effect: a client sending far too much (huge cookies, over warp's
50 KiB header limit) may now get a reset instead of warp's 400, since
warp closes with its bytes still unread.
-}

-- | A socket bound to 127.0.0.1 and listening.
data Listener = Listener
  { -- | The real port (the OS picks one when we ask for none).
    port :: Port,
    -- | Run warp on the socket. The action runs once warp accepts
    -- connections. Returns only if warp stops on its own.
    serve :: IO () -> Application -> IO (),
    -- | Close the socket.
    close :: IO ()
  }

-- | We could not bind or listen.
data ListenError = ListenError
  { -- | The port we asked for, if any.
    port :: Maybe Port,
    -- | Short reason from the OS, e.g. @Address already in use@.
    reason :: Text
  }
  deriving (Eq, Show)

-- | Message for the user.
renderListenError :: ListenError -> Text
renderListenError err =
  "cannot listen on 127.0.0.1" <> portText <> ": " <> err.reason
  where
    portText = case err.port of
      Just (Port p) -> ":" <> Text.pack (show p)
      Nothing -> ""

-- | Bind 127.0.0.1 on the given port, or on a free one, and listen.
--
-- Only the socket is opened here, so a busy port is an error we can
-- report before printing the URL. The logger gets warp's internal errors
-- at verbose level. We never log requests: the path holds the token.
openListener :: Logger -> Maybe Port -> IO (Either ListenError Listener)
openListener logger wanted = do
  result <- try @IOException $
    bracketOnError newSocket Socket.close $ \s -> do
      Socket.withFdSocket s Socket.setCloseOnExecIfNeeded
      setMss logger s
      Socket.bind s (Socket.SockAddrInet portNumber localhost)
      Socket.listen s (max 2048 Socket.maxListenQueue)
      real <- Socket.socketPort s
      pure (s, Port (fromIntegral real))
  pure $ case result of
    Left e -> Left (ListenError wanted (osReason e))
    Right (s, real) ->
      Right
        Listener
          { port = real,
            serve = \ready app -> Warp.runSettingsSocket (settings ready) s app,
            close = Socket.close s
          }
  where
    newSocket = Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    localhost = Socket.tupleToHostAddress (127, 0, 0, 1)
    portNumber = case wanted of
      Just (Port p) -> fromIntegral p
      Nothing -> 0
    -- No setPort or setHost: runSettingsSocket uses our socket.
    settings ready =
      Warp.setBeforeMainLoop ready
        . Warp.setServerName ""
        -- Browsers never speak HTTP/2 without TLS. Less code exposed.
        . Warp.setHTTP2Disabled
        . Warp.setAccept (acceptWithMss logger)
        . Warp.setOnExceptionResponse
          (addSecurityHeaders . Warp.defaultOnExceptionResponse)
        . Warp.setOnException (\_ e -> logException e)
        $ Warp.defaultSettings
    -- Never print the request: its path holds the token.
    logException :: SomeException -> IO ()
    logException e =
      when (Warp.defaultShouldDisplayException e) $
        logVerbose logger ("warp: " <> exceptionMessage e)

-- * Internal

-- | Accept, then set the MSS on the new socket (best effort). See
-- Note [Loopback MSS].
acceptWithMss :: Logger -> Socket.Socket -> IO (Socket.Socket, Socket.SockAddr)
acceptWithMss logger s = do
  (conn, addr) <- Socket.accept s
  setMss logger conn
  pure (conn, addr)

-- | See Note [Loopback MSS].
loopbackMss :: Int
loopbackMss = 1460

-- | Set TCP_MAXSEG, or log at debug level why not.
setMss :: Logger -> Socket.Socket -> IO ()
setMss logger s = do
  r <- try @IOException (Socket.setSocketOption s Socket.MaxSegment loopbackMss)
  case r of
    Right () -> pure ()
    Left e -> logDebug logger ("cannot set TCP_MAXSEG: " <> osReason e)

-- | The OS message (e.g. @Address already in use@), not just the error
-- kind ('ioeGetErrorString' would give @resource busy@).
osReason :: IOException -> Text
osReason e
  | null e.ioe_description = Text.pack (ioeGetErrorString e)
  | otherwise = Text.pack e.ioe_description
