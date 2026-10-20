-- | The listening socket and warp.
module HaskellReviewer.Server.Listen
  ( -- * Types
    Listener (..),
    ListenError (..),

    -- * Operations
    openListener,
    renderListenError,
  )
where

import Control.Exception (SomeException, bracketOnError, displayException, try)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Exception (IOException (..))
import HaskellReviewer.Core.Address (Port (..))
import HaskellReviewer.Effects.Log (Logger, logVerbose)
import HaskellReviewer.Server.Security (addSecurityHeaders)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import System.IO.Error (ioeGetErrorString)

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
        . Warp.setOnExceptionResponse
          (addSecurityHeaders . Warp.defaultOnExceptionResponse)
        . Warp.setOnException (\_ e -> logException e)
        $ Warp.defaultSettings
    -- Never print the request: its path holds the token.
    logException :: SomeException -> IO ()
    logException e =
      when (Warp.defaultShouldDisplayException e) $
        logVerbose logger ("warp: " <> Text.pack (displayException e))

-- * Internal

-- | The OS message (e.g. @Address already in use@), not just the error
-- kind ('ioeGetErrorString' would give @resource busy@).
osReason :: IOException -> Text
osReason e
  | null e.ioe_description = Text.pack (ioeGetErrorString e)
  | otherwise = Text.pack e.ioe_description
