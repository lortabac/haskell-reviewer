-- | The listening socket. See Note [Loopback MSS] in
-- "HaskellReviewer.Server.Listen".
module HaskellReviewer.Server.ListenTest (tests) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Address (Port (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Server.Listen (Listener (..), acceptWithMss, openListener)
import Network.HTTP.Types (status200)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Network.Wai (Application, responseLBS)
import System.Info qualified
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
    testCaseInfo,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "Listen"
    [ -- The listener's MSS goes in its SYN-ACK: it caps what the
      -- client sends.
      testCaseInfo "the client sees a small MSS" $ do
        (logger, failed) <- recordingLogger
        withListenerLogging logger $ \l ->
          withClient l.port $ \s -> do
            mss <- Socket.getSocketOption s Socket.MaxSegment
            checkMss failed mss,
      -- Caps what we send. Set up like openListener: MSS on the listener
      -- too: Linux copies it from there, macOS may need the set after
      -- accept.
      testCaseInfo "accepted sockets get a small MSS" $ do
        (logger, failed) <- recordingLogger
        bracket plainListener Socket.close $ \ls -> do
          port <- Socket.socketPort ls
          withClient (Port (fromIntegral port)) $ \_ ->
            bracket (fst <$> acceptWithMss logger ls) Socket.close $ \conn -> do
              mss <- Socket.getSocketOption conn Socket.MaxSegment
              checkMss failed mss,
      -- TIMING TEST. Without the MSS clamp, the body came about 200 ms
      -- (or a multiple) after the headers. 150 ms leaves room for a
      -- slow machine.
      -- Linux only: the stall was seen there, and we can't check the
      -- timers of other systems.
      testCase "timing: a big body comes right after the headers" $
        unless (System.Info.os == "darwin") $
          withListener $ \l -> do
            ready <- newEmptyMVar
            withAsync (l.serve (putMVar ready ()) bigApp) $ \_ -> do
              takeMVar ready
              forM_ [1 :: Int .. 10] $ \_ -> do
                r <- timeout 10_000_000 (fetch l.port)
                case r of
                  Nothing -> assertFailure "no response within 10 s"
                  Just (bodyLen, gap) -> do
                    bodyLen @?= bigSize
                    let ms = round (gap * 1000) :: Int
                    assertBool
                      ("body " <> show ms <> " ms after the headers")
                      (gap < 0.150)
    ]

-- | 13 chunks of 32 KB, about 400 KB.
bigApp :: Application
bigApp _ respond =
  respond $
    responseLBS
      status200
      [ ("Content-Type", "application/octet-stream"),
        ("Content-Length", Char8.pack (show bigSize)),
        -- Real responses have about 1 KB of headers (CSP, ...). With
        -- tiny headers the stall didn't show.
        ("X-Pad", BS.replicate 1000 97)
      ]
      (LBS.fromChunks (replicate 13 (BS.replicate 32768 120)))

bigSize :: Int
bigSize = 13 * 32768

-- | One GET with Connection: close. Gives back the body length and the
-- seconds between the end of the headers and the end of the body.
fetch :: Port -> IO (Int, Double)
fetch port =
  withClient port $ \s -> do
    Socket.BS.sendAll s "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
    (rest, t0) <- headers s ""
    (n, t1) <- body s (BS.length rest)
    pure (n, t1 - t0)
  where
    headers s acc = do
      chunk <- Socket.BS.recv s 65536
      t <- getMonotonicTime
      when (BS.null chunk) (assertFailure "closed before the headers")
      let all' = acc <> chunk
          (_, after) = BS.breakSubstring "\r\n\r\n" all'
      if BS.null after
        then headers s all'
        else pure (BS.drop 4 after, t)
    body s n = do
      chunk <- Socket.BS.recv s 65536
      if BS.null chunk
        then (\t -> (n, t)) <$> getMonotonicTime
        else body s (n + BS.length chunk)

-- * Helpers

withListener :: (Listener -> IO a) -> IO a
withListener = withListenerLogging quiet

withListenerLogging :: Logger -> (Listener -> IO a) -> IO a
withListenerLogging logger action = do
  opened <- openListener logger Nothing
  case opened of
    Left e -> assertFailure (show e)
    Right l -> action l `finally` l.close

withClient :: Port -> (Socket.Socket -> IO a) -> IO a
withClient (Port p) = bracket open Socket.close
  where
    open = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.connect
        s
        (Socket.SockAddrInet (fromIntegral p) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      pure s

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | A listening socket on a free port of 127.0.0.1, MSS set if the OS
-- lets us.
plainListener :: IO Socket.Socket
plainListener = do
  s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  _ <- try @IOException (Socket.setSocketOption s Socket.MaxSegment 1460)
  Socket.bind s (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen s 5
  pure s

-- | A debug logger, and whether it saw "cannot set TCP_MAXSEG".
recordingLogger :: IO (Logger, IORef Bool)
recordingLogger = do
  failed <- newIORef False
  let emit msg =
        when ("cannot set TCP_MAXSEG" `Text.isInfixOf` msg) $
          writeIORef failed True
  pure (newLogger LevelDebug emit, failed)

-- | At most 1460. If setting it failed, that's allowed on macOS only.
checkMss :: IORef Bool -> Int -> IO String
checkMss failed mss = do
  refused <- readIORef failed
  if refused && System.Info.os == "darwin"
    then pure "skipped: the OS refused TCP_MAXSEG"
    else do
      assertBool ("MSS " <> show mss) (mss <= 1460)
      pure ""
