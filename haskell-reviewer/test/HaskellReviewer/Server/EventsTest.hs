-- | The event stream: in process, then with a real warp on a socket.
module HaskellReviewer.Server.EventsTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception, bracket, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Events (EventStream (..), keepAliveFrame, renderEvent)
import HaskellReviewer.Server.Router (application)
import HaskellReviewer.Server.Security (contentSecurityPolicy)
import HaskellReviewer.Session.Clients (EventId (..), newClients, openCount)
import Network.HTTP.Types (Header, Status (..))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Network.Wai (Request (..), Response, defaultRequest, responseToStream)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Internal (ResponseReceived (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Events"
    [ inProcessTests,
      warpTests
    ]

-- * In process

inProcessTests :: TestTree
inProcessTests =
  testGroup
    "in process"
    [ testCase "GET: 200, event stream headers" $ do
        events <- newEvents (Microseconds 10000)
        resp <- call events (request "GET" goodHost streamPath)
        let (st, hs, _) = responseToStream resp
        statusCode st @?= 200
        lookup "Content-Type" hs @?= Just "text/event-stream"
        [v | (n, v) <- hs, n == "Cache-Control"] @?= ["no-store"]
        lookup "Content-Security-Policy" hs @?= Just contentSecurityPolicy
        lookup "X-Content-Type-Options" hs @?= Just "nosniff"
        lookup "Referrer-Policy" hs @?= Just "no-referrer"
        lookup "Content-Length" hs @?= Nothing,
      testCase "exact bytes: hello, then keep-alives" $ do
        events <- newEvents (Microseconds 10000)
        body <- streamBody events 3 (\_ -> pure ())
        body
          @?= "id: 1\nevent: hello\ndata: {}\n\n\
              \: keep-alive\n\n\
              \: keep-alive\n\n",
      testCase "counted as open while streaming, closed after" $ do
        events <- newEvents (Microseconds 10000)
        counts <- newIORef []
        _ <- streamBody events 3 $ \_ -> do
          n <- openCount events.clients
          modifyIORef' counts (n :)
        readIORef counts >>= (@?= [1, 1, 1])
        openCount events.clients >>= (@?= 0),
      testCase "second stream: id 2" $ do
        events <- newEvents (Microseconds 10000)
        _ <- streamBody events 1 (\_ -> pure ())
        body <- streamBody events 1 (\_ -> pure ())
        body @?= "id: 2\nevent: hello\ndata: {}\n\n",
      testCase "POST, HEAD, PUT: 405, Allow: GET" $ do
        events <- newEvents (Microseconds 10000)
        mapM_
          ( \method -> do
              r <- collect =<< call events (request method goodHost streamPath)
              (method, r.status) @?= (method, 405)
              lookup "Allow" r.headers @?= Just "GET"
              [v | (n, v) <- r.headers, n == "Cache-Control"] @?= ["no-store"]
          )
          ["POST", "HEAD", "PUT"],
      testCase "bad token: same 404 as an unknown page" $ do
        events <- newEvents (Microseconds 10000)
        let badPath = [Text.replicate 32 "A", "api", "events"]
        bad <- collect =<< call events (request "GET" goodHost badPath)
        unknown <-
          collect =<< call events (request "GET" goodHost [tokenText token, "nope"])
        bad.status @?= 404
        (bad.status, bad.headers, bad.body)
          @?= (unknown.status, unknown.headers, unknown.body)
        openCount events.clients >>= (@?= 0),
      testCase "bad Host: 403" $ do
        events <- newEvents (Microseconds 10000)
        r <- collect =<< call events (request "GET" (Just "evil.com:8080") streamPath)
        r.status @?= 403,
      testCase "renderEvent: one data line per line" $ do
        render (renderEvent (EventId 7) "x" "a\nb")
          @?= "id: 7\nevent: x\ndata: a\ndata: b\n\n"
        render (renderEvent (EventId 8) "y" "")
          @?= "id: 8\nevent: y\ndata: \n\n"
        render keepAliveFrame @?= ": keep-alive\n\n"
    ]

-- | Thrown by the test sink to end the stream.
data StopStream = StopStream
  deriving (Eq, Show)

instance Exception StopStream

-- | Run the stream body until the given number of flushes. The hook
-- runs at each flush, before the stop. Gives back all bytes written.
streamBody :: EventStream -> Int -> (Int -> IO ()) -> IO LBS.ByteString
streamBody events flushes onFlush = do
  resp <- call events (request "GET" goodHost streamPath)
  let (_, _, withBody) = responseToStream resp
  chunks <- newIORef mempty
  count <- newIORef (0 :: Int)
  let write b = modifyIORef' chunks (<> b)
      flush = do
        n <- atomicModifyIORef' count (\c -> (c + 1, c + 1))
        onFlush n
        when (n >= flushes) (throwIO StopStream)
  r <- timeout 5_000_000 (try @StopStream (withBody (\body -> body write flush)))
  r @?= Just (Left StopStream)
  Builder.toLazyByteString <$> readIORef chunks

data Result = Result
  { status :: Int,
    headers :: [Header],
    body :: LBS.ByteString
  }

-- | Status, headers and whole body of a response that ends.
collect :: Response -> IO Result
collect resp = do
  let (st, hs, withBody) = responseToStream resp
  chunks <- newIORef mempty
  withBody $ \streaming ->
    streaming (\b -> modifyIORef' chunks (<> b)) (pure ())
  b <- readIORef chunks
  pure Result {status = statusCode st, headers = hs, body = Builder.toLazyByteString b}

call :: EventStream -> Request -> IO Response
call events req = do
  saved <- newIORef Nothing
  ResponseReceived <-
    application (Port 8080) token events req $ \resp -> do
      writeIORef saved (Just resp)
      pure ResponseReceived
  readIORef saved >>= \case
    Nothing -> assertFailure "no response"
    Just resp -> pure resp

request :: ByteString -> Maybe ByteString -> [Text] -> Request
request method host path =
  defaultRequest
    { requestMethod = method,
      requestHeaderHost = host,
      requestHeaders = maybe [] (\h -> [("Host", h)]) host,
      pathInfo = path
    }

render :: Builder -> LBS.ByteString
render = Builder.toLazyByteString

-- * Real warp

warpTests :: TestTree
warpTests =
  testGroup
    "warp"
    [ testCase "no warp timeout: open after 6 s, keep-alives come" $ do
        -- warp's timeout set to 1 s. The keep-alive comes every 2.5 s,
        -- so a timeout would cut the stream between two of them.
        events <- newEvents (Microseconds 2_500_000)
        withWarp events $ \port ->
          withStream port $ \s -> do
            bytes <-
              readFor s 6 >>= \case
                StillOpen b -> pure b
                Closed b -> assertFailure ("closed early: " <> show b)
            assertBool ("no hello: " <> show bytes) ("event: hello" `BS.isInfixOf` bytes)
            assertBool ("keep-alives: " <> show bytes) (countKeepAlives bytes >= 2),
      testCase "client closes: stream counted as closed" $ do
        events <- newEvents (Microseconds 100_000)
        withWarp events $ \port -> do
          withStream port $ \s -> do
            _ <- readUntil s "event: hello"
            openCount events.clients >>= (@?= 1)
          ok <- waitUntil 2 ((== 0) <$> openCount events.clients)
          assertBool "stream still open 2 s after the client left" ok
    ]

countKeepAlives :: ByteString -> Int
countKeepAlives s = case BS.breakSubstring ": keep-alive" s of
  (_, rest)
    | BS.null rest -> 0
    | otherwise -> 1 + countKeepAlives (BS.drop 1 rest)

-- | Run warp on a free port of 127.0.0.1, with a 1 s timeout.
withWarp :: EventStream -> (Int -> IO a) -> IO a
withWarp events action =
  bracket open Socket.close $ \sock -> do
    port <- fromIntegral <$> Socket.socketPort sock
    let app = application (Port (fromIntegral port)) token events
        settings =
          Warp.setTimeout 1
            . Warp.setOnException (\_ _ -> pure ())
            $ Warp.defaultSettings
    withAsync (Warp.runSettingsSocket settings sock app) $ \_ -> action port
  where
    open = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.bind s (Socket.SockAddrInet 0 localhost)
      Socket.listen s 16
      pure s

-- | Connect and send the stream request (no Connection: close).
withStream :: Int -> (Socket.Socket -> IO a) -> IO a
withStream port action =
  bracket connect Socket.close $ \s -> do
    Socket.BS.sendAll s $
      "GET /"
        <> Char8.pack (Text.unpack (tokenText token))
        <> "/api/events HTTP/1.1\r\nHost: 127.0.0.1:"
        <> Char8.pack (show port)
        <> "\r\n\r\n"
    action s
  where
    connect = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.connect s (Socket.SockAddrInet (fromIntegral port) localhost)
      pure s

-- | What 'readFor' read, and whether the server closed meanwhile.
data ReadResult
  = StillOpen ByteString
  | Closed ByteString

-- | Read for this many seconds.
readFor :: Socket.Socket -> Double -> IO ReadResult
readFor s secs = do
  t0 <- getMonotonicTime
  let go acc = do
        t <- getMonotonicTime
        let left = secs - (t - t0)
        if left <= 0
          then pure (StillOpen acc)
          else do
            r <- timeout (ceiling (left * 1_000_000)) (Socket.BS.recv s 65536)
            case r of
              Nothing -> pure (StillOpen acc)
              Just chunk
                | BS.null chunk -> pure (Closed acc)
                | otherwise -> go (acc <> chunk)
  go mempty

-- | Read until the bytes hold the text (5 s at most).
readUntil :: Socket.Socket -> ByteString -> IO ByteString
readUntil s needle = do
  r <- timeout 5_000_000 (go mempty)
  case r of
    Just bytes -> pure bytes
    Nothing -> assertFailure ("not seen within 5 s: " <> show needle)
  where
    go acc
      | needle `BS.isInfixOf` acc = pure acc
      | otherwise = do
          chunk <- Socket.BS.recv s 65536
          if BS.null chunk
            then assertFailure ("closed before " <> show needle <> ": " <> show acc)
            else go (acc <> chunk)

-- | Poll every 20 ms, up to the given seconds.
waitUntil :: Double -> IO Bool -> IO Bool
waitUntil secs check = do
  t0 <- getMonotonicTime
  let go = do
        ok <- check
        t <- getMonotonicTime
        if ok || t - t0 > secs then pure ok else threadDelay 20000 >> go
  go

localhost :: Socket.HostAddress
localhost = Socket.tupleToHostAddress (127, 0, 0, 1)

-- * Shared

token :: Token
token = tokenFromBytes (BS.pack [1 .. 24])

goodHost :: Maybe ByteString
goodHost = Just "127.0.0.1:8080"

streamPath :: [Text]
streamPath = [tokenText token, "api", "events"]

newEvents :: Microseconds -> IO EventStream
newEvents keepAlive = do
  clients <- newClients
  pure
    EventStream
      { clients = clients,
        keepAlive = keepAlive,
        logger = newLogger LevelInfo (\_ -> pure ())
      }
