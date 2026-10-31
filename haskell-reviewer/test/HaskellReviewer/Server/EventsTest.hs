-- | The event stream: in process, then with a real warp on a socket.
module HaskellReviewer.Server.EventsTest (tests) where

import ApiSupport (fixedPrefs)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Exception (Exception, bracket, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (FilesApi (..))
import HaskellReviewer.Server.Events (EventStream (..), keepAliveFrame, renderEvent)
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Server.Security (contentSecurityPolicy)
import HaskellReviewer.Server.Wire (encodeIndexing)
import HaskellReviewer.Session.Clients (EventId (..), newClients, openCount)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot (Generation (..), IndexingState (..))
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
      testCase "indexing: the state right after hello" $ do
        state <- newTVarIO (Just (at 0))
        events <- newEventsWith (Microseconds 10_000_000) (readTVar state)
        body <- streamBody events 2 (\_ -> pure ())
        body @?= hello <> indexingEvent 2 0,
      testCase "indexing: a change sends the last state" $ do
        state <- newTVarIO (Just (at 0))
        events <- newEventsWith (Microseconds 10_000_000) (readTVar state)
        -- Two changes before the next read: only the last one goes out.
        body <- streamBody events 3 $ \case
          2 -> setAt state 1 >> setAt state 2
          _ -> pure ()
        body @?= hello <> indexingEvent 2 0 <> indexingEvent 3 2,
      testCase "indexing: no change, only keep-alives" $ do
        state <- newTVarIO (Just (at 0))
        events <- newEventsWith (Microseconds 10_000) (readTVar state)
        body <- streamBody events 4 $ \case
          -- The same state again is no change.
          3 -> setAt state 0
          _ -> pure ()
        body @?= hello <> indexingEvent 2 0 <> keepAlives 2,
      testCase "indexing: not listed yet, then listing failed: no event" $ do
        listed <- newTVarIO False
        let source = readTVar listed >>= \l -> if l then pure Nothing else retry
        events <- newEventsWith (Microseconds 10_000) source
        body <- streamBody events 3 $ \case
          2 -> atomically (writeTVar listed True)
          _ -> pure ()
        body @?= hello <> keepAlives 2,
      testCase "indexing: at most one event per gap, the last state gets there" $ do
        state <- newTVarIO (Just (at 0))
        let gap = Microseconds 200_000
        events <- newEventsGap (Microseconds 10_000_000) gap (readTVar state)
        -- 50 changes, 1 ms apart, once the first event is out. With no
        -- gap that would be about 50 events; with 200 ms, a few.
        let changes = mapM_ (\n -> setAt state n >> threadDelay 1_000) [1 .. 50]
            lastOne = "\"ready\":50"
        body <- streamUntil events lastOne $ \case
          2 -> void (forkIO changes)
          _ -> pure ()
        let names = Char8.lines (LBS.toStrict body)
            count = length (filter (== "event: indexing") names)
            final = indexingEvent (fromIntegral count + 1) 50
        assertBool ("events: " <> show count) (count <= 20)
        assertBool (show body) (final `LBS.isSuffixOf` body),
      testCase "renderEvent: one data line per line" $ do
        render (renderEvent (EventId 7) "x" "a\nb")
          @?= "id: 7\nevent: x\ndata: a\ndata: b\n\n"
        render (renderEvent (EventId 8) "y" "")
          @?= "id: 8\nevent: y\ndata: \n\n"
        render keepAliveFrame @?= ": keep-alive\n\n"
    ]

hello :: LBS.ByteString
hello = "id: 1\nevent: hello\ndata: {}\n\n"

keepAlives :: Int -> LBS.ByteString
keepAlives n = mconcat (replicate n ": keep-alive\n\n")

-- | An indexing event with this id, for 'at' this many.
indexingEvent :: Word64 -> Int -> LBS.ByteString
indexingEvent i n =
  render $
    renderEvent (EventId i) "indexing" (encodeIndexing (Generation 1) (progressAt n))

-- | Generation 1, 100 modules, this many ready.
at :: Int -> IndexingState
at n = IndexingState {generation = Generation 1, progress = progressAt n}

progressAt :: Int -> Progress
progressAt n = Progress {total = 100, ready = n, failed = 0}

setAt :: TVar (Maybe IndexingState) -> Int -> IO ()
setAt v n = atomically (writeTVar v (Just (at n)))

-- | Run the stream body until the bytes hold the text (5 s at most).
-- The hook runs at each flush, with its number.
streamUntil :: EventStream -> ByteString -> (Int -> IO ()) -> IO LBS.ByteString
streamUntil events needle onFlush = do
  resp <- call events (request "GET" goodHost streamPath)
  let (_, _, withBody) = responseToStream resp
  chunks <- newIORef mempty
  count <- newIORef (0 :: Int)
  let write b = modifyIORef' chunks (<> b)
      flush = do
        n <- atomicModifyIORef' count (\c -> (c + 1, c + 1))
        onFlush n
        bytes <- LBS.toStrict . Builder.toLazyByteString <$> readIORef chunks
        when (needle `BS.isInfixOf` bytes) (throwIO StopStream)
  r <- timeout 5_000_000 (try @StopStream (withBody (\body -> body write flush)))
  r @?= Just (Left StopStream)
  Builder.toLazyByteString <$> readIORef chunks

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
    application (Port 8080) token (routesFor events) req $ \resp -> do
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
      -- Origin too: methods other than GET and HEAD need it.
      requestHeaders = maybe [] (\h -> [("Host", h), ("Origin", "http://" <> h)]) host,
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
    let app = application (Port (fromIntegral port)) token (routesFor events)
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

-- | The routes, with file routes that never list.
routesFor :: EventStream -> Routes
routesFor events =
  Routes
    { events = events,
      files =
        FilesApi
          { snapshot = pure (Left (ListCrashed "no files here")),
            readEntry = \_ _ _ -> pure (Left ReadGone),
            logger = newLogger LevelInfo (\_ -> pure ())
          },
      prefs = fixedPrefs
    }

-- | No indexing event ever.
newEvents :: Microseconds -> IO EventStream
newEvents keepAlive = newEventsWith keepAlive (pure Nothing)

-- | With this indexing state, and no gap between events.
newEventsWith :: Microseconds -> STM (Maybe IndexingState) -> IO EventStream
newEventsWith keepAlive = newEventsGap keepAlive (Microseconds 0)

-- | With this indexing state and gap between events.
newEventsGap ::
  Microseconds -> Microseconds -> STM (Maybe IndexingState) -> IO EventStream
newEventsGap keepAlive gap indexing = do
  clients <- newClients
  pure
    EventStream
      { clients = clients,
        keepAlive = keepAlive,
        indexing = indexing,
        minGap = gap,
        logger = newLogger LevelInfo (\_ -> pure ())
      }
