module HaskellReviewer.App.RunTest (tests) where

import ApiSupport (ApiResult (..), callApp)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async (async, poll, wait, withAsync)
import Control.Concurrent.MVar
  ( MVar,
    isEmptyMVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Exception (AsyncException (..), throwIO, try)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import HaskellReviewer.App.Run (Deps (..), run)
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Core.FileList (FileList (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.Git (GitVersion (..))
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Effects.Clock
  ( Clock (..),
    Microseconds (..),
    MonotonicTime (..),
    Nanoseconds (..),
  )
import HaskellReviewer.Effects.FileSystem (ListError (..), Lister (..), RootError (..))
import HaskellReviewer.Effects.Git (GitError (..), TooOld (..))
import HaskellReviewer.Effects.Shutdown (Shutdown (..))
import HaskellReviewer.Effects.Spawn (SoftLimit (..), SpawnSetup (..))
import HaskellReviewer.Effects.Trace (Span (..), TraceFileError (..), Tracer (..))
import HaskellReviewer.Server.Listen (ListenError (..), Listener (..))
import Network.Wai (Application, Request (..), defaultRequest, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import System.Exit (ExitCode (..))
import System.Posix.Resource (ResourceLimit (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Run"
    [ testCase "unexpected exception: exit 1" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> ioError (userError "boom")}
        code <- run deps []
        code @?= ExitFailure 1
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: internal error" out,
      testCase "async exception goes through" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> throwIO UserInterrupt}
        result <- try (run deps [])
        result @?= Left UserInterrupt,
      testCase "ExitCode exception goes through" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> throwIO (ExitFailure 3)}
        result <- try (run deps [])
        result @?= Left (ExitFailure 3),
      testCase "missing ROOT: exit 2" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \p -> pure (Left (RootMissing p))}
        code <- run deps ["nope"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out @?= ["haskell-reviewer: ROOT does not exist: nope"],
      testCase "bad trace file: exit 2" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { openTracer = \_ p -> pure (Left (TraceFileError p "does not exist"))
                }
        code <- run deps ["--trace-file", "x/t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: cannot open trace file x/t.jsonl" out,
      testCase "success: URL line, startup span, browser, all closed, exit 0" $ do
        fake <- newFake
        code <- run fake.deps ["--trace-file", "t.jsonl"]
        code @?= ExitSuccess
        out <- readIORef fake.console
        out @?= ["haskell-reviewer: serving at " <> fakeUrl]
        spans <- readIORef fake.spans
        spans
          @?= [ Span
                  { name = "startup",
                    start = fakeWallTime,
                    duration = Nanoseconds 4000
                  }
              ]
        closed <- readIORef fake.closed
        closed @?= True
        listenerClosed <- readIORef fake.listenerClosed
        listenerClosed @?= True
        opened <- readIORef fake.opened
        opened @?= [fakeUrl],
      testCase "git too old: exit 2, no listen, tracer closed" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { probeFiles = \_ _ ->
                    pure . Left . GitTooOld $
                      TooOld {exe = "/usr/bin/git", version = GitVersion 2 43 0},
                  listen = \_ _ -> assertFailure "listen called"
                }
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out
          @?= [ "haskell-reviewer: git at /usr/bin/git is version 2.43.0; \
                \need 2.44 or newer"
              ]
        readIORef fake.closed >>= (@?= True),
      listingTests,
      testCase "--port is passed to listen" $ do
        fake <- newFake
        ports <- newIORef []
        let deps =
              (fake.deps)
                { listen = \l p -> modifyIORef' ports (<> [p]) >> fake.deps.listen l p
                }
        code <- run deps ["--port", "1234"]
        code @?= ExitSuccess
        readIORef ports >>= (@?= [Just (Port 1234)]),
      testCase "--no-open: browser not opened" $ do
        fake <- newFake
        code <- run fake.deps ["--no-open"]
        code @?= ExitSuccess
        opened <- readIORef fake.opened
        opened @?= [],
      testCase "listen error: exit 2, tracer closed" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { listen = \_ p -> pure (Left (ListenError p "Address already in use"))
                }
        code <- run deps ["--port", "8080", "--trace-file", "t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out
          @?= [ "haskell-reviewer: cannot listen on 127.0.0.1:8080: \
                \Address already in use"
              ]
        closed <- readIORef fake.closed
        closed @?= True,
      testCase "server throws: exit 1, all closed" $ do
        fake <- newFake
        let failing = fake.listener {serve = \_ _ -> ioError (userError "boom")}
            deps = (fake.deps) {listen = \_ _ -> pure (Right failing)}
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitFailure 1
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: internal error" out
        closed <- readIORef fake.closed
        closed @?= True
        listenerClosed <- readIORef fake.listenerClosed
        listenerClosed @?= True,
      testCase "signal during start-up: exit 0, no listen, no URL" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { installShutdown = pure alreadyStopped,
                  listen = \_ _ -> assertFailure "listen called"
                }
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitSuccess
        readIORef fake.console >>= (@?= [])
        readIORef fake.spans >>= (@?= [])
        readIORef fake.opened >>= (@?= [])
        readIORef fake.closed >>= (@?= True),
      testCase "signal while listening: no URL, no span, no browser" $ do
        fake <- newFake
        stoppedRef <- newIORef False
        let deps =
              (fake.deps)
                { installShutdown =
                    pure
                      Shutdown
                        { wait = pure (),
                          stopped = readIORef stoppedRef
                        },
                  listen = \l p -> do
                    modifyIORef' stoppedRef (const True)
                    fake.deps.listen l p
                }
        code <- run deps []
        code @?= ExitSuccess
        readIORef fake.console >>= (@?= [])
        readIORef fake.spans >>= (@?= [])
        readIORef fake.opened >>= (@?= [])
        readIORef fake.listenerClosed >>= (@?= True),
      testCase "start-up error wins over an early signal" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { installShutdown = pure alreadyStopped,
                  checkRoot = \p -> pure (Left (RootMissing p))
                }
        code <- run deps ["nope"]
        code @?= ExitFailure 2,
      testCase "signals caught before ROOT is checked" $ do
        fake <- newFake
        installed <- newIORef False
        let deps =
              (fake.deps)
                { installShutdown = do
                    modifyIORef' installed (const True)
                    fake.deps.installShutdown,
                  checkRoot = \p -> do
                    ok <- readIORef installed
                    assertBool "handlers not installed yet" ok
                    pure (Left (RootMissing p))
                }
        code <- run deps ["nope"]
        code @?= ExitFailure 2,
      testCase "server returns: exit 1" $ do
        fake <- newFake
        let stopping = fake.listener {serve = \_ _ -> pure ()}
            deps = (fake.deps) {listen = \_ _ -> pure (Right stopping)}
        code <- run deps []
        code @?= ExitFailure 1,
      testCase "--version: exit 0, ROOT not checked" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> assertFailure "checkRoot called"}
        code <- run deps ["--version"]
        code @?= ExitSuccess
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer " out,
      testCase "usage error: exit 2" $ do
        fake <- newFake
        code <- run fake.deps ["--bogus"]
        code @?= ExitFailure 2,
      idleTests
    ]

-- * Listing

listingTests :: TestTree
listingTests =
  testGroup
    "listing"
    [ testCase "files are listed after the URL line"
        $ withRunning
          ( \fake -> fakeLister $ do
              fake.deps.console "listing"
              pure (Right (mkFileList FromDisk 0 []))
          )
        $ \fake _ -> do
          ok <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
          assertBool "not listed" ok
          out <- readIORef fake.console
          take 2 out @?= ["haskell-reviewer: serving at " <> fakeUrl, "listing"],
      testCase "listing fails: a line, 500, no exit" $
        withRunning (\_ -> fakeLister (pure (Left (ListCrashed "boom")))) $
          \fake app -> do
            r <- getFiles app
            r.status @?= 500
            r.body @?= "cannot list files"
            -- The line comes after the slot is filled: wait for it.
            _ <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
            out <- readIORef fake.console
            out
              @?= [ "haskell-reviewer: serving at " <> fakeUrl,
                    "haskell-reviewer: cannot list files: boom"
                  ],
      testCase "lister throws: a line, 500, no hang" $
        withRunning (\_ -> fakeLister (ioError (userError "boom"))) $ \fake app -> do
          r <- timeout 5_000_000 (getFiles app)
          fmap (.status) r @?= Just 500
          _ <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
          out <- readIORef fake.console
          drop 1 out @?= ["haskell-reviewer: cannot list files: user error (boom)"],
      testCase "a request before the list is done waits, then 200" $ do
        release <- newEmptyMVar
        let lister _ = fakeLister $ do
              readMVar release
              pure (Right (mkFileList FromDisk 0 []))
        withRunning lister $ \_ app ->
          withAsync (getFiles app) $ \request -> do
            threadDelay 200_000
            early <- poll request
            assertBool "answered before the list was done" (isNothing early)
            putMVar release ()
            r <- timeout 5_000_000 (wait request)
            fmap (.status) r @?= Just 200,
      testCase "warnings for skipped names and the cap" $ do
        let big = (mkFileList FromDisk 3 []) {truncated = True}
        withRunning (\_ -> fakeLister (pure (Right big))) $ \fake app -> do
          _ <- getFiles app
          ok <- waitFor 5 ((== 3) . length <$> readIORef fake.console)
          assertBool "no lines" ok
          out <- readIORef fake.console
          drop 1 out
            @?= [ "haskell-reviewer: skipped 3 file name(s) that are not UTF-8",
                  "haskell-reviewer: more than 100000 files under ROOT; \
                  \listing the first 100000"
                ]
    ]

-- | Run with this lister until the action is done, then stop with a
-- signal. The action gets the app warp would serve.
withRunning :: (Fake -> Lister) -> (Fake -> Application -> IO a) -> IO a
withRunning mkLister action = do
  fake <- newFake
  stop <- newEmptyMVar
  appVar <- newEmptyMVar
  let serve ready app = ready >> putMVar appVar app >> blockForever
      deps =
        (idleDeps fake stop serve)
          { probeFiles = \_ _ -> pure (Right (mkLister fake))
          }
  withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
    app <- timeout 5_000_000 (takeMVar appVar)
    result <- maybe (assertFailure "not ready") (action fake) app
    putMVar stop ()
    code <- timeout 5_000_000 (wait running)
    code @?= Just ExitSuccess
    pure result

-- | GET api/files/tree on the app.
getFiles :: Application -> IO ApiResult
getFiles app =
  callApp app "GET" "127.0.0.1:4321" [tokenText fakeToken, "api", "files", "tree"]

-- * Idle exit

-- | A "minute" here is 100 ms: short, but with room for slow machines.
-- The URL line must come before the timer fires.
idleTests :: TestTree
idleTests =
  testGroup
    "idle exit"
    [ testCase "no tab: exit 0 with a line, all closed" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        code <-
          timeout 5_000_000 (run deps ["--idle-exit", "1", "--trace-file", "t.jsonl"])
        code @?= Just ExitSuccess
        readIORef fake.console
          >>= ( @?=
                  [ "haskell-reviewer: serving at " <> fakeUrl,
                    "haskell-reviewer: no browser tab for 1 minute, exiting"
                  ]
              )
        readIORef fake.closed >>= (@?= True)
        readIORef fake.listenerClosed >>= (@?= True),
      testCase "plural minutes" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        code <- timeout 5_000_000 (run deps ["--idle-exit", "3"])
        code @?= Just ExitSuccess
        out <- readIORef fake.console
        drop (length out - 1) out
          @?= ["haskell-reviewer: no browser tab for 3 minutes, exiting"],
      testCase "--idle-exit 0: no idle exit, a signal still stops it" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        running <- async (run deps ["--idle-exit", "0"])
        threadDelay 1_000_000
        poll running >>= assertRunning
        putMVar stop ()
        code <- timeout 5_000_000 (wait running)
        code @?= Just ExitSuccess
        readIORef fake.console >>= (@?= ["haskell-reviewer: serving at " <> fakeUrl]),
      testCase "an open stream keeps it alive; closing it starts the timer" $ do
        fake <- newFake
        stop <- newEmptyMVar
        opened <- newEmptyMVar
        streamThread <- newEmptyMVar
        let serve ready app = do
              ready
              t <- forkIO (openStream app opened)
              putMVar streamThread t
              blockForever
            deps = idleDeps fake stop serve
        -- 5 "minutes" (500 ms) to get the stream open.
        running <- async (run deps ["--idle-exit", "5"])
        t <- takeMVar streamThread
        counted <- timeout 5_000_000 (takeMVar opened)
        counted @?= Just ()
        threadDelay 1_500_000
        poll running >>= assertRunning
        killThread t
        code <- timeout 5_000_000 (wait running)
        code @?= Just ExitSuccess
        out <- readIORef fake.console
        drop 1 out @?= ["haskell-reviewer: no browser tab for 5 minutes, exiting"]
    ]

-- | Fake deps with a 100 ms minute, the given serve, and a stop signal
-- the test sends.
idleDeps :: Fake -> MVar () -> (IO () -> Application -> IO ()) -> Deps
idleDeps fake stop serve =
  (fake.deps)
    { idleMinute = Microseconds 100_000,
      listen = \_ _ -> pure (Right fake.listener {serve = serve}),
      installShutdown =
        pure
          Shutdown
            { wait = readMVar stop,
              stopped = not <$> isEmptyMVar stop
            }
    }

-- | Open the event stream on the app, like a browser tab, and keep it
-- open. Writes go nowhere. The MVar is filled at the first write: the
-- stream is counted as open by then.
openStream :: Application -> MVar () -> IO ()
openStream app opened = do
  let req =
        defaultRequest
          { requestMethod = "GET",
            requestHeaderHost = Just "127.0.0.1:4321",
            pathInfo = [tokenText fakeToken, "api", "events"]
          }
  ResponseReceived <- app req $ \resp -> do
    let (_, _, withBody) = responseToStream resp
    withBody (\body -> body (\_ -> void (tryPutMVar opened ())) (pure ()))
    pure ResponseReceived
  pure ()

-- | A listing that never ends.
neverDone :: IO (Either ListError FileList)
neverDone = Left (ListCrashed "") <$ blockForever

-- | A lister for a fake ROOT.
fakeLister :: IO (Either ListError FileList) -> Lister
fakeLister action =
  Lister {root = CanonicalRoot "/abs/x" "/abs/x", list = action}

-- | Poll every 20 ms, up to the given seconds.
waitFor :: Double -> IO Bool -> IO Bool
waitFor secs check = go (ceiling (secs * 50) :: Int)
  where
    go n = do
      ok <- check
      if ok || n <= 0 then pure ok else threadDelay 20_000 >> go (n - 1)

blockForever :: IO ()
blockForever = forever (threadDelay 1_000_000)

assertRunning :: Maybe a -> Assertion
assertRunning = \case
  Nothing -> pure ()
  Just _ -> assertFailure "run exited too early"

data Fake = Fake
  { deps :: Deps,
    listener :: Listener,
    console :: IORef [Text],
    spans :: IORef [Span],
    closed :: IORef Bool,
    listenerClosed :: IORef Bool,
    -- | URLs passed to openBrowser.
    opened :: IORef [Text]
  }

-- | A signal came before 'run' even started.
alreadyStopped :: Shutdown
alreadyStopped = Shutdown {wait = pure (), stopped = pure True}

fakeToken :: Token
fakeToken = tokenFromBytes (BS.replicate 24 0)

fakeUrl :: Text
fakeUrl = "http://127.0.0.1:4321/" <> tokenText fakeToken <> "/"

fakeWallTime :: UTCTime
fakeWallTime = UTCTime (fromGregorian 2026 10 20) 0

-- | Fake deps. The monotonic clock goes up by 4000 ns at each call.
--
-- The fake server calls ready, then sends the stop signal itself and
-- blocks, like warp would.
newFake :: IO Fake
newFake = do
  console <- newIORef []
  spans <- newIORef []
  closed <- newIORef False
  listenerClosed <- newIORef False
  opened <- newIORef []
  stop <- newEmptyMVar
  ticks <- newIORef 1000
  let tick = atomicModifyIORef' ticks (\t -> (t + 4000, MonotonicTime t))
      tracer =
        Tracer
          { record = \s -> modifyIORef' spans (<> [s]),
            close = modifyIORef' closed (const True)
          }
      listener =
        Listener
          { port = Port 4321,
            serve = \ready _ -> do
              ready
              void (tryPutMVar stop ())
              forever (threadDelay 1000000),
            close = modifyIORef' listenerClosed (const True)
          }
      deps =
        Deps
          { console = \t -> modifyIORef' console (<> [t]),
            clock = Clock {now = pure fakeWallTime, monotonic = tick},
            checkRoot = \p -> pure (Right (Root ("/abs/" <> p))),
            openTracer = \_ _ -> pure (Right tracer),
            newToken = pure fakeToken,
            listen = \_ _ -> pure (Right listener),
            -- Never done: no line or span from the listing.
            probeFiles = \_ _ -> pure (Right (fakeLister neverDone)),
            openBrowser = \_ _ url -> modifyIORef' opened (<> [url]),
            installShutdown =
              pure
                Shutdown
                  { wait = readMVar stop,
                    stopped = not <$> isEmptyMVar stop
                  },
            -- A real minute: the default idle exit (5 min) never
            -- fires in these tests.
            idleMinute = Microseconds 60_000_000,
            spawnSetup = Marked 0 (KeptAt (ResourceLimit 1024))
          }
  pure
    Fake
      { deps = deps,
        listener = listener,
        console = console,
        spans = spans,
        closed = closed,
        listenerClosed = listenerClosed,
        opened = opened
      }

assertOneLineWith :: Text -> [Text] -> Assertion
assertOneLineWith prefix out = case out of
  [line] ->
    assertBool
      (show line <> " should start with " <> show prefix)
      (prefix `Text.isPrefixOf` line)
  _ -> assertFailure ("expected one line, got " <> show out)
