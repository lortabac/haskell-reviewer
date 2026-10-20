module HaskellReviewer.App.RunTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (AsyncException (..), throwIO, try)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import HaskellReviewer.App.Run (Deps (..), run)
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Clock (..), MonotonicTime (..), Nanoseconds (..))
import HaskellReviewer.Effects.FileSystem (RootError (..))
import HaskellReviewer.Effects.Shutdown (Shutdown (..))
import HaskellReviewer.Effects.Trace (Span (..), TraceFileError (..), Tracer (..))
import HaskellReviewer.Server.Listen (ListenError (..), Listener (..))
import System.Exit (ExitCode (..))
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
        code @?= ExitFailure 2
    ]

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
            openBrowser = \_ _ url -> modifyIORef' opened (<> [url]),
            installShutdown =
              pure
                Shutdown
                  { wait = readMVar stop,
                    stopped = not <$> isEmptyMVar stop
                  }
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
