module HaskellReviewer.App.RunTest (tests) where

import Control.Exception (AsyncException (..), throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import HaskellReviewer.App.Run (Deps (..), run)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Clock (..), MonotonicTime (..), Nanoseconds (..))
import HaskellReviewer.Effects.FileSystem (RootError (..))
import HaskellReviewer.Effects.Trace (Span (..), TraceFileError (..), Tracer (..))
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
      testCase "success: exit 0, one startup span" $ do
        fake <- newFake
        code <- run fake.deps ["--trace-file", "t.jsonl"]
        code @?= ExitSuccess
        spans <- readIORef fake.spans
        spans
          @?= [ Span
                  { name = "startup",
                    start = fakeWallTime,
                    duration = Nanoseconds 4000
                  }
              ]
        closed <- readIORef fake.closed
        closed @?= True,
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
    console :: IORef [Text],
    spans :: IORef [Span],
    closed :: IORef Bool
  }

fakeWallTime :: UTCTime
fakeWallTime = UTCTime (fromGregorian 2026 10 20) 0

-- | Fake deps. The monotonic clock goes up by 4000 ns at each call.
newFake :: IO Fake
newFake = do
  console <- newIORef []
  spans <- newIORef []
  closed <- newIORef False
  ticks <- newIORef 1000
  let tick = atomicModifyIORef' ticks (\t -> (t + 4000, MonotonicTime t))
      tracer =
        Tracer
          { record = \s -> modifyIORef' spans (<> [s]),
            close = modifyIORef' closed (const True)
          }
      deps =
        Deps
          { console = \t -> modifyIORef' console (<> [t]),
            clock = Clock {now = pure fakeWallTime, monotonic = tick},
            checkRoot = \p -> pure (Right (Root ("/abs/" <> p))),
            openTracer = \_ _ -> pure (Right tracer)
          }
  pure Fake {deps = deps, console = console, spans = spans, closed = closed}

assertOneLineWith :: Text -> [Text] -> Assertion
assertOneLineWith prefix out = case out of
  [line] ->
    assertBool
      (show line <> " should start with " <> show prefix)
      (prefix `Text.isPrefixOf` line)
  _ -> assertFailure ("expected one line, got " <> show out)
