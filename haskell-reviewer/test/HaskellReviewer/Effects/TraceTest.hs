module HaskellReviewer.Effects.TraceTest (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Word (Word64)
import HaskellReviewer.Effects.Clock (Nanoseconds (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Effects.Trace (Span (..), Tracer (..), encodeSpan, openFileTracer)
import System.Directory (doesPathExist)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Trace"
    [ testCase "encodeSpan gives one JSON object" $ do
        let bytes = encodeSpan (sampleSpan 1234567)
        assertBool "no newline" (LBS.notElem 10 bytes)
        case Aeson.decode @Aeson.Object bytes of
          Nothing -> fail ("not a JSON object: " <> show bytes)
          Just obj -> do
            KeyMap.lookup "name" obj @?= Just (Aeson.String "startup")
            KeyMap.lookup "start" obj
              @?= Just (Aeson.String "2026-10-20T01:00:00Z")
            KeyMap.lookup "duration_us" obj @?= Just (Aeson.Number 1234)
            KeyMap.size obj @?= 3,
      testCase "write errors: one warning, no exception" $ do
        -- /dev/full fails every write. Linux only: elsewhere, nothing to do.
        hasDevFull <- doesPathExist "/dev/full"
        if not hasDevFull
          then pure ()
          else do
            logs <- newIORef []
            let logger = newLogger LevelInfo (\t -> modifyIORef' logs (<> [t]))
            result <- openFileTracer logger "/dev/full"
            case result of
              Left e -> assertFailure ("cannot open /dev/full: " <> show e)
              Right tracer -> do
                tracer.record (sampleSpan 1000)
                tracer.record (sampleSpan 2000)
                tracer.close
            out <- readIORef logs
            case out of
              [line] ->
                assertBool
                  (show line)
                  ("haskell-reviewer: warning: trace file: " `Text.isPrefixOf` line)
              _ -> assertFailure ("expected one warning, got " <> show out)
    ]

sampleSpan :: Word64 -> Span
sampleSpan ns =
  Span
    { name = "startup",
      start = UTCTime (fromGregorian 2026 10 20) (secondsToDiffTime 3600),
      duration = Nanoseconds ns
    }
