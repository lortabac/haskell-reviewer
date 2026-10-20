-- | Tracer handle: timing spans for @--trace-file@ (SPEC §11.9).
--
-- A trace file has one JSON object per line:
--
-- > {"name":"startup","start":"2026-10-20T06:50:27.123Z","duration_us":1234}
module HaskellReviewer.Effects.Trace
  ( -- * Types
    Span (..),
    Tracer (..),
    TraceFileError (..),

    -- * Handles
    noTracer,
    openFileTracer,

    -- * Encoding
    encodeSpan,
    renderTraceFileError,
  )
where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import HaskellReviewer.Effects.Clock (Nanoseconds (..))
import HaskellReviewer.Effects.Log (Logger, logInfo)
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import System.IO.Error (ioeGetErrorString)

-- | One timed piece of work.
data Span = Span
  { -- | What was timed, e.g. @startup@.
    name :: Text,
    -- | Wall clock time when it started.
    start :: UTCTime,
    -- | How long it took, from the monotonic clock.
    duration :: Nanoseconds
  }
  deriving (Eq, Show)

-- | Where spans go.
data Tracer = Tracer
  { -- | Save one span.
    record :: Span -> IO (),
    -- | Done: flush and close. Call it once, at the end.
    close :: IO ()
  }

-- | Drops every span. Used without @--trace-file@.
noTracer :: Tracer
noTracer = Tracer {record = \_ -> pure (), close = pure ()}

-- | The trace file could not be opened.
data TraceFileError = TraceFileError
  { path :: FilePath,
    -- | Short reason from the OS, e.g. @does not exist@.
    reason :: Text
  }
  deriving (Eq, Show)

-- | Message for the user.
renderTraceFileError :: TraceFileError -> Text
renderTraceFileError err =
  "cannot open trace file " <> Text.pack err.path <> ": " <> err.reason

-- | Open (and truncate) a trace file.
--
-- Each span is flushed right away, so the file is useful even if we crash.
--
-- Tracing is only diagnostics, so a failed write (e.g. a full disk) must
-- not stop the program. On the first failure we log one warning at info
-- level and stop writing.
openFileTracer :: Logger -> FilePath -> IO (Either TraceFileError Tracer)
openFileTracer logger file = do
  result <- try @IOException (openBinaryFile file WriteMode)
  case result of
    Left e ->
      pure (Left (TraceFileError file (Text.pack (ioeGetErrorString e))))
    Right h -> Right <$> fileTracer logger h

-- | One span as a JSON object, without the trailing newline.
--
-- Built by hand with 'Data.Aeson.pairs' to keep the key order stable and
-- readable (name first).
encodeSpan :: Span -> LBS.ByteString
encodeSpan s =
  Aeson.Encoding.encodingToLazyByteString $
    Aeson.pairs $
      ("name" .= s.name)
        <> ("start" .= iso8601Show s.start)
        <> ("duration_us" .= (ns `div` 1000))
  where
    Nanoseconds ns = s.duration

-- * Internal

fileTracer :: Logger -> Handle -> IO Tracer
fileTracer logger h = do
  broken <- newIORef False
  let -- Warn on the first failure only.
      onError :: IOException -> IO ()
      onError e = do
        wasBroken <- atomicModifyIORef' broken (True,)
        unless wasBroken $
          logInfo logger $
            "haskell-reviewer: warning: trace file: "
              <> Text.pack (displayException e)
      guarded action = try @IOException action >>= either onError pure
  pure
    Tracer
      { record = \s -> do
          isBroken <- readIORef broken
          unless isBroken $
            guarded (LBS.hPut h (encodeSpan s <> "\n") >> hFlush h),
        close = guarded (hClose h)
      }
