-- | Logger handle. Logs go to stderr (stdout is unused, see SPEC §2.1).
module HaskellReviewer.Effects.Log
  ( -- * Types
    LogLevel (..),
    Logger (..),

    -- * Handles
    newLogger,

    -- * Operations
    logInfo,
    logVerbose,
    logDebug,

    -- * Messages
    exceptionMessage,
    ioErrorMessage,
  )
where

import Control.Exception (SomeException (..), displayException)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Exception (IOException (..))

-- | How much to log. Ordered: a logger at 'LevelDebug' also prints
-- verbose and info messages.
data LogLevel
  = -- | Default: progress and warnings only.
    LevelInfo
  | -- | @--verbose@.
    LevelVerbose
  | -- | @--debug@.
    LevelDebug
  deriving (Eq, Ord, Show)

-- | Where log messages go.
newtype Logger = Logger
  { -- | Write a message at the given level. May drop it.
    emit :: LogLevel -> Text -> IO ()
  }

-- | A logger that keeps messages at or below the given level and passes
-- them to the given writer, one line each.
--
-- No lock here: the writer must be safe to call from several threads (the
-- real console in "HaskellReviewer.App.Run" takes a lock).
newLogger :: LogLevel -> (Text -> IO ()) -> Logger
newLogger threshold write =
  Logger
    { emit = \level msg -> when (level <= threshold) (write msg)
    }

-- | Log at 'LevelInfo'.
logInfo :: Logger -> Text -> IO ()
logInfo logger = logger.emit LevelInfo

-- | Log at 'LevelVerbose'.
logVerbose :: Logger -> Text -> IO ()
logVerbose logger = logger.emit LevelVerbose

-- | Log at 'LevelDebug'.
logDebug :: Logger -> Text -> IO ()
logDebug logger = logger.emit LevelDebug

-- | The message of an exception, on one line, for the user.
--
-- We unwrap the 'SomeException': on GHC 9.10 its displayException
-- adds a backtrace block, which is noise in a log line.
exceptionMessage :: SomeException -> Text
exceptionMessage (SomeException inner) =
  Text.unwords (Text.lines (Text.pack (displayException inner)))

-- | An IO error on one line, without its file name: the caller puts
-- the path where it fits, so it isn't said twice.
ioErrorMessage :: IOException -> Text
ioErrorMessage e = exceptionMessage (SomeException e {ioe_filename = Nothing})
