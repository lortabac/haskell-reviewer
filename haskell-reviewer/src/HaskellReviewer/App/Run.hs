-- | Program entry point: parse the command line, check inputs, start up.
module HaskellReviewer.App.Run
  ( -- * Types
    Deps (..),
    StartupError (..),

    -- * Running
    main,
    run,
    realDeps,
    renderStartupError,
  )
where

import Control.Exception
  ( SomeAsyncException,
    SomeException,
    catch,
    displayException,
    finally,
    fromException,
    throwIO,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import HaskellReviewer.App.Cli (CliResult (..), Command (..), Options (..), parseArgs)
import HaskellReviewer.App.Version (versionText)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Clock (..), elapsed, systemClock)
import HaskellReviewer.Effects.FileSystem (RootError, renderRootError)
import HaskellReviewer.Effects.FileSystem qualified as FileSystem
import HaskellReviewer.Effects.Log (Logger, logDebug, logVerbose, newLogger)
import HaskellReviewer.Effects.Trace
  ( Span (..),
    TraceFileError,
    Tracer (..),
    noTracer,
    openFileTracer,
    renderTraceFileError,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hSetEncoding, stderr, utf8)

{- Note [Layers]

The library has layers. Lower layers never import upper ones:

1. Core (HaskellReviewer.Core.*): pure code.
2. Effects (HaskellReviewer.Effects.*): handles for IO (clock, files, ...).
3. Session: state shared while the server runs (not there yet).
4. Server: HTTP (not there yet).
5. App (HaskellReviewer.App.*): CLI parsing and wiring. Imports all
   layers; none imports it.

The executable is just `main = Run.main`. See SPEC §11.2.
-}

{- Note [Exit codes]

SPEC §2.1:

- 0: normal exit, --help, --version.
- 1: internal error, i.e. a bug. Any exception we didn't expect ends up
  here. We print "haskell-reviewer: internal error: " and the exception,
  instead of GHC's default output.
- 2: bad usage or bad input (unknown flag, bad ROOT, bad trace file). These
  are errors as data, never exceptions.

The catch-all in 'run' lets async exceptions (Ctrl-C, killThread) and
ExitCode through untouched: they are not bugs.
-}

-- | Everything 'run' needs from the outside world. Tests plug in fakes.
data Deps = Deps
  { -- | Write one line on stderr.
    console :: Text -> IO (),
    clock :: Clock,
    checkRoot :: FilePath -> IO (Either RootError Root),
    -- | Open a trace file. The logger is for warnings while writing.
    openTracer :: Logger -> FilePath -> IO (Either TraceFileError Tracer)
  }

-- | The real world.
realDeps :: Deps
realDeps =
  Deps
    { console = Text.IO.hPutStrLn stderr,
      clock = systemClock,
      checkRoot = FileSystem.checkRoot,
      openTracer = openFileTracer
    }

-- | Bad input found at start-up. All exit with code 2.
data StartupError
  = BadRoot RootError
  | BadTraceFile TraceFileError
  deriving (Eq, Show)

-- | Message for the user, with the program name in front.
renderStartupError :: StartupError -> Text
renderStartupError err =
  "haskell-reviewer: " <> case err of
    BadRoot e -> renderRootError e
    BadTraceFile e -> renderTraceFileError e

-- | The real program.
main :: IO ()
main = do
  -- Always UTF-8: with LC_ALL=C, printing a non-ASCII path would throw
  -- (exit 1 instead of 2). Bytes that aren't valid in the locale show up
  -- as U+FFFD, since Text can't hold them.
  hSetEncoding stderr utf8
  getArgs >>= run realDeps >>= exitWith

-- | Run with the given arguments and return the exit code.
-- See Note [Exit codes].
run :: Deps -> [String] -> IO ExitCode
run deps args = startUp deps args `catch` internalError deps

-- * Internal

internalError :: Deps -> SomeException -> IO ExitCode
internalError deps e
  | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
  | Just (_ :: ExitCode) <- fromException e = throwIO e
  | otherwise = do
      deps.console
        ("haskell-reviewer: internal error: " <> Text.pack (displayException e))
      pure (ExitFailure 1)

startUp :: Deps -> [String] -> IO ExitCode
startUp deps args = do
  t0 <- deps.clock.monotonic
  w0 <- deps.clock.now
  case parseArgs args of
    CliExit code msg -> do
      deps.console msg
      pure code
    CliCommand ShowVersion -> do
      deps.console versionText
      pure ExitSuccess
    CliCommand (Run opts) -> do
      let logger = newLogger opts.logLevel deps.console
      rootResult <- deps.checkRoot opts.root
      case rootResult of
        Left e -> startupFailed deps (BadRoot e)
        Right root -> do
          tracerResult <- openTracerFor deps logger opts
          case tracerResult of
            Left e -> startupFailed deps (BadTraceFile e)
            Right tracer -> do
              logVerbose logger ("root: " <> Text.pack root.path)
              logDebug logger ("options: " <> Text.pack (show opts))
              t1 <- deps.clock.monotonic
              let startup =
                    Span
                      { name = "startup",
                        start = w0,
                        duration = elapsed t0 t1
                      }
              tracer.record startup `finally` tracer.close
              pure ExitSuccess

openTracerFor :: Deps -> Logger -> Options -> IO (Either TraceFileError Tracer)
openTracerFor deps logger opts = case opts.traceFile of
  Nothing -> pure (Right noTracer)
  Just file -> deps.openTracer logger file

startupFailed :: Deps -> StartupError -> IO ExitCode
startupFailed deps err = do
  deps.console (renderStartupError err)
  pure (ExitFailure 2)
