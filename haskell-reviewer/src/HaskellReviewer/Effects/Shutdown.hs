-- | Ctrl-C and SIGTERM: stop the program with exit code 0 (SPEC §2.1).
module HaskellReviewer.Effects.Shutdown
  ( -- * Types
    Shutdown (..),

    -- * Signals
    installShutdownHandlers,
  )
where

import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Monad (unless)
import System.Exit (ExitCode (..))
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)

-- | Did a stop signal come?
data Shutdown = Shutdown
  { -- | Block until a signal comes (returns at once if one did).
    wait :: IO (),
    -- | Has one come already? Doesn't block.
    stopped :: IO Bool
  }

-- | Catch SIGINT and SIGTERM. A second signal exits at once with code 0,
-- so a stuck shutdown can't trap the user.
--
-- This replaces GHC's own SIGINT handler, so Ctrl-C no longer throws
-- 'Control.Exception.UserInterrupt'. SIGHUP keeps its default (the
-- process dies). See Note [Shutdown] in "HaskellReviewer.App.Run".
installShutdownHandlers :: IO Shutdown
installShutdownHandlers = do
  stop <- newEmptyMVar
  let handler = Catch $ do
        first <- tryPutMVar stop ()
        unless first (exitImmediately ExitSuccess)
  _ <- installHandler sigINT handler Nothing
  _ <- installHandler sigTERM handler Nothing
  pure Shutdown {wait = readMVar stop, stopped = not <$> isEmptyMVar stop}
