-- | Ctrl-C and SIGTERM: stop the program with exit code 0 (SPEC §2.1).
module HaskellReviewer.Effects.Shutdown
  ( -- * Types
    Shutdown (..),

    -- * Signals
    installShutdownHandlers,
  )
where

import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import HaskellReviewer.Effects.Log (LogLevel (..))
import System.Exit (ExitCode (..))
import System.Posix.IO (fdWrite, stdError)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals
  ( Handler (Catch),
    Signal,
    addSignal,
    emptySignalSet,
    installHandler,
    sigHUP,
    sigINT,
    sigTERM,
    unblockSignals,
  )

-- | Did a stop signal come?
data Shutdown = Shutdown
  { -- | Block until a signal comes (returns at once if one did).
    wait :: IO (),
    -- | Has one come already? Doesn't block.
    stopped :: IO Bool
  }

{- Note [Blocked signals]

The signal mask is inherited from the parent. If we start with SIGINT
or SIGTERM blocked, the signal stays pending, our handler never runs
and Ctrl-C does nothing.

So after installing the handlers we unblock SIGINT, SIGTERM and SIGHUP.
It's a cheap guard against a parent that left them blocked, not a known
fix for anything we saw. The main thread is bound, so on Linux this
unblocks them in the main OS thread: enough for the kernel to deliver
there.

How a mask could get stuck: `process` blocks the signals that have a
Haskell handler around a fork (close_fds = True takes that path) and
the RTS puts back a mask it saved in one global. Spawns don't race on
it: `process` runs them one at a time (runInteractiveProcess_lock). But
on macOS sigprocmask sets the mask of the whole process, so a handler
install racing a spawn can leave signals blocked for good
(haskell/process#384). We install our handlers before any spawn.

We don't unblock after each spawn: that runs outside the spawn lock and
could undo the blocking another thread's spawn needs.
-}

-- | Catch SIGINT and SIGTERM. A second signal exits at once with code 0,
-- so a stuck shutdown can't trap the user.
--
-- This replaces GHC's own SIGINT handler, so Ctrl-C no longer throws
-- 'Control.Exception.UserInterrupt'. SIGHUP keeps its default (the
-- process dies). See Note [Shutdown] in "HaskellReviewer.App.Run".
--
-- Also unblocks them: see Note [Blocked signals].
--
-- At verbose level or more, the handler writes a line on stderr as
-- soon as it runs. See 'rawNote'.
installShutdownHandlers :: LogLevel -> IO Shutdown
installShutdownHandlers level = do
  stop <- newEmptyMVar
  let note = when (level >= LevelVerbose) . rawNote
      install :: String -> Signal -> IO ()
      install name sig = void $ installHandler sig (Catch (onSignal name)) Nothing
      onSignal name = do
        note ("stop: got " <> name)
        first <- tryPutMVar stop ()
        unless first $ do
          note "stop: second signal, exiting now"
          exitImmediately ExitSuccess
  install "SIGINT" sigINT
  install "SIGTERM" sigTERM
  -- After installHandler: it puts back the mask it found.
  unblockStopSignals
  pure Shutdown {wait = readMVar stop, stopped = not <$> isEmptyMVar stop}

-- | Write a line straight to fd 2: no Handle, no lock, no logger. So it
-- shows up even if one of those is stuck. Errors are dropped.
rawNote :: String -> IO ()
rawNote msg = void (try @IOException (fdWrite stdError (msg <> "\n")))

-- | Unblock SIGINT, SIGTERM and SIGHUP, in the calling OS thread (the
-- whole process on macOS). See Note [Blocked signals].
unblockStopSignals :: IO ()
unblockStopSignals =
  unblockSignals (foldr addSignal emptySignalSet [sigINT, sigTERM, sigHUP])
