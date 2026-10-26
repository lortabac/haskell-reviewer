-- | Experiments for Q-MACOS-SIGINT: on macOS a stop signal sent early is
-- sometimes delivered but never reaches the Haskell handler.
--
-- Each experiment runs many attempts, keeps going after a loss, and
-- fails at the end with a summary a CI log can answer from. They run on
-- every platform: Linux is the control (0 losses expected).
--
-- 1. Late signals, real server: start-up done, then idle for 1 s.
-- 2. A tiny program (this test binary, re-run with 'probeEnv' set):
--    plain installHandler, none of our code. Idle, or busy for 50 ms.
module SignalTest
  ( tests,
    probeEnv,
    probeMain,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (IOException, bracket, evaluate, try)
import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as Map
import ExeTest (findExe)
import GHC.Clock (getMonotonicTime)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..), exitSuccess)
import System.IO (Handle, hGetLine, hIsEOF, hPutStrLn, stderr)
import System.Posix.Signals
  ( Handler (Catch),
    Signal,
    installHandler,
    sigINT,
    sigKILL,
    sigTERM,
    signalProcess,
  )
import System.Process
  ( CreateProcess (..),
    StdStream (CreatePipe),
    createProcess,
    getPid,
    proc,
    waitForProcess,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCaseInfo)
import TestSupport (withCanonicalTemp)

tests :: TestTree
tests =
  testGroup
    "signal experiments (Q-MACOS-SIGINT)"
    [ testCaseInfo "late signals, real server: none lost" $
        withCanonicalTemp $ \dir -> do
          exe <- findExe
          let cp = proc exe ["--no-open", "--verbose", dir]
          experiment "late signals, real server" 20 5 . attempt $
            Setup
              { command = cp,
                isReady = ("parser warm-up:" `isPrefixOf`),
                delay = 1000000,
                isGot = ("stop: got" `isPrefixOf`)
              },
      testCaseInfo "tiny program, idle, early signal: none lost" (probe "idle"),
      testCaseInfo "tiny program, busy, early signal: none lost" (probe "busy")
    ]
  where
    probe mode = do
      self <- getExecutablePath
      env <- getEnvironment
      let cp = (proc self []) {env = Just ((probeEnv, mode) : env)}
      experiment ("tiny program, " <> mode) 40 4 . attempt $
        Setup
          { command = cp,
            isReady = (== "ready"),
            delay = 0,
            isGot = ("got " `isPrefixOf`)
          }

-- * The tiny program

-- | Set to @idle@ or @busy@, the test binary runs 'probeMain' instead of
-- the tests.
probeEnv :: String
probeEnv = "HASKELL_REVIEWER_SIGNAL_PROBE"

-- | Catch SIGINT and SIGTERM like our first-signal path (tryPutMVar),
-- write "ready", wait, write "got <signal>", exit 0. With @busy@, a
-- thread does 50 ms of allocation-heavy work after "ready".
probeMain :: String -> IO ()
probeMain mode = do
  got <- newEmptyMVar
  let install name sig =
        void (installHandler sig (Catch (void (tryPutMVar got name))) Nothing)
  install "SIGINT" sigINT
  install "SIGTERM" sigTERM
  when (mode == "busy") (void (forkIO busyWork))
  hPutStrLn stderr "ready"
  name <- readMVar got
  hPutStrLn stderr ("got " <> name)
  exitSuccess

-- | Build small maps for 50 ms.
busyWork :: IO ()
busyWork = getMonotonicTime >>= go 0
  where
    go :: Int -> Double -> IO ()
    go k t0 = do
      _ <- evaluate (Map.size (Map.fromList [(i, show i) | i <- [k .. k + 2000]]))
      t <- getMonotonicTime
      when (t - t0 < 0.05) (go (k + 1) t0)

-- * Running attempts

data Outcome
  = Stopped
  | -- | No exit and no handler line within 3 s.
    Lost Sent
  | -- | The handler line came, but no exit within 3 s.
    Stuck Sent
  | -- | Anything else: no ready line, bad exit code.
    Broken Int String

-- | When the signal went out.
data Sent = Sent
  { attemptNo :: Int,
    signalName :: String,
    -- | Milliseconds since the spawn.
    sinceStart :: Double,
    -- | Milliseconds since the ready line.
    sinceReady :: Double
  }

-- | Run n attempts, k at a time. Fail at the end if any went wrong;
-- the summary is the test's output either way.
experiment :: String -> Int -> Int -> (Int -> IO Outcome) -> IO String
experiment name n k run = do
  outcomes <- concat <$> mapM (mapConcurrently run) (chunks [1 .. n])
  let lost = [s | Lost s <- outcomes]
      stuck = [s | Stuck s <- outcomes]
      broken = [(i, why) | Broken i why <- outcomes]
      summary =
        unlines $
          [ name
              <> ": lost "
              <> count lost
              <> ", stuck "
              <> count stuck
              <> ", broken "
              <> count broken
          ]
            <> map (("  lost: " <>) . showSent) lost
            <> map (("  stuck: " <>) . showSent) stuck
            <> map (\(i, why) -> "  broken: #" <> show i <> ": " <> why) broken
  if null lost && null stuck && null broken
    then pure summary
    else assertFailure summary
  where
    count :: [a] -> String
    count xs = show (length xs) <> "/" <> show n
    chunks xs = case splitAt k xs of
      ([], _) -> []
      (a, rest) -> a : chunks rest

showSent :: Sent -> String
showSent s =
  intercalate
    ", "
    [ "#" <> show s.attemptNo,
      s.signalName,
      ms s.sinceStart <> " ms after start",
      ms s.sinceReady <> " ms after ready"
    ]
  where
    ms d = show (round d :: Int)

-- | What to run and what to look for.
data Setup = Setup
  { command :: CreateProcess,
    -- | The line after which we send the signal.
    isReady :: String -> Bool,
    -- | How long to wait after it, in µs.
    delay :: Int,
    -- | The line the handler writes.
    isGot :: String -> Bool
  }

-- | One attempt: spawn, wait for the ready line and the delay, send
-- SIGINT (even attempts) or SIGTERM (odd), wait 3 s for exit 0. A
-- process that doesn't exit is killed.
attempt :: Setup -> Int -> IO Outcome
attempt setup n = do
  start <- getMonotonicTime
  let cp =
        setup.command
          { std_out = CreatePipe,
            std_err = CreatePipe,
            close_fds = True
          }
  bracket (createProcess cp) kill $ \case
    (_, _, Just err, ph) -> do
      seen <- newIORef []
      chan <- newChan
      withAsync (pump err seen chan) $ \_ -> do
        ready <- timeout 10000000 (waitLine chan setup.isReady)
        case ready of
          Just True -> do
            readyAt <- getMonotonicTime
            threadDelay setup.delay
            let (name, sig) = signalFor n
            sentAt <- getMonotonicTime
            getPid ph >>= mapM_ (signalProcess sig)
            code <- timeout 3000000 (waitForProcess ph)
            let sent =
                  Sent
                    { attemptNo = n,
                      signalName = name,
                      sinceStart = (sentAt - start) * 1000,
                      sinceReady = (sentAt - readyAt) * 1000
                    }
            case code of
              Just ExitSuccess -> pure Stopped
              Just c -> broken ("exit " <> show c) seen
              Nothing -> do
                got <- any setup.isGot <$> readIORef seen
                pure (if got then Stuck sent else Lost sent)
          _ -> broken "no ready line" seen
    _ -> pure (Broken n "no pipes")
  where
    broken why seen = do
      ls <- readIORef seen
      pure (Broken n (why <> "; stderr: " <> show ls))
    kill (_, _, _, ph) = do
      getPid ph >>= mapM_ (\p -> try @IOException (signalProcess sigKILL p))
      void (waitForProcess ph)

signalFor :: Int -> (String, Signal)
signalFor n
  | even n = ("SIGINT", sigINT)
  | otherwise = ("SIGTERM", sigTERM)

-- | Read stderr lines into the list and the channel; Nothing at the end.
pump :: Handle -> IORef [String] -> Chan (Maybe String) -> IO ()
pump h seen chan = do
  eof <- either (const True) id <$> try @IOException (hIsEOF h)
  if eof
    then writeChan chan Nothing
    else do
      l <- hGetLine h
      modifyIORef' seen (<> [l])
      writeChan chan (Just l)
      pump h seen chan

-- | Wait for a matching line. False at the end of stderr.
waitLine :: Chan (Maybe String) -> (String -> Bool) -> IO Bool
waitLine chan match =
  readChan chan >>= \case
    Nothing -> pure False
    Just l
      | match l -> pure True
      | otherwise -> waitLine chan match
