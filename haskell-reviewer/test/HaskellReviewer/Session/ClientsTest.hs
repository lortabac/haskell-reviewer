-- | The idle timer, in real time. Upper limits are generous: test
-- machines can be slow.
module HaskellReviewer.Session.ClientsTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll, wait, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), throwIO, try)
import Control.Monad (void)
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Session.Clients
  ( Clients,
    EventId (..),
    newClients,
    nextEventId,
    openCount,
    waitIdle,
    withClient,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Clients"
    [ testCase "no stream: returns after the timeout" $ do
        clients <- newClients
        t0 <- getMonotonicTime
        done <- timeout 5_000_000 (waitIdle clients (ms 200))
        t1 <- getMonotonicTime
        done @?= Just ()
        assertBool ("returned after " <> show (t1 - t0) <> " s") (t1 - t0 >= 0.2),
      testCase "open stream: no return; after close, returns a timeout later" $ do
        clients <- newClients
        release <- hold clients
        waiter <- async (waitIdle clients (ms 200))
        threadDelay 600_000
        poll waiter >>= assertNotDone
        t0 <- getMonotonicTime
        release
        waitFor waiter
        t1 <- getMonotonicTime
        assertBool ("returned " <> show (t1 - t0) <> " s after close") (t1 - t0 >= 0.19),
      testCase "reconnect cancels the timer" $ do
        clients <- newClients
        release1 <- hold clients
        waiter <- async (waitIdle clients (ms 1000))
        release1
        threadDelay 100_000
        release2 <- hold clients
        threadDelay 2_000_000
        poll waiter >>= assertNotDone
        release2
        waitFor waiter,
      testCase "withClient: closed on exception" $ do
        clients <- newClients
        r <- try (withClient clients (throwIO (ErrorCall "boom") :: IO ()))
        r @?= Left (ErrorCall "boom")
        openCount clients >>= (@?= 0),
      testCase "withClient: closed when the thread is killed" $ do
        clients <- newClients
        inside <- newEmptyMVar
        t <- async (withClient clients (putMVar inside () >> threadDelay 10_000_000))
        takeMVar inside
        openCount clients >>= (@?= 1)
        cancel t
        void (waitCatch t)
        openCount clients >>= (@?= 0),
      testCase "event ids count up from 1" $ do
        clients <- newClients
        ids <- mapM (const (nextEventId clients)) [1 :: Int .. 3]
        ids @?= map EventId [1, 2, 3]
    ]

ms :: Int -> Microseconds
ms n = Microseconds (n * 1000)

-- | Open a stream in another thread. Gives back the action that closes
-- it (and waits until it's closed).
hold :: Clients -> IO (IO ())
hold clients = do
  inside <- newEmptyMVar
  release <- newEmptyMVar
  t <- async (withClient clients (putMVar inside () >> takeMVar release))
  takeMVar inside
  pure (putMVar release () >> wait t)

assertNotDone :: Maybe a -> Assertion
assertNotDone = \case
  Nothing -> pure ()
  Just _ -> assertFailure "waitIdle returned while a stream was open"

waitFor :: Async () -> Assertion
waitFor waiter = do
  r <- timeout 5_000_000 (wait waiter)
  r @?= Just ()
