module HaskellReviewer.Session.ReloadTest (tests) where

import ApiSupport (ApiResult (..), RequestBody (..), callAppBody)
import Control.Concurrent.Async (Async, async, poll, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Data.Maybe (isNothing)
import HaskellReviewer.Server.Reload (reloadApp)
import HaskellReviewer.Session.Reload
  ( ReloadOutcome (..),
    answerReloads,
    closeReloads,
    newReloadQueue,
    requestReload,
    takeReloads,
  )
import HaskellReviewer.Session.Snapshot (Generation (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Reload queue"
    [ testCase "requests during a build share the next one" $ do
        q <- newReloadQueue
        queued <- newEmptyMVar
        let ask = async (requestReload q (putMVar queued ()))
        first <- ask
        takeMVar queued
        batch1 <- atomically (takeReloads q)
        -- The build runs: these two wait for the next one.
        second <- ask
        third <- ask
        takeMVar queued
        takeMVar queued
        answerReloads q batch1 (Reloaded (Generation 2))
        outcome first >>= (@?= Just (Reloaded (Generation 2)))
        stillWaiting second
        stillWaiting third
        batch2 <- atomically (takeReloads q)
        answerReloads q batch2 (Reloaded (Generation 3))
        outcome second >>= (@?= Just (Reloaded (Generation 3)))
        outcome third >>= (@?= Just (Reloaded (Generation 3))),
      testCase "a failed build answers its batch only" $ do
        q <- newReloadQueue
        queued <- newEmptyMVar
        first <- async (requestReload q (putMVar queued ()))
        takeMVar queued
        batch <- atomically (takeReloads q)
        answerReloads q batch ReloadFailed
        outcome first >>= (@?= Just ReloadFailed),
      testCase "two batches taken: both kept, both answered" $ do
        q <- newReloadQueue
        queued <- newEmptyMVar
        let ask = async (requestReload q (putMVar queued ()))
        first <- ask
        takeMVar queued
        batch1 <- atomically (takeReloads q)
        second <- ask
        takeMVar queued
        _ <- atomically (takeReloads q)
        -- The first batch is answered; the second one is still kept.
        answerReloads q batch1 (Reloaded (Generation 2))
        outcome first >>= (@?= Just (Reloaded (Generation 2)))
        stillWaiting second
        closeReloads q ReloadStopping
        outcome second >>= (@?= Just ReloadStopping),
      testCase "close: taken and waiting get its answer, later ones at once" $ do
        q <- newReloadQueue
        queued <- newEmptyMVar
        let ask = async (requestReload q (putMVar queued ()))
        taken <- ask
        takeMVar queued
        batch <- atomically (takeReloads q)
        waiting <- ask
        takeMVar queued
        closeReloads q (ReloadUnavailable "boom")
        outcome taken >>= (@?= Just (ReloadUnavailable "boom"))
        outcome waiting >>= (@?= Just (ReloadUnavailable "boom"))
        -- Only the first close counts.
        closeReloads q ReloadStopping
        late <- timeout 5_000_000 (requestReload q (pure ()))
        late @?= Just (ReloadUnavailable "boom")
        -- An answer after the close does nothing.
        answerReloads q batch (Reloaded (Generation 2)),
      testCase "route: each outcome's answer" $ do
        let post o = do
              r <-
                callAppBody
                  (reloadApp (Just (pure o)))
                  "POST"
                  [("Content-Type", "application/json")]
                  (Sized "{}")
                  ""
                  "127.0.0.1:8080"
                  []
              pure (r.status, r.body)
        post (Reloaded (Generation 3)) >>= (@?= (200, "{\"generation\":3}"))
        post ReloadFailed >>= (@?= (500, "cannot list files"))
        post (ReloadCrashed "x") >>= (@?= (500, "cannot reload"))
        post ReloadStopping >>= (@?= (503, "the server is stopping"))
        post (ReloadUnavailable "boom")
          >>= (@?= (503, "reload is not available: boom"))
    ]

outcome :: Async ReloadOutcome -> IO (Maybe ReloadOutcome)
outcome a = timeout 5_000_000 (wait a)

-- | Not answered. Its answer can only come from a later
-- 'answerReloads', so no wait is needed to check it.
stillWaiting :: Async ReloadOutcome -> IO ()
stillWaiting a = poll a >>= assertBool "answered too early" . isNothing
