module HaskellReviewer.Session.SnapshotTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Data.Maybe (isNothing)
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Session.Snapshot (awaitSnapshot, fillSnapshot, newSnapshotSlot)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Snapshot"
    [ testCase "await waits for the fill" $ do
        slot <- newSnapshotSlot
        withAsync (awaitSnapshot slot) $ \waiting -> do
          threadDelay 100_000
          early <- poll waiting
          assertBool "returned before the fill" (isNothing early)
          _ <- fillSnapshot slot (Left (ListCrashed "a"))
          r <- timeout 5_000_000 (wait waiting)
          fmap (either Just (const Nothing)) r @?= Just (Just (ListCrashed "a")),
      testCase "first fill wins" $ do
        slot <- newSnapshotSlot
        fillSnapshot slot (Left (ListCrashed "first")) >>= (@?= True)
        fillSnapshot slot (Left (ListCrashed "second")) >>= (@?= False)
        r <- awaitSnapshot slot
        either Just (const Nothing) r @?= Just (ListCrashed "first")
    ]
