module HaskellReviewer.Session.SnapshotTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.STM (atomically, orElse)
import Data.Maybe (isNothing)
import HaskellReviewer.Core.FileList (ListSource (..), mkFileList)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Server.Api (newSnapshot)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot
  ( Generation (..),
    IndexingState (..),
    awaitSnapshot,
    fillSnapshot,
    indexingState,
    newSnapshotSlot,
  )
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
        either Just (const Nothing) r @?= Just (ListCrashed "first"),
      testCase "indexing state: waits for the fill; none if the listing failed" $ do
        let now slot = atomically ((Just <$> indexingState slot) `orElse` pure Nothing)
        failed <- newSnapshotSlot
        now failed >>= (@?= Nothing)
        _ <- fillSnapshot failed (Left (ListCrashed "a"))
        now failed >>= (@?= Just Nothing)
        listed <- newSnapshotSlot
        snap <-
          newSnapshot
            (CanonicalRoot "/x" "/x")
            (mkFileList FromDisk 0 [])
            Project
              { source = ImplicitRootCabalFiles,
                withCompiler = Nothing,
                ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
                packages = [],
                brokenPackages = [],
                warnings = []
              }
        _ <- fillSnapshot listed (Right snap)
        now listed
          >>= ( @?=
                  Just
                    ( Just
                        IndexingState
                          { generation = Generation 1,
                            progress = Progress {total = 0, ready = 0, failed = 0}
                          }
                    )
              )
    ]
