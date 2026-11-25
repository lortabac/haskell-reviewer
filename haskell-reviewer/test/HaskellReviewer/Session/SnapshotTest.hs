module HaskellReviewer.Session.SnapshotTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.STM (atomically)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import HaskellReviewer.Core.FileList (ListSource (..), mkFileList)
import HaskellReviewer.Core.Modules (buildModuleIndex)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Core.Sides (Pair (..), Side (..), Sides (..))
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Effects.Source (EntrySource (..))
import HaskellReviewer.Server.Api (newSnapshot)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot
  ( Generation (..),
    IndexingState (..),
    Snapshot,
    awaitSnapshot,
    fillSnapshot,
    indexingStates,
    newSessionShare,
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
      testCase "indexing states: none before the fill or if the listing failed" $ do
        let now = atomically . indexingStates
        failed <- newSnapshotSlot
        now (BrowseSides failed) >>= (@?= [])
        _ <- fillSnapshot failed (Left (ListCrashed "a"))
        now (BrowseSides failed) >>= (@?= [])
        listed <- newSnapshotSlot
        snap <- emptySnapshot TreeSide
        _ <- fillSnapshot listed (Right snap)
        now (BrowseSides listed) >>= (@?= [stateOn TreeSide]),
      testCase "indexing states: one per listed side, head first" $ do
        let now = atomically . indexingStates
        headSlot <- newSnapshotSlot
        baseSlot <- newSnapshotSlot
        let sides = DiffSides Pair {head = headSlot, base = baseSlot}
        now sides >>= (@?= [])
        _ <- fillSnapshot baseSlot . Right =<< emptySnapshot BaseSide
        now sides >>= (@?= [stateOn BaseSide])
        _ <- fillSnapshot headSlot . Right =<< emptySnapshot HeadSide
        now sides >>= (@?= [stateOn HeadSide, stateOn BaseSide])
        failed <- newSnapshotSlot
        _ <- fillSnapshot failed (Left (ListCrashed "a"))
        now (DiffSides Pair {head = headSlot, base = failed}) >>= (@?= [stateOn HeadSide])
    ]

-- | No modules, nothing indexed.
stateOn :: Side -> IndexingState
stateOn side =
  IndexingState
    { generation = Generation 1,
      side = side,
      progress = Progress {total = 0, ready = 0, failed = 0}
    }

-- | A snapshot with no files, on this side.
emptySnapshot :: Side -> IO Snapshot
emptySnapshot side = do
  share <- newSessionShare
  let fl = mkFileList FromDisk 0 []
  newSnapshot
    (Generation 1)
    side
    share
    (DiskEntries (CanonicalRoot "/x" "/x"))
    fl
    (buildModuleIndex project fl)
    Map.empty
  where
    project =
      Project
        { source = ImplicitRootCabalFiles,
          withCompiler = Nothing,
          ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
          packages = [],
          brokenPackages = [],
          warnings = []
        }
