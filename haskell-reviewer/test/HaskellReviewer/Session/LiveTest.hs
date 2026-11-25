module HaskellReviewer.Session.LiveTest (tests) where

import Control.Concurrent.Async (concurrently, forConcurrently)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forM_, unless)
import Data.Text qualified as Text
import Data.Word (Word64)
import HaskellReviewer.Core.Sides (Pair (..), Side (..))
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Session.Diff
  ( DiffProblem (..),
    awaitDiff,
    fillDiff,
    newDiffSlot,
  )
import HaskellReviewer.Session.Live
  ( Current (..),
    DiffLive (..),
    LiveSlots (..),
    awaitSideSTM,
    newLive,
    readCurrent,
    swapLive,
  )
import HaskellReviewer.Session.Snapshot
  ( Generation (..),
    awaitSnapshot,
    fillSnapshot,
    newSnapshotSlot,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Live"
    [ testCase "readers see one generation per read, never going down" $ do
        live <- newLive =<< generation 1
        done <- newTVarIO False
        let swaps = 1000 :: Word64
            writer = do
              forM_ [2 .. swaps + 1] $ \g -> swapLive live =<< generation g
              atomically (writeTVar done True)
            reader = go 0 (0 :: Int)
              where
                go seen n = do
                  c <- readCurrent live
                  let Generation g = c.generation
                  unless (g >= seen) $
                    assertFailure ("went down: " <> show (seen, g))
                  tags <- tagsOf c
                  tags @?= replicate 3 (Just g)
                  -- One transaction: the slot of the generation read.
                  side <- atomically (awaitSideSTM live HeadSide)
                  case side of
                    Just (Left (ListCrashed t)) ->
                      unless (read (Text.unpack t) >= g) $
                        assertFailure "side read went down"
                    other -> assertFailure (show (either Just (const Nothing) <$> other))
                  finished <- readTVarIO done
                  if finished then pure (n + 1) else go g (n + 1)
        let readers = forConcurrently [1 .. 8 :: Int] (const reader)
        r <- timeout 30_000_000 (concurrently writer readers)
        case r of
          Nothing -> assertFailure "did not end"
          Just (_, counts) -> unless (all (> 0) counts) (assertFailure (show counts))
        c <- readCurrent live
        c.generation @?= Generation (swaps + 1)
    ]

-- | A diff-mode generation whose slots say their generation: head and
-- base as a listing error, the pairing as a pairing error.
generation :: Word64 -> IO Current
generation g = do
  h <- newSnapshotSlot
  b <- newSnapshotSlot
  d <- newDiffSlot
  _ <- fillSnapshot h (Left (ListCrashed tag))
  _ <- fillSnapshot b (Left (ListCrashed tag))
  _ <- fillDiff d (Left (DiffNotPaired tag))
  pure
    Current
      { generation = Generation g,
        slots = DiffSlots DiffLive {pairing = d, sides = Pair {head = h, base = b}}
      }
  where
    tag = Text.pack (show g)

-- | What each slot says, head, base, pairing.
tagsOf :: Current -> IO [Maybe Word64]
tagsOf c = case c.slots of
  DiffSlots live -> do
    h <- awaitSnapshot live.sides.head
    b <- awaitSnapshot live.sides.base
    x <- awaitDiff live.pairing
    pure
      [ case h of
          Left (ListCrashed t) -> Just (read (Text.unpack t))
          _ -> Nothing,
        case b of
          Left (ListCrashed t) -> Just (read (Text.unpack t))
          _ -> Nothing,
        case x of
          Left (DiffNotPaired t) -> Just (read (Text.unpack t))
          _ -> Nothing
      ]
  BrowseSlots _ -> assertFailure "not a diff generation"
