-- | Marking fds close-on-exec and capping the soft fd limit.
--
-- Main already called 'prepareSpawning' once. These tests call it
-- again, and put the fd limit back as they found it. They hold
-- 'withFdLock' while they do.
module HaskellReviewer.Effects.SpawnTest (tests) where

import Control.Exception (bracket, finally)
import Control.Monad (when)
import HaskellReviewer.Effects.Spawn
  ( SoftLimit (..),
    SpawnSetup (..),
    prepareSpawning,
    renderSpawnSetup,
    spawnFdCap,
  )
import System.Posix.IO
  ( FdOption (CloseOnExec),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    dupTo,
    openFd,
    queryFdOption,
  )
import System.Posix.Resource
  ( Resource (ResourceOpenFiles),
    ResourceLimit (..),
    ResourceLimits (..),
    getResourceLimit,
    setResourceLimit,
  )
import System.Posix.Types (Fd (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport (withFdLock)

tests :: TestTree
tests =
  testGroup
    "Spawn"
    [ testCase "fds get close-on-exec, soft limit capped" $
        keepLimits $
          withOpenFd $ \fd -> do
            queryFdOption fd CloseOnExec >>= (@?= False)
            setup <- prepareSpawning
            queryFdOption fd CloseOnExec >>= (@?= True)
            case setup of
              Marked n _ -> assertBool "nothing marked" (n >= 1)
              CannotMark why -> assertFailure (show why)
            assertCapped,
      testCase "an fd above the cap gets close-on-exec too" $
        keepLimits $ do
          lim <- getResourceLimit ResourceOpenFiles
          let high = spawnFdCap + 10
              -- No room above the cap: nothing to test.
              room = case lim.hardLimit of
                ResourceLimit h -> h > high
                ResourceLimitInfinity -> True
                ResourceLimitUnknown -> False
          when room $ do
            setResourceLimit
              ResourceOpenFiles
              lim {softLimit = ResourceLimit (high + 1)}
            withOpenFd $ \fd -> do
              let above = Fd (fromIntegral high)
              bracket (dupTo fd above) closeFd $ \_ -> do
                -- dup2 clears close-on-exec.
                queryFdOption above CloseOnExec >>= (@?= False)
                setup <- prepareSpawning
                queryFdOption above CloseOnExec >>= (@?= True)
                case setup of
                  Marked _ limit ->
                    limit @?= Lowered (ResourceLimit (high + 1))
                  CannotMark why -> assertFailure (show why)
                assertCapped,
      testCase "renderSpawnSetup" $ do
        renderSpawnSetup (Marked 3 (Lowered (ResourceLimit 524288)))
          @?= "spawn: 3 fd(s) marked close-on-exec; soft fd limit 524288 -> 4096"
        renderSpawnSetup (CannotMark "cannot list /dev/fd: x")
          @?= "spawn: cannot list /dev/fd: x; fd limit left alone"
    ]

-- * Helpers

-- | Take the fd lock, put the fd limits back afterwards.
keepLimits :: IO a -> IO a
keepLimits action = withFdLock $ do
  before <- getResourceLimit ResourceOpenFiles
  action `finally` setResourceLimit ResourceOpenFiles before

-- | A plain fd, not close-on-exec.
withOpenFd :: (Fd -> IO a) -> IO a
withOpenFd = bracket (openFd "/dev/null" ReadOnly defaultFileFlags) closeFd

assertCapped :: IO ()
assertCapped = do
  lim <- getResourceLimit ResourceOpenFiles
  case lim.softLimit of
    ResourceLimit n -> assertBool ("soft limit " <> show n) (n <= spawnFdCap)
    other -> assertFailure ("soft limit " <> show other)
