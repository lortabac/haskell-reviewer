-- | Make spawning children cheap and leak-free. See Note [No inherited fds].
module HaskellReviewer.Effects.Spawn
  ( -- * Types
    SpawnSetup (..),
    SoftLimit (..),

    -- * Operations
    prepareSpawning,
    renderSpawnSetup,
    spawnFdCap,
  )
where

import Control.Exception (IOException, displayException, throwIO, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C.Error (Errno (..), eBADF)
import Foreign.C.Types (CInt)
import GHC.IO.Exception (IOException (..))
import System.Directory qualified as Dir
import System.Posix.IO (FdOption (CloseOnExec), setFdOption)
import System.Posix.Resource
  ( Resource (ResourceOpenFiles),
    ResourceLimit (..),
    ResourceLimits (..),
    getResourceLimit,
    setResourceLimit,
  )
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

{- Note [No inherited fds]

A child (git, the browser opener) must get only stdin, stdout and
stderr. Our other fds (the listening socket, client connections, the
trace file, ...) are none of its business.

`process` does this with close_fds = True: in the forked child it
closes every fd from 3 up to sysconf(_SC_OPEN_MAX), the soft
RLIMIT_NOFILE, read once and cached. Some systems set that limit very
high (524288 is common). Then each spawn would make half a million
close() calls, about 100 ms. We spawn git several times at start-up,
so the URL would be slow.

Just dropping close_fds is not safe: GHC's openFile doesn't use
O_CLOEXEC, an RTS pipe doesn't either, `process` makes child pipes
with plain pipe(), and we spawn from several threads at once.

So 'prepareSpawning' runs first thing in main, before any spawn:

1. List /dev/fd (Linux and macOS) and mark every fd >= 3
   close-on-exec. The fd of the dir stream itself is gone by then
   (EBADF): ignored. The listing must show at least one fd >= 3 (its
   own dir stream, at least): a /dev/fd with only 0-2 (macOS without
   fdescfs) proves nothing.
2. Only if 1 worked: lower the soft RLIMIT_NOFILE to 'spawnFdCap'
   (4096), if it's higher. The hard limit stays as it is.

Why this is enough: the kernel never hands out a new fd at or above
the soft limit. So every fd >= 4096 existed before step 2, and step 1
made it close-on-exec. The child's loop closes 3..4095, which covers
everything opened later. At most 4096 close() calls per spawn: cheap.

Children inherit the lowered soft limit. 4096 is plenty for git. If
/dev/fd can't be read we leave the limit alone: slow, but no leak.
-}

-- | What 'prepareSpawning' did. Logged at debug level.
data SpawnSetup
  = -- | /dev/fd could not be read, showed no fd >= 3, or an fd could
    -- not be marked. The limit was left alone.
    CannotMark Text
  | -- | This many fds marked close-on-exec, then the limit handled.
    Marked Int SoftLimit
  deriving (Eq, Show)

-- | What happened to the soft RLIMIT_NOFILE.
data SoftLimit
  = -- | Lowered from this value to 'spawnFdCap'.
    Lowered ResourceLimit
  | -- | Already at or below the cap.
    KeptAt ResourceLimit
  | -- | setrlimit or getrlimit failed.
    CannotLower Text
  deriving (Eq, Show)

-- | The soft fd limit we lower to. See Note [No inherited fds].
spawnFdCap :: Integer
spawnFdCap = 4096

-- | Mark fds close-on-exec and cap the soft fd limit. Call it first in
-- main, before anything spawns. See Note [No inherited fds].
prepareSpawning :: IO SpawnSetup
prepareSpawning = do
  listed <- try @IOException (Dir.listDirectory "/dev/fd")
  case listed of
    Left e -> pure (CannotMark ("cannot list /dev/fd: " <> showIOError e))
    Right names -> do
      let fds = [fd | Just fd <- map (readMaybe @CInt) names, fd >= 3]
      if null fds
        then pure (CannotMark "/dev/fd shows no fd above 2")
        else do
          marked <- try @IOException (sum <$> mapM markFd fds)
          case marked of
            Left e -> pure (CannotMark ("cannot mark an fd: " <> showIOError e))
            Right n -> Marked n <$> capSoftLimit

-- | One line for the debug log.
renderSpawnSetup :: SpawnSetup -> Text
renderSpawnSetup = \case
  CannotMark why -> "spawn: " <> why <> "; fd limit left alone"
  Marked n limit ->
    "spawn: "
      <> Text.pack (show n)
      <> " fd(s) marked close-on-exec; "
      <> case limit of
        Lowered from -> "soft fd limit " <> renderLimit from <> " -> " <> cap
        KeptAt soft -> "soft fd limit " <> renderLimit soft <> ", kept"
        CannotLower why -> "cannot lower the soft fd limit: " <> why
  where
    cap = Text.pack (show spawnFdCap)

-- * Internal

-- | 1 if marked, 0 if the fd is already gone (the dir stream's own).
markFd :: CInt -> IO Int
markFd fd = do
  r <- try @IOException (setFdOption (Fd fd) CloseOnExec True)
  case r of
    Right () -> pure 1
    Left e
      | fmap Errno e.ioe_errno == Just eBADF -> pure 0
      | otherwise -> throwIO e

capSoftLimit :: IO SoftLimit
capSoftLimit = do
  r <- try @IOException $ do
    lim <- getResourceLimit ResourceOpenFiles
    if tooHigh lim.softLimit
      then do
        setResourceLimit
          ResourceOpenFiles
          ResourceLimits
            { softLimit = ResourceLimit spawnFdCap,
              hardLimit = lim.hardLimit
            }
        pure (Lowered lim.softLimit)
      else pure (KeptAt lim.softLimit)
  pure (either (CannotLower . showIOError) id r)
  where
    tooHigh = \case
      ResourceLimitInfinity -> True
      ResourceLimit n -> n > spawnFdCap
      ResourceLimitUnknown -> False

renderLimit :: ResourceLimit -> Text
renderLimit = \case
  ResourceLimitInfinity -> "unlimited"
  ResourceLimitUnknown -> "unknown"
  ResourceLimit n -> Text.pack (show n)

showIOError :: IOException -> Text
showIOError = Text.pack . displayException
