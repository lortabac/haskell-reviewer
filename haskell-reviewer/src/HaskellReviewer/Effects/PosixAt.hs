-- | The few @*at@ calls we need that the unix package doesn't have.
module HaskellReviewer.Effects.PosixAt
  ( -- * Types
    Kind (..),
    Stat (..),

    -- * Calls
    statAt,
    lstatAt,
    readLinkAt,
    listDirFd,
    withFileAt,
  )
where

import Control.Exception (bracket, finally, mask, onException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Error
  ( eOK,
    getErrno,
    throwErrno,
    throwErrnoPathIfMinus1_,
  )
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt (..), CLLong (..), CLong (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import HaskellReviewer.Core.WorkTree (FullStat (..), StatTime (..))
import System.IO (Handle, hClose)
import System.Posix.Files.ByteString (FileStatus, getFdStatus)
import System.Posix.IO.ByteString
  ( OpenFileFlags,
    OpenMode (ReadOnly),
    closeFd,
    fdToHandle,
    openFdAt,
  )
import System.Posix.Types (Fd (..))

{- Note [Posix at]

To read a file under ROOT without following a symlink anywhere, we
walk down with fds: open each dir with openat(O_NOFOLLOW|O_DIRECTORY)
in its parent, then look at the last name in the last dir. Then no one
can swap a dir for a symlink between our check and our use.

The unix package has openat, but not fstatat, readlinkat or fdopendir.
So we call them through small C wrappers (cbits/posix_at.c). Wrappers,
not direct imports: struct stat and DIR differ between systems, and
the C compiler knows them from the headers.

Errors come back as IOException with errno set, like the unix package.
-}

{- Note [Fd ownership]

Every fd is closed once. A second close is a real bug, not a no-op:
by then another thread may have the same number for its own file, and
we close that one. A Handle is safer: hClose twice is fine.

Async exceptions (a reload's StopWorkers, a cancel, a timeout) can
come between any two steps. So:

- Open an fd and hand it to its owner (a bracket's release, a Handle,
  the walker's list) in one masked step. 'withFileAt' does it for a
  file: open, fstat, fdToHandle are masked, then the action runs
  unmasked (restore), with hClose in a finally.
- 'listDirFd' borrows the fd: it lists a dup, made and closed inside
  its own bracket. The caller still closes its fd.
- A list of fds is closed in full: each close under try, then the
  first error is thrown. A raw fd has no finalizer, so one we skip is
  lost for good.
- Take an fd out of a list of open fds and close it in one masked
  step (the walker in "HaskellReviewer.Effects.WorkTree").

Only short syscalls run masked (openat, fstat, dup, close). They are
FFI calls, which an async exception can't cut short anyway. The reads
run under restore. But GHC runs each Handle op masked, so one big hGet
(up to 64 MiB when hashing) can still delay a cancel by tens of ms.
-}

-- | What a name is, without following it.
data Kind = KindFile | KindDir | KindSymlink | KindOther
  deriving (Eq, Show)

-- | What 'statAt' tells.
data Stat = Stat
  { kind :: Kind,
    -- | In bytes.
    size :: Int
  }
  deriving (Eq, Show)

-- | fstatat with AT_SYMLINK_NOFOLLOW: the name in the dir.
statAt :: Fd -> ByteString -> IO Stat
statAt (Fd dir) name =
  BS.useAsCString name $ \cname ->
    alloca $ \kindPtr -> alloca $ \sizePtr -> do
      throwErrnoPathIfMinus1_ "statAt" (show name) $
        c_stat_at dir cname kindPtr sizePtr
      -- The codes are set by hr_stat_at in cbits/posix_at.c.
      k <- peek kindPtr
      CLLong n <- peek sizePtr
      pure
        Stat
          { kind = case k of
              1 -> KindFile
              2 -> KindDir
              3 -> KindSymlink
              _ -> KindOther,
            size = fromIntegral n
          }

-- | fstatat with AT_SYMLINK_NOFOLLOW, with everything the stat check
-- compares. See Note [Stat check] in "HaskellReviewer.Core.WorkTree".
lstatAt :: Fd -> ByteString -> IO FullStat
lstatAt (Fd dir) name =
  BS.useAsCString name $ \cname ->
    allocaArray 10 $ \out -> do
      throwErrnoPathIfMinus1_ "lstatAt" (show name) $ c_lstat_full dir cname out
      -- The order is set by hr_lstat_full in cbits/posix_at.c.
      vals <- map (\(CLLong n) -> n) <$> peekArray 10 out
      case vals of
        [m, size, ms, mns, cs, cns, ino, dev, uid, gid] ->
          pure
            FullStat
              { mode = fromIntegral m,
                size = size,
                mtime = time ms mns,
                ctime = time cs cns,
                ino = fromIntegral ino,
                dev = fromIntegral dev,
                uid = fromIntegral uid,
                gid = fromIntegral gid
              }
        _ -> ioError (userError "lstatAt: short result")
  where
    -- Cut to 32 bits, as git keeps them in the index.
    time s ns = StatTime {sec = fromIntegral s, nsec = fromIntegral ns}

-- | readlinkat: the target of the symlink, as bytes.
readLinkAt :: Fd -> ByteString -> IO ByteString
readLinkAt (Fd dir) name =
  BS.useAsCString name $ \cname -> go cname 256
  where
    go cname len =
      allocaBytes len $ \buf -> do
        n <- c_readlink_at dir cname buf (fromIntegral len)
        when (n < 0) (throwErrno "readLinkAt")
        if fromIntegral n >= len
          then go cname (len * 4)
          else BS.packCStringLen (buf, fromIntegral n)

-- | The names in a dir, @.@ and @..@ included. Lists a dup of the fd
-- (close-on-exec, from the start): the caller still owns its fd and
-- closes it. See Note [Fd ownership].
listDirFd :: Fd -> IO [ByteString]
listDirFd (Fd fd) = bracket open c_closedir (go [])
  where
    open = do
      d <- c_dup_opendir fd
      when (d == nullPtr) (throwErrno "listDirFd")
      pure d
    go acc d = do
      name <- c_readdir d
      if name /= nullPtr
        then do
          n <- BS.packCString name
          go (n : acc) d
        else do
          errno <- getErrno
          if errno == eOK then pure acc else throwErrno "listDirFd"

-- | Open a name in a dir, read only. If its fstat passes the check,
-- run the action on a Handle for it, then close it. 'Nothing' if the
-- check fails. See Note [Fd ownership].
withFileAt ::
  Fd ->
  ByteString ->
  OpenFileFlags ->
  (FileStatus -> Bool) ->
  (Handle -> FileStatus -> IO a) ->
  IO (Maybe a)
withFileAt dir name flags check act = mask $ \restore -> do
  fd <- openFdAt (Just dir) name ReadOnly flags
  st <- getFdStatus fd `onException` closeFd fd
  if not (check st)
    then Nothing <$ closeFd fd
    else do
      -- Masked, so nothing can land after the Handle is made and
      -- before onException is gone: the fd is closed by one of them.
      h <- fdToHandle fd `onException` closeFd fd
      Just <$> restore (act h st) `finally` hClose h

foreign import ccall safe "hr_stat_at"
  c_stat_at :: CInt -> CString -> Ptr CInt -> Ptr CLLong -> IO CInt

foreign import ccall safe "hr_lstat_full"
  c_lstat_full :: CInt -> CString -> Ptr CLLong -> IO CInt

foreign import ccall safe "hr_readlink_at"
  c_readlink_at :: CInt -> CString -> Ptr CChar -> CSize -> IO CLong

foreign import ccall safe "hr_dup_opendir"
  c_dup_opendir :: CInt -> IO (Ptr ())

foreign import ccall safe "hr_readdir"
  c_readdir :: Ptr () -> IO CString

foreign import ccall safe "hr_closedir"
  c_closedir :: Ptr () -> IO CInt
