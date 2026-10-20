-- | The few @*at@ calls we need that the unix package doesn't have.
module HaskellReviewer.Effects.PosixAt
  ( -- * Types
    Kind (..),
    Stat (..),

    -- * Calls
    statAt,
    readLinkAt,
    listDirFd,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.Error
  ( eOK,
    errnoToIOError,
    getErrno,
    throwErrno,
    throwErrnoPathIfMinus1_,
  )
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt (..), CLLong (..), CLong (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.IO (closeFd)
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

-- | The names in a dir, @.@ and @..@ included. Takes the fd: it is
-- closed at the end, also on error.
listDirFd :: Fd -> IO [ByteString]
listDirFd (Fd fd) = bracket open c_closedir (go [])
  where
    open = do
      dir <- c_fdopendir fd
      when (dir == nullPtr) $ do
        -- Not taken on failure: close it ourselves.
        errno <- getErrno
        closeFd (Fd fd)
        ioError (errnoToIOError "listDirFd" errno Nothing Nothing)
      pure dir
    go acc dir = do
      name <- c_readdir dir
      if name /= nullPtr
        then do
          n <- BS.packCString name
          go (n : acc) dir
        else do
          errno <- getErrno
          if errno == eOK then pure acc else throwErrno "listDirFd"

foreign import ccall safe "hr_stat_at"
  c_stat_at :: CInt -> CString -> Ptr CInt -> Ptr CLLong -> IO CInt

foreign import ccall safe "hr_readlink_at"
  c_readlink_at :: CInt -> CString -> Ptr CChar -> CSize -> IO CLong

foreign import ccall safe "hr_fdopendir"
  c_fdopendir :: CInt -> IO (Ptr ())

foreign import ccall safe "hr_readdir"
  c_readdir :: Ptr () -> IO CString

foreign import ccall safe "hr_closedir"
  c_closedir :: Ptr () -> IO CInt
