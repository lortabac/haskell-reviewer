-- | N long-lived @cat-file --batch-command@, each behind an MVar; free
-- ones wait in a TQueue.
module Pool
  ( Pool (..),
    CatFile (..),
    Blob (..),
    startPool,
    stopPool,
    withCatFile,
    readObject,
    vmHwmKb,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, finally, mask, try)
import Control.Monad (forM, forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Proc (GitCfg, gitProc)
import System.IO
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (CPid)
import System.Process

data CatFile = CatFile
  { inH :: Handle,
    outH :: Handle,
    ph :: ProcessHandle,
    pid :: Maybe CPid
  }

data Pool = Pool
  { free :: TQueue (MVar CatFile),
    members :: [MVar CatFile]
  }

data Blob = Blob {kind :: ByteString, bytes :: ByteString}

startPool :: GitCfg -> Int -> IO Pool
startPool g n = do
  cats <- forM [1 .. n] $ \_ -> do
    let cp =
          (gitProc g ["cat-file", "--batch-command"])
            { std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = Inherit
            }
    (Just i, Just o, _, p) <- createProcess cp
    hSetBinaryMode i True
    hSetBinaryMode o True
    hSetBuffering i (BlockBuffering Nothing)
    pid <- getPid p
    newMVar CatFile {inH = i, outH = o, ph = p, pid = pid}
  q <- newTQueueIO
  atomically (mapM_ (writeTQueue q) cats)
  pure Pool {free = q, members = cats}

stopPool :: Pool -> IO ()
stopPool pool = forM_ pool.members $ \mv -> do
  c <- readMVar mv
  void (try @IOException (hClose c.inH))
  forM_ c.pid $ \p -> void (try @IOException (signalProcess sigKILL p))
  void (try @IOException (hClose c.outH))
  void (waitForProcess c.ph)

-- | Take a free handle, use it, give it back. The MVar is the lock;
-- the TQueue only says which one is free.
withCatFile :: Pool -> (CatFile -> IO a) -> IO a
withCatFile pool act = mask $ \restore -> do
  mv <- atomically (readTQueue pool.free)
  restore (withMVar mv act) `finally` atomically (writeTQueue pool.free mv)

-- | @contents <oid>@ (or @info@ if the flag is False), default format.
-- Nothing for a missing object.
readObject :: Bool -> CatFile -> ByteString -> IO (Maybe Blob)
readObject wantContents c oid = do
  BS.hPut c.inH ((if wantContents then "contents " else "info ") <> oid <> "\n")
  hFlush c.inH
  header <- BC.hGetLine c.outH
  case BC.words header of
    [_, "missing"] -> pure Nothing
    [_, ty, size]
      | Just (n, "") <- BC.readInt size ->
          if wantContents
            then do
              body <- BS.hGet c.outH n
              nl <- BS.hGet c.outH 1
              if BS.length body /= n || nl /= "\n"
                then ioError (userError "cat-file: short read")
                else pure (Just Blob {kind = ty, bytes = body})
            else pure (Just Blob {kind = ty, bytes = BS.empty})
    _ -> ioError (userError ("cat-file: bad header " <> show header))

-- | VmHWM of a process, in kB.
vmHwmKb :: CPid -> IO (Maybe Int)
vmHwmKb p = do
  r <- try @IOException (BS.readFile ("/proc/" <> show p <> "/status"))
  pure $ case r of
    Left _ -> Nothing
    Right s ->
      case [l | l <- BC.lines s, "VmHWM:" `BS.isPrefixOf` l] of
        (l : _) | (_ : v : _) <- BC.words l -> fst <$> BC.readInt v
        _ -> Nothing
