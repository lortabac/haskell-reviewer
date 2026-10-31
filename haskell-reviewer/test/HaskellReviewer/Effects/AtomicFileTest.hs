module HaskellReviewer.Effects.AtomicFileTest (tests) where

import Control.Concurrent.Async (mapConcurrently_, wait, withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import HaskellReviewer.Effects.AtomicFile
  ( AtomicWriteError (..),
    discardTemp,
    writeFileAtomic,
    writeFileAtomicWith,
  )
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO (BufferMode (..), IOMode (WriteMode), hSetBuffering, openBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "AtomicFile"
    [ testCase "makes the parent dirs" $
        withTemp $ \dir -> do
          let file = dir </> "a" </> "b" </> "ui.json"
          writeFileAtomic file "x" >>= (@?= Right ())
          BS.readFile file >>= (@?= "x"),
      testCase "new dirs get mode 0700; a dir already there is left alone" $
        withTemp $ \dir -> do
          let old = dir </> "old"
          Dir.createDirectory old
          setFileMode old 0o755
          writeFileAtomic (old </> "a" </> "b" </> "ui.json") "x" >>= (@?= Right ())
          let mode d = (.&. 0o777) . fileMode <$> getFileStatus d
          mode old >>= (@?= 0o755)
          mode (old </> "a") >>= (@?= 0o700)
          mode (old </> "a" </> "b") >>= (@?= 0o700),
      testCase "a write that fails: Left, old file kept, no temp file" $
        withTemp $ \dir -> do
          let file = dir </> "ui.json"
              failing h _ = do
                LBS.hPut h "half"
                ioError (userError "disk full")
          writeFileAtomic file "old" >>= (@?= Right ())
          r <- writeFileAtomicWith failing file "new"
          case r of
            Left e -> e.target @?= file
            Right () -> assertFailure "no error"
          BS.readFile file >>= (@?= "old")
          Dir.listDirectory dir >>= (@?= ["ui.json"]),
      testCase "discardTemp: removes the temp file even when the close fails" $
        withTemp $ \dir -> do
          -- A write to /dev/full fails when flushed, so the close throws.
          full <- Dir.doesFileExist "/dev/full"
          when full $ do
            let tmp = dir </> "ui.json123.tmp"
            BS.writeFile tmp "half"
            h <- openBinaryFile "/dev/full" WriteMode
            hSetBuffering h (BlockBuffering Nothing)
            BS.hPut h "pending"
            discardTemp tmp h
            Dir.doesFileExist tmp >>= (@?= False),
      testCase "replaces the file; no temp file left" $
        withTemp $ \dir -> do
          let file = dir </> "ui.json"
          writeFileAtomic file "old" >>= (@?= Right ())
          writeFileAtomic file "new" >>= (@?= Right ())
          BS.readFile file >>= (@?= "new")
          Dir.listDirectory dir >>= (@?= ["ui.json"]),
      testCase "read-only dir: Left, old file kept, no temp file" $
        withTemp $ \dir -> do
          let sub = dir </> "ro"
              file = sub </> "ui.json"
          Dir.createDirectory sub
          writeFileAtomic file "old" >>= (@?= Right ())
          let perms w =
                Dir.setOwnerWritable w $
                  Dir.setOwnerSearchable True $
                    Dir.setOwnerReadable True Dir.emptyPermissions
          flip finally (Dir.setPermissions sub (perms True)) $ do
            Dir.setPermissions sub (perms False)
            now <- Dir.getPermissions sub
            -- Root can write anyway: nothing to test then.
            unless (Dir.writable now) $ do
              r <- writeFileAtomic file "new"
              assertBool (show r) (isLeft r)
              BS.readFile file >>= (@?= "old")
              Dir.listDirectory sub >>= (@?= ["ui.json"]),
      testCase "a symlink stays a link; the file it points to changes" $
        withTemp $ \dir -> do
          let real = dir </> "real" </> "ui.json"
              link = dir </> "state" </> "ui.json"
          Dir.createDirectoryIfMissing True (dir </> "real")
          Dir.createDirectoryIfMissing True (dir </> "state")
          BS.writeFile real "old"
          Dir.createFileLink real link
          writeFileAtomic link "new" >>= (@?= Right ())
          Dir.pathIsSymbolicLink link >>= (@?= True)
          BS.readFile real >>= (@?= "new")
          Dir.listDirectory (dir </> "real") >>= (@?= ["ui.json"])
          Dir.listDirectory (dir </> "state") >>= (@?= ["ui.json"]),
      testCase "200 writers: a reader never sees half a file" $
        withTemp $ \dir -> do
          let file = dir </> "ui.json"
              size = 64 * 1024
              content i = LBS.replicate size (fromIntegral (65 + i `mod` 26))
          writeFileAtomic file (content (0 :: Int)) >>= (@?= Right ())
          done <- newTVarIO False
          bad <- newTVarIO Nothing
          let reader = do
                b <- BS.readFile file
                let whole =
                      BS.length b == fromIntegral size && Char8.all (== Char8.head b) b
                unless whole $ atomically (writeTVar bad (Just (BS.length b)))
                stop <- atomically (readTVar done)
                unless stop reader
          let write i = writeFileAtomic file (content i) >>= (@?= Right ())
          withAsync reader $ \r -> do
            mapConcurrently_ write [1 .. 200]
            atomically (writeTVar done True)
            -- A read that failed fails here.
            wait r
          readTVarIO bad >>= \case
            Nothing -> pure ()
            Just n -> assertFailure ("read " <> show n <> " bytes")
          Dir.listDirectory dir >>= (@?= ["ui.json"])
    ]

withTemp :: (FilePath -> IO a) -> IO a
withTemp = withSystemTempDirectory "haskell-reviewer-atomic"
