-- | The text store. See Note [Text store] in
-- "HaskellReviewer.Session.TextStore".
module HaskellReviewer.Session.TextStoreTest (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, mapConcurrently, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector qualified as Vector
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
  )
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromBytes, relPathText)
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Session.TextStore
  ( BlobTexts,
    Skipped (..),
    StoreError (..),
    TextFile (..),
    TextFiles (..),
    awaitTextFiles,
    loadTextFiles,
    newBlobTexts,
    newTextStore,
    storeBytes,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Session.TextStore"
    [ testCase "text kept; binary, too large, unreadable counted; links not read" $ do
        (fs, asked) <- loadWith maxBound
        names fs @?= ["a.txt", "b.txt", "c.txt"]
        fs.skipped
          @?= Skipped {binary = 1, tooLarge = 1, unreadable = 3, overBudget = 0}
        map relPathText asked @?= [relPathText p | (p, EntryFile, _) <- entries]
        [f.bytes | f <- Vector.toList fs.files]
          @?= ["h\xC3\xA9llo", "hello", "\xEF\xBB\xBFhi"],
      testCase "the budget: the file that doesn't fit and those after it" $ do
        (fs, asked) <- loadWith 12
        names fs @?= ["a.txt", "b.txt"]
        storeBytes fs @?= 11
        -- c.txt doesn't fit: it and the 5 files after it are over.
        fs.skipped
          @?= Skipped {binary = 0, tooLarge = 0, unreadable = 0, overBudget = 6}
        map relPathText asked @?= ["a.txt", "b.txt", "c.txt"],
      testCase "one load for many requests at once" $ do
        store <- newTextStore =<< newBlobTexts
        loads <- newIORef (0 :: Int)
        let load = do
              atomicModifyIORef' loads (\n -> (n + 1, ()))
              threadDelay 50_000
              fst <$> loadWith maxBound
        let ask = const (awaitTextFiles silent store load)
        rs <- within (mapConcurrently ask [1 :: Int .. 20])
        readIORef loads >>= (@?= 1)
        map (fmap names) rs @?= replicate 20 (Right ["a.txt", "b.txt", "c.txt"])
        -- Later ones don't load again.
        _ <- awaitTextFiles silent store load
        readIORef loads >>= (@?= 1),
      testCase "a request that goes away doesn't stop the load" $ do
        store <- newTextStore =<< newBlobTexts
        started <- newEmptyMVar
        gate <- newEmptyMVar
        let load = do
              putMVar started ()
              readMVar gate
              fst <$> loadWith maxBound
        withAsync (awaitTextFiles silent store load) $ \first -> do
          -- The load runs: now the request goes away.
          within (readMVar started)
          cancel first
        putMVar gate ()
        r <- within (awaitTextFiles silent store (assertFailure "loaded twice"))
        fmap names r @?= Right ["a.txt", "b.txt", "c.txt"],
      testCase "a crashed load fails every request, logged once, not retried" $ do
        store <- newTextStore =<< newBlobTexts
        logged <- newIORef []
        let keep m = atomicModifyIORef' logged (\ms -> (m : ms, ()))
            logger = newLogger LevelInfo keep
        r1 <- within (awaitTextFiles logger store (throwIO (userError "boom")))
        r2 <- within (awaitTextFiles logger store (assertFailure "retried"))
        map (either (\(StoreError _) -> "failed") (const "ok")) [r1, r2]
          @?= ["failed", "failed" :: String]
        readIORef logged
          >>= (@?= ["haskell-reviewer: cannot load the text store: user error (boom)"]),
      testCase "sharing: a second load skips kept blobs, same result" $ do
        table <- newBlobTexts
        (alone, _) <- loadWith maxBound
        (first, asked1) <- loadShared maxBound table blobOf
        (second, asked2) <- loadShared maxBound table blobOf
        map relPathText asked1 @?= [relPathText p | (p, EntryFile, _) <- entries]
        -- a.txt and c.txt are kept, with a blob; b.txt has none.
        map relPathText asked2
          @?= ["b.txt", "d.bin", "e.csv", "f.txt", "g.txt", "h.txt"]
        mapM_ (\fs -> summary fs @?= summary alone) [first, second],
      testCase "sharing: the budget is each store's own" $ do
        table <- newBlobTexts
        _ <- loadShared maxBound table blobOf
        (alone, _) <- loadWith 12
        (shared, _) <- loadShared 12 table blobOf
        summary shared @?= summary alone,
      testCase "sharing: two loads at once both work" $ do
        table <- newBlobTexts
        (alone, _) <- loadWith maxBound
        let load = const (loadShared maxBound table blobOf)
        rs <- within (mapConcurrently load [1 :: Int, 2])
        map (summary . fst) rs @?= [summary alone, summary alone]
    ]
  where
    -- Every file has a blob but b.txt.
    blobOf p = case relPathText p of
      "b.txt" -> Nothing
      t -> Just (ObjectId ("blob-" <> t))
    summary fs =
      ([(relPathText f.path, f.bytes) | f <- Vector.toList fs.files], fs.skipped)
    silent = newLogger LevelInfo (\_ -> pure ())
    names fs = [relPathText f.path | f <- Vector.toList fs.files]
    within act = timeout 5_000_000 act >>= maybe (assertFailure "no answer in 5 s") pure

-- | Load the fake list, and say which paths were read.
loadWith :: Int -> IO (TextFiles, [RelPath])
loadWith budget = do
  table <- newBlobTexts
  loadShared budget table (const Nothing)

-- | The same, sharing bytes through this table, with these blobs.
loadShared :: Int -> BlobTexts -> (RelPath -> Maybe ObjectId) -> IO (TextFiles, [RelPath])
loadShared budget table blobOf = do
  asked <- newIORef []
  let readOne p = do
        atomicModifyIORef' asked (\ps -> (p : ps, ()))
        pure (fromMaybe (Left ReadGone) (lookup p [(q, c) | (q, _, Just c) <- entries]))
  fs <- loadTextFiles budget table blobOf readOne list
  (fs,) . reverse <$> readIORef asked
  where
    list =
      FileList
        { source = FromGit,
          entries = Map.fromList [(p, k) | (p, k, _) <- entries],
          skippedNames = 0,
          truncated = False
        }

-- | Path order. Nothing: the reader says gone.
entries :: [(RelPath, EntryKind, Maybe (Either ReadError FileContent))]
entries =
  mapMaybe
    (\(p, k, c) -> (,k,c) <$> relPathFromBytes p)
    ( [ ("a.txt", EntryFile, Just (Right (TextContent "héllo"))),
        ("b.txt", EntryFile, Just (Right (TextContent "hello"))),
        ("c.txt", EntryFile, Just (Right (TextContent "\xFEFFhi"))),
        ("d.bin", EntryFile, Just (Right (Binary (FileSize 10)))),
        ( "e.csv",
          EntryFile,
          Just (Right (TooLarge big))
        ),
        ("f.txt", EntryFile, Just (Left (ReadFailed "no permission"))),
        ("g.txt", EntryFile, Nothing),
        -- A file that became a link.
        ("h.txt", EntryFile, Just (Right (Link link))),
        ("link.txt", EntrySymlink, Nothing),
        ("nested", EntryNestedRepo, Nothing),
        ("sub", EntrySubmodule (CommitId "0"), Nothing)
      ] ::
        [(ByteString, EntryKind, Maybe (Either ReadError FileContent))]
    )
  where
    big = TooLargeInfo {size = FileSize 2_000_000, limit = textLimit}
    link = LinkInfo {target = "a.txt", resolved = Nothing}
