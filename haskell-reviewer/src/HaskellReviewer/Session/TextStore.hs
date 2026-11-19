{-# LANGUAGE StrictData #-}

-- | The texts of a snapshot's files, for text search: loaded once, on
-- the first text search. See Note [Text store].
module HaskellReviewer.Session.TextStore
  ( -- * Types
    TextStore,
    BlobTexts,
    TextFiles (..),
    TextFile (..),
    Skipped (..),
    StoreError (..),

    -- * Operations
    newBlobTexts,
    newTextStore,
    storeBlobTexts,
    awaitTextFiles,
    loadTextFiles,
    textStoreBudget,
    storeBytes,
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, evaluate, finally, mask, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as Vector
import HaskellReviewer.Core.FileContent (FileContent (..))
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..))
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Effects.FileSystem (ReadError)
import HaskellReviewer.Effects.Log (Logger, exceptionMessage, logInfo)

{- Note [Text store]

D-306. Text search reads every text file of the snapshot. Reading them
from disk on each query would cost thousands of opens and decodes per
keystroke, so each snapshot has a store, loaded on the first text
search and kept for the snapshot's life.

- What goes in: every EntryFile of the file list, read with the file
  routes' readEntry (Note [File containment] in
  "HaskellReviewer.Core.RelPath"), kept if it is text (valid UTF-8, no
  NUL in the first 8000 bytes, at most 1 MiB). Each is a strict UTF-8
  ByteString, so the search can use memchr. Symlinks, submodules and
  nested repos are not read: a symlink's target, if listed, is read
  itself.
- What is counted, not kept: binary, too large, unreadable (a read
  error, or not a regular file any more), and over the budget.
- Budget: 'textStoreBudget' bytes in all. Files go in path order; once
  one doesn't fit, it and every later file count as over the budget,
  without being read. So a hostile tree (100k files of 1 MiB) can't
  use more.
- Memory: about the size of the project's text files. Haskell modules
  are held twice (here and in their slot); simpler than tying search
  to indexing.
- Sharing (D-320): a session's stores share kept bytes by blob
  ('BlobTexts'), for files read from a blob (a commit, or a clean
  working-tree file: 'sharedBlob' in "HaskellReviewer.Effects.Source").
  A file whose blob is in the table is not read again. Binary and too
  large files are not in it: they are read and counted each time. The
  budget and the counts are each store's own, as without sharing, so
  a store's files don't depend on which side loaded first. Only bytes
  a store keeps go in the table. A reload must drop the blobs no
  store uses any more.

Loading: the first request finds the store Idle, sets it Loading and
starts the load in a thread of its own, so a request that goes away
(the client aborted) doesn't stop it halfway. Requests wait in STM
until it's Ready, never on a thunk (Q-RTS-BLACKHOLE): the files are
forced before they are published. A load that crashes is logged once
and leaves the store Failed, for good: each request then gets a 500.

The disk may change after start-up, as for every route that reads
files: the store holds the files as they were at the first text
search.
-}

-- * Types

-- | One snapshot's store. See Note [Text store].
data TextStore = TextStore
  { state :: TVar StoreState,
    blobTexts :: BlobTexts
  }

-- | Kept texts by blob, shared by a session's stores. See Note [Text
-- store].
newtype BlobTexts = BlobTexts (TVar (Map ObjectId ByteString))

data StoreState
  = Idle
  | Loading
  | Ready TextFiles
  | Failed Text

-- | The loaded files, in path order.
data TextFiles = TextFiles
  { files :: Vector.Vector TextFile,
    skipped :: Skipped
  }

-- | One text file.
data TextFile = TextFile
  { path :: RelPath,
    -- | Its text, as UTF-8.
    bytes :: ByteString
  }

-- | Files not searched, by why. See Note [Text store].
data Skipped = Skipped
  { binary :: Int,
    tooLarge :: Int,
    unreadable :: Int,
    overBudget :: Int
  }
  deriving (Eq, Show)

-- | The load crashed: the message.
newtype StoreError = StoreError Text
  deriving (Eq, Show)

-- * Operations

-- | An empty table, for one session.
newBlobTexts :: IO BlobTexts
newBlobTexts = BlobTexts <$> newTVarIO Map.empty

-- | An empty store that shares bytes through this table: nothing is
-- read until the first text search.
newTextStore :: BlobTexts -> IO TextStore
newTextStore t = do
  v <- newTVarIO Idle
  pure TextStore {state = v, blobTexts = t}

-- | The table the store shares bytes through, for 'loadTextFiles'.
storeBlobTexts :: TextStore -> BlobTexts
storeBlobTexts store = store.blobTexts

-- | The files: loaded with this action the first time, in a thread of
-- its own; later calls wait for that load. A crashed load is logged
-- once. See Note [Text store].
awaitTextFiles ::
  Logger -> TextStore -> IO TextFiles -> IO (Either StoreError TextFiles)
awaitTextFiles logger store load = mask $ \restore -> do
  -- Masked: once Loading is set, the loader starts.
  first <-
    atomically $
      readTVar v >>= \case
        Idle -> True <$ writeTVar v Loading
        _ -> pure False
  when first (void (forkIOWithUnmask loader))
  restore . atomically $
    readTVar v >>= \case
      Ready fs -> pure (Right fs)
      Failed why -> pure (Left (StoreError why))
      _ -> retry
  where
    v = store.state
    -- Masked but for the load itself: an async exception can't leave
    -- the store Loading.
    loader :: (forall a. IO a -> IO a) -> IO ()
    loader unmask = do
      r <- try @SomeException (unmask (load >>= evaluate . forceFiles))
      case r of
        Right fs -> atomically (writeTVar v (Ready fs))
        Left e -> do
          -- Forced, so the TVar holds no thunk (Q-RTS-BLACKHOLE). A
          -- message that throws gets a fixed one.
          why <-
            either (\(_ :: SomeException) -> "the load crashed") id
              <$> try (evaluate (exceptionMessage e))
          -- Logged first, so a waiter sees the line; published even if
          -- the log throws.
          logInfo logger ("haskell-reviewer: cannot load the text store: " <> why)
            `finally` atomically (writeTVar v (Failed why))
    forceFiles fs = Vector.foldl' (\() f -> f `seq` ()) () fs.files `seq` fs

-- | The store's budget: 256 MiB of text in all.
textStoreBudget :: Int
textStoreBudget = 256 * 1024 * 1024

-- | Read the listed files, up to a budget in bytes. A file with a blob
-- (the second argument) whose bytes are in the table is not read. See
-- Note [Text store].
loadTextFiles ::
  Int ->
  BlobTexts ->
  (RelPath -> Maybe ObjectId) ->
  (RelPath -> IO (Either ReadError FileContent)) ->
  FileList ->
  IO TextFiles
loadTextFiles budget (BlobTexts table) blobOf readOne list =
  go [] noneSkipped 0 (Map.toAscList list.entries)
  where
    noneSkipped = Skipped {binary = 0, tooLarge = 0, unreadable = 0, overBudget = 0}
    go acc !sk !used = \case
      [] -> pure (done acc sk)
      (p, EntryFile) : rest ->
        known p >>= \case
          Just b
            | fits b -> go (kept p b acc) sk (used + BS.length b) rest
            | otherwise -> pure (over acc sk rest)
          Nothing ->
            readOne p >>= \case
              Right (TextContent t) -> do
                b <- evaluate (Text.Encoding.encodeUtf8 t)
                if fits b
                  then do
                    b' <- remember p b
                    go (kept p b' acc) sk (used + BS.length b') rest
                  else pure (over acc sk rest)
              Right (Binary _) -> go acc sk {binary = sk.binary + 1} used rest
              Right (TooLarge _) -> go acc sk {tooLarge = sk.tooLarge + 1} used rest
              -- Gone, a read error, or not a regular file any more.
              _ -> go acc sk {unreadable = sk.unreadable + 1} used rest
      _ : rest -> go acc sk used rest
      where
        fits b = used + BS.length b <= budget
    kept p b acc = TextFile {path = p, bytes = b} : acc
    -- This file and every later one are over the budget.
    over acc sk rest = done acc sk {overBudget = 1 + length [() | (_, EntryFile) <- rest]}
    done acc sk = TextFiles {files = Vector.fromList (reverse acc), skipped = sk}
    known p = case blobOf p of
      Nothing -> pure Nothing
      Just o -> Map.lookup o <$> readTVarIO table
    -- Insert if absent, and keep what the table has: two loads at once
    -- leave one copy.
    remember p b = case blobOf p of
      Nothing -> pure b
      Just o@(ObjectId t) -> do
        _ <- evaluate t
        atomically $ do
          m <- readTVar table
          case Map.lookup o m of
            Just old -> pure old
            Nothing -> b <$ (writeTVar table $! Map.insert o b m)

-- | How many bytes of text the store holds.
storeBytes :: TextFiles -> Int
storeBytes fs = Vector.foldl' (\n f -> n + BS.length f.bytes) 0 fs.files
