-- | File pairing on two listed sides: the reads around the pure core in
-- "HaskellReviewer.Core.Renames" (D-324, D-326). See Note [Pairing
-- reads].
module HaskellReviewer.Effects.Pairing
  ( -- * Pairing
    FilePairing (..),
    pairListed,
  )
where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import HaskellReviewer.Core.BlobId (blobId)
import HaskellReviewer.Core.FileList (FileList (..))
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Renames
  ( Blob (..),
    DiffEntry (..),
    Needed (..),
    Pairing,
    Settings (..),
    runPairing,
    startPairing,
    unknownBlobs,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (Spans, hashChars)
import HaskellReviewer.Effects.Source
  ( CommitSource (..),
    EntrySource (..),
    Listed (..),
    WorkTreeSource (..),
    readForPairing,
  )

{- Note [Pairing reads]

Runs in the listing thread, once both sides are listed and their
projects read.

1. The entries: each side's, from its source (a disk side has none and
   is never paired). If a side's list was cut at
   'HaskellReviewer.Core.FileList.fileListCap', both sides keep only
   the paths up to the smaller last path of the cut lists: past it, a
   path would look added or removed only because the other list ends.
   'cut' says so.
2. Unknown blobs ('unknownBlobs': working-tree files we could not hash
   at listing time, like untracked ones) are read and hashed, head then
   base, in path order, so they can pair exactly. Only 'hashable' ones
   (untracked and intent-to-add, with a sure line-end rule): the others
   stay unknown, so they show as modified. They get no span table
   either ('readForPairing' doesn't read them), so they are never
   inexact rename sources or destinations: a staged @git mv@ plus an
   edit of such a file shows as added and removed. Hashing counts
   against 'readBudget': once it is used up, the rest stay unknown, and
   the core gets what is left. A read may pass the budget by one file
   (at most 'sizeCap').
3. The pure core asks for contents in rounds; each one is read again
   and turned into its span table. A file hashed in step 2 is read
   again if asked (and counted again): span tables cost several times
   a hash, and most hashed files are never asked. Bytes are not kept
   between steps: memory stays at one file at a time.

Reads go through 'readForPairing': the object reader for blobs, the
clean step for disk files, at most 'sizeCap' bytes each. A file read
here may differ from what a later read shows, if it changes on disk
meanwhile: the same race as D-320.
-}

-- | What 'pairListed' found.
data FilePairing = FilePairing
  { pairing :: Pairing,
    -- | A side's list was cut, so both were cut there (step 1 of Note
    -- [Pairing reads]).
    cut :: Bool
  }

-- | Pair two sides. 'Nothing' if a side is read from disk (only tests
-- have such diff sessions). See Note [Pairing reads].
pairListed :: Settings -> Pair Listed -> IO (Maybe FilePairing)
pairListed settings sides = case traverse (entriesOf . (.source)) sides of
  Nothing -> pure Nothing
  Just all' -> do
    let (entries0, cut) = cutAtCap sides all'
        unknown = unknownBlobs entries0
    (headEntries, used) <- hashUnknown sides.head.source unknown.head entries0.head 0
    (baseEntries, used') <- hashUnknown sides.base.source unknown.base entries0.base used
    let entries = Pair {head = headEntries, base = baseEntries}
        left = settings {readBudget = max 0 (settings.readBudget - used')}
        load n = do
          h <- spansOf sides.head.source entries.head n.files.head
          b <- spansOf sides.base.source entries.base n.files.base
          pure Pair {head = h, base = b}
    p <- runPairing load (startPairing left entries)
    pure (Just FilePairing {pairing = p, cut = cut})
  where
    entriesOf = \case
      CommitEntries c -> Just c.entries
      WorkTreeEntries w -> Just w.entries
      DiskEntries _ -> Nothing
    -- Step 2. Only the working tree has unknown blobs.
    hashUnknown source paths entries used0 = case source of
      WorkTreeEntries w -> foldM (hashOne w) (entries, used0) paths
      _ -> pure (entries, used0)
    hashOne w (es, used) p = case Map.lookup p es of
      Just e | used < settings.readBudget -> do
        r <- readForPairing settings.sizeCap (WorkTreeEntries w) p e
        case r of
          Nothing -> pure (es, used)
          Just bytes -> do
            let n = BS.length bytes
                blob = Blob {oid = Just (blobId w.format bytes), size = Just n}
            e' <- evaluate (withBlob blob e)
            pure (Map.insert p e' es, used + n)
      _ -> pure (es, used)
    withBlob blob = \case
      File x _ -> File x blob
      Symlink _ -> Symlink blob
      Submodule c -> Submodule c
    spansOf :: EntrySource -> Map RelPath DiffEntry -> [RelPath] -> IO (Map RelPath Spans)
    spansOf source entries paths =
      Map.fromList . concat
        <$> mapM
          ( \p -> case Map.lookup p entries of
              Nothing -> pure []
              Just e ->
                readForPairing settings.sizeCap source p e >>= \case
                  Nothing -> pure []
                  Just bytes -> do
                    spans <- evaluate (hashChars bytes)
                    pure [(p, spans)]
          )
          paths

-- * Internal

-- | Step 1 of Note [Pairing reads]: both sides' entries, cut at the
-- smaller last path of the lists that were cut, and whether any was.
cutAtCap ::
  Pair Listed -> Pair (Map RelPath DiffEntry) -> (Pair (Map RelPath DiffEntry), Bool)
cutAtCap sides entries = case NonEmpty.nonEmpty lastPaths of
  Nothing -> (entries, False)
  Just ps -> (keepTo (minimum ps) <$> entries, True)
  where
    -- A cut list is never empty, so it has a last path.
    lastPaths =
      mapMaybe
        (\l -> fst <$> Map.lookupMax l.files.entries)
        (filter (\l -> l.files.truncated) [sides.head, sides.base])
    keepTo c m = fst (Map.spanAntitone (<= c) m)
