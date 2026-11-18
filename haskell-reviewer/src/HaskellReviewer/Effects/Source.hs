-- | Where a snapshot's files come from: the disk, or a commit. See
-- Note [Entry sources].
module HaskellReviewer.Effects.Source
  ( -- * Types
    EntrySource (..),
    CommitSource (..),
    Lister (..),
    Listed (..),

    -- * Listing
    probeFiles,
    diskLister,
    commitLister,

    -- * Reading
    readSourceEntry,

    -- * Warm-up
    warmUpListing,
  )
where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import HaskellReviewer.Core.CommitFiles
  ( BlobRef (..),
    BlobSize (..),
    CommitFiles (..),
    commitLink,
    linkTargetCap,
    mkCommitFiles,
  )
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    contentFromBytes,
    textLimit,
  )
import HaskellReviewer.Core.FileList (EntryKind (..), FileList)
import HaskellReviewer.Core.Git (ObjectId (..), RepoInfo, parseTreeEntries)
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Root (CanonicalRoot, Root)
import HaskellReviewer.Effects.FileSystem
  ( ListError,
    ReadError (..),
    canonicalRoot,
    listCommit,
    listFiles,
    readEntry,
  )
import HaskellReviewer.Effects.Git (Git, GitError, Listing, probeGit)
import HaskellReviewer.Effects.GitObjects
  ( BlobBytes (..),
    ObjectReader,
    readBlob,
    renderObjectError,
  )
import HaskellReviewer.Effects.Log (Logger)

{- Note [Entry sources]

Each side's snapshot reads its files from one 'EntrySource':

- 'DiskEntries': ROOT on disk, as it is now (browse mode). See Note
  [File containment] in "HaskellReviewer.Core.RelPath".
- 'CommitEntries': the blobs of one commit, through the object reader
  (Note [Object reader] in "HaskellReviewer.Effects.GitObjects").

A sum type, not a read function, so code can tell what a side reads:
a reload only matters when a side reads the disk (G18).

Commit reads, by the kind in the list:

- a submodule is its commit id, nothing is read;
- a path with no blob is 'ReadGone';
- a file is read up to the text limit + 1, then classified as on disk;
- a symlink is read up to 'linkTargetCap' ('commitLink');
- any object error is 'ReadFailed', so the routes answer 500 "cannot
  read file". A blob missing from a partial clone too, for now.
-}

-- | Where a snapshot's files come from. See Note [Entry sources].
data EntrySource
  = DiskEntries CanonicalRoot
  | CommitEntries CommitSource

-- | The files of one commit.
data CommitSource = CommitSource
  { -- | Not read yet: the header's revision will show it (T03b, T10).
    commit :: ObjectId,
    -- | The blob of every file and symlink of the list.
    blobs :: Map RelPath BlobRef,
    reader :: ObjectReader
  }

-- | Makes one side's file list. Built at start-up, run after the URL
-- is printed.
newtype Lister = Lister
  { list :: IO (Either ListError Listed)
  }

-- | A file list, and where to read its files.
data Listed = Listed
  { files :: FileList,
    source :: EntrySource
  }

-- | Check git (see Note [Git hardening] in "HaskellReviewer.Effects.Git")
-- and get ready to list ROOT on disk. A 'GitError' is exit 2.
probeFiles :: Logger -> Root -> IO (Either GitError Lister)
probeFiles logger root = do
  canon <- canonicalRoot root
  fmap (diskLister logger canon) <$> probeGit logger root

-- | List ROOT as it is on disk, with git or without.
diskLister :: Logger -> CanonicalRoot -> Listing -> Lister
diskLister logger canon listing =
  Lister
    { list = fmap listed <$> listFiles logger canon listing
    }
  where
    listed fl = Listed {files = fl, source = DiskEntries canon}

-- | List one commit, read through this reader. Every blob ref is forced
-- before it returns: request threads and workers read them
-- (Q-RTS-BLACKHOLE).
commitLister :: Logger -> Git -> RepoInfo -> ObjectReader -> ObjectId -> Lister
commitLister logger git info reader commit =
  Lister
    { list =
        listCommit logger git info commit >>= \case
          Left e -> pure (Left e)
          Right cf -> do
            mapM_ forceRef (Map.elems cf.blobs)
            pure . Right $
              Listed
                { files = cf.files,
                  source =
                    CommitEntries
                      CommitSource {commit = commit, blobs = cf.blobs, reader = reader}
                }
    }
  where
    forceRef b = do
      let ObjectId o = b.oid
      _ <- evaluate o
      case b.size of
        BlobSize (FileSize n) -> () <$ evaluate n
        BlobMissing -> pure ()
        BlobSizeUnknown -> pure ()

-- | Read one entry of the list from its source. See Note [Entry
-- sources].
readSourceEntry ::
  EntrySource -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readSourceEntry = \case
  DiskEntries canon -> readEntry canon
  CommitEntries src -> readCommitEntry src

-- | Read one entry of a commit. See Note [Entry sources].
readCommitEntry ::
  CommitSource -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readCommitEntry src p = \case
  EntrySubmodule c -> pure (Right (Submodule c))
  EntryNestedRepo -> pure (Right NestedRepo)
  EntryFile -> blob (maxText + 1) (\b -> contentFromBytes b.size b.bytes)
  EntrySymlink -> blob linkTargetCap (\b -> Link (commitLink p b.size b.bytes))
  where
    FileSize maxText = textLimit
    blob cap k = case Map.lookup p src.blobs of
      Nothing -> pure (Left ReadGone)
      Just ref ->
        readBlob src.reader cap ref.oid >>= \case
          Left e -> pure (Left (ReadFailed (renderObjectError e)))
          Right b -> pure (Right (k b))

-- | Build the shared values of commit listing (the ls-tree parser's
-- literals, the path checks, the list builder) before two sides list at
-- once (Q-RTS-BLACKHOLE). Parses a fixed sample of every row kind, with
-- and without sizes, and forces the whole result. Run before warp.
-- Throws if the sample doesn't parse (a bug).
warmUpListing :: IO ()
warmUpListing =
  mapM_ (\out -> forceListed (parseTreeEntries out)) samples
  where
    -- The sample is fixed: a parse error is a bug here, and the test
    -- suite runs this at start.
    forceListed = \case
      Left e -> ioError (userError ("warmUpListing: " <> show e))
      Right entries -> () <$ evaluate (length (show (mkCommitFiles "p/" entries)))
    samples = [BS.intercalate "\0" withSizes <> "\0", BS.intercalate "\0" plain <> "\0"]
    sha1 = BS.replicate 40 0x61
    sha256 = BS.replicate 64 0x62
    row mode kind oid size path =
      mode <> " " <> kind <> " " <> oid <> maybe "" (" " <>) size <> "\t" <> path
    withSizes =
      [ row "100644" "blob" sha1 (Just "12") "p/a.hs",
        row "100755" "blob" sha1 (Just "3") "p/run.sh",
        row "120000" "blob" sha1 (Just "4") "p/link",
        row "160000" "commit" sha1 (Just "-") "p/sub",
        row "100644" "blob" sha1 (Just "BAD") "p/gone.hs",
        row "100644" "blob" sha256 (Just "1") "p/b.hs",
        row "040000" "tree" sha1 (Just "-") "p/odd",
        row "100644" "blob" sha1 (Just "1") "p/.git/x",
        row "100644" "blob" sha1 (Just "1") "p/caf\xe9.txt",
        row "100644" "blob" sha1 (Just "1") "outside.txt"
      ]
    plain =
      [ row "100644" "blob" sha1 Nothing "p/a.hs",
        row "120000" "blob" sha1 Nothing "p/link",
        row "160000" "commit" sha1 Nothing "p/sub"
      ]
