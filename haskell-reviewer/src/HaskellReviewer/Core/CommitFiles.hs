-- | The files of a commit, from @ls-tree@. See Note [Commit files].
module HaskellReviewer.Core.CommitFiles
  ( -- * Types
    CommitFiles (..),
    BlobRef (..),
    BlobSize (..),
    Dropped (..),
    DroppedRow (..),

    -- * Building
    mkCommitFiles,

    -- * Reading
    linkTargetCap,
    commitLink,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileContent (FileSize (..), LinkInfo (..))
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
    mkFileList,
  )
import HaskellReviewer.Core.Git
  ( BlobItem (..),
    ObjectId (..),
    TreeEntry (..),
    TreeItem (..),
    TreeSize (..),
  )
import HaskellReviewer.Core.RelPath
  ( BadPath (..),
    RelPath,
    parseRelPath,
    resolveLinkTarget,
  )

{- Note [Commit files]

A commit side is listed with @ls-tree -r -z --long --full-tree@ (D-311),
so every path is from the top of the repo. We cut ROOT's prefix (from
@rev-parse --show-prefix@) by bytes: @libs/@ matches @libs/a.hs@, not
@libsx/a.hs@. The rest is as in Note [File list] in
"HaskellReviewer.Core.FileList": non-UTF-8 names are counted, bad paths
(a @.git@ segment) are dropped, the list is capped.

- A file or symlink keeps its blob id and size, for reads later.
- A gitlink is a submodule with its commit id.
- An LFS pointer is just a file: we show the pointer text.
- Rows outside ROOT are kept by raw path (D-43), capped at
  'fileListCap' too. The cap is on the result: the whole ls-tree output
  is in memory anyway (at most 256 MiB).

Sizes: @BAD@ (git 2.56, a partial clone) is a missing blob. Without
@--long@ (the 2.44.1 fallback, D-312) we don't know the size until the
blob is read.
-}

-- | The files of one commit. See Note [Commit files].
data CommitFiles = CommitFiles
  { -- | What the sidebar shows. Source 'FromGit'.
    files :: FileList,
    -- | The blob of every file and symlink in 'files'.
    blobs :: Map RelPath BlobRef,
    -- | Rows outside ROOT, by raw path from the top (D-43).
    outside :: Map ByteString TreeItem,
    -- | More than 'fileListCap' rows outside ROOT: 'outside' holds the
    -- first ones.
    outsideTruncated :: Bool,
    -- | Rows under ROOT we skipped, for the log.
    dropped :: [Dropped]
  }
  deriving (Eq, Show)

-- | Where a file's text is.
data BlobRef = BlobRef
  { oid :: ObjectId,
    size :: BlobSize
  }
  deriving (Eq, Show)

-- | A blob's size, as the listing knows it.
data BlobSize
  = BlobSize FileSize
  | -- | Not in the local repo (partial clone): a "not in the local
    -- repo" note later.
    BlobMissing
  | -- | Listed without sizes: known once read (D-312).
    BlobSizeUnknown
  deriving (Eq, Show)

-- | A row under ROOT that is not in the list.
data Dropped
  = -- | A bad path, e.g. with a @.git@ segment. Raw, from ROOT.
    DroppedPath ByteString
  | -- | A mode we don't know.
    DroppedMode DroppedRow
  deriving (Eq, Show)

-- | A row with a mode we don't know.
data DroppedRow = DroppedRow
  { path :: ByteString,
    mode :: ByteString
  }
  deriving (Eq, Show)

-- | Build the list from ROOT's prefix and the rows of ls-tree.
mkCommitFiles :: ByteString -> [TreeEntry] -> CommitFiles
mkCommitFiles prefix entries =
  CommitFiles
    { files = files,
      blobs = Map.restrictKeys allBlobs (Map.keysSet files.entries),
      outside = Map.take fileListCap allOutside,
      outsideTruncated = Map.size allOutside > fileListCap,
      dropped = [d | Skipped d <- inside]
    }
  where
    (insideRows, outsideRows) = splitRows entries
    allOutside = Map.fromList [(e.path, e.item) | e <- outsideRows]
    inside = map row insideRows
    notUtf8 = length [() | NotUtf8Name <- inside]
    files = mkFileList FromGit notUtf8 (concatMap listed inside)
    listed = \case
      KeptFile p _ -> [(p, EntryFile)]
      KeptSymlink p _ -> [(p, EntrySymlink)]
      KeptSubmodule p c -> [(p, EntrySubmodule c)]
      _ -> []
    allBlobs =
      Map.fromList
        ([(p, b) | KeptFile p b <- inside] <> [(p, b) | KeptSymlink p b <- inside])
    splitRows :: [TreeEntry] -> ([TreeEntry], [TreeEntry])
    splitRows = foldr place ([], [])
    place :: TreeEntry -> ([TreeEntry], [TreeEntry]) -> ([TreeEntry], [TreeEntry])
    place e (ins, outs) = case BS.stripPrefix prefix e.path of
      Just rel | not (BS.null rel) -> (TreeEntry {path = rel, item = e.item} : ins, outs)
      _ -> (ins, e : outs)
    row :: TreeEntry -> Row
    row e = case parseRelPath e.path of
      Left NotUtf8 -> NotUtf8Name
      Left BadSegment -> Skipped (DroppedPath e.path)
      Right p -> case e.item of
        ItemBlob b -> KeptFile p (ref b)
        ItemSymlink b -> KeptSymlink p (ref b)
        ItemGitlink (ObjectId o) -> KeptSubmodule p (CommitId o)
        ItemOther m -> Skipped (DroppedMode DroppedRow {path = e.path, mode = m})
    ref b = BlobRef {oid = b.oid, size = sizeOf b.size}
    sizeOf = \case
      SizeKnown s -> BlobSize s
      SizeMissing -> BlobMissing
      SizeNotListed -> BlobSizeUnknown

-- * Reading

-- | We read at most this many bytes of a symlink's target: 4 KiB, as
-- Linux's PATH_MAX.
linkTargetCap :: Int
linkTargetCap = 4096

-- | A symlink of a commit, from its blob: the link's path, the blob's
-- full size, and its bytes, at most 'linkTargetCap'. A target that was
-- cut is shown cut, and points nowhere.
commitLink :: RelPath -> FileSize -> ByteString -> LinkInfo
commitLink p (FileSize size) bytes =
  LinkInfo
    { target = Text.Encoding.decodeUtf8Lenient bytes,
      resolved =
        if size > BS.length bytes
          then Nothing
          else resolveLinkTarget p bytes
    }

-- * Internal

-- | What 'mkCommitFiles' does with one row under ROOT.
data Row
  = KeptFile RelPath BlobRef
  | KeptSymlink RelPath BlobRef
  | KeptSubmodule RelPath CommitId
  | NotUtf8Name
  | Skipped Dropped
