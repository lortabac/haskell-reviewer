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

    -- * Outside ROOT
    OutsideRows (..),
    mkOutsideRows,
    OutsideChanges (..),
    outsideChanges,
    outsidePathCap,

    -- * Reading
    linkTargetCap,
    commitLink,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
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
    Exec,
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
import HaskellReviewer.Core.Renames (Blob (..), DiffEntry (..))
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Commit files]

A commit side is listed with @ls-tree -r -z --long --full-tree@ (D-311),
so every path is from the top of the repo. We cut ROOT's prefix (from
@rev-parse --show-prefix@) by bytes: @libs/@ matches @libs/a.hs@, not
@libsx/a.hs@. The rest is as in Note [File list] in
"HaskellReviewer.Core.FileList": non-UTF-8 names are counted, bad paths
(a @.git@ segment) are dropped, the list is capped.

- A file or symlink keeps its blob id and size, for reads later, and
  its exec bit, for pairing ('entries', D-324).
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
    -- | Every path of 'files' as pairing sees it (D-324).
    entries :: Map RelPath DiffEntry,
    -- | Rows outside ROOT (D-43).
    outside :: OutsideRows,
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
      entries = Map.restrictKeys allEntries (Map.keysSet files.entries),
      outside = mkOutsideRows False allOutside Set.empty,
      dropped = [d | Skipped d <- inside]
    }
  where
    (insideRows, outsideList) = splitRows entries
    allOutside = Map.fromList [(e.path, e.item) | e <- outsideList]
    inside = map row insideRows
    notUtf8 = length [() | NotUtf8Name <- inside]
    files = mkFileList FromGit notUtf8 (concatMap listed inside)
    listed = \case
      KeptFile p _ _ -> [(p, EntryFile)]
      KeptSymlink p _ -> [(p, EntrySymlink)]
      KeptSubmodule p c -> [(p, EntrySubmodule c)]
      _ -> []
    allBlobs =
      Map.fromList
        ([(p, b) | KeptFile p _ b <- inside] <> [(p, b) | KeptSymlink p b <- inside])
    allEntries =
      Map.fromList $
        [(p, File x (diffBlob b)) | KeptFile p x b <- inside]
          <> [(p, Symlink (diffBlob b)) | KeptSymlink p b <- inside]
          <> [(p, Submodule (ObjectId c)) | KeptSubmodule p (CommitId c) <- inside]
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
        ItemBlob x b -> KeptFile p x (ref b)
        ItemSymlink b -> KeptSymlink p (ref b)
        ItemGitlink (ObjectId o) -> KeptSubmodule p (CommitId o)
        ItemOther m -> Skipped (DroppedMode DroppedRow {path = e.path, mode = m})
    ref b = BlobRef {oid = b.oid, size = sizeOf b.size}
    sizeOf = \case
      SizeKnown s -> BlobSize s
      SizeMissing -> BlobMissing
      SizeNotListed -> BlobSizeUnknown
    diffBlob b =
      Blob
        { oid = Just b.oid,
          size = case b.size of
            BlobSize (FileSize n) -> Just n
            _ -> Nothing
        }

-- * Outside ROOT

-- | One side's rows outside ROOT (D-43).
data OutsideRows = OutsideRows
  { -- | By raw path from the top.
    rows :: Map ByteString TreeItem,
    -- | Paths that count as changed whatever the base has: the working
    -- tree's edited and untracked files (D-329). Empty for a commit.
    dirty :: Set ByteString,
    -- | Over 'fileListCap': 'rows' and 'dirty' have the first ones.
    truncated :: Bool
  }
  deriving (Eq, Show)

-- | Cut to 'fileListCap'. The flag is true if the caller cut already.
mkOutsideRows :: Bool -> Map ByteString TreeItem -> Set ByteString -> OutsideRows
mkOutsideRows cut allRows allDirty =
  OutsideRows
    { rows = Map.take fileListCap allRows,
      dirty = Set.take fileListCap allDirty,
      truncated =
        cut || Map.size allRows > fileListCap || Set.size allDirty > fileListCap
    }

-- | The paths outside ROOT that changed between two commits (G21).
data OutsideChanges = OutsideChanges
  { total :: Int,
    -- | The first 'outsidePathCap', in byte order, decoded leniently.
    paths :: [Text],
    -- | A side had too many rows: the count may be off either way
    -- (rows past one side's cut look one-sided).
    truncated :: Bool
  }
  deriving (Eq, Show)

-- | At most this many paths are sent: 100.
outsidePathCap :: Int
outsidePathCap = 100

-- | Paths on one side only, or whose mode or blob differ. No renames
-- across ROOT: a move into ROOT is one path outside, gone.
outsideChanges :: Pair OutsideRows -> OutsideChanges
outsideChanges sides =
  OutsideChanges
    { total = length changed,
      paths = map Text.Encoding.decodeUtf8Lenient (take outsidePathCap changed),
      truncated = sides.head.truncated || sides.base.truncated
    }
  where
    h = sides.head.rows
    b = sides.base.rows
    changed =
      Set.toAscList . Set.unions $
        [ Map.keysSet (Map.difference h b),
          Map.keysSet (Map.difference b h),
          Map.keysSet (Map.filter id (Map.intersectionWith differ h b)),
          sides.head.dirty,
          sides.base.dirty
        ]
    -- Sizes don't count: a working-tree row has none, and a commit
    -- listed without sizes (D-312) has none either.
    differ x y = noSize x /= noSize y
    noSize = \case
      ItemBlob e i -> ItemBlob e (unsized i)
      ItemSymlink i -> ItemSymlink (unsized i)
      i@(ItemGitlink _) -> i
      i@(ItemOther _) -> i
    unsized i = BlobItem {oid = i.oid, size = SizeNotListed}

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
  = KeptFile RelPath Exec BlobRef
  | KeptSymlink RelPath BlobRef
  | KeptSubmodule RelPath CommitId
  | NotUtf8Name
  | Skipped Dropped
