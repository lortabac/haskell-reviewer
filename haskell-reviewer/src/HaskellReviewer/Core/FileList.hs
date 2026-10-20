-- | The flat list of files under ROOT, for one snapshot.
module HaskellReviewer.Core.FileList
  ( -- * Types
    FileList (..),
    EntryKind (..),
    CommitId (..),
    ListSource (..),

    -- * Building
    mkFileList,
    fileListCap,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.RelPath (RelPath)

{- Note [File list]

The file list is what the sidebar shows: every file under ROOT, flat,
sorted by path. Paths are relative to ROOT (D-43).

With git (ROOT in a work tree), it's the tracked files plus the
untracked ones that are not ignored, like `git ls-files` and
`git ls-files --others --exclude-standard` give them. Then we look at
each one on disk:

- gone from disk (deleted, or skip-worktree): dropped;
- the kind comes from disk (a tracked file that is now a symlink is a
  symlink), except for submodules;
- a submodule is one entry with its commit id. A nested repo that is
  not a submodule (git shows "dir/") is one entry too, with no id;
- an entry under a parent dir that is a symlink is dropped: it would
  point outside the tree git knows about.

Without git, we walk the disk. Symlinks are listed but never followed,
.git is skipped at every level, and so are FIFOs, sockets and devices.

Names that are not UTF-8 are skipped and counted: the API speaks JSON.
Other bad paths (a .git segment, from a crafted index; see
Note [File containment] in "HaskellReviewer.Core.RelPath") are dropped
with a verbose log line, not counted.

At most 'fileListCap' entries; past that the list is cut and marked.
With git we cut the sorted list, so we keep the first ones by path.
The disk walk stops once it has enough: each dir is walked in name
order, so the cut is the same each time, but it is not the first ones
by path (a/b/c comes before a.hs in the walk, after it by path).
-}

-- | What a list entry is.
data EntryKind
  = EntryFile
  | -- | Shown as its target, never followed.
    EntrySymlink
  | -- | A submodule. Not entered.
    EntrySubmodule CommitId
  | -- | A git repo inside the tree that is not a submodule. Not entered.
    EntryNestedRepo
  deriving (Eq, Show)

-- | A git commit id, in hex.
newtype CommitId = CommitId Text
  deriving (Eq, Show)

-- | Where the list came from.
data ListSource = FromGit | FromDisk
  deriving (Eq, Show)

-- | The files of one snapshot. See Note [File list].
data FileList = FileList
  { source :: ListSource,
    -- | Sorted by path (code point order).
    entries :: Map RelPath EntryKind,
    -- | Names skipped because they are not UTF-8.
    skippedNames :: Int,
    -- | More than 'fileListCap' files: the list holds the first ones.
    truncated :: Bool
  }
  deriving (Eq, Show)

-- | At most this many entries.
fileListCap :: Int
fileListCap = 100_000

-- | Sort, drop duplicates (the first one wins) and cut at 'fileListCap'.
mkFileList :: ListSource -> Int -> [(RelPath, EntryKind)] -> FileList
mkFileList source skipped list =
  FileList
    { source = source,
      entries = Map.take fileListCap all',
      skippedNames = skipped,
      truncated = Map.size all' > fileListCap
    }
  where
    all' = Map.fromListWith (\_new old -> old) list
