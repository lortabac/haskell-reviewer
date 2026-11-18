-- | What the file view shows for one entry of the file list.
module HaskellReviewer.Core.FileContent
  ( -- * Types
    FileContent (..),
    FileSize (..),
    TooLargeInfo (..),
    LinkInfo (..),

    -- * Notes
    FileNotes (..),
    unfiltered,

    -- * Classifying
    contentFromBytes,
    textLimit,
    binaryProbeSize,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileList (CommitId)
import HaskellReviewer.Core.RelPath (RelPath)

-- | A size in bytes.
newtype FileSize = FileSize Int
  deriving (Eq, Ord, Show)

-- | One entry, ready to show.
data FileContent
  = -- | UTF-8 text, at most 'textLimit' bytes.
    TextContent Text
  | Binary FileSize
  | TooLarge TooLargeInfo
  | Link LinkInfo
  | Submodule CommitId
  | NestedRepo
  deriving (Eq, Show)

-- | A text file over the limit.
data TooLargeInfo = TooLargeInfo
  { size :: FileSize,
    limit :: FileSize
  }
  deriving (Eq, Show)

-- | A symlink. See Note [File containment] in "HaskellReviewer.Core.RelPath".
data LinkInfo = LinkInfo
  { -- | The target as written in the link. Bytes that are not UTF-8
    -- show as U+FFFD.
    target :: Text,
    -- | The file of the snapshot it points to, if any.
    resolved :: Maybe RelPath
  }
  deriving (Eq, Show)

-- | What a side says about one file, besides its content. Sent with
-- the file view and the source view.
data FileNotes = FileNotes
  { -- | Read from disk, and git would run more than the line-end rule
    -- on it (a filter, ident, working-tree-encoding): what git stores
    -- may differ.
    filtered :: Filtering
  }
  deriving (Eq, Show)

-- | No notes: every file of a commit, and of browse mode.
unfiltered :: FileNotes
unfiltered = FileNotes {filtered = Unfiltered}

-- | Text files bigger than this are not shown: 1 MiB.
textLimit :: FileSize
textLimit = FileSize 1_048_576

-- | A NUL in this many first bytes means binary. Same as git.
binaryProbeSize :: Int
binaryProbeSize = 8000

-- | Classify a regular file. The size is from fstat; the bytes are the
-- start of the file, at most 'textLimit' + 1 of them (so we can tell
-- it's over).
--
-- Binary: a NUL in the first 'binaryProbeSize' bytes, or not valid
-- UTF-8. A NUL wins over the size limit, so a big binary file is
-- "binary", not "too large".
contentFromBytes :: FileSize -> ByteString -> FileContent
contentFromBytes (FileSize statSize) bytes
  | BS.elem 0 (BS.take binaryProbeSize bytes) = Binary total
  | len > maxText = TooLarge TooLargeInfo {size = total, limit = textLimit}
  | otherwise = case Text.Encoding.decodeUtf8' bytes of
      Left _ -> Binary total
      Right t -> TextContent t
  where
    FileSize maxText = textLimit
    len = BS.length bytes
    -- The file may have grown since fstat.
    total = FileSize (max statSize len)
