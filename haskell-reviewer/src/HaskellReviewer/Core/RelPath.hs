-- | Paths of files under ROOT, checked.
module HaskellReviewer.Core.RelPath
  ( -- * Types
    RelPath,
    BadPath (..),

    -- * Building
    relPathFromSegments,
    relPathFromBytes,
    parseRelPath,

    -- * Reading
    relPathText,
    relPathSegments,
    relPathBytes,
    relPathParents,

    -- * Symlinks
    resolveLinkTarget,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (inits)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

{- Note [File containment]

The browser asks for files by path, and the path comes from a URL. So
it's untrusted. A file is served only if all of these hold:

1. The path parses as a 'RelPath': no empty, @.@ or @..@ segment, no
   @/@ inside a segment (a percent-encoded %2F), no NUL, no .git
   segment (in any case: .GIT is the same dir on macOS). So it can't
   name anything above ROOT, lexically, or in a repo's .git.
2. The path is in the snapshot's file list. Ignored files (.env, ...)
   and directories are not in it, so they are a 404. The list is made
   of RelPaths too, so a crafted index that names .git/config can't
   put it there.
3. On disk, every parent dir is a real dir, not a symlink. We open them
   one by one with O_NOFOLLOW, from ROOT down (openat).
4. The last name is looked at with fstatat(AT_SYMLINK_NOFOLLOW) in its
   dir. A regular file is opened with openat and O_NOFOLLOW, and must
   still be a regular file (fstat). O_NONBLOCK keeps a FIFO swapped in
   meanwhile from hanging the open. A symlink is read with readlinkat.
   Anything else is a 404. See Note [Posix at] in
   "HaskellReviewer.Effects.PosixAt".

A symlink in the list is shown as its target text. We never follow it.
If the target, read as a path relative to the link's dir, is another
file of the snapshot, the UI can link to it ('resolveLinkTarget'). That
is lexical: a link through another symlink may point elsewhere for
real, but we only ever read files of the snapshot, the safe way above.

Known limits, accepted:

- A hard link to a file outside ROOT is read like any file. Only the
  user can make one (git never does), and the user can read the file
  anyway.
- A .git file pointing at another repo: git lists that repo's files,
  but we look each name up on disk under ROOT, the same way, so we
  only read what is under ROOT.
- The git listing checks names on disk with lstat by full path (after
  checking each parent dir). Under a swap done at that very moment, it
  can only tell whether a name outside ROOT exists and its kind. Reads
  stay fd-based.
- Case-insensitive file systems and Unicode normalisation (macOS):
  paths are exact keys of the list. A request for A.HS when the list
  has a.hs is a 404, even if the disk would find it.
-}

-- | A path relative to ROOT, with @/@ between segments.
--
-- Invariant: at least one segment; no segment is empty, @.@, @..@ or
-- @.git@ (any case, see 'isDotGit'), or holds @/@ or NUL. Valid UTF-8,
-- since it's 'Text'. The constructor is hidden: build one with
-- 'relPathFromSegments' or 'relPathFromBytes'.
--
-- 'Ord' is code point order. Same as UTF-8 byte order, so it matches
-- git's order for UTF-8 names.
newtype RelPath = RelPath Text
  deriving (Eq, Ord, Show)

-- | Join segments. 'Nothing' if any segment is bad, or if there are none.
relPathFromSegments :: [Text] -> Maybe RelPath
relPathFromSegments segs
  | null segs = Nothing
  | all goodSegment segs = Just (RelPath (Text.intercalate "/" segs))
  | otherwise = Nothing

-- | Why bytes are not a 'RelPath'.
data BadPath
  = -- | Not UTF-8. The API speaks JSON, so we can't show it.
    NotUtf8
  | -- | UTF-8, but a bad segment (empty, @..@, @.git@, ...).
    BadSegment
  deriving (Eq, Show)

-- | Parse a path as git or the OS gives it.
parseRelPath :: ByteString -> Either BadPath RelPath
parseRelPath bytes = case Text.Encoding.decodeUtf8' bytes of
  Left _ -> Left NotUtf8
  Right t -> maybe (Left BadSegment) Right (relPathFromSegments (Text.splitOn "/" t))

-- | 'parseRelPath', without the reason.
relPathFromBytes :: ByteString -> Maybe RelPath
relPathFromBytes = either (const Nothing) Just . parseRelPath

-- | The path as text, e.g. @src\/Foo.hs@.
relPathText :: RelPath -> Text
relPathText (RelPath t) = t

-- | The segments. Never empty.
relPathSegments :: RelPath -> [Text]
relPathSegments (RelPath t) = Text.splitOn "/" t

-- | The path as UTF-8 bytes, for the OS.
relPathBytes :: RelPath -> ByteString
relPathBytes (RelPath t) = Text.Encoding.encodeUtf8 t

-- | The parent dirs, shortest first: @a\/b\/c@ gives @a@ and @a\/b@.
relPathParents :: RelPath -> [RelPath]
relPathParents p =
  [RelPath (Text.intercalate "/" s) | s <- inits (dirSegments p), not (null s)]

-- | Where a symlink points, as a path under ROOT. The target is read
-- relative to the link's dir, lexically (no disk access). 'Nothing' for
-- absolute targets, targets that go above ROOT or that are not UTF-8.
--
-- Only used to link to another file of the snapshot. Never to read.
-- See Note [File containment].
resolveLinkTarget :: RelPath -> ByteString -> Maybe RelPath
resolveLinkTarget link target
  | BS.null target || "/" `BS.isPrefixOf` target = Nothing
  | otherwise = case Text.Encoding.decodeUtf8' target of
      Left _ -> Nothing
      Right t -> do
        segs <- go (reverse (dirSegments link)) (Text.splitOn "/" t)
        relPathFromSegments segs
  where
    -- The dir so far is kept reversed.
    go dir [] = Just (reverse dir)
    go dir (s : rest)
      | s == "" || s == "." = go dir rest
      | s == ".." = case dir of
          [] -> Nothing
          _ : up -> go up rest
      | otherwise = go (s : dir) rest

-- * Internal

-- | Is this name the .git dir, on some file system? Case-folded, and
-- without the code points HFS+ ignores (like git's is_hfs_dotgit), so
-- ".G\x200Cit" counts too.
isDotGit :: Text -> Bool
isDotGit s = Text.toCaseFold (Text.filter (not . ignorable) s) == ".git"
  where
    ignorable c =
      (c >= '\x200C' && c <= '\x200F')
        || (c >= '\x202A' && c <= '\x202E')
        || (c >= '\x206A' && c <= '\x206F')
        || c == '\xFEFF'

-- | All segments but the last.
dirSegments :: RelPath -> [Text]
dirSegments p = case reverse (relPathSegments p) of
  [] -> []
  _ : up -> reverse up

goodSegment :: Text -> Bool
goodSegment s =
  not (Text.null s)
    && s /= "."
    && s /= ".."
    && not (isDotGit s)
    && not (Text.any (\c -> c == '/' || c == '\0') s)
