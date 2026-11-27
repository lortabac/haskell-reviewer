{-# LANGUAGE MultiWayIf #-}

-- | File system access.
module HaskellReviewer.Effects.FileSystem
  ( -- * Types
    RootError (..),
    ListError (..),
    ReadError (..),

    -- * ROOT
    checkRoot,
    renderRootError,

    -- * Listing
    canonicalRoot,
    listFiles,
    listCommit,
    renderListError,

    -- * Reading
    readEntry,
    missingReason,
    readDiskEntry,
    DiskBytes (..),
    readDiskBytes,

    -- * For the working-tree side
    dirFlags,
    fileFlags,
    gone,
    reasonOf,
    logSome,

    -- * Internal (for tests)
    listCommitPlain,
    walkFiles,
    WalkResult (..),
  )
where

import Control.Exception (IOException, bracket, evaluate, onException, try)
import Control.Monad (foldM, join, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Foreign.C.Error (Errno (..), eLOOP, eNOTDIR)
import GHC.Foreign qualified
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Exception (IOException (..))
import HaskellReviewer.Core.CommitFiles
  ( CommitFiles (..),
    Dropped (..),
    DroppedRow (..),
    OutsideRows (..),
    mkCommitFiles,
  )
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    contentFromBytes,
    textLimit,
  )
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
    mkFileList,
  )
import HaskellReviewer.Core.Git
  ( IndexMode (..),
    ObjectId (..),
    OtherEntry (..),
    RepoInfo (..),
    StageEntry (..),
    parseOthers,
    parseStageEntries,
    parseTreeEntries,
  )
import HaskellReviewer.Core.RelPath
  ( BadPath (..),
    RelPath,
    parseRelPath,
    relPathBytes,
    relPathFromSegments,
    relPathParents,
    relPathSegments,
    resolveLinkTarget,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Effects.Git
  ( Failure (..),
    Git,
    GitError (..),
    Listing (..),
    LsFilesOutput (..),
    TreeListing (..),
    lsFiles,
    lsTree,
    renderGitError,
  )
import HaskellReviewer.Effects.Log (Logger, logInfo, logVerbose)
import HaskellReviewer.Effects.PosixAt
  ( Kind (..),
    Stat (..),
    listDirFd,
    readLinkAt,
    statAt,
    withFileAt,
  )
import System.Directory qualified as Dir
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import System.Posix.Files.ByteString qualified as PosixFiles
import System.Posix.IO.ByteString
  ( OpenFileFlags (..),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    openFdAt,
  )
import System.Posix.Types (Fd)

-- | Why ROOT can't be used. Each one holds the path as the user gave it.
data RootError
  = RootMissing FilePath
  | RootNotADirectory FilePath
  | -- | No read or list permission, or some other IO error.
    RootUnreadable FilePath
  deriving (Eq, Show)

-- | Message for the user.
renderRootError :: RootError -> Text
renderRootError = \case
  RootMissing p -> "ROOT does not exist: " <> Text.pack p
  RootNotADirectory p -> "ROOT is not a directory: " <> Text.pack p
  RootUnreadable p -> "cannot read ROOT: " <> Text.pack p

-- | Check that a path is a readable directory and make it absolute.
--
-- Symlinks are not resolved: we only make the path absolute.
checkRoot :: FilePath -> IO (Either RootError Root)
checkRoot p = do
  result <- try @IOException check
  pure $ case result of
    Left _ -> Left (RootUnreadable p)
    Right r -> r
  where
    check = do
      exists <- Dir.doesPathExist p
      isDir <- Dir.doesDirectoryExist p
      canRead <-
        if isDir
          then readableDir <$> Dir.getPermissions p
          else pure False
      if
        | not exists -> pure (Left (RootMissing p))
        | not isDir -> pure (Left (RootNotADirectory p))
        | not canRead -> pure (Left (RootUnreadable p))
        | otherwise -> Right . Root <$> Dir.makeAbsolute p
    readableDir perms = Dir.readable perms && Dir.searchable perms

-- | Resolve the symlinks of ROOT. Its bytes are the ones the OS gave:
-- GHC's file system encoding round-trips any name, even under LC_ALL=C.
canonicalRoot :: Root -> IO CanonicalRoot
canonicalRoot root = do
  p <- Dir.canonicalizePath root.path
  enc <- getFileSystemEncoding
  b <- GHC.Foreign.withCStringLen enc p BS.packCStringLen
  pure CanonicalRoot {path = p, bytes = b}

-- * Listing

-- | The file list could not be made.
data ListError
  = ListGitFailed GitError
  | -- | git printed something we can't parse.
    ListBadGitOutput Text
  | -- | An exception we didn't expect, in the listing thread. Here, not
    -- in App.Run, so the listing has one error type.
    ListCrashed Text
  deriving (Eq, Show)

-- | The reason, for "cannot list files: ...".
renderListError :: ListError -> Text
renderListError = \case
  ListGitFailed e -> renderGitError e
  ListBadGitOutput t -> t
  ListCrashed t -> t

-- | Make the file list. See Note [File list] in
-- "HaskellReviewer.Core.FileList". Warnings go to the logger.
listFiles :: Logger -> CanonicalRoot -> Listing -> IO (Either ListError FileList)
listFiles logger canon = \case
  ListFromDisk _ -> do
    w <- walkFiles canon (fileListCap + 1)
    logSome (logInfo logger) warning "unreadable directories" w.warnings
    Right <$> built (mkFileList FromDisk w.skippedNames w.entries)
  ListFromGit git -> do
    result <- lsFiles git
    case result of
      Left e -> pure (Left (ListGitFailed e))
      Right out -> case parseStageEntries out.staged of
        Left e -> pure (Left (ListBadGitOutput e))
        Right staged -> do
          let raw =
                [(e.path, k) | e <- staged, Right k <- [kindOf e]]
                  <> map fromOther (parseOthers out.others)
              parsed = [(parseRelPath p, p, k) | (p, k) <- raw]
              good = [(p, k) | (Right p, _, k) <- parsed]
              notUtf8 = length [() | (Left NotUtf8, _, _) <- parsed]
              bad = [decode p | (Left BadSegment, p, _) <- parsed]
              oddModes =
                [ decode e.path <> " (mode " <> decode m <> ")"
                | e <- staged,
                  Left m <- [kindOf e]
                ]
          logSome (logVerbose logger) "files: skipped bad path " "bad paths" bad
          logSome (logVerbose logger) "files: skipped " "unknown modes" oddModes
          kept <- statEntries canon (Map.fromListWith (\_new old -> old) good)
          Right <$> built (mkFileList FromGit notUtf8 (Map.toList kept))
  where
    kindOf e = case e.mode of
      ModeFile -> Right EntryFile
      ModeSymlink -> Right EntrySymlink
      ModeGitlink -> let ObjectId o = e.oid in Right (EntrySubmodule (CommitId o))
      ModeOther m -> Left m
    fromOther = \case
      OtherFile p -> (p, EntryFile)
      OtherRepo p -> (p, EntryNestedRepo)
    decode = Text.Encoding.decodeUtf8Lenient
    -- Build the list here, not in the first request: its time counts in
    -- the files span, and a crash in it is a listing error.
    built fl = fl <$ evaluate (Map.size fl.entries)
    warning = "haskell-reviewer: warning: "

-- | List a commit's files under ROOT. See Note [Commit files] in
-- "HaskellReviewer.Core.CommitFiles".
--
-- If @ls-tree --long@ fails, we list again without sizes. On git
-- 2.44.1 it dies on a blob missing from a partial clone (D-311); the
-- sizes then come when a blob is read (D-312). A time out or too much
-- output is an error.
listCommit ::
  Logger -> Git -> RepoInfo -> ObjectId -> IO (Either ListError CommitFiles)
listCommit logger git info commit = do
  result <- lsTree git WithSizes commit
  case result of
    Left (GitFailed f) -> do
      let firstLine = Text.takeWhile (/= '\n') f.stderr
      logVerbose logger $
        "files: ls-tree --long failed, listing without sizes: " <> firstLine
      listCommitPlain logger git info commit
    Left e -> pure (Left (ListGitFailed e))
    Right out -> commitFiles logger info out

-- | 'listCommit' without sizes.
listCommitPlain ::
  Logger -> Git -> RepoInfo -> ObjectId -> IO (Either ListError CommitFiles)
listCommitPlain logger git info commit = do
  result <- lsTree git WithoutSizes commit
  case result of
    Left e -> pure (Left (ListGitFailed e))
    Right out -> commitFiles logger info out

commitFiles :: Logger -> RepoInfo -> ByteString -> IO (Either ListError CommitFiles)
commitFiles logger info out = case parseTreeEntries out of
  Left e -> pure (Left (ListBadGitOutput e))
  Right entries -> do
    let cf = mkCommitFiles info.prefix entries
        bad = [decode p | DroppedPath p <- cf.dropped]
        oddModes =
          [ decode d.path <> " (mode " <> decode d.mode <> ")"
          | DroppedMode d <- cf.dropped
          ]
    logSome (logVerbose logger) "files: skipped bad path " "bad paths" bad
    logSome (logVerbose logger) "files: skipped " "unknown modes" oddModes
    -- As in listFiles: build it here, so a crash is a listing error.
    let sizes = Map.size cf.files.entries + Map.size cf.blobs + Map.size cf.outside.rows
    Right cf <$ evaluate sizes
  where
    decode = Text.Encoding.decodeUtf8Lenient

-- | Check git's entries on disk. Drops the ones that are gone, or under
-- a parent dir that is a symlink. The kind comes from disk, except for
-- submodules and nested repos (a dir on disk). See Note [File list] in
-- "HaskellReviewer.Core.FileList".
statEntries :: CanonicalRoot -> Map RelPath EntryKind -> IO (Map RelPath EntryKind)
statEntries canon entries = do
  dirs <- newIORef Map.empty
  kept <- traverse (check dirs) (Map.toList entries)
  pure (Map.fromDistinctAscList (catMaybes kept))
  where
    check dirs (p, kind) = do
      parentsOk <- allM (realDir dirs) (relPathParents p)
      if not parentsOk
        then pure Nothing
        else do
          st <- lstat (pathOf canon p)
          pure $ case st of
            Nothing -> Nothing
            Just s
              | isRepo kind, PosixFiles.isDirectory s -> Just (p, kind)
              | PosixFiles.isRegularFile s -> Just (p, EntryFile)
              | PosixFiles.isSymbolicLink s -> Just (p, EntrySymlink)
              | otherwise -> Nothing
    isRepo = \case
      EntrySubmodule _ -> True
      EntryNestedRepo -> True
      _ -> False
    -- Each dir is looked at once.
    realDir :: IORef (Map RelPath Bool) -> RelPath -> IO Bool
    realDir dirs d = do
      known <- Map.lookup d <$> readIORef dirs
      case known of
        Just ok -> pure ok
        Nothing -> do
          st <- lstat (pathOf canon d)
          let ok = maybe False PosixFiles.isDirectory st
          modifyIORef' dirs (Map.insert d ok)
          pure ok

-- | What a disk walk found.
data WalkResult = WalkResult
  { entries :: [(RelPath, EntryKind)],
    -- | Names skipped because they are not UTF-8.
    skippedNames :: Int,
    -- | For the user, e.g. a dir we can't read.
    warnings :: [Text]
  }

-- | List the files under ROOT without git, up to the given number. See
-- Note [File list] in "HaskellReviewer.Core.FileList". Each dir is
-- opened in its parent with O_NOFOLLOW (Note [Posix at] in
-- "HaskellReviewer.Effects.PosixAt"), so we never leave ROOT.
--
-- We hold one fd per level of depth. A tree deeper than the fd limit
-- (ulimit -n, often 1024) stops there, with a warning per dir we can't
-- open.
walkFiles :: CanonicalRoot -> Int -> IO WalkResult
walkFiles canon limit = do
  found <- newIORef []
  count <- newIORef (0 :: Int)
  skipped <- newIORef (0 :: Int)
  warnings <- newIORef []
  let isFull = (>= limit) <$> readIORef count
      add p k = modifyIORef' found ((p, k) :) >> modifyIORef' count (+ 1)
      warn rel e =
        modifyIORef' warnings $
          (<> ["cannot read directory " <> shown rel <> ": " <> reasonOf e])
      walk :: Fd -> [Text] -> ByteString -> IO ()
      walk fd segs rel = do
        names <- try @IOException (listDirFd fd)
        case names of
          Left e -> warn rel e
          Right ns -> mapM_ (entry fd segs rel) (sort ns)
      entry fd segs rel name = do
        stop <- isFull
        unless (stop || name == "." || name == "..") $
          case parseRelPath name of
            Left NotUtf8 -> modifyIORef' skipped (+ 1)
            -- .git, in any case.
            Left BadSegment -> pure ()
            Right one -> do
              let segs' = segs <> relPathSegments one
                  rel' = joinRaw rel name
              st <- try @IOException (statAt fd name)
              case (relPathFromSegments segs', st) of
                (Just p, Right s) -> case s.kind of
                  KindSymlink -> add p EntrySymlink
                  KindFile -> add p EntryFile
                  KindDir -> withDirAt (Just fd) name $ \case
                    Left e -> warn rel' e
                    Right subFd -> walk subFd segs' rel'
                  KindOther -> pure ()
                _ -> pure ()
  withDirAt Nothing canon.bytes $ \case
    Left e -> warn "" e
    Right fd -> walk fd [] ""
  WalkResult
    <$> (reverse <$> readIORef found)
    <*> readIORef skipped
    <*> readIORef warnings
  where
    shown rel
      | BS.null rel = "ROOT"
      | otherwise = Text.Encoding.decodeUtf8Lenient rel

-- * Reading

-- | Why an entry can't be read.
data ReadError
  = -- | Not there any more, or not a regular file or symlink now. A 404.
    ReadGone
  | -- | Some other IO error, e.g. no permission.
    ReadFailed Text
  | -- | A blob not in the local repo (partial clone). The diff,
    -- file, source and Markdown routes and the index slots tell it apart
    -- (D-343, D-359); the project files answer as for 'ReadFailed'.
    ReadMissing ObjectId
  deriving (Eq, Show)

-- | What to log for 'ReadMissing'.
missingReason :: ObjectId -> Text
missingReason (ObjectId o) = "object " <> o <> " is not in the local repo"

-- | Read one entry of the file list, as it is on disk now. See
-- Note [File containment] in "HaskellReviewer.Core.RelPath".
--
-- A symlink's 'resolved' is set when the target is lexically under
-- ROOT; the caller checks it's in the snapshot.
readEntry :: CanonicalRoot -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readEntry canon = readDiskEntry canon id

-- | Like 'readEntry', with a change to a file's bytes before they are
-- classified: the working-tree side's line-end clean step. It is only
-- applied when we read the whole file (at most the text limit): a cut
-- file stays as it is, so it is still too large.
readDiskEntry ::
  CanonicalRoot ->
  (ByteString -> ByteString) ->
  RelPath ->
  EntryKind ->
  IO (Either ReadError FileContent)
readDiskEntry canon clean p = \case
  EntrySubmodule commit -> pure (Right (Submodule commit))
  EntryNestedRepo -> pure (Right NestedRepo)
  _ -> do
    result <- try @IOException (bracket (openParents canon p) closeFd readIn)
    pure $ case result of
      Left e
        | gone e -> Left ReadGone
        | otherwise -> Left (ReadFailed (reasonOf e))
      Right r -> r
  where
    name = case reverse (relPathSegments p) of
      n : _ -> Text.Encoding.encodeUtf8 n
      [] -> ""
    readIn dirFd = do
      st <- statAt dirFd name
      case st.kind of
        KindSymlink -> do
          target <- readLinkAt dirFd name
          pure . Right . Link $
            LinkInfo
              { target = Text.Encoding.decodeUtf8Lenient target,
                resolved = resolveLinkTarget p target
              }
        KindFile -> readRegular dirFd
        _ -> pure (Left ReadGone)
    readRegular dirFd = do
      opened <- withFileAt dirFd name fileFlags PosixFiles.isRegularFile $ \h st -> do
        let FileSize maxText = textLimit
            size = FileSize (fromIntegral (PosixFiles.fileSize st))
        bytes <- BS.hGet h (maxText + 1)
        let cleaned
              | BS.length bytes <= maxText = clean bytes
              | otherwise = bytes
        pure (Right (contentFromBytes size cleaned))
      pure (fromMaybe (Left ReadGone) opened)

-- | What 'readDiskBytes' found.
data DiskBytes
  = -- | A regular file's bytes, all of them.
    DiskFileBytes ByteString
  | -- | A symlink's target.
    DiskLinkBytes ByteString
  deriving (Eq, Show)

-- | All of a file's bytes, or a symlink's target, as on disk now,
-- never through a symlink (pairing, D-324). 'Nothing' if it is gone,
-- not a file or symlink, over the cap, or can't be read.
readDiskBytes :: CanonicalRoot -> Int -> RelPath -> IO (Maybe DiskBytes)
readDiskBytes canon cap p =
  either (const Nothing) id
    <$> try @IOException (bracket (openParents canon p) closeFd readIn)
  where
    name = case reverse (relPathSegments p) of
      n : _ -> Text.Encoding.encodeUtf8 n
      [] -> ""
    readIn dirFd = do
      st <- statAt dirFd name
      case st.kind of
        KindSymlink -> Just . DiskLinkBytes <$> readLinkAt dirFd name
        KindFile -> readRegular dirFd
        _ -> pure Nothing
    readRegular dirFd =
      fmap join . withFileAt dirFd name fileFlags PosixFiles.isRegularFile $ \h st -> do
        let size = fromIntegral (PosixFiles.fileSize st) :: Int
        if size > cap
          then pure Nothing
          else do
            -- The fstat size, not the cap: hGet allocates all it may
            -- read up front.
            bytes <- BS.hGet h size
            more <- BS.hGetSome h 1
            pure $
              if BS.length bytes /= size || not (BS.null more)
                then Nothing
                else Just (DiskFileBytes bytes)

-- * Shared with the working-tree side

-- | Log the first 10 lines, then a count of the rest.
logSome :: (Text -> IO ()) -> Text -> Text -> [Text] -> IO ()
logSome emit prefix what items = do
  mapM_ (emit . (prefix <>)) (take 10 items)
  let more = length items - 10
  unless (more <= 0) $
    emit (prefix <> "and " <> Text.pack (show more) <> " more " <> what)

-- | A file to read, never through a symlink. O_NONBLOCK: a FIFO
-- doesn't block the open.
fileFlags :: OpenFileFlags
fileFlags =
  defaultFileFlags {nofollow = True, nonBlock = True, noctty = True, cloexec = True}

-- | A dir, never through a symlink.
dirFlags :: OpenFileFlags
dirFlags =
  defaultFileFlags {nofollow = True, directory = True, noctty = True, cloexec = True}

-- | Missing, a symlink where a dir should be (ELOOP), or a file where a
-- dir should be (ENOTDIR).
gone :: IOException -> Bool
gone e =
  isDoesNotExistError e
    || e.ioe_errno == Just eloop
    || e.ioe_errno == Just enotdir
  where
    Errno eloop = eLOOP
    Errno enotdir = eNOTDIR

-- | The OS's reason, for a message.
reasonOf :: IOException -> Text
reasonOf e
  | null e.ioe_description = Text.pack (ioeGetErrorString e)
  | otherwise = Text.pack e.ioe_description

-- * Internal

-- | Open a dir (a name in a dir fd, or a path), run, close it. The
-- action gets the open error, if any. See Note [Fd ownership] in
-- "HaskellReviewer.Effects.PosixAt".
withDirAt :: Maybe Fd -> ByteString -> (Either IOException Fd -> IO a) -> IO a
withDirAt dir name =
  bracket
    (try @IOException (openFdAt dir name ReadOnly dirFlags))
    (either (const (pure ())) closeFd)

-- | Open ROOT, then each parent dir of the path in turn, never
-- following a symlink. Gives back the fd of the last one.
openParents :: CanonicalRoot -> RelPath -> IO Fd
openParents canon p = do
  rootFd <- openFdAt Nothing canon.bytes ReadOnly dirFlags
  foldM step rootFd (dirs (relPathSegments p))
  where
    dirs segs = take (length segs - 1) segs
    step fd seg = do
      next <-
        try @IOException $
          openFdAt (Just fd) (Text.Encoding.encodeUtf8 seg) ReadOnly dirFlags
      -- Runs masked (a bracket acquire): close the new fd too if this
      -- close throws.
      closeFd fd `onException` either (const (pure ())) closeFd next
      either ioError pure next

-- | 'Nothing' if it can't be read (gone, no permission, ...).
lstat :: ByteString -> IO (Maybe PosixFiles.FileStatus)
lstat p =
  either (const Nothing) Just
    <$> try @IOException (PosixFiles.getSymbolicLinkStatus p)

pathOf :: CanonicalRoot -> RelPath -> ByteString
pathOf canon p = canon.bytes <> "/" <> relPathBytes p

-- | Join two raw paths; an empty one is left out.
joinRaw :: ByteString -> ByteString -> ByteString
joinRaw a b
  | BS.null a = b
  | BS.null b = a
  | otherwise = a <> "/" <> b

allM :: (a -> IO Bool) -> [a] -> IO Bool
allM _ [] = pure True
allM f (x : xs) = do
  ok <- f x
  if ok then allM f xs else pure False
