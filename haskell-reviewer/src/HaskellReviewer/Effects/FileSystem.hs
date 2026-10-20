{-# LANGUAGE MultiWayIf #-}

-- | File system access.
module HaskellReviewer.Effects.FileSystem
  ( -- * Types
    RootError (..),
    ListError (..),
    ReadError (..),
    Lister (..),

    -- * ROOT
    checkRoot,
    renderRootError,

    -- * Listing
    probeFiles,
    renderListError,

    -- * Reading
    readEntry,

    -- * Internal (for tests)
    canonicalRoot,
    listFiles,
    walkFiles,
    WalkResult (..),
  )
where

import Control.Exception (IOException, bracket, bracketOnError, evaluate, finally, try)
import Control.Monad (foldM, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Foreign.C.Error (Errno (..), eLOOP, eNOTDIR)
import GHC.Foreign qualified
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Exception (IOException (..))
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
    StageEntry (..),
    parseOthers,
    parseStageEntries,
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
  ( GitError,
    Listing (..),
    LsFilesOutput (..),
    lsFiles,
    probeGit,
    renderGitError,
  )
import HaskellReviewer.Effects.Log (Logger, logInfo, logVerbose)
import HaskellReviewer.Effects.PosixAt
  ( Kind (..),
    Stat (..),
    listDirFd,
    readLinkAt,
    statAt,
  )
import System.Directory qualified as Dir
import System.IO (hClose)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError)
import System.Posix.Files.ByteString qualified as PosixFiles
import System.Posix.IO.ByteString
  ( OpenFileFlags (..),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    dup,
    fdToHandle,
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

-- | Makes the file list of ROOT. Built at start-up, run after the URL
-- is printed.
data Lister = Lister
  { root :: CanonicalRoot,
    list :: IO (Either ListError FileList)
  }

-- | Check git (see Note [Git hardening] in "HaskellReviewer.Effects.Git")
-- and get ready to list. A 'GitError' is exit 2.
probeFiles :: Logger -> Root -> IO (Either GitError Lister)
probeFiles logger root = do
  canon <- canonicalRoot root
  listing <- probeGit logger root
  pure (fmap (\l -> Lister {root = canon, list = listFiles logger canon l}) listing)

-- | Make the file list. See Note [File list] in
-- "HaskellReviewer.Core.FileList". Warnings go to the logger.
listFiles :: Logger -> CanonicalRoot -> Listing -> IO (Either ListError FileList)
listFiles logger canon = \case
  ListFromDisk -> do
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

-- | Log the first 10 lines, then a count of the rest.
logSome :: (Text -> IO ()) -> Text -> Text -> [Text] -> IO ()
logSome emit prefix what items = do
  mapM_ (emit . (prefix <>)) (take 10 items)
  let more = length items - 10
  unless (more <= 0) $
    emit (prefix <> "and " <> Text.pack (show more) <> " more " <> what)

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
        names <- try @IOException (dup fd >>= listDirFd)
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
                  KindDir -> do
                    sub <- try @IOException (openFdAt (Just fd) name ReadOnly dirFlags)
                    case sub of
                      Left e -> warn rel' e
                      Right subFd -> walk subFd segs' rel' `finally` closeFd subFd
                  KindOther -> pure ()
                _ -> pure ()
  root <- try @IOException (openFdAt Nothing canon.bytes ReadOnly dirFlags)
  case root of
    Left e -> warn "" e
    Right fd -> walk fd [] "" `finally` closeFd fd
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
  deriving (Eq, Show)

-- | Read one entry of the file list, as it is on disk now. See
-- Note [File containment] in "HaskellReviewer.Core.RelPath".
--
-- A symlink's 'resolved' is set when the target is lexically under
-- ROOT; the caller checks it's in the snapshot.
readEntry :: CanonicalRoot -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readEntry canon p = \case
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
      -- The fd is ours until it becomes a handle.
      opened <- bracketOnError (openFdAt (Just dirFd) name ReadOnly fileFlags) closeFd $
        \fd -> do
          st <- PosixFiles.getFdStatus fd
          if PosixFiles.isRegularFile st
            then Just . (,st) <$> fdToHandle fd
            else closeFd fd >> pure Nothing
      case opened of
        Nothing -> pure (Left ReadGone)
        Just (h, st) -> (`finally` hClose h) $ do
          let FileSize maxText = textLimit
              size = FileSize (fromIntegral (PosixFiles.fileSize st))
          bytes <- BS.hGet h (maxText + 1)
          pure (Right (contentFromBytes size bytes))
    fileFlags =
      defaultFileFlags {nofollow = True, nonBlock = True, noctty = True, cloexec = True}

-- * Internal

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
      closeFd fd
      either ioError pure next

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

reasonOf :: IOException -> Text
reasonOf e
  | null e.ioe_description = Text.pack (ioeGetErrorString e)
  | otherwise = Text.pack e.ioe_description
