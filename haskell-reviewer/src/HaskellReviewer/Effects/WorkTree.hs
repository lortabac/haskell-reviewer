-- | The working-tree side's listing: which files are clean (read from
-- the index blob) and which are read from disk (D-75, D-311, D-318).
-- See Note [Working-tree listing].
module HaskellReviewer.Effects.WorkTree
  ( -- * Listing
    WorkTreeFiles (..),
    listWorkTree,
    hashCap,

    -- * Internal (for tests)
    Walker,
    withWalker,
    lookPath,
    withDirOf,
    ReadResult (..),
    readForHash,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception
  ( IOException,
    bracket,
    bracketOnError,
    evaluate,
    finally,
    mask_,
    try,
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Foreign.C.Types (CTime (..))
import HaskellReviewer.Core.BlobId (blobId, emptyBlobId)
import HaskellReviewer.Core.Eol
  ( Attrs,
    CleanEol (..),
    CleanForms (..),
    CoreConfig (..),
    EolRule (..),
    Filtering (..),
    attrNames,
    cleanForms,
    eolRule,
    eolSure,
    filtering,
    hasCrlfInIndex,
    parseCheckAttr,
    parseCoreConfig,
  )
import HaskellReviewer.Core.FileContent (FileSize (..))
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (FromGit),
    mkFileList,
  )
import HaskellReviewer.Core.Git
  ( ObjectId (..),
    OtherEntry (..),
    RepoInfo (..),
    parseOthers,
  )
import HaskellReviewer.Core.RelPath
  ( BadPath (..),
    RelPath,
    parseRelPath,
    relPathBytes,
    relPathSegments,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Core.WorkTree
  ( CheckSettings (..),
    DiskFile (..),
    FullStat (..),
    IndexEntry (..),
    IndexKind (..),
    IndexTime (..),
    Look (..),
    Seconds (..),
    StatKind (..),
    Verdict (..),
    WorkFile (..),
    checkEntry,
    parseDebugEntries,
    statKind,
  )
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    dirFlags,
    fileFlags,
    gone,
    logSome,
    reasonOf,
  )
import HaskellReviewer.Effects.Git
  ( Git,
    LsFilesOutput (..),
    checkAttr,
    coreConfig,
    gitIndexPath,
    lsFilesDebug,
  )
import HaskellReviewer.Effects.GitObjects (BlobBytes (..), ObjectReader, readBlob)
import HaskellReviewer.Effects.Log (Logger, logVerbose)
import HaskellReviewer.Effects.PosixAt (listDirFd, lstatAt, readLinkAt)
import System.IO (hClose)
import System.Posix.Files.ByteString qualified as PosixFiles
import System.Posix.IO.ByteString (OpenMode (ReadOnly), closeFd, fdToHandle, openFdAt)
import System.Posix.Types (Fd)

{- Note [Working-tree listing]

The working-tree side lists ROOT as git sees it, and decides per file
whether to read the index blob (clean) or the disk (dirty). It runs in
the listing thread, after the URL. In order:

1. @config -z --get-regexp@ for the core keys, and @rev-parse
   --git-path index@.
2. The index file's mtime, and that of each @sharedindex.*@ next to
   it: the oldest. Before ls-files: a rewrite in between can only make
   more entries racy. If we can't stat it, unknown: every entry is
   racy.
3. @ls-files --stage --debug@ and @--others@, at once.
4. lstat of every entry, walking down from ROOT with an fd per
   directory level ('Walker'): one openat per directory, never through
   a symlink. Then the pure check (Note [Stat check] in
   "HaskellReviewer.Core.WorkTree").
5. One @check-attr@ over every path we will read from disk.
6. Read and hash the candidates and the modified files: open with
   O_NOFOLLOW|O_NONBLOCK, fstat, and it must be a regular file with the
   same inode and device as the lstat; else it changed under us: dirty,
   blob unknown. Over 'hashCap': dirty, blob unknown. The hash is of
   git's clean bytes (Note [Clean step] in "HaskellReviewer.Core.Eol").
   Same id as the index: clean, unless the file is filtered or its
   line-end rule is unsure ('decide'): then dirty, blob unknown.
7. The list, with each 'WorkFile' forced: request threads and workers
   read them (Q-RTS-BLACKHOLE).

Only the first 'fileListCap' entries by path are hashed. A dirty file
under text=auto with CRLF needs its index blob, to know if git keeps
the CRs (has_crlf_in_index): read through the object reader.
-}

-- | What the working-tree side found. Every file and symlink of the
-- list has an entry in 'work', and nothing else does: submodules and
-- nested repos are not read.
data WorkTreeFiles = WorkTreeFiles
  { files :: FileList,
    -- | How to read each file and symlink of the list.
    work :: Map RelPath WorkFile
  }

-- | Files bigger than this are not hashed: dirty, blob unknown. It
-- bounds the memory of the listing: 64 MiB.
hashCap :: Int
hashCap = 64 * 1024 * 1024

-- | List ROOT's working tree. See Note [Working-tree listing].
listWorkTree ::
  Logger ->
  Git ->
  RepoInfo ->
  ObjectReader ->
  CanonicalRoot ->
  IO (Either ListError WorkTreeFiles)
listWorkTree logger git info reader canon = do
  (cfgOut, indexOut) <- concurrently (coreConfig git) (gitIndexPath git)
  andThen (gitStep cfgOut >>= parsed parseCoreConfig) $ \cfg ->
    andThen (gitStep indexOut) $ \indexPath -> do
      mtime <- indexTime canon indexPath
      out <- lsFilesDebug git
      andThen (gitStep out) $ \ls ->
        andThen (parsed parseDebugEntries (Right ls.staged)) $ \entries -> do
          let paths = parsePaths entries (parseOthers ls.others)
              settings =
                CheckSettings
                  { indexMtime = mtime,
                    execBit = cfg.fileMode,
                    symlinks = cfg.symlinks,
                    emptyBlob = emptyBlobId info.format
                  }
          logSome (logVerbose logger) "files: skipped bad path " "bad paths" paths.bad
          logSome (logVerbose logger) "files: skipped " "unknown modes" paths.oddModes
          checked <- checkPaths canon settings paths
          andThen (attributes git (diskPaths checked)) $ \attrs -> do
            hashed <- hashFiles reader info cfg canon (hashJobs checked attrs)
            let wt = finish cfg checked attrs hashed
            _ <- evaluate (Map.size wt.files.entries)
            mapM_ forceWorkFile (Map.elems wt.work)
            logVerbose logger $
              "files: working tree: "
                <> showText (Map.size wt.files.entries)
                <> " entries, "
                <> showText (Map.size hashed)
                <> " hashed, "
                <> showText (Map.size (Map.filter isDisk wt.work))
                <> " read from disk"
            pure (Right wt)
  where
    gitStep = pure . either (Left . ListGitFailed) Right
    parsed f = pure . (>>= either (Left . ListBadGitOutput) Right . f)
    isDisk = \case
      FromDisk _ -> True
      FromIndex _ -> False
    showText = Text.pack . show

-- * Steps

-- | The entries by path, with what was skipped.
data ParsedPaths = ParsedPaths
  { tracked :: Map RelPath IndexEntry,
    untracked :: Map RelPath OtherEntry,
    notUtf8 :: Int,
    bad :: [Text],
    oddModes :: [Text]
  }

-- | Paths as 'RelPath', one entry per tracked path. Bad paths and
-- unknown modes are dropped, as in browse mode.
parsePaths :: [IndexEntry] -> [OtherEntry] -> ParsedPaths
parsePaths entries others =
  ParsedPaths
    { tracked = tracked,
      untracked =
        Map.fromList
          [(p, o) | (Right p, o) <- parsedOthers, not (Map.member p tracked)],
      notUtf8 =
        length [() | (Left NotUtf8, _) <- parsedTracked]
          + length [() | (Left NotUtf8, _) <- parsedOthers],
      bad =
        [decode e.path | (Left BadSegment, e) <- parsedTracked]
          <> [decode (otherPath o) | (Left BadSegment, o) <- parsedOthers],
      oddModes =
        [ decode e.path <> " (mode " <> decode m <> ")"
        | (Right _, e) <- parsedTracked,
          IndexOther m <- [e.kind]
        ]
    }
  where
    reps = map representative (NonEmpty.groupWith (.path) entries)
    parsedTracked = [(parseRelPath e.path, e) | e <- reps]
    parsedOthers = [(parseRelPath (otherPath o), o) | o <- others]
    tracked = Map.fromList [(p, e) | (Right p, e) <- parsedTracked, known e.kind]
    decode = Text.Encoding.decodeUtf8Lenient
    otherPath = \case
      OtherFile p -> p
      OtherRepo p -> p
    known = \case
      IndexOther _ -> False
      _ -> True

-- | One tracked entry, what lstat said, and the check's verdict.
data Checked = Checked
  { entry :: IndexEntry,
    look :: Look,
    verdict :: Verdict
  }

-- | Steps 4 and the first part of 7: lstat every path, check the
-- tracked ones, make the list. Only what is on the list is kept.
data CheckedPaths = CheckedPaths
  { -- | Tracked and on the list.
    tracked :: Map RelPath Checked,
    -- | Untracked files on the list (not nested repos).
    untrackedFiles :: Set RelPath,
    list :: FileList
  }

-- | Step 4: lstat each path in sorted order, through one 'Walker',
-- then the pure check (Note [Stat check] in
-- "HaskellReviewer.Core.WorkTree").
checkPaths :: CanonicalRoot -> CheckSettings -> ParsedPaths -> IO CheckedPaths
checkPaths canon settings paths = do
  let everyPath = Map.union (() <$ paths.tracked) (() <$ paths.untracked)
  looks <- withWalker canon $ \w -> Map.traverseWithKey (\p () -> lookPath w p) everyPath
  let checked =
        Map.intersectionWith
          (\e l -> Checked {entry = e, look = l, verdict = checkEntry settings e l})
          paths.tracked
          looks
      untrackedLooks = Map.intersectionWith (,) paths.untracked looks
      trackedKinds = Map.mapMaybe (\c -> kindFor c.entry c.verdict c.look) checked
      untrackedKinds = Map.mapMaybe (uncurry untrackedKind) untrackedLooks
      fl =
        mkFileList
          FromGit
          paths.notUtf8
          (Map.toList trackedKinds <> Map.toList untrackedKinds)
      kept = Map.keysSet fl.entries
      isFile = \case
        OtherFile _ -> True
        OtherRepo _ -> False
  pure
    CheckedPaths
      { tracked = Map.restrictKeys checked kept,
        untrackedFiles =
          Map.keysSet (Map.restrictKeys (Map.filter isFile paths.untracked) kept),
        list = fl
      }

-- | The paths we read from disk, so the ones check-attr must tell
-- about: tracked and not clean by stat, and untracked files.
diskPaths :: CheckedPaths -> [RelPath]
diskPaths c =
  Set.toAscList
    ( Map.keysSet (Map.filter (fromDisk . (.verdict)) c.tracked)
        <> c.untrackedFiles
    )
  where
    fromDisk v = v `elem` [Candidate, Modified, Unhashed]

-- | Step 5: one check-attr over these paths. Each one must come back.
attributes :: Git -> [RelPath] -> IO (Either ListError (Map RelPath Attrs))
attributes _ [] = pure (Right Map.empty)
attributes git paths = do
  result <- checkAttr git attrNames (map relPathBytes paths)
  pure $ case result of
    Left e -> Left (ListGitFailed e)
    Right out -> case parseCheckAttr out of
      Left e -> Left (ListBadGitOutput e)
      Right attrs ->
        case traverse (\p -> (p,) <$> Map.lookup (relPathBytes p) attrs) paths of
          Nothing -> Left (ListBadGitOutput "check-attr: a path is missing")
          Just found -> Right (Map.fromList found)

-- | A file or symlink to read and hash.
data HashJob = HashJob
  { entry :: IndexEntry,
    verdict :: Verdict,
    stat :: FullStat,
    attrs :: Attrs
  }

-- | The candidates and modified entries that lstat saw as a file or
-- symlink. A candidate always is one: its type matched the index.
hashJobs :: CheckedPaths -> Map RelPath Attrs -> Map RelPath HashJob
hashJobs c attrs = Map.mapMaybe id (Map.intersectionWith job c.tracked attrs)
  where
    job ch a = case ch.look of
      LookFound st
        | ch.verdict `elem` [Candidate, Modified],
          statKind st `elem` [StatRegular, StatSymlink] ->
            Just HashJob {entry = ch.entry, verdict = ch.verdict, stat = st, attrs = a}
      _ -> Nothing

-- | Step 6: read and hash, walking again in sorted order.
hashFiles ::
  ObjectReader ->
  RepoInfo ->
  CoreConfig ->
  CanonicalRoot ->
  Map RelPath HashJob ->
  IO (Map RelPath (HashJob, WorkFile))
hashFiles reader info cfg canon jobs =
  withWalker canon $ \w ->
    Map.traverseWithKey
      ( \p j -> do
          r <- withDirOf w p (\fd name -> readForHash fd name j.stat)
          (j,) <$> decide reader info cfg j r
      )
      jobs

-- | Step 7: the work map, and the list with dirty kinds from disk.
finish ::
  CoreConfig ->
  CheckedPaths ->
  Map RelPath Attrs ->
  Map RelPath (HashJob, WorkFile) ->
  WorkTreeFiles
finish cfg c attrs hashed =
  WorkTreeFiles
    { files = c.list {entries = Map.union diskKinds c.list.entries},
      work = work
    }
  where
    unhashed a =
      FromDisk
        DiskFile
          { blob = Nothing,
            eol = eolOf (eolRule cfg.autocrlf a),
            filtered = filtering cfg a
          }
    clean = Map.mapMaybe cleanFile c.tracked
    cleanFile ch
      | ch.verdict == StatClean && fileLike ch.entry = Just (FromIndex ch.entry.oid)
      | otherwise = Nothing
    -- Unhashed, and modified ones we can't read (a FIFO, say): the
    -- modified ones we could read are in 'hashed', which wins.
    notHashed =
      Map.intersectionWith
        (\_ a -> unhashed a)
        (Map.filter ((`elem` [Modified, Unhashed]) . (.verdict)) c.tracked)
        attrs
    work =
      Map.unions
        [ snd <$> hashed,
          clean,
          notHashed,
          unhashed <$> Map.restrictKeys attrs c.untrackedFiles
        ]
    -- A clean symlink entry that is a plain file on disk (core.symlinks
    -- off) shows as a file once dirty. Keys are on the list already.
    diskKinds = Map.mapMaybe diskKindOf hashed
    diskKindOf (j, wf) = case wf of
      FromDisk _ -> Just (diskKind j.stat)
      FromIndex _ -> Nothing
    fileLike e = case e.kind of
      IndexRegular _ -> True
      IndexSymlink -> True
      _ -> False

-- * Helpers

-- | One entry per path: an unmerged path has several. Prefer stage 2
-- ("ours"), as git does for the index blob of a conflict.
representative :: NonEmpty IndexEntry -> IndexEntry
representative (e :| []) = e
representative g@(e :| _) = case filter ((== 2) . (.stage)) (NonEmpty.toList g) of
  o : _ -> o
  [] -> e

-- | The list kind of a tracked entry, or 'Nothing' if it is not on the
-- side. Clean: from the index. Dirty: from disk.
kindFor :: IndexEntry -> Verdict -> Look -> Maybe EntryKind
kindFor e v look = case v of
  Deleted -> Nothing
  StatClean -> Just (indexKind e)
  Candidate -> Just (indexKind e)
  _ -> case look of
    LookFound st -> Just (diskKind st)
    _ -> Just (indexKind e)

indexKind :: IndexEntry -> EntryKind
indexKind e = case e.kind of
  IndexSymlink -> EntrySymlink
  IndexGitlink -> let ObjectId o = e.oid in EntrySubmodule (CommitId o)
  _ -> EntryFile

-- | A dirty entry's kind. Not a file or symlink (a FIFO, say): a file,
-- which can't be read (a 404).
diskKind :: FullStat -> EntryKind
diskKind st = case statKind st of
  StatSymlink -> EntrySymlink
  _ -> EntryFile

-- | An untracked entry, if it is still there and of a kind we list.
untrackedKind :: OtherEntry -> Look -> Maybe EntryKind
untrackedKind o look = case (o, look) of
  (OtherRepo _, LookFound st) | statKind st == StatDir -> Just EntryNestedRepo
  (OtherFile _, LookFound st) -> case statKind st of
    StatRegular -> Just EntryFile
    StatSymlink -> Just EntrySymlink
    _ -> Nothing
  -- Doubt: listed. A read then fails.
  (OtherFile _, LookFailed _) -> Just EntryFile
  _ -> Nothing

-- | How to clean a dirty file when we don't know the index blob.
eolOf :: EolRule -> CleanEol
eolOf = \case
  EolKeep -> KeepRaw
  EolText -> DropCrBeforeLf
  EolAuto -> AutoOnRead

-- | Step 6 for one entry: clean, or how to read it from disk. Doubt
-- is dirty: a filtered file is never clean by hash (we can't run its
-- filter), nor is one whose line-end rule we can't be sure of
-- ('eolSure').
decide :: ObjectReader -> RepoInfo -> CoreConfig -> HashJob -> ReadResult -> IO WorkFile
decide reader info cfg j = \case
  ReadLinkTarget t -> do
    let oid = blobId info.format t
    pure $
      if candidate && oid == j.entry.oid
        then FromIndex j.entry.oid
        else FromDisk DiskFile {blob = Just oid, eol = KeepRaw, filtered = Unfiltered}
  ReadBytes b
    | filt == Filtered -> pure unknown
    | not (eolSure cfg.autocrlf j.attrs b) -> pure unknown
    | otherwise -> case cleanForms rule b of
        OneForm f -> do
          let oid = blobId info.format f
          pure $
            if candidate && oid == j.entry.oid
              then FromIndex j.entry.oid
              else known oid (eolOf rule)
        AutoForms raw converted -> do
          let rawId = blobId info.format raw
              convertedId = blobId info.format converted
          if candidate && j.entry.oid `elem` [rawId, convertedId]
            then pure (FromIndex j.entry.oid)
            else do
              -- Dirty: git keeps the CRs if the index blob has CRLF.
              blob <- readBlob reader hashCap j.entry.oid
              pure $ case blob of
                Right bb
                  | BS.length bb.bytes == sizeOf bb ->
                      if hasCrlfInIndex bb.bytes
                        then known rawId KeepRaw
                        else known convertedId AutoOnRead
                _ -> FromDisk DiskFile {blob = Nothing, eol = KeepRaw, filtered = filt}
  ReadTooBig -> pure unknown
  ReadChanged _ -> pure unknown
  where
    candidate = j.verdict == Candidate
    rule = eolRule cfg.autocrlf j.attrs
    filt = filtering cfg j.attrs
    known oid eol = FromDisk DiskFile {blob = Just oid, eol = eol, filtered = filt}
    unknown = FromDisk DiskFile {blob = Nothing, eol = eolOf rule, filtered = filt}
    sizeOf bb = let FileSize n = bb.size in n

-- | Force a work file and everything in it.
forceWorkFile :: WorkFile -> IO ()
forceWorkFile = \case
  FromIndex (ObjectId o) -> () <$ evaluate o
  FromDisk d -> do
    _ <- evaluate d
    case d.blob of
      Just (ObjectId o) -> () <$ evaluate o
      Nothing -> pure ()

-- | The oldest mtime of the index file and its shared index files.
-- Unknown if any of it fails. The path is relative to ROOT, or
-- absolute.
indexTime :: CanonicalRoot -> ByteString -> IO IndexTime
indexTime canon raw = do
  result <- try @IOException $ do
    st <- PosixFiles.getFileStatus path
    names <- bracketOnError (openFdAt Nothing dir ReadOnly dirFlags) closeFd listDirFd
    shared <-
      mapM
        (\n -> PosixFiles.getFileStatus (dir <> "/" <> n))
        (filter ("sharedindex." `BS.isPrefixOf`) names)
    pure (IndexTime (minimum (map seconds (st : shared))))
  pure (either (const IndexTimeUnknown) id result)
  where
    path
      | "/" `BS.isPrefixOf` raw = raw
      | otherwise = canon.bytes <> "/" <> raw
    dir = case BS.breakEnd (== 0x2f) path of
      (d, _) | BS.length d > 1 -> BS.init d
      _ -> "/"
    -- Cut to 32 bits, as git keeps it.
    seconds st = let CTime t = PosixFiles.modificationTime st in Seconds (fromIntegral t)

-- * Walking

-- | Open directories along a walk, one fd per level, never through a
-- symlink (Note [Posix at] in "HaskellReviewer.Effects.PosixAt"). Paths
-- in sorted order reuse the open ones.
newtype Walker = Walker (IORef [Frame])

-- | One open (or failed) directory, from ROOT.
data Frame = Frame
  { dir :: [Text],
    fd :: Either IOException Fd
  }

-- | Open ROOT, run, close every fd.
withWalker :: CanonicalRoot -> (Walker -> IO a) -> IO a
withWalker canon = bracket open close
  where
    open = mask_ $ do
      r <- try @IOException (openFdAt Nothing canon.bytes ReadOnly dirFlags)
      Walker <$> newIORef [Frame {dir = [], fd = r}]
    close (Walker ref) = readIORef ref >>= mapM_ closeFrame

closeFrame :: Frame -> IO ()
closeFrame f = either (const (pure ())) closeFd f.fd

-- | The fd of a directory under ROOT.
dirFd :: Walker -> [Text] -> IO (Either IOException Fd)
dirFd (Walker ref) want = do
  frames <- readIORef ref
  let (done, kept) = span (\f -> not (f.dir `isPrefixOf` want)) frames
  mapM_ closeFrame done
  writeIORef ref kept
  descend
  where
    descend =
      readIORef ref >>= \case
        [] -> pure (Left (userError "no root"))
        top : _ -> case top.fd of
          Left e -> pure (Left e)
          Right fd -> case drop (length top.dir) want of
            [] -> pure (Right fd)
            next : _ -> do
              mask_ $ do
                r <-
                  try @IOException $
                    openFdAt (Just fd) (Text.Encoding.encodeUtf8 next) ReadOnly dirFlags
                modifyRef (Frame {dir = top.dir <> [next], fd = r} :)
              descend
    modifyRef f = readIORef ref >>= writeIORef ref . f

-- | Run with the fd of the path's directory and its last name. A
-- failure to get there is given to the action as an error.
withDirOf ::
  Walker -> RelPath -> (Fd -> ByteString -> IO ReadResult) -> IO ReadResult
withDirOf w p k = do
  let (dirs, name) = splitPath p
  r <- dirFd w dirs
  case r of
    Left e -> pure (ReadChanged (reasonOf e))
    Right fd -> k fd name

-- | lstat a path under ROOT. See 'Look'.
lookPath :: Walker -> RelPath -> IO Look
lookPath w p = do
  let (dirs, name) = splitPath p
  r <- dirFd w dirs
  case r of
    Left e -> pure (failed e)
    Right fd -> either failed LookFound <$> try @IOException (lstatAt fd name)
  where
    failed e
      | gone e = LookGone
      | otherwise = LookFailed (reasonOf e)

-- | The directories of a path, and its last name.
splitPath :: RelPath -> ([Text], ByteString)
splitPath p = case reverse (relPathSegments p) of
  name : dirs -> (reverse dirs, Text.Encoding.encodeUtf8 name)
  -- A RelPath has at least one segment.
  [] -> error "splitPath: a RelPath with no segment"

-- * Reading

-- | What a read for hashing gave.
data ReadResult
  = -- | A regular file's bytes, all of them.
    ReadBytes ByteString
  | -- | A symlink's target.
    ReadLinkTarget ByteString
  | -- | Over 'hashCap'.
    ReadTooBig
  | -- | Not the file the lstat saw, or gone, or an error.
    ReadChanged Text
  deriving (Eq, Show)

-- | Read a name in a directory for hashing. It must still be what the
-- lstat saw: a regular file with the same inode and device, or a
-- symlink.
readForHash :: Fd -> ByteString -> FullStat -> IO ReadResult
readForHash dir name st = case statKind st of
  StatSymlink ->
    either (ReadChanged . reasonOf) ReadLinkTarget
      <$> try @IOException (readLinkAt dir name)
  StatRegular -> do
    result <- try @IOException $ do
      opened <- bracketOnError (openFdAt (Just dir) name ReadOnly fileFlags) closeFd $
        \fd -> do
          now <- PosixFiles.getFdStatus fd
          if PosixFiles.isRegularFile now
            && fromIntegral (PosixFiles.fileID now) == st.ino
            && fromIntegral (PosixFiles.deviceID now) == st.dev
            then Just . (,now) <$> fdToHandle fd
            else closeFd fd >> pure Nothing
      case opened of
        Nothing -> pure (ReadChanged "swapped")
        Just (h, now)
          | size > hashCap -> ReadTooBig <$ hClose h
          | otherwise -> (`finally` hClose h) $ do
              -- The fstat size, not the cap: hGet allocates all it
              -- may read up front.
              bytes <- BS.hGet h size
              more <- BS.hGetSome h 1
              pure $
                if BS.length bytes /= size || not (BS.null more)
                  then ReadChanged "size changed while reading"
                  else ReadBytes bytes
          where
            size = fromIntegral (PosixFiles.fileSize now) :: Int
    pure (either (ReadChanged . reasonOf) id result)
  _ -> pure (ReadChanged "not a file")

-- * Internal

-- | Run the next step on success.
andThen :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
andThen m k = m >>= either (pure . Left) k
