-- | Where a snapshot's files come from: the disk, or a commit. See
-- Note [Entry sources].
module HaskellReviewer.Effects.Source
  ( -- * Types
    EntrySource (..),
    CommitSource (..),
    WorkTreeSource (..),
    Lister (..),
    Listed (..),
    Probe (..),
    ProbeError (..),

    -- * Listing
    probeFiles,
    renderProbeError,
    probeFilesWith,
    diskLister,
    commitLister,
    workTreeLister,

    -- * Reading
    readSourceEntry,
    fileNotes,
    readForPairing,

    -- * Blobs
    sourceBlob,
    sharedBlob,

    -- * Warm-up
    warmUpListing,
    warmUpObjects,
    warmUpWorkTree,
  )
where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.BlobId (blobId, emptyBlobId)
import HaskellReviewer.Core.CommitFiles
  ( BlobRef (..),
    BlobSize (..),
    CommitFiles (..),
    OutsideRows (..),
    commitLink,
    linkTargetCap,
    mkCommitFiles,
  )
import HaskellReviewer.Core.Eol
  ( AttrState (..),
    Attrs (..),
    AutoCrlf (..),
    CleanEol (..),
    EolRule (..),
    ExecBit (..),
    Filtering (..),
    Symlinks (..),
    applyEol,
    cleanForms,
    convertIsBinary,
    defaultCoreConfig,
    dropCrBeforeLf,
    eolRule,
    eolSure,
    filtering,
    gitBool,
    hasCrlfInIndex,
    parseCheckAttr,
    parseCoreConfig,
    textStats,
    unspecifiedAttrs,
  )
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    contentFromBytes,
    textLimit,
    unfiltered,
  )
import HaskellReviewer.Core.FileList (EntryKind (..), FileList)
import HaskellReviewer.Core.Git
  ( ObjectFormat (..),
    ObjectId (..),
    RepoInfo (..),
    parseTreeEntries,
  )
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Renames (DiffEntry (File, Symlink))
import HaskellReviewer.Core.Revspec
  ( CommitsInfo (..),
    DiffInfo (..),
    Revspec (..),
    WorkTreeInfo (..),
    printable,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.WorkTree
  ( CheckSettings (..),
    DiskFile (..),
    FullStat (..),
    IndexTime (..),
    Look (..),
    Seconds (..),
    StatTime (..),
    WorkFile (..),
    checkEntry,
    parseDebugEntries,
  )
import HaskellReviewer.Effects.FileSystem
  ( DiskBytes (..),
    ListError,
    ReadError (..),
    canonicalRoot,
    listCommit,
    listFiles,
    readDiskBytes,
    readDiskEntry,
    readEntry,
  )
import HaskellReviewer.Effects.Git
  ( DiskReason (..),
    Git,
    GitError,
    GitSetup,
    Listing (..),
    probeGitWith,
    renderGitError,
    repoInfo,
    systemGitSetup,
  )
import HaskellReviewer.Effects.GitObjects
  ( BlobBytes (..),
    ObjectReader,
    defaultReaderSettings,
    readBlob,
    renderObjectError,
    warmUpReader,
    withObjectReader,
  )
import HaskellReviewer.Effects.Log (Logger)
import HaskellReviewer.Effects.Revisions
  ( DiffError,
    renderDiffError,
    resolveAgainstWorkTree,
    resolveDiff,
  )
import HaskellReviewer.Effects.WorkTree (WorkTreeFiles (..), listWorkTree)

{- Note [Entry sources]

Each side's snapshot reads its files from one 'EntrySource':

- 'DiskEntries': ROOT on disk, as it is now (browse mode). See Note
  [File containment] in "HaskellReviewer.Core.RelPath". The bytes are
  shown as they are: no line-end rule, no index (D-318).
- 'CommitEntries': the blobs of one commit, through the object reader
  (Note [Object reader] in "HaskellReviewer.Effects.GitObjects").
- 'WorkTreeEntries': the working tree as git sees it (@--diff REV@'s
  head). A clean file is read from its index blob, like a commit
  read; a dirty or untracked one from disk, as in browse mode, then
  with git's line-end clean step. See Note [Working-tree listing] in
  "HaskellReviewer.Effects.WorkTree".

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
  | WorkTreeEntries WorkTreeSource

-- | The working tree, as git sees it.
data WorkTreeSource = WorkTreeSource
  { root :: CanonicalRoot,
    -- | For the clean files.
    reader :: ObjectReader,
    -- | The repo's, for hashing disk files.
    format :: ObjectFormat,
    -- | Each listed path but nested repos, for pairing (D-324).
    entries :: Map RelPath DiffEntry,
    -- | The disk files with an unknown blob that pairing may hash
    -- ('HaskellReviewer.Effects.WorkTree.hashable').
    hashable :: Set RelPath,
    -- | How to read each file and symlink of the side's list: one
    -- entry each, none for submodules and nested repos.
    files :: Map RelPath WorkFile,
    -- | The rows outside ROOT (D-329). 'Nothing': not known.
    outside :: Maybe OutsideRows
  }

-- | The files of one commit.
data CommitSource = CommitSource
  { -- | The commit listed.
    commit :: ObjectId,
    -- | The blob of every file and symlink of the list.
    blobs :: Map RelPath BlobRef,
    -- | Each listed path, for pairing (D-324).
    entries :: Map RelPath DiffEntry,
    -- | The commit's rows outside ROOT (D-43).
    outside :: OutsideRows,
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

-- | What the probe found: how to list each side.
data Probe
  = -- | Browse mode: ROOT on disk.
    BrowseProbe Lister
  | -- | Diff mode: what @--diff@ resolved to, and a way to run with a
    -- lister per side. The object reader lives as long as the action,
    -- so it must wrap the whole server.
    DiffProbe DiffInfo (forall a. (Pair Lister -> IO a) -> IO a)

-- | Why the probe failed. All are exit 2.
data ProbeError
  = -- | git can't be used (both modes).
    ProbeGit GitError
  | -- | @--diff@ and no git on PATH.
    ProbeNoGit
  | -- | @--diff@ and ROOT is not in a git work tree. The ROOT.
    ProbeNotWorkTree FilePath
  | -- | The revisions of @--diff@ don't give the commits.
    ProbeDiff DiffError
  deriving (Eq, Show)

-- | Message for the user, without the program name. One line.
renderProbeError :: ProbeError -> Text
renderProbeError = \case
  ProbeGit e -> renderGitError e
  ProbeNoGit -> "--diff needs git, and there is none on PATH"
  ProbeNotWorkTree root ->
    "--diff needs ROOT in a git work tree: " <> printable (Text.pack root)
  ProbeDiff e -> renderDiffError e

-- | Check git (see Note [Git hardening] in "HaskellReviewer.Effects.Git")
-- and get ready to list each side: ROOT on disk in browse mode; with
-- @--diff@, two commits, or a commit and the working tree (D-316). Only
-- the revisions are resolved here, before the URL line.
probeFiles :: Logger -> Root -> Maybe Revspec -> IO (Either ProbeError Probe)
probeFiles logger root spec = do
  setup <- systemGitSetup
  probeFilesWith setup logger root spec

-- | Like 'probeFiles', with PATH, environment and time limits given.
probeFilesWith ::
  GitSetup -> Logger -> Root -> Maybe Revspec -> IO (Either ProbeError Probe)
probeFilesWith setup logger root = \case
  Nothing -> do
    canon <- canonicalRoot root
    probeGitWith setup logger root >>= \case
      Left e -> pure (Left (ProbeGit e))
      Right listing -> pure (Right (BrowseProbe (diskLister logger canon listing)))
  Just (RangeSpec range) ->
    inRepo $ \git info ->
      orProbeError (resolveDiff git info range) $ \ci ->
        pure (commitsProbe logger git info ci)
  Just (AgainstWorkTree rev) ->
    inRepo $ \git info ->
      orProbeError (resolveAgainstWorkTree git info rev) $ \wi -> do
        canon <- canonicalRoot root
        pure (workTreeProbe logger git info canon wi)
  where
    -- ROOT in a git work tree, and the repo's info.
    inRepo :: (Git -> RepoInfo -> IO (Either ProbeError a)) -> IO (Either ProbeError a)
    inRepo k =
      probeGitWith setup logger root >>= \case
        Left e -> pure (Left (ProbeGit e))
        Right (ListFromDisk NoGitFound) -> pure (Left ProbeNoGit)
        Right (ListFromDisk _) -> pure (Left (ProbeNotWorkTree root.path))
        Right (ListFromGit git) ->
          repoInfo git >>= \case
            Left e -> pure (Left (ProbeGit e))
            Right info -> k git info
    -- The revisions resolved, or why not.
    orProbeError ::
      IO (Either GitError (Either DiffError x)) ->
      (x -> IO Probe) ->
      IO (Either ProbeError Probe)
    orProbeError resolve k =
      resolve >>= \case
        Left e -> pure (Left (ProbeGit e))
        Right (Left e) -> pure (Left (ProbeDiff e))
        Right (Right x) -> Right <$> k x

-- | Two commit sides, read through one object reader. It starts no
-- process before the first read.
commitsProbe :: Logger -> Git -> RepoInfo -> CommitsInfo -> Probe
commitsProbe logger git info ci =
  DiffProbe (CommitsDiff ci) $ \k ->
    withObjectReader logger defaultReaderSettings git $ \r ->
      k Pair {head = lister r ci.sides.head, base = lister r ci.sides.base}
  where
    lister = commitLister logger git info

-- | Base a commit, head the working tree, read through one object
-- reader (head reads its clean files from the index). Head is listed
-- after the URL, like base.
workTreeProbe ::
  Logger -> Git -> RepoInfo -> CanonicalRoot -> WorkTreeInfo -> Probe
workTreeProbe logger git info canon wi =
  DiffProbe (WorkTreeDiff wi) $ \k ->
    withObjectReader logger defaultReaderSettings git $ \r ->
      k
        Pair
          { head = workTreeLister logger git info r canon,
            base = commitLister logger git info r wi.base
          }

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
            -- Only the listing thread reads these: WHNF is enough.
            _ <- evaluate (Map.foldl' (\() e -> e `seq` ()) () cf.entries)
            pure . Right $
              Listed
                { files = cf.files,
                  source =
                    CommitEntries
                      CommitSource
                        { commit = commit,
                          blobs = cf.blobs,
                          entries = cf.entries,
                          outside = cf.outside,
                          reader = reader
                        }
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

-- | List the working tree, read through this reader. See Note
-- [Working-tree listing] in
-- "HaskellReviewer.Effects.WorkTree".
workTreeLister ::
  Logger -> Git -> RepoInfo -> ObjectReader -> CanonicalRoot -> Lister
workTreeLister logger git info reader canon =
  Lister
    { list =
        fmap listed <$> listWorkTree logger git info reader canon
    }
  where
    listed wt =
      Listed
        { files = wt.files,
          source =
            WorkTreeEntries
              WorkTreeSource
                { root = canon,
                  reader = reader,
                  format = info.format,
                  entries = wt.diffEntries,
                  hashable = wt.hashable,
                  files = wt.work,
                  outside = wt.outside
                }
        }

-- | Read one entry of the list from its source. See Note [Entry
-- sources].
readSourceEntry ::
  EntrySource -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readSourceEntry = \case
  DiskEntries canon -> readEntry canon
  CommitEntries src -> readCommitEntry src
  WorkTreeEntries src -> readWorkTreeEntry src

-- | The notes for one file. Only a working-tree file read from disk
-- can be filtered.
fileNotes :: EntrySource -> RelPath -> FileNotes
fileNotes source p = case source of
  WorkTreeEntries src
    | Just (FromDisk d) <- Map.lookup p src.files -> FileNotes {filtered = d.filtered}
  _ -> unfiltered

-- | The blob of a path, if known: what the side holds there. For the
-- changed modules (D-317).
sourceBlob :: EntrySource -> RelPath -> Maybe ObjectId
sourceBlob source p = case source of
  CommitEntries c -> (.oid) <$> Map.lookup p c.blobs
  DiskEntries _ -> Nothing
  WorkTreeEntries w -> case Map.lookup p w.files of
    Just (FromIndex o) -> Just o
    Just (FromDisk d) -> d.blob
    Nothing -> Nothing

-- | The blob a path is read from, if it is read from one: a commit's
-- blob, or a clean working-tree file's index blob. Never a disk read:
-- what a read gives may differ from our hash (D-320). Reads keyed by
-- it can be shared across sides.
sharedBlob :: EntrySource -> RelPath -> Maybe ObjectId
sharedBlob source p = case source of
  CommitEntries c -> (.oid) <$> Map.lookup p c.blobs
  DiskEntries _ -> Nothing
  WorkTreeEntries w -> case Map.lookup p w.files of
    Just (FromIndex o) -> Just o
    _ -> Nothing

-- | git's clean bytes of a file, or a symlink's target, for pairing
-- (D-324), read in full up to the cap. 'Nothing' if it can't be read,
-- is over the cap, is not of the entry's kind any more, is a filtered
-- disk file (its disk bytes are not git's), or is a disk file whose
-- blob was left unknown on purpose (not 'hashable'). A disk side never
-- pairs.
readForPairing :: Int -> EntrySource -> RelPath -> DiffEntry -> IO (Maybe ByteString)
readForPairing cap source p entry = case source of
  DiskEntries _ -> pure Nothing
  CommitEntries c -> case Map.lookup p c.blobs of
    Nothing -> pure Nothing
    Just ref -> fromBlob c.reader ref.oid
  WorkTreeEntries w -> case Map.lookup p w.files of
    Nothing -> pure Nothing
    Just (FromIndex oid) -> fromBlob w.reader oid
    Just (FromDisk d)
      | d.filtered == Filtered -> pure Nothing
      | isNothing d.blob && Set.notMember p w.hashable -> pure Nothing
      | otherwise ->
          readDiskBytes w.root cap p >>= \r -> pure $ case (entry, r) of
            (File _ _, Just (DiskFileBytes b)) -> Just (applyEol d.eol b)
            (Symlink _, Just (DiskLinkBytes t)) -> Just t
            _ -> Nothing
  where
    fromBlob reader oid =
      readBlob reader cap oid >>= \case
        Right b | b.size == FileSize (BS.length b.bytes) -> pure (Just b.bytes)
        _ -> pure Nothing

-- | Read one entry of a commit. See Note [Entry sources].
readCommitEntry ::
  CommitSource -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readCommitEntry src p kind = case kind of
  EntrySubmodule c -> pure (Right (Submodule c))
  EntryNestedRepo -> pure (Right NestedRepo)
  _ -> case Map.lookup p src.blobs of
    Nothing -> pure (Left ReadGone)
    Just ref -> readBlobEntry src.reader p kind ref.oid

-- | Read one entry of the working tree. See Note [Entry sources].
readWorkTreeEntry ::
  WorkTreeSource -> RelPath -> EntryKind -> IO (Either ReadError FileContent)
readWorkTreeEntry src p kind = case kind of
  EntrySubmodule c -> pure (Right (Submodule c))
  EntryNestedRepo -> pure (Right NestedRepo)
  _ -> case Map.lookup p src.files of
    Nothing -> pure (Left ReadGone)
    Just (FromIndex oid) -> readBlobEntry src.reader p kind oid
    Just (FromDisk d) -> readDiskEntry src.root (applyEol d.eol) p kind

-- | Read a file or symlink from its blob: a file up to the text limit
-- + 1, then classified as on disk; a symlink up to 'linkTargetCap'
-- ('commitLink'). Any object error is 'ReadFailed'.
readBlobEntry ::
  ObjectReader -> RelPath -> EntryKind -> ObjectId -> IO (Either ReadError FileContent)
readBlobEntry reader p kind oid = case kind of
  EntrySymlink -> blob linkTargetCap (\b -> Link (commitLink p b.size b.bytes))
  _ -> blob (maxText + 1) (\b -> contentFromBytes b.size b.bytes)
  where
    FileSize maxText = textLimit
    blob cap k =
      readBlob reader cap oid >>= \case
        Left e -> pure (Left (ReadFailed (renderObjectError e)))
        Right b -> pure (Right (k b))

-- | Build the shared values of commit reads before workers and
-- request threads read at once (Q-RTS-BLACKHOLE): the reader's
-- ('warmUpReader'), and classifying a text blob, a binary one, a
-- non-UTF-8 one and a link. Run before warp.
warmUpObjects :: IO ()
warmUpObjects = do
  warmUpReader
  mapM_
    (\b -> evaluate (length (show (contentFromBytes (FileSize (BS.length b)) b))))
    ["module A where\n", "a\0b", "\xff\xfe"]
  case parseRelPath "p/link" of
    Right p -> () <$ evaluate (length (show (commitLink p (FileSize 4) "a.hs")))
    Left e -> ioError (userError ("warmUpObjects: " <> show e))
  () <$ evaluate (length (show (ReadFailed "x")))

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

-- | Build the shared values of the working-tree side before the
-- listing thread, workers and request threads use them at once
-- (Q-RTS-BLACKHOLE): the @--debug@, check-attr and config parsers,
-- 'gitBool', every line-end rule, the binary test, the clean step on
-- read with 'contentFromBytes', the stat check, blob ids in both
-- formats, and 'fileNotes'. Run before warp. Throws if a fixed sample
-- doesn't parse (a bug).
warmUpWorkTree :: IO ()
warmUpWorkTree = do
  entries <- case parseDebugEntries debugSample of
    Right es -> es <$ evaluate (length (show es))
    Left e -> ioError (userError ("warmUpWorkTree: " <> show e))
  force (parseDebugEntries "100644 junk 0\tx\0  ctime: 1\n")
  force (parseCheckAttr attrSample)
  force (parseCoreConfig configSample)
  force (map gitBool bools)
  force
    [ (eolRule ac (attrsOf t c e), eolSure ac (attrsOf t c e) "a\r\n")
    | ac <- autocrlfs,
      t <- states,
      c <- states,
      e <- states
    ]
  forM_ byteSamples $ \b -> do
    let s = textStats b
    force (s, convertIsBinary s, hasCrlfInIndex b, dropCrBeforeLf b)
    force [cleanForms r b | r <- [EolKeep, EolText, EolAuto]]
    force
      [ contentFromBytes (FileSize (BS.length b)) (applyEol c b)
      | c <- [KeepRaw, DropCrBeforeLf, AutoOnRead]
      ]
    force [blobId f b | f <- [Sha1, Sha256]]
  force [emptyBlobId f | f <- [Sha1, Sha256]]
  let settings t =
        CheckSettings
          { indexMtime = t,
            execBit = TrustExecBit,
            symlinks = SymlinksOff,
            emptyBlob = emptyBlobId Sha1
          }
      looks =
        LookGone
          : LookFailed "x"
          : [ LookFound
                FullStat
                  { mode = m,
                    size = 3,
                    mtime = StatTime 1 2,
                    ctime = StatTime 1 2,
                    ino = 4,
                    dev = 5,
                    uid = 6,
                    gid = 7
                  }
            | m <- [0o100644, 0o100755, 0o120777, 0o040755, 0o010644]
            ]
  force
    [ checkEntry (settings t) e l
    | t <- [IndexTime (Seconds 1_800_000_000), IndexTimeUnknown],
      e <- entries,
      l <- looks
    ]
  case parseRelPath "a.hs" of
    Right p -> force (fileNotes (DiskEntries (CanonicalRoot "/x" "/x")) p)
    Left e -> ioError (userError ("warmUpWorkTree: " <> show e))
  let lfs = unspecifiedAttrs {filter = AttrValue "lfs"}
  force [filtering defaultCoreConfig a | a <- [unspecifiedAttrs, lfs]]
  where
    force :: (Show a) => a -> IO ()
    force x = () <$ evaluate (length (show x))
    oid = BS.replicate 40 0x61
    block mode stage path flags =
      mode
        <> " "
        <> oid
        <> " "
        <> stage
        <> "\t"
        <> path
        <> "\0  ctime: 1700000000:1\n  mtime: 1700000000:2\n  dev: 1\tino: 2\n\
           \  uid: 3\tgid: 4\n  size: 5\tflags: "
        <> flags
        <> "\n"
    debugSample =
      BS.concat
        [ block "100644" "0" "a.hs" "0",
          block "100755" "0" "run.sh" "8000",
          block "120000" "0" "link" "40000000",
          block "160000" "0" "sub" "0",
          block "100644" "1" "c.txt" "1000",
          block "100644" "2" "c.txt" "2000",
          block "100644" "0" "new.hs" "20000000",
          block "040000" "0" "odd" "0"
        ]
    attrSample =
      BS.intercalate
        "\0"
        [ "a.hs",
          "text",
          "set",
          "a.hs",
          "eol",
          "crlf",
          "a.hs",
          "crlf",
          "unset",
          "a.hs",
          "filter",
          "lfs",
          "a.hs",
          "ident",
          "unspecified",
          "a.hs",
          "working-tree-encoding",
          "UTF-16",
          ""
        ]
    configSample =
      "core.autocrlf\ninput\0core.filemode\0core.symlinks\nfalse\0\
      \core.checkstat\nminimal\0"
    bools =
      Nothing
        : map
          Just
          [ "",
            "true",
            "Yes",
            "ON",
            "off",
            "1",
            "0",
            "0x10",
            "010",
            "-2k",
            "1m",
            "bad",
            " 5"
          ]
    autocrlfs = [AutoCrlfFalse, AutoCrlfTrue, AutoCrlfInput]
    states =
      [AttrSet, AttrUnset, AttrUnspecified]
        <> map AttrValue ["input", "auto", "lf", "crlf", "x"]
    attrsOf t c e = unspecifiedAttrs {text = t, crlf = c, eol = e}
    byteSamples =
      [ "module A where\r\n",
        "a\0b\r\n",
        "a\rb",
        BS.replicate 300 1 <> "\r\n",
        "a\r\nb\26",
        ""
      ]
