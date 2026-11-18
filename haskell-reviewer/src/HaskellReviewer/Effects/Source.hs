-- | Where a snapshot's files come from: the disk, or a commit. See
-- Note [Entry sources].
module HaskellReviewer.Effects.Source
  ( -- * Types
    EntrySource (..),
    CommitSource (..),
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

    -- * Reading
    readSourceEntry,

    -- * Warm-up
    warmUpListing,
    warmUpObjects,
  )
where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
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
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Revspec (DiffInfo (..), Revspec (..), printable)
import HaskellReviewer.Core.Root (CanonicalRoot, Root (..))
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Effects.FileSystem
  ( ListError,
    ReadError (..),
    canonicalRoot,
    listCommit,
    listFiles,
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
import HaskellReviewer.Effects.Revisions (DiffError, renderDiffError, resolveDiff)

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
  { -- | The commit listed.
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
  | -- | @--diff REV@: not supported yet.
    ProbeWorkTreeNotYet
  | -- | The revisions of the range don't give two commits.
    ProbeDiff DiffError
  deriving (Eq, Show)

-- | Message for the user, without the program name. One line.
renderProbeError :: ProbeError -> Text
renderProbeError = \case
  ProbeGit e -> renderGitError e
  ProbeNoGit -> "--diff needs git, and there is none on PATH"
  ProbeNotWorkTree root ->
    "--diff needs ROOT in a git work tree: " <> printable (Text.pack root)
  ProbeWorkTreeNotYet ->
    "--diff REV (against the working tree) is not supported yet; \
    \use BASE..HEAD or BASE...HEAD"
  ProbeDiff e -> renderDiffError e

-- | Check git (see Note [Git hardening] in "HaskellReviewer.Effects.Git")
-- and get ready to list each side: ROOT on disk in browse mode, two
-- commits with @--diff@ (D-316). Before the URL line.
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
  Just (AgainstWorkTree _) -> pure (Left ProbeWorkTreeNotYet)
  Just (RangeSpec range) ->
    probeGitWith setup logger root >>= \case
      Left e -> pure (Left (ProbeGit e))
      Right (ListFromDisk NoGitFound) -> pure (Left ProbeNoGit)
      Right (ListFromDisk _) -> pure (Left (ProbeNotWorkTree root.path))
      Right (ListFromGit git) ->
        repoInfo git >>= \case
          Left e -> pure (Left (ProbeGit e))
          Right info ->
            resolveDiff git info range >>= \case
              Left e -> pure (Left (ProbeGit e))
              Right (Left e) -> pure (Left (ProbeDiff e))
              Right (Right di) -> pure (Right (diffProbe logger git info di))

-- | Two commit sides, read through one object reader. It starts no
-- process before the first read.
diffProbe :: Logger -> Git -> RepoInfo -> DiffInfo -> Probe
diffProbe logger git info di =
  DiffProbe di $ \k ->
    withObjectReader logger defaultReaderSettings git $ \r ->
      k Pair {head = lister r di.sides.head, base = lister r di.sides.base}
  where
    lister = commitLister logger git info

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
