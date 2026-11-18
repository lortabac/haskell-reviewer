{-# LANGUAGE StrictData #-}

-- | The working-tree side's stat check (D-311, D-318), pure: index
-- entries from @ls-files --stage --debug@, what lstat says, and the
-- verdict per entry. Running git and lstat is in
-- "HaskellReviewer.Effects.WorkTree". See Note [Stat check].
module HaskellReviewer.Core.WorkTree
  ( -- * Index entries
    IndexEntry (..),
    IndexKind (..),
    Exec (..),
    IndexStat (..),
    StatTime (..),
    parseDebugEntries,
    isSkipWorktree,
    isIntentToAdd,

    -- * Disk
    FullStat (..),
    StatKind (..),
    statKind,
    Look (..),

    -- * The check
    Seconds (..),
    IndexTime (..),
    CheckSettings (..),
    Verdict (..),
    checkEntry,

    -- * What a side reads
    WorkFile (..),
    DiskFile (..),
  )
where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word32, Word64)
import HaskellReviewer.Core.Eol (CleanEol, ExecBit (..), Filtering, Symlinks (..))
import HaskellReviewer.Core.Git (ObjectId, parseObjectId)
import Numeric (readHex, readOct)

{- Note [Stat check]

git's index keeps, per entry, the stat data the file had when git last
looked at it. If the disk still says the same, git trusts it is
clean. We do the same check ourselves (D-311), and never run
diff-files or status: those can run clean filters (SPEC §9.1).

Per entry, from 'checkEntry':

- skip-worktree: clean, read from the index (git ignores the disk).
- A gitlink: a directory on disk is kept with the index's commit, else
  deleted. The submodule's HEAD is not compared (a known gap).
- Gone on disk (missing, or a symlink or file in a leading
  directory): deleted. A directory in place of a file: deleted too
  (its files show as untracked).
- Any other lstat error: dirty, read from disk ('Unhashed'). Doubt is
  always dirty.
- Unmerged (stage > 0) or intent-to-add: dirty, like an untracked file.
- Type change, or the exec bit with core.filemode on: 'Modified'.
- A size change, when the recorded size is not 0: 'Modified' too, as
  git does (it doesn't look at the content then).
- Recorded size 0 but not the empty blob (a smudged entry, after
  read-tree), any other stat field changed (mtime, ctime, inode, uid,
  gid, and the size when the recorded one is 0; 32 bits, as the index
  keeps them), or mtime seconds not older than the index file's
  (racy, or the index time unknown): 'Candidate'. We hash it to know.
- Otherwise clean.

assume-unchanged is ignored: we check the stat anyway. trustctime,
checkstat and ignorecase are not used: comparing everything only adds
candidates. With core.symlinks off, a plain file is fine for a
symlink entry. dev is never compared, as in git by default.
-}

-- | What an index entry is.
data IndexKind
  = IndexRegular Exec
  | IndexSymlink
  | IndexGitlink
  | -- | A mode we don't know. Skipped, as in browse mode.
    IndexOther ByteString
  deriving (Eq, Show)

-- | The owner's exec bit.
data Exec = NotExecutable | Executable
  deriving (Eq, Show)

-- | Seconds and nanoseconds, 32 bits each, as the index keeps them.
data StatTime = StatTime
  { sec :: Word32,
    nsec :: Word32
  }
  deriving (Eq, Show)

-- | The stat data of an index entry.
data IndexStat = IndexStat
  { ctime :: StatTime,
    mtime :: StatTime,
    ino :: Word32,
    uid :: Word32,
    gid :: Word32,
    size :: Word32
  }
  deriving (Eq, Show)

-- | One record of @ls-files -z --stage --debug@.
data IndexEntry = IndexEntry
  { kind :: IndexKind,
    oid :: ObjectId,
    stage :: Int,
    -- | Raw bytes, relative to the dir git ran in.
    path :: ByteString,
    stat :: IndexStat,
    -- | git's in-memory entry flags.
    flags :: Word32
  }
  deriving (Eq, Show)

-- The flags stay raw, as git prints them: these read the bits we use.

-- | CE_SKIP_WORKTREE.
isSkipWorktree :: IndexEntry -> Bool
isSkipWorktree e = e.flags .&. 0x40000000 /= 0

-- | CE_INTENT_TO_ADD (@git add -N@).
isIntentToAdd :: IndexEntry -> Bool
isIntentToAdd e = e.flags .&. 0x20000000 /= 0

-- | CE_REMOVE.
isRemoved :: IndexEntry -> Bool
isRemoved e = e.flags .&. 0x20000 /= 0

-- | Parse @ls-files -z --stage --debug@. Each record is
-- @\<mode> \<oid> \<stage>\\t\<path>\\0@, then five lines:
--
-- >   ctime: <s>:<ns>
-- >   mtime: <s>:<ns>
-- >   dev: <n>\tino: <n>
-- >   uid: <n>\tgid: <n>
-- >   size: <n>\tflags: <hex>
--
-- Anything else is an error: never "everything clean".
parseDebugEntries :: ByteString -> Either Text [IndexEntry]
parseDebugEntries = go []
  where
    go acc s
      | BS.null s = Right (reverse acc)
      | otherwise = do
          let (hdr, rest0) = BS.break (== 0) s
          rest1 <- note ("no NUL after " <> lenient hdr) (BS.stripPrefix "\0" rest0)
          (kind, oid, stage, path) <- header hdr
          (ls, rest2) <- takeLines (5 :: Int) rest1
          e <- case ls of
            [l1, l2, l3, l4, l5] -> do
              ct <- time =<< field "  ctime: " l1
              mt <- time =<< field "  mtime: " l2
              (_dev, ino) <- pair "  dev: " "\tino: " l3
              (uid, gid) <- pair "  uid: " "\tgid: " l4
              (size, fl) <- pair "  size: " "\tflags: " l5
              ino' <- decimal ino
              uid' <- decimal uid
              gid' <- decimal gid
              size' <- decimal size
              flags <- hex fl
              Right
                IndexEntry
                  { kind = kind,
                    oid = oid,
                    stage = stage,
                    path = path,
                    stat =
                      IndexStat
                        { ctime = ct,
                          mtime = mt,
                          ino = ino',
                          uid = uid',
                          gid = gid',
                          size = size'
                        },
                    flags = flags
                  }
            _ -> Left "short --debug block"
          go (e : acc) rest2
    header hdr = case Char8.break (== '\t') hdr of
      (meta, rest)
        | Just ('\t', path) <- Char8.uncons rest,
          not (BS.null path),
          [m, o, st] <- Char8.words meta,
          Just oid <- parseObjectId o,
          Just stage <- lookup st [("0", 0), ("1", 1), ("2", 2), ("3", 3)],
          Just mode <- octal m ->
            Right (kindOf m mode, oid, stage, path)
      _ -> Left ("bad ls-files record: " <> lenient hdr)
    kindOf m mode = case mode .&. 0o170000 of
      0o100000
        | mode .&. 0o100 /= 0 -> IndexRegular Executable
        | otherwise -> IndexRegular NotExecutable
      0o120000 -> IndexSymlink
      0o160000 -> IndexGitlink
      _ -> IndexOther m
    takeLines 0 s = Right ([], s)
    takeLines n s = case BS.elemIndex 10 s of
      Nothing -> Left "short --debug block"
      Just i -> do
        (more, rest) <- takeLines (n - 1) (BS.drop (i + 1) s)
        Right (BS.take i s : more, rest)
    field pre l = note ("bad --debug line: " <> lenient l) (BS.stripPrefix pre l)
    pair pre mid l = do
      r <- field pre l
      let (a, b) = BS.breakSubstring mid r
      b' <- field mid b
      Right (a, b')
    time t = case Char8.split ':' t of
      [a, b] -> StatTime <$> decimal a <*> decimal b
      _ -> Left ("bad --debug time: " <> lenient t)
    decimal :: ByteString -> Either Text Word32
    decimal b
      | not (BS.null b),
        BS.length b <= 10,
        Char8.all (`elem` ['0' .. '9']) b,
        Just (n, "") <- Char8.readInteger b,
        n <= 0xffffffff =
          Right (fromInteger n)
      | otherwise = Left ("bad --debug number: " <> lenient b)
    hex b = case readHex (Char8.unpack b) of
      [(n, "")] | BS.length b <= 8 -> Right (fromInteger n)
      _ -> Left ("bad --debug flags: " <> lenient b)
    octal m = case readOct (Char8.unpack m) of
      [(n, "")] | BS.length m <= 7 -> Just (fromInteger n :: Word32)
      _ -> Nothing
    note e = maybe (Left e) Right

-- | lstat of a name, all we compare. The size and inode are full
-- width: the check cuts them to 32 bits, as the index does. Times are
-- cut already (seconds as an unsigned 32-bit number, as git does).
data FullStat = FullStat
  { -- | st_mode: type and permission bits.
    mode :: Word32,
    size :: Int64,
    mtime :: StatTime,
    ctime :: StatTime,
    ino :: Word64,
    dev :: Word64,
    uid :: Word32,
    gid :: Word32
  }
  deriving (Eq, Show)

-- | The file type in a 'FullStat'.
data StatKind = StatRegular | StatSymlink | StatDir | StatOther
  deriving (Eq, Show)

-- | From st_mode. The S_IFMT values are the same on Linux and macOS.
statKind :: FullStat -> StatKind
statKind st = case st.mode .&. 0o170000 of
  0o100000 -> StatRegular
  0o120000 -> StatSymlink
  0o040000 -> StatDir
  _ -> StatOther

-- | What we found on disk for a path.
data Look
  = -- | Missing, or a symlink or a non-directory on the way.
    LookGone
  | -- | Some other error. Doubt: dirty.
    LookFailed Text
  | LookFound FullStat
  deriving (Eq, Show)

-- | Seconds, cut to 32 bits as the index keeps them.
newtype Seconds = Seconds Word32
  deriving (Eq, Ord, Show)

-- | The index file's mtime (the oldest, with a split index).
data IndexTime
  = IndexTime Seconds
  | -- | We couldn't stat it: every entry is racy.
    IndexTimeUnknown
  deriving (Eq, Show)

-- | What the check needs besides the entry.
data CheckSettings = CheckSettings
  { indexMtime :: IndexTime,
    execBit :: ExecBit,
    symlinks :: Symlinks,
    emptyBlob :: ObjectId
  }
  deriving (Eq, Show)

-- | What the check says about one entry. See Note [Stat check].
data Verdict
  = -- | Not on the side.
    Deleted
  | -- | Read the index blob.
    StatClean
  | -- | Hash it: same id as the index means clean.
    Candidate
  | -- | Dirty without comparing. Hashed only for its blob id.
    Modified
  | -- | Dirty, read from disk, blob id unknown.
    Unhashed
  deriving (Eq, Show)

-- | The check for one entry. See Note [Stat check].
checkEntry :: CheckSettings -> IndexEntry -> Look -> Verdict
checkEntry s e look
  | isSkipWorktree e = StatClean
  | otherwise = case (e.kind, look) of
      (IndexGitlink, LookFound st)
        | statKind st == StatDir -> StatClean
        | otherwise -> Deleted
      (IndexGitlink, LookGone) -> Deleted
      -- Never read: kept, with the index's commit.
      (IndexGitlink, LookFailed _) -> StatClean
      (_, LookGone) -> Deleted
      (_, LookFailed _) -> Unhashed
      (_, LookFound st)
        | statKind st == StatDir -> Deleted
        | e.stage > 0 || isIntentToAdd e -> Unhashed
        | isRemoved e -> Modified
        | not (typeOk st) -> Modified
        | modeChanged st -> Modified
        | e.stat.size /= 0 && e.stat.size /= cut st.size -> Modified
        | e.stat.size == 0 && e.oid /= s.emptyBlob -> Candidate
        | statDiffers st -> Candidate
        | racy -> Candidate
        | otherwise -> StatClean
  where
    typeOk st = case e.kind of
      IndexSymlink ->
        statKind st == StatSymlink
          || (s.symlinks == SymlinksOff && statKind st == StatRegular)
      _ -> statKind st == StatRegular
    modeChanged st = case e.kind of
      IndexRegular x
        | s.execBit == TrustExecBit ->
            (x == Executable) /= (st.mode .&. 0o100 /= 0)
      _ -> False
    -- The size too: a recorded size of 0 can still differ.
    statDiffers st =
      e.stat.mtime /= st.mtime
        || e.stat.ctime /= st.ctime
        || e.stat.ino /= cut st.ino
        || e.stat.uid /= st.uid
        || e.stat.gid /= st.gid
        || e.stat.size /= cut st.size
    racy = case s.indexMtime of
      IndexTime (Seconds t) -> e.stat.mtime.sec >= t
      IndexTimeUnknown -> True
    cut :: (Integral a) => a -> Word32
    cut = fromIntegral

-- | How the working-tree side reads one file or symlink.
data WorkFile
  = -- | Clean, or skip-worktree: the index blob.
    FromIndex ObjectId
  | -- | Dirty, unmerged, intent-to-add or untracked: the disk.
    FromDisk DiskFile
  deriving (Eq, Show)

-- | A file read from disk.
data DiskFile = DiskFile
  { -- | Our hash of git's clean bytes, as the listing read them.
    -- 'Nothing' when unknown: filtered, untracked, intent-to-add,
    -- unmerged, too big to hash, or changed while we read it.
    blob :: Maybe ObjectId,
    eol :: CleanEol,
    filtered :: Filtering
  }
  deriving (Eq, Show)

-- * Internal

lenient :: ByteString -> Text
lenient = Text.Encoding.decodeUtf8Lenient
