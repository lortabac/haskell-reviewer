-- | Parsing what git prints. Pure: running git is in
-- "HaskellReviewer.Effects.Git".
module HaskellReviewer.Core.Git
  ( -- * Version
    GitVersion (..),
    minimumGitVersion,
    parseGitVersion,
    renderGitVersion,

    -- * ls-files
    IndexMode (..),
    ObjectId (..),
    StageEntry (..),
    OtherEntry (..),
    parseStageEntries,
    parseOthers,

    -- * Object ids and revisions
    parseObjectId,
    parseCommitIds,
    ObjectFormat (..),
    History (..),
    RepoInfo (..),
    parseRepoInfo,

    -- * ls-tree
    TreeEntry (..),
    TreeItem (..),
    Exec (..),
    BlobItem (..),
    TreeSize (..),
    parseTreeEntries,

    -- * cat-file
    ObjectType (..),
    renderObjectType,
    CatFileHeader (..),
    FoundHeader (..),
    parseCatFileHeader,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.Foldable (minimumBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileContent (FileSize (..))
import Text.Read (readMaybe)

-- | A git version, e.g. 2.44.0.
data GitVersion = GitVersion
  { major :: Int,
    minor :: Int,
    patch :: Int
  }
  deriving (Eq, Ord, Show)

-- | We need GIT_NO_LAZY_FETCH (SPEC §11.4.1, D-311): new in 2.45.0, and
-- backported to 2.44.1. 2.44.0 does lazy-fetch.
minimumGitVersion :: GitVersion
minimumGitVersion = GitVersion 2 44 1

-- | Parse the output of @git --version@. Takes vendor suffixes, as in
-- @git version 2.39.3 (Apple Git-146)@ or @2.45.1.windows.1@. A missing
-- patch number is 0.
parseGitVersion :: Text -> Maybe GitVersion
parseGitVersion out = case Text.words (Text.strip out) of
  "git" : "version" : v : _ -> numbers v
  _ -> Nothing
  where
    numbers v = case Text.splitOn "." v of
      ma : mi : rest -> do
        major <- number ma
        minor <- number mi
        patch <- case rest of
          [] -> Just 0
          p : _ -> number (Text.takeWhile isDigit p)
        Just (GitVersion major minor patch)
      _ -> Nothing
    number t
      | Text.null t || not (Text.all isDigit t) = Nothing
      | otherwise = readMaybe (Text.unpack t)

-- | E.g. @2.44.0@.
renderGitVersion :: GitVersion -> Text
renderGitVersion v =
  Text.intercalate "." (map (Text.pack . show) [v.major, v.minor, v.patch])

-- | The mode of an index entry, as far as we care.
data IndexMode
  = ModeFile
  | ModeSymlink
  | -- | A submodule (mode 160000).
    ModeGitlink
  | -- | A mode we don't know. Such entries are skipped.
    ModeOther ByteString
  deriving (Eq, Show)

-- | A git object id, in hex. Ids from ls-files are taken as they are;
-- 'parseObjectId' checks one.
newtype ObjectId = ObjectId Text
  deriving (Eq, Ord, Show)

-- | One entry of @git ls-files --stage -z@.
data StageEntry = StageEntry
  { -- | Raw bytes, relative to the dir git ran in.
    path :: ByteString,
    mode :: IndexMode,
    -- | The object id. For a gitlink, the commit.
    oid :: ObjectId
  }
  deriving (Eq, Show)

-- | One entry of @git ls-files -z --others --exclude-standard@.
data OtherEntry
  = OtherFile ByteString
  | -- | A nested repo: git prints it as @dir\/@. The slash is removed.
    OtherRepo ByteString
  deriving (Eq, Show)

-- | Parse @git ls-files --stage -z@: records
-- @\<mode> \<oid> \<stage>\\t\<path>\\0@. A path in conflict has
-- several records (stages 1 to 3): we keep one, preferring stage 2
-- ("ours"). 100xxx is a file, 120000 a symlink, 160000 a submodule;
-- anything else is 'ModeOther'. Gives back the first malformed record on failure.
parseStageEntries :: ByteString -> Either Text [StageEntry]
parseStageEntries out = do
  parsed <- traverse record (records out)
  -- git sorts by path, so the stages of one path are next to each other.
  pure (map best (NonEmpty.groupWith (\(_, e) -> e.path) parsed))
  where
    best = snd . minimumBy (comparing (rank . fst))
    record r = case Char8.break (== '\t') r of
      (meta, rest)
        | Just ('\t', path) <- Char8.uncons rest,
          [m, oid, stage] <- Char8.words meta,
          Just st <- readMaybe (Char8.unpack stage) :: Maybe Int,
          not (BS.null path) ->
            Right
              (st, StageEntry {path = path, mode = modeOf m, oid = ObjectId (ascii oid)})
      _ -> Left ("bad ls-files record: " <> Text.Encoding.decodeUtf8Lenient r)
    modeOf m
      | "100" `BS.isPrefixOf` m = ModeFile
      | m == "120000" = ModeSymlink
      | m == "160000" = ModeGitlink
      | otherwise = ModeOther m
    rank :: Int -> Int
    rank = \case
      0 -> 0
      2 -> 1
      3 -> 2
      _ -> 3
    ascii = Text.Encoding.decodeLatin1

-- | Parse @git ls-files -z --others --exclude-standard@.
parseOthers :: ByteString -> [OtherEntry]
parseOthers out = map entry (records out)
  where
    entry r = case Char8.unsnoc r of
      Just (dir, '/') -> OtherRepo dir
      _ -> OtherFile r

-- * Object ids and revisions

-- | A SHA-1 or SHA-256 id: 40 or 64 lowercase hex digits, nothing else.
parseObjectId :: ByteString -> Maybe ObjectId
parseObjectId b
  | BS.length b `elem` [40, 64],
    BS.all hex b =
      Just (ObjectId (Text.Encoding.decodeLatin1 b))
  | otherwise = Nothing
  where
    hex c = (c >= 48 && c <= 57) || (c >= 97 && c <= 102)

-- | One id per line, as @rev-parse@ and @merge-base --all@ print them.
-- Empty output is no id.
parseCommitIds :: ByteString -> Either Text [ObjectId]
parseCommitIds out = traverse one (filter (not . BS.null) (Char8.lines out))
  where
    one l = maybe (Left ("bad commit id: " <> lenient l)) Right (parseObjectId l)

-- | The hash a repo uses.
data ObjectFormat = Sha1 | Sha256
  deriving (Eq, Show)

-- | Whether the repo has all of its history.
data History = FullHistory | ShallowClone
  deriving (Eq, Show)

-- | What @rev-parse --show-prefix --show-object-format
-- --is-shallow-repository@ says, run in ROOT.
data RepoInfo = RepoInfo
  { -- | ROOT from the top of the work tree: empty at the top, else
    -- like @libs/a/@. Raw bytes.
    prefix :: ByteString,
    format :: ObjectFormat,
    history :: History
  }
  deriving (Eq, Show)

-- | Exactly three lines. A ROOT path with a newline gives more, and
-- fails: git has no @-z@ here (a known gap, D-311).
parseRepoInfo :: ByteString -> Either Text RepoInfo
parseRepoInfo out = case Char8.split '\n' (fromMaybe out (BS.stripSuffix "\n" out)) of
  [prefix, f, h] -> do
    format <- case f of
      "sha1" -> Right Sha1
      "sha256" -> Right Sha256
      _ -> Left ("unknown object format: " <> lenient f)
    history <- case h of
      "false" -> Right FullHistory
      "true" -> Right ShallowClone
      _ -> Left ("bad --is-shallow-repository answer: " <> lenient h)
    Right RepoInfo {prefix = prefix, format = format, history = history}
  _ -> Left ("bad rev-parse output: " <> lenient out)

-- * ls-tree

-- | One row of @ls-tree -r -z [--long] --full-tree@.
data TreeEntry = TreeEntry
  { -- | Raw bytes, from the top of the repo.
    path :: ByteString,
    item :: TreeItem
  }
  deriving (Eq, Show)

-- | What a tree row is, as far as we care.
data TreeItem
  = -- | Mode 100644 or 100755.
    ItemBlob Exec BlobItem
  | -- | Mode 120000. The blob holds the target.
    ItemSymlink BlobItem
  | -- | Mode 160000: a submodule, with its commit.
    ItemGitlink ObjectId
  | -- | Any other mode. Skipped.
    ItemOther ByteString
  deriving (Eq, Show)

-- | The owner's exec bit of a regular file: mode 100755 in git.
data Exec = NotExecutable | Executable
  deriving (Eq, Show)

-- | A file or symlink in a tree.
data BlobItem = BlobItem
  { oid :: ObjectId,
    size :: TreeSize
  }
  deriving (Eq, Show)

-- | The size column of @ls-tree --long@.
data TreeSize
  = SizeKnown FileSize
  | -- | @BAD@: the blob is not in the repo (a partial clone, git 2.56).
    SizeMissing
  | -- | Plain @ls-tree@, without @--long@.
    SizeNotListed
  deriving (Eq, Show)

-- | Parse @ls-tree -r -z@ with or without @--long@: records
-- @\<mode> \<type> \<oid>[ \<size>]\\t\<path>\\0@. The size is padded
-- with spaces; it is @-@ for a gitlink and @BAD@ for a missing blob.
-- The path is everything after the first TAB (it can hold a TAB or a
-- newline). Gives back the first bad record on failure.
parseTreeEntries :: ByteString -> Either Text [TreeEntry]
parseTreeEntries = traverse record . records
  where
    record r = case Char8.break (== '\t') r of
      (meta, rest)
        | Just ('\t', path) <- Char8.uncons rest,
          not (BS.null path),
          Just item <- itemOf (Char8.words meta) ->
            Right TreeEntry {path = path, item = item}
      _ -> Left ("bad ls-tree record: " <> lenient r)
    itemOf = \case
      [m, _, o] -> build m o Nothing
      [m, _, o, s] -> build m o (Just s)
      _ -> Nothing
    build m o size = do
      oid <- parseObjectId o
      case m of
        "100644" -> blob oid (ItemBlob NotExecutable) size
        "100755" -> blob oid (ItemBlob Executable) size
        "120000" -> blob oid ItemSymlink size
        "160000"
          | maybe True (== "-") size -> Just (ItemGitlink oid)
          | otherwise -> Nothing
        _ -> Just (ItemOther m)
    blob oid wrap size = do
      s <- case size of
        Nothing -> Just SizeNotListed
        Just "BAD" -> Just SizeMissing
        Just n -> SizeKnown . FileSize <$> decimal n
      Just (wrap BlobItem {oid = oid, size = s})

-- * cat-file

-- | The type of a git object.
data ObjectType = TypeBlob | TypeTree | TypeCommit | TypeTag
  deriving (Eq, Show)

-- | E.g. @blob@.
renderObjectType :: ObjectType -> Text
renderObjectType = \case
  TypeBlob -> "blob"
  TypeTree -> "tree"
  TypeCommit -> "commit"
  TypeTag -> "tag"

-- | The header line of a @cat-file --batch-command@ answer, default
-- format.
data CatFileHeader
  = HeaderFound FoundHeader
  | -- | @\<oid> missing@.
    HeaderMissing ObjectId
  deriving (Eq, Show)

-- | @\<oid> \<type> \<size>@.
data FoundHeader = FoundHeader
  { oid :: ObjectId,
    kind :: ObjectType,
    size :: FileSize
  }
  deriving (Eq, Show)

-- | Parse one header line, without its LF. Anything else, as
-- @\<oid> ambiguous@ or junk, is an error.
parseCatFileHeader :: ByteString -> Either Text CatFileHeader
parseCatFileHeader line = case Char8.split ' ' line of
  [o, "missing"] | Just oid <- parseObjectId o -> Right (HeaderMissing oid)
  [o, t, s]
    | Just oid <- parseObjectId o,
      Just kind <- typeOf t,
      Just size <- decimal s ->
        Right (HeaderFound FoundHeader {oid = oid, kind = kind, size = FileSize size})
  _ -> Left ("bad cat-file header: " <> lenient line)
  where
    typeOf = \case
      "blob" -> Just TypeBlob
      "tree" -> Just TypeTree
      "commit" -> Just TypeCommit
      "tag" -> Just TypeTag
      _ -> Nothing

-- * Internal

-- | NUL-terminated records, empty ones dropped.
records :: ByteString -> [ByteString]
records = filter (not . BS.null) . BS.split 0

-- | A non-negative decimal number, digits only.
decimal :: ByteString -> Maybe Int
decimal b
  | BS.null b || BS.length b > 18 || not (Char8.all isDigit b) = Nothing
  | otherwise = fst <$> Char8.readInt b

lenient :: ByteString -> Text
lenient = Text.Encoding.decodeUtf8Lenient
