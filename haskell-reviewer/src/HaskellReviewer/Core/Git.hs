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
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.Foldable (minimumBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Text.Read (readMaybe)

-- | A git version, e.g. 2.44.0.
data GitVersion = GitVersion
  { major :: Int,
    minor :: Int,
    patch :: Int
  }
  deriving (Eq, Ord, Show)

-- | We need 2.44 for GIT_NO_LAZY_FETCH (SPEC §11.4.1).
minimumGitVersion :: GitVersion
minimumGitVersion = GitVersion 2 44 0

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

-- | A git object id, in hex.
newtype ObjectId = ObjectId Text
  deriving (Eq, Show)

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

-- * Internal

-- | NUL-terminated records, empty ones dropped.
records :: ByteString -> [ByteString]
records = filter (not . BS.null) . BS.split 0
