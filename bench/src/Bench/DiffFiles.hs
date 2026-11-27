-- | Reads @api\/diff-files@ (D-325) and @git ls-tree -r -l -z@, and
-- picks the files the bench times on @api\/diff@ (D-358): modified and
-- renamed ones, largest first.
module Bench.DiffFiles
  ( -- * Types
    FileStatus (..),
    ChangedFile (..),
    DiffFilesAnswer (..),
    StatusCounts (..),
    BlobSizes (..),
    Candidate (..),
    DiffPick (..),
    DiffFilesError (..),
    LsTreeError (..),

    -- * Reading
    parseDiffFiles,
    statusCounts,
    statusText,
    parseLsTree,

    -- * Picking
    largestChanged,
    pickFiles,
    pickCount,
    askLimit,
  )
where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding

-- | A file's status in the pairing.
data FileStatus = Added | Removed | Modified | Renamed
  deriving (Eq, Show)

-- | One entry of @files@.
data ChangedFile = ChangedFile
  { path :: Text,
    -- | Renamed only.
    oldPath :: Maybe Text,
    status :: FileStatus
  }
  deriving (Eq, Show)

-- | The parts of the answer the bench keeps.
data DiffFilesAnswer = DiffFilesAnswer
  { files :: [ChangedFile],
    renameLimitHit :: Bool,
    truncated :: Bool,
    -- | Changes outside ROOT; 'Nothing' if the product doesn't know.
    outsideTotal :: Maybe Int
  }
  deriving (Eq, Show)

-- | How many files of each status.
data StatusCounts = StatusCounts
  { added :: Int,
    removed :: Int,
    modified :: Int,
    renamed :: Int
  }
  deriving (Eq, Show)

-- | Blob sizes by path, per side (from @ls-tree@).
data BlobSizes = BlobSizes
  { base :: Map Text Integer,
    head :: Map Text Integer
  }
  deriving (Eq, Show)

-- | A file to ask @api\/diff@ for, with its sizes from @ls-tree@.
data Candidate = Candidate
  { file :: ChangedFile,
    baseBytes :: Integer,
    headBytes :: Integer
  }
  deriving (Eq, Show)

-- | What the list session learns in diff mode.
data DiffPick = DiffPick
  { answer :: DiffFilesAnswer,
    -- | Modified and renamed files.
    candidates :: Int,
    -- | How many @api\/diff@ requests the pick took.
    asked :: Int,
    -- | Largest first; up to 'pickCount'.
    picked :: [Candidate]
  }
  deriving (Eq, Show)

-- | Not a diff-files answer: aeson's message.
newtype DiffFilesError = DiffFilesError String
  deriving (Eq, Show)

-- | A line of @ls-tree@ output we can't read.
newtype LsTreeError = LsTreeError ByteString
  deriving (Eq, Show)

-- | Parse the JSON body.
parseDiffFiles :: ByteString -> Either DiffFilesError DiffFilesAnswer
parseDiffFiles body = either (Left . DiffFilesError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "diff-files" $ \o -> do
      fs <- o .: "files" >>= mapM file
      lim <- o .: "renameLimitHit"
      cut <- o .: "truncated"
      outside <- o .:? "outside"
      total <- traverse (Aeson.withObject "outside" (.: "total")) (outside >>= nonNull)
      pure
        DiffFilesAnswer
          { files = fs,
            renameLimitHit = lim,
            truncated = cut,
            outsideTotal = total
          }
    nonNull Aeson.Null = Nothing
    nonNull x = Just x
    file = Aeson.withObject "file" $ \o -> do
      p <- o .: "path"
      old <- o .:? "oldPath"
      st <- o .: "status" >>= status
      pure ChangedFile {path = p, oldPath = old, status = st}
    status :: Text -> Aeson.Types.Parser FileStatus
    status = \case
      "added" -> pure Added
      "removed" -> pure Removed
      "modified" -> pure Modified
      "renamed" -> pure Renamed
      other -> fail ("unknown status: " <> Text.unpack other)

-- | Count the files by status.
statusCounts :: [ChangedFile] -> StatusCounts
statusCounts fs =
  StatusCounts
    { added = count Added,
      removed = count Removed,
      modified = count Modified,
      renamed = count Renamed
    }
  where
    count s = length [() | f <- fs, f.status == s]

-- | The name in the JSON.
statusText :: FileStatus -> Text
statusText Added = "added"
statusText Removed = "removed"
statusText Modified = "modified"
statusText Renamed = "renamed"

-- | Blob sizes by path from @git ls-tree -r -l -z@: lines
-- @MODE TYPE OBJECT SIZE\\tPATH@, NUL-ended, the size padded with
-- spaces. A submodule's size is @-@: left out.
parseLsTree :: ByteString -> Either LsTreeError (Map Text Integer)
parseLsTree out =
  Map.fromList . concat <$> mapM one (filter (not . BS.null) (BS.split 0 out))
  where
    one l = case Char8.break (== '\t') l of
      (meta, rest) | Just (_, p) <- Char8.uncons rest -> case Char8.words meta of
        [_, _, _, "-"] -> Right []
        [_, _, _, size] -> case Char8.readInteger size of
          Just (n, "") -> Right [(Text.Encoding.decodeUtf8Lenient p, n)]
          _ -> bad l
        _ -> bad l
      _ -> bad l
    bad l = Left (LsTreeError l)

-- | Modified and renamed files (both sides have content; an added or
-- removed file's diff is all inserts or all deletes), by base + head
-- bytes, largest first, ties by path. A renamed file's base size is
-- its old path's. A path missing from a side counts 0.
largestChanged :: BlobSizes -> [ChangedFile] -> [Candidate]
largestChanged sizes fs =
  sortOn
    (\c -> (Down (c.baseBytes + c.headBytes), c.file.path))
    [ Candidate
        { file = f,
          baseBytes = size sizes.base (fromMaybe f.path f.oldPath),
          headBytes = size sizes.head f.path
        }
    | f <- fs,
      f.status `elem` [Modified, Renamed]
    ]
  where
    size m p = Map.findWithDefault 0 p m

-- | How many files the @diff@ phase asks for.
pickCount :: Int
pickCount = 10

-- | Stop picking after this many asks.
askLimit :: Int
askLimit = 40

-- | Ask for each candidate in order; keep the first 'pickCount' that
-- pass, up to 'askLimit' asks. Also says how many it asked. With
-- "Bench.DiffAnswer"'s 'worthTiming', a file over the product's text
-- limit drops out by the product's own rule.
pickFiles :: (Monad m) => (Candidate -> m Bool) -> [Candidate] -> m ([Candidate], Int)
pickFiles keep = go [] 0
  where
    go kept n cs
      | length kept >= pickCount || n >= askLimit = pure (reverse kept, n)
      | otherwise = case cs of
          [] -> pure (reverse kept, n)
          c : rest -> do
            ok <- keep c
            go (if ok then c : kept else kept) (n + 1) rest
