-- | Percent-encoding for the URLs the bench asks for.
module Bench.Url
  ( -- * API paths
    modulesPath,
    modulePath,
    sourcePath,
    searchPath,
    refsPath,
    textSearchPath,
    indexingPath,
    filesPath,
    sessionPath,
    diffFilesPath,
    diffPath,

    -- * Encoding
    encodePath,
    encodeQueryValue,
  )
where

import Bench.Mode (Side, sideText)
import Bench.Queries (RefsQuery (..))
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word8)
import Numeric (showHex)

-- | The module tree of a side (D-124), under the token. Like every
-- path here: no leading @/@.
modulesPath :: Side -> String
modulesPath side = "api/modules/" <> seg side

-- | A module page (D-124).
modulePath :: Side -> Text -> String
modulePath side ref = "api/module/" <> seg side <> "/" <> encodePath ref

-- | A source page, by file path (relative to the root).
sourcePath :: Side -> FilePath -> String
sourcePath side p = "api/source/" <> seg side <> "/" <> encodePath (Text.pack p)

-- | A search (D-207).
searchPath :: Side -> Text -> String
searchPath side q = "api/search/" <> seg side <> "?q=" <> encodeQueryValue q

-- | Find references (D-294).
refsPath :: Side -> RefsQuery -> String
refsPath side q =
  "api/refs/"
    <> seg side
    <> "?module="
    <> encodeQueryValue q.moduleRef
    <> "&decl="
    <> encodeQueryValue q.decl

-- | A text search (D-307) with a limit.
textSearchPath :: Side -> Text -> Int -> String
textSearchPath side q limit =
  "api/text-search/"
    <> seg side
    <> "?q="
    <> encodeQueryValue q
    <> "&limit="
    <> show limit

-- | How far indexing is on a side (D-160).
indexingPath :: Side -> String
indexingPath side = "api/indexing/" <> seg side

-- | The file list of a side.
filesPath :: Side -> String
filesPath side = "api/files/" <> seg side

-- | The session: browse or diff, and the commits (D-332).
sessionPath :: String
sessionPath = "api/session"

-- | The changed files, diff mode only (D-325).
diffFilesPath :: String
diffFilesPath = "api/diff-files"

-- | One file's diff, by its head path (or base path if removed; D-331).
-- Plain: no @ws=ignore@.
diffPath :: Text -> String
diffPath p = "api/diff/" <> encodePath p

-- | A side as a path segment.
seg :: Side -> String
seg = Text.unpack . sideText

-- | A path (a module ref, say), as UTF-8 with every byte but the
-- unreserved ones and @/@ encoded. @/@ stays: it splits segments, as a
-- module ref that is a path needs (D-124).
encodePath :: Text -> String
encodePath = encodeExcept (== slash)
  where
    slash = 0x2F

-- | A query value: everything but the unreserved bytes encoded, so @+@
-- is @%2B@ and a space is @%20@.
encodeQueryValue :: Text -> String
encodeQueryValue = encodeExcept (const False)

encodeExcept :: (Word8 -> Bool) -> Text -> String
encodeExcept keep = concatMap one . BS.unpack . Text.Encoding.encodeUtf8
  where
    one b
      | unreserved b || keep b = [chr (fromIntegral b)]
      | otherwise = '%' : hex b
    hex b = case showHex b "" of
      [c] -> ['0', upper c]
      cs -> map upper cs
    upper c = if c >= 'a' && c <= 'f' then chr (fromEnum c - 32) else c
    unreserved b =
      (b >= 0x41 && b <= 0x5A) -- A-Z
        || (b >= 0x61 && b <= 0x7A) -- a-z
        || (b >= 0x30 && b <= 0x39) -- 0-9
        || b `elem` [0x2D, 0x2E, 0x5F, 0x7E] -- - . _ ~
