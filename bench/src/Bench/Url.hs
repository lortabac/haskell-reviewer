-- | Percent-encoding for the URLs the bench asks for.
module Bench.Url
  ( -- * API paths
    modulePath,
    searchPath,

    -- * Encoding
    encodePath,
    encodeQueryValue,
  )
where

import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word8)
import Numeric (showHex)

-- | A module page (D-124), under the token.
modulePath :: Text -> String
modulePath ref = "api/module/tree/" <> encodePath ref

-- | A search (D-207), under the token.
searchPath :: Text -> String
searchPath q = "api/search/tree?q=" <> encodeQueryValue q

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
