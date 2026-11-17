-- | Git blob ids in process, and the EOL part of git's clean step.
module BlobId
  ( ObjectFormat (..),
    parseObjectFormat,
    blobId,
    blobIdWith,
    Eol (..),
    eolFor,
    cleanEol,
  )
where

import Crypto.Hash (Digest, SHA1, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data ObjectFormat = Sha1 | Sha256
  deriving (Eq, Show)

parseObjectFormat :: ByteString -> Maybe ObjectFormat
parseObjectFormat s = case BC.strip s of
  "sha1" -> Just Sha1
  "sha256" -> Just Sha256
  _ -> Nothing

-- | Hex id of @blob <len>\0<bytes>@.
blobId :: ObjectFormat -> ByteString -> ByteString
blobId fmt bytes = blobIdWith fmt [header, bytes]
  where
    header = "blob " <> BC.pack (show (BS.length bytes)) <> "\0"

blobIdWith :: ObjectFormat -> [ByteString] -> ByteString
blobIdWith Sha1 chunks = BC.pack (show (hashFinalize (hashUpdates hashInit chunks) :: Digest SHA1))
blobIdWith Sha256 chunks = BC.pack (show (hashFinalize (hashUpdates hashInit chunks) :: Digest SHA256))

-- | What git's clean step does to line ends, from check-attr and
-- core.autocrlf. Minimal: enough for the spike.
data Eol
  = -- | No change.
    EolNone
  | -- | CRLF -> LF, always (@text@, @eol=...@).
    EolText
  | -- | @text=auto@ (or core.autocrlf): CRLF -> LF if the file looks
    -- like text and the index blob has no CR. We can't see the index
    -- blob cheaply, so the caller accepts either hash (see 'cleanEol').
    EolAuto
  deriving (Eq, Show)

-- | From the check-attr values (@set@, @unset@, @unspecified@ or a
-- value) and whether core.autocrlf converts on add (true or input).
-- @text@ and @crlf@ are read the same way, as git does.
eolFor :: Map ByteString ByteString -> Bool -> Eol
eolFor attrs autocrlf =
  case (textLike (get "text"), textLike (get "crlf"), get "eol") of
    (Just t, _, _) -> t
    (_, Just t, _) -> t
    (_, _, e) | e `elem` ["lf", "crlf"] -> EolText
    _ -> if autocrlf then EolAuto else EolNone
  where
    get k = Map.findWithDefault "unspecified" k attrs
    textLike = \case
      "set" -> Just EolText
      "input" -> Just EolText
      "unset" -> Just EolNone
      "auto" -> Just EolAuto
      _ -> Nothing

-- | The contents git would hash, for the EOL rule. For 'EolAuto' both
-- the raw and the converted bytes can be right: raw if the index blob
-- has CRLF (then git doesn't convert), converted if not. A match on
-- either is the answer git gives.
cleanEol :: Eol -> ByteString -> [ByteString]
cleanEol EolNone b = [b]
cleanEol EolText b = [crlfToLf b]
cleanEol EolAuto b
  | looksBinary b || not (hasCrlf b) = [b]
  | otherwise = [b, crlfToLf b]

crlfToLf :: ByteString -> ByteString
crlfToLf b = case BS.breakSubstring "\r\n" b of
  (pre, rest)
    | BS.null rest -> pre
    | otherwise -> pre <> "\n" <> crlfToLf (BS.drop 2 rest)

hasCrlf :: ByteString -> Bool
hasCrlf b = not (BS.null (snd (BS.breakSubstring "\r\n" b)))

-- | git's rule, roughly: a NUL or a lone CR means binary.
looksBinary :: ByteString -> Bool
looksBinary b = BS.elem 0 b || loneCr b
  where
    loneCr s = case BS.elemIndex 13 s of
      Nothing -> False
      Just i
        | i + 1 >= BS.length s -> True
        | BS.index s (i + 1) /= 10 -> True
        | otherwise -> loneCr (BS.drop (i + 1) s)
