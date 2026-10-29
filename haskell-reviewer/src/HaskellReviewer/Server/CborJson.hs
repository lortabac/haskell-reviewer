-- | CBOR bodies as JSON, for the fixtures and for `make cbor-json`.
-- See Note [CBOR bodies] in "HaskellReviewer.Server.Cbor".
module HaskellReviewer.Server.CborJson
  ( cborToJson,
    CborJsonError (..),
    renderCborJsonError,
  )
where

import Codec.CBOR.Read (DeserialiseFailure (..), deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text

-- | Why a body can't be dumped.
data CborJsonError
  = -- | Not CBOR, or cut short: the byte offset and cborg's message.
    BadCbor Int64 Text
  | -- | Bytes after the item.
    TrailingBytes
  | -- | An item our bodies never have (a float, a tag, ...).
    NotAllowed Text
  | -- | A map key that is not text.
    KeyNotText
  | -- | An integer the browser can't hold exactly (over 2^53 - 1).
    IntOutOfRange Integer
  deriving (Eq, Show)

-- | One line, for `make cbor-json`.
renderCborJsonError :: CborJsonError -> Text
renderCborJsonError = \case
  BadCbor at why -> "bad CBOR at byte " <> showText at <> ": " <> why
  TrailingBytes -> "bytes after the CBOR item"
  NotAllowed what -> "not in our bodies: " <> what
  KeyNotText -> "map key is not text"
  IntOutOfRange n -> "integer out of range: " <> showText n

-- | One CBOR item as compact JSON, map fields in the same order. Only
-- the items our bodies use: anything else, or bytes after the item,
-- is an error.
cborToJson :: LBS.ByteString -> Either CborJsonError LBS.ByteString
cborToJson bytes = case deserialiseFromBytes decodeTerm bytes of
  Left (DeserialiseFailure at why) -> Left (BadCbor at (Text.pack why))
  Right (rest, term)
    | not (LBS.null rest) -> Left TrailingBytes
    | otherwise -> Encoding.encodingToLazyByteString <$> toJson term

-- * Internal

toJson :: Term -> Either CborJsonError Encoding.Encoding
toJson = \case
  TInt n -> Encoding.integer <$> inRange (toInteger n)
  -- Out of Int's range, or a bignum (tags 2 and 3).
  TInteger n -> Encoding.integer <$> inRange n
  TString t -> Right (Encoding.text t)
  TBool b -> Right (Encoding.bool b)
  TNull -> Right Encoding.null_
  TList xs -> Encoding.list id <$> traverse toJson xs
  TMap kvs -> Encoding.pairs . mconcat <$> traverse pair kvs
  TBytes _ -> notAllowed "byte string"
  TBytesI _ -> notAllowed "byte string"
  TStringI _ -> notAllowed "text of unknown length"
  TListI _ -> notAllowed "array of unknown length"
  TMapI _ -> notAllowed "map of unknown length"
  TTagged n _ -> notAllowed ("tag " <> showText n)
  TSimple n -> notAllowed ("simple value " <> showText n)
  THalf _ -> notAllowed "float"
  TFloat _ -> notAllowed "float"
  TDouble _ -> notAllowed "float"
  where
    pair = \case
      (TString k, v) -> Encoding.pair (Key.fromText k) <$> toJson v
      _ -> Left KeyNotText
    notAllowed = Left . NotAllowed

-- | What a JS number holds exactly: the client's decoder takes no more.
inRange :: Integer -> Either CborJsonError Integer
inRange n
  | abs n <= 2 ^ (53 :: Int) - 1 = Right n
  | otherwise = Left (IntOutOfRange n)

showText :: (Show a) => a -> Text
showText = Text.pack . show
