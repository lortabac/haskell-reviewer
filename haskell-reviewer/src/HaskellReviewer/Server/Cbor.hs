-- | Helpers to write CBOR bodies by hand (D-51), like the JSON ones in
-- "HaskellReviewer.Server.Wire". See Note [CBOR bodies].
module HaskellReviewer.Server.Cbor
  ( -- * Values
    obj,
    list,
    text,
    int,
    word64,
    bool,
    null_,
    nullable,

    -- * Bodies
    cborBody,
  )
where

import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as Cbor
import Codec.CBOR.Write qualified as Cbor
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Word (Word64)

{- Note [CBOR bodies]

We only write the CBOR items that have a JSON twin: maps with text
keys, arrays, text, ints, bools and null. Ints stay within ±(2^53 - 1),
what a JS number holds exactly (the generation is small). Maps and arrays have a fixed
length, and map fields keep the order we give, like the JSON bodies.
So every body can be dumped as JSON with no loss
("HaskellReviewer.Server.CborJson"): the fixtures have such a dump
next to them, and `make cbor-json` turns a response into JSON by hand.

No floats, tags, byte strings, undefined or bigger ints: the dump
rejects them, and the client's decoder too.
-}

-- | A map with these text keys, in this order.
obj :: [(Text, Encoding)] -> Encoding
obj fields =
  Cbor.encodeMapLen (fromIntegral (length fields))
    <> foldMap (\(k, v) -> Cbor.encodeString k <> v) fields

-- | An array.
list :: (a -> Encoding) -> [a] -> Encoding
list f xs = Cbor.encodeListLen (fromIntegral (length xs)) <> foldMap f xs

-- | A text string.
text :: Text -> Encoding
text = Cbor.encodeString

-- | An int, as small as CBOR allows.
int :: Int -> Encoding
int = Cbor.encodeInt

-- | Like 'int', for the generation.
word64 :: Word64 -> Encoding
word64 = Cbor.encodeWord64

-- | true or false.
bool :: Bool -> Encoding
bool = Cbor.encodeBool

-- | null.
null_ :: Encoding
null_ = Cbor.encodeNull

-- | The value, or null.
nullable :: (a -> Encoding) -> Maybe a -> Encoding
nullable = maybe null_

-- | The bytes of a whole body.
cborBody :: Encoding -> LBS.ByteString
cborBody = Cbor.toLazyByteString
