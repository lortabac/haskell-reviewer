-- | CBOR bodies and their JSON dump.
module HaskellReviewer.Server.CborTest (tests) where

import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as Encoding
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word64)
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.CborJson (CborJsonError (..), cborToJson)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Cbor"
    [ testCase "every value we write, dumped as JSON in order" $
        dump
          ( Cbor.obj
              [ ("z", Cbor.int 1),
                ("a", Cbor.text "é \"x\" \n"),
                ("list", Cbor.list Cbor.bool [True, False]),
                ("none", Cbor.null_),
                ("maybe", Cbor.nullable Cbor.int (Just (-3))),
                ("empty", Cbor.obj []),
                ("nested", Cbor.list (Cbor.list Cbor.int) [[], [0]]),
                ("generation", Cbor.word64 7)
              ]
          )
          @?= Right
            -- é in UTF-8.
            "{\"z\":1,\"a\":\"\195\169 \\\"x\\\" \\n\",\"list\":[true,false],\
            \\"none\":null,\"maybe\":-3,\"empty\":{},\"nested\":[[],[0]],\
            \\"generation\":7}",
      testCase "ints: only what a JS number holds exactly" $ do
        dump (Cbor.int safe) @?= Right "9007199254740991"
        dump (Cbor.int (-safe)) @?= Right "-9007199254740991"
        dump (Cbor.int (safe + 1)) @?= Left (IntOutOfRange (toInteger safe + 1))
        dump (Cbor.int (-safe - 1)) @?= Left (IntOutOfRange (-(toInteger safe) - 1))
        dump (Cbor.int maxBound) @?= Left (IntOutOfRange (toInteger (maxBound :: Int)))
        dump (Cbor.int minBound) @?= Left (IntOutOfRange (toInteger (minBound :: Int)))
        dump (Cbor.word64 maxBound)
          @?= Left (IntOutOfRange (toInteger (maxBound :: Word64)))
        -- Bignums (tags 2 and 3).
        dump (Encoding.encodeInteger big) @?= Left (IntOutOfRange big)
        dump (Encoding.encodeInteger (-big)) @?= Left (IntOutOfRange (-big)),
      testCase "the bytes are the smallest form" $ do
        Cbor.cborBody (Cbor.obj [("a", Cbor.int 1)]) @?= "\xa1\x61\x61\x01"
        Cbor.cborBody (Cbor.list Cbor.int []) @?= "\x80",
      testCase "rejects what our bodies never have" $
        mapM_
          (\(what :: String, e, err) -> (what, dump e) @?= (what, Left err))
          [ ("float", Encoding.encodeDouble 1.5, NotAllowed "float"),
            ("half float", Encoding.encodeFloat16 1, NotAllowed "float"),
            ("tag", Encoding.encodeTag 1 <> Encoding.encodeInt 0, NotAllowed "tag 1"),
            ("bytes", Encoding.encodeBytes "x", NotAllowed "byte string"),
            ("undefined", Encoding.encodeUndef, NotAllowed "simple value 23"),
            ("simple", Encoding.encodeSimple 7, NotAllowed "simple value 7"),
            ("int key", Encoding.encodeMapLen 1 <> Cbor.int 1 <> Cbor.int 1, KeyNotText),
            ( "open list",
              Encoding.encodeListLenIndef <> Encoding.encodeBreak,
              NotAllowed "array of unknown length"
            ),
            ( "open map",
              Encoding.encodeMapLenIndef <> Encoding.encodeBreak,
              NotAllowed "map of unknown length"
            ),
            ( "open text",
              Encoding.encodeStringIndef <> Cbor.text "x" <> Encoding.encodeBreak,
              NotAllowed "text of unknown length"
            ),
            ( "float inside",
              Cbor.obj [("a", Cbor.list id [Encoding.encodeFloat 1])],
              NotAllowed "float"
            )
          ],
      testCase "rejects bytes after the item, and cut or empty input" $ do
        cborToJson (bytes (Cbor.int 1) <> "\x01") @?= Left TrailingBytes
        cborToJson (bytes (Cbor.int 1 <> Cbor.int 2)) @?= Left TrailingBytes
        assertBool "cut" (isBad (cborToJson (LBS.take 3 (bytes (Cbor.text "hello")))))
        assertBool "empty" (isBad (cborToJson ""))
    ]
  where
    dump = cborToJson . bytes
    safe = 2 ^ (53 :: Int) - 1
    big = 2 ^ (64 :: Int)
    isBad = \case
      Left (BadCbor _ _) -> True
      _ -> False

bytes :: Encoding -> LBS.ByteString
bytes = toLazyByteString
