module Bench.DiffAnswerTest (tests) where

import Bench.DiffAnswer (DiffKind (..), parseDiffAnswer, worthTiming)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import HaskellReviewer.Server.Cbor (cborBody, int, list, obj, text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Tests run from the package dir.
fixture :: FilePath -> IO DiffKind
fixture name = do
  body <- BS.readFile ("../fixtures/api/" <> name)
  either (assertFailure . show) pure (parseDiffAnswer body)

tests :: TestTree
tests =
  testGroup
    "Bench.DiffAnswer"
    [ testCase "a line diff" $ do
        fixture "diff-modified.cbor" >>= (@?= LineDiff)
        fixture "diff-renamed.cbor" >>= (@?= LineDiff),
      testCase "only equal runs: not a real line diff" $ do
        fixture "diff-unchanged.cbor" >>= (@?= EqualLines)
        -- Whitespace-only, asked with ws=ignore: all equal runs.
        fixture "diff-ws-ignore.cbor" >>= (@?= EqualLines)
        -- An exact rename, as the bench sees it (plain).
        parseDiffAnswer (LBS.toStrict exactRename) @?= Right EqualLines,
      testCase "too slow" $
        fixture "diff-too-slow.cbor" >>= (@?= DiffTooSlow),
      testCase "too large and binary: not text" $ do
        fixture "diff-too-large.cbor" >>= (@?= DiffNotText)
        fixture "diff-binary.cbor" >>= (@?= DiffNotText),
      testCase "what goes into the sample" $
        map worthTiming [LineDiff, DiffTooSlow, EqualLines, DiffNotText]
          @?= [True, True, False, False],
      testCase "not CBOR: an error" $
        assertBool "error" (isLeft (parseDiffAnswer "{}"))
    ]
  where
    -- Only the keys the bench reads.
    exactRename =
      cborBody . obj $
        [ ("status", text "renamed"),
          ("diff", obj [("kind", text "lines"), ("runs", list run [("equal", 3)])])
        ]
    run (op, n) = list id [text op, int n]
