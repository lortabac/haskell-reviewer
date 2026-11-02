module Bench.FallbacksTest (tests) where

import Bench.Fallbacks
  ( Category (..),
    Diff (..),
    Expected (..),
    FallbackVerdict (..),
    FallbacksError (..),
    Hit (..),
    Lookup (..),
    NotChecked (..),
    PageError (..),
    Slot (..),
    Strictness (..),
    classifyPage,
    compareCensus,
    expectedTextOnly,
    failsBench,
    fallbackVerdict,
    lookupExpected,
    parseFallbacksFile,
    renderDiff,
  )
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.String (IsString, fromString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | Classify a module page fixture (tests run from the package dir).
classifyFixture :: String -> Slot -> IO (Maybe Category)
classifyFixture name slot = do
  body <- BS.readFile ("../fixtures/api/module-" <> name <> ".cbor.json")
  v <- either assertFailure pure (Aeson.eitherDecodeStrict body)
  either (assertFailure . show) (pure . fmap fst) (classifyPage "X" slot v)

shaA, shaB :: (IsString s) => s
shaA = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
shaB = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

tests :: TestTree
tests =
  testGroup
    "Bench.Fallbacks"
    [ testCase "classifier on the module page fixtures" $ do
        let cases =
              [ ("parsed", HasSlot, Nothing),
                ("cpp", HasSlot, Nothing),
                ("parse-error", HasSlot, Just ParseError),
                ("pragma-error", HasSlot, Just PragmaError),
                ("crashed", HasSlot, Just ParserCrashed),
                ("too-slow", HasSlot, Just TooSlow),
                ("preprocessed", HasSlot, Just Preprocessed),
                ("preprocessed", NoSlot, Just PreprocessedByFlags),
                ("generated", NoSlot, Just Generated),
                ("no-source", NoSlot, Just NoSourceMissing),
                ("not-text", HasSlot, Just NotText),
                ("too-large", HasSlot, Just TooLarge)
              ]
        mapM_ (\(n, slot, want) -> (n,) <$> classifyFixture n slot >>= (@?= (n, want))) cases,
      testCase "the detail is the message" $ do
        body <- BS.readFile "../fixtures/api/module-parse-error.cbor.json"
        v <- either assertFailure pure (Aeson.eitherDecodeStrict body)
        classifyPage "M" HasSlot v
          @?= Right (Just (ParseError, Hit {ref = "M", detail = "parse error on input `='"})),
      testCase "file: rows, comments, zero counts dropped" $
        parseFallbacksFile
          ( "# comment\n\nwire-server "
              <> shaA
              <> " preprocessed=11 generated=1 no-source/autogen=85 too-slow=0\n"
          )
          @?= Right
            [ Expected
                { name = "wire-server",
                  sha = shaA,
                  counts =
                    Map.fromList [(Preprocessed, 11), (Generated, 1), (NoSourceAutogen, 85)]
                }
            ],
      testCase "file: bad lines" $ do
        parseFallbacksFile "p notasha\n" @?= Left (BadFallbacksLine 1 "bad sha: notasha")
        parseFallbacksFile ("p " <> shaA <> " nope=1\n")
          @?= Left (BadFallbacksLine 1 "bad category=count: nope=1")
        parseFallbacksFile ("p " <> shaA <> " generated=-1\n")
          @?= Left (BadFallbacksLine 1 "bad category=count: generated=-1")
        parseFallbacksFile ("p " <> shaA <> " generated=1 generated=2\n")
          @?= Left (BadFallbacksLine 1 "a category twice")
        parseFallbacksFile "p\n"
          @?= Left (BadFallbacksLine 1 "expected: name sha category=count ..."),
      testCase "file: the same row twice" $
        parseFallbacksFile ("p " <> shaA <> "\np " <> shaA <> " generated=1\n")
          @?= Left (DuplicateRow "p" shaA),
      testCase "lookup by name and sha" $ do
        let rows = [Expected {name = "p", sha = shaA, counts = Map.empty}]
        lookupExpected "p" shaA rows @?= Found (Expected "p" shaA Map.empty)
        lookupExpected "p" shaB rows @?= OtherShas [shaA]
        lookupExpected "q" shaA rows @?= NoRow,
      testCase "compare: only the categories that differ" $ do
        let census = Map.fromList [(Generated, [Hit "G" "alex"]), (TooSlow, [Hit "S" ""])]
            expected = Map.fromList [(Generated, 1), (Preprocessed, 2)]
        compareCensus expected census
          @?= [ Diff {category = TooSlow, expected = 0, got = 1, hits = [Hit "S" ""]},
                Diff {category = Preprocessed, expected = 2, got = 0, hits = []}
              ],
      testCase "diff rendering: count, modules, the rest counted" $
        renderDiff
          Diff
            { category = ParseError,
              expected = 0,
              got = 7,
              hits = [Hit (pack' i) (detailOf i) | i <- [1 .. 7 :: Int]]
            }
          @?= [ "parse-error: expected 0, got 7",
                "  M1: boom",
                "  M2",
                "  M3",
                "  M4",
                "  M5",
                "  ... 2 more"
              ],
      testCase "too-slow says it depends on load" $
        take 1 (renderDiff (Diff TooSlow 0 1 [Hit "S" ""]))
          @?= ["too-slow: expected 0, got 1 (depends on load)"],
      testCase "not a module page" $
        classifyPage "M" HasSlot (Aeson.object [])
          @?= Left (PageError "Error in $: key \"parse\" not found"),
      testCase "verdict: row lookup comes first" $ do
        fallbackVerdict Nothing NoRow [] @?= NotCheckedBecause NoRowForProject
        fallbackVerdict (Just Map.empty) (OtherShas [shaA]) []
          @?= NotCheckedBecause (OtherShasOnly [shaA])
        fallbackVerdict Nothing found [] @?= NotCheckedBecause NoCensusRun,
      testCase "verdict: too-slow only vs any other category" $ do
        fallbackVerdict (Just Map.empty) found [] @?= AsExpected
        fallbackVerdict (Just Map.empty) found [Diff TooSlow 0 1 []] @?= TooSlowDiffers
        fallbackVerdict (Just Map.empty) found [Diff TooSlow 0 1 [], Diff Generated 1 0 []]
          @?= Mismatch,
      testCase "exit 1: mismatch always, a missing or stale row only if required" $ do
        failsBench RowOptional Mismatch @?= True
        failsBench RowRequired TooSlowDiffers @?= False
        failsBench RowRequired (NotCheckedBecause NoRowForProject) @?= True
        failsBench RowOptional (NotCheckedBecause NoRowForProject) @?= False
        failsBench RowRequired (NotCheckedBecause (OtherShasOnly [shaA])) @?= True
        failsBench RowOptional (NotCheckedBecause (OtherShasOnly [shaA])) @?= False
        failsBench RowRequired (NotCheckedBecause NoCensusRun) @?= False,
      testCase "expected text only: slots only" $
        expectedTextOnly
          ( Map.fromList
              [(Preprocessed, 11), (Generated, 1), (NoSourceAutogen, 85), (ParseError, 2)]
          )
          @?= 13
    ]
  where
    found = Found Expected {name = "p", sha = shaA, counts = Map.empty}
    detailOf i = if i == 1 then "boom\nmore" else ""
    pack' i = "M" <> fromString (show i)
