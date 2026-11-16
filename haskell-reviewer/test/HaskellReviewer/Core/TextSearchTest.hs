-- | Text search, pure. See Note [Text search] in
-- "HaskellReviewer.Core.TextSearch".
module HaskellReviewer.Core.TextSearchTest (tests) where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.References (lineCap)
import HaskellReviewer.Core.RelPath (RelPath, relPathFromBytes, relPathText)
import HaskellReviewer.Core.TextSearch
  ( FileHits (..),
    LineHit (..),
    Match (..),
    Needle,
    TextAnswer (..),
    TextHit (..),
    forceTextAnswer,
    needle,
    searchFile,
    searchTexts,
    spansPerLine,
    textResultsLimit,
  )
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, testCaseInfo, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup
    "Core.TextSearch"
    [ testProperty "same hits as a plain Text search" sameAsNaive,
      testProperty "a limit keeps the first results, the counts stay" limitKeepsFirst,
      testCase "no needle: empty, a line break" $ do
        assertBool "empty" (isNothing (needle ""))
        assertBool "\\n" (isNothing (needle "a\nb"))
        assertBool "\\r" (isNothing (needle "a\r")),
      testCase "case-sensitive" $ do
        hits "main" "Main main MAIN\n" @?= [(1, [(6, 10)])]
        hits "Ü" "ü Ü\n" @?= [(1, [(3, 4)])],
      testCase "columns in code points" $
        hits "main" "naïve λ main\n" @?= [(1, [(9, 13)])],
      testCase "a BOM is skipped, not counted on line 1" $ do
        hits "main" "\xFEFFmain\nmain\n" @?= [(1, [(1, 5)]), (2, [(1, 5)])]
        hits "\xFEFF" "\xFEFFa\n" @?= []
        hits "\xFEFF" "a\xFEFF\n" @?= [(1, [(2, 3)])],
      testCase "CRLF: the \\r isn't in the line" $ do
        h <- single (full "b" "a\r\nab\r\n")
        (h.line, h.text, cols h) @?= (2, "ab", [(2, 3)])
        hits "a b" "a\rb a b\r\n" @?= [(1, [(5, 8)])],
      testCase "a lone \\r is a code point" $
        hits "b" "a\rb\n" @?= [(1, [(3, 4)])],
      testCase "no overlap" $ do
        hits "aa" "aaa\n" @?= [(1, [(1, 3)])]
        hits "aa" "aaaa\n" @?= [(1, [(1, 3), (3, 5)])],
      testCase "inner quotes and spaces" $
        hits "a \"b" "x a \"b\" y\n" @?= [(1, [(3, 7)])],
      testCase "several hits on a line, and on several lines" $
        hits "ab" "ab ab\nx\nab\n" @?= [(1, [(1, 3), (4, 6)]), (3, [(1, 3)])],
      testCase "at most 20 spans per line, the line counts once" $ do
        let line = Text.unwords (replicate 25 "x")
        h <- single (full "x" line)
        length h.matches @?= spansPerLine
        lineCount "x" line @?= 1,
      testCase "no ASCII byte (the slow path)" $ do
        hits "λλ" "λ λλλ λλ\n" @?= [(1, [(3, 5), (7, 9)])]
        hits "ü" "a\nü ü\n" @?= [(2, [(1, 2), (3, 4)])],
      testCase "an anchor in the middle of the needle" $
        hits "eeeqee" "eeeeqeeeqee\n" @?= [(1, [(2, 8)])],
      testCase "no hit" $ do
        hits "zz" "" @?= []
        hits "zz" "z z\n" @?= [],
      testCase "a long line: 240 code points around the hit" $ do
        let line = Text.replicate 1000 "." <> "main" <> Text.replicate 1000 "."
        h <- single (full "main" line)
        Text.length h.text @?= lineCap
        cols h @?= [(1001, 1005)]
        assertBool "the hit is in the window" $
          h.lineCol <= 1001 && 1005 <= h.lineCol + lineCap
        Text.take 4 (Text.drop (1001 - h.lineCol) h.text) @?= "main",
      testCase "a long line, hit near the start or the end" $ do
        let line = "main" <> Text.replicate 1000 "." <> "end"
        start <- single (full "main" line)
        end <- single (full "end" line)
        (start.lineCol, Text.take 4 start.text) @?= (1, "main")
        end.lineCol @?= 1008 - lineCap
        Text.takeEnd 3 end.text @?= "end",
      testCase "wanted lines, the rest only counted" $ do
        let FileHits {lineCount = n, lines = ls} =
              searchFile (needleOf "a") 2 (utf8 "a\nb\na\na a\n")
        n @?= 3
        map (.line) ls @?= [1, 3],
      testCase "limit, total, files, path order" $ do
        let a =
              searchTexts
                (const True)
                (needleOf "x")
                3
                [ (path "a.txt", "x\nx\n"),
                  (path "b.txt", "no\n"),
                  (path "c.txt", "x\nx\n")
                ]
        (a.total, a.files, a.stopped) @?= (4, 2, False)
        [(r.path, r.hit.line) | r <- a.results]
          @?= [(path "a.txt", 1), (path "a.txt", 2), (path "c.txt", 1)],
      testCase "the path filter leaves files out of every count" $ do
        let files = [(path p, "x\n") | p <- ["a.hs", "b.md", "c.hs"]]
            onlyHs p = ".hs" `Text.isSuffixOf` relPathText p
            a = searchTexts onlyHs (needleOf "x") 10 files
        (a.total, a.files) @?= (2, 2)
        [relPathText r.path | r <- a.results] @?= ["a.hs", "c.hs"],
      testCase "limit: at most 1000" $ do
        let file = utf8 (Text.replicate 1500 "x\n")
            a = searchTexts (const True) (needleOf "x") 5000 [(path "a.txt", file)]
        (a.total, length a.results) @?= (1500, textResultsLimit),
      timing
    ]

-- * Properties

sameAsNaive :: Property
sameAsNaive = property $ do
  q <- forAll genNeedle
  file <- forAll genFile
  case needle q of
    Nothing -> Text.any (`elem` ['\n', '\r']) q === True
    Just n -> do
      let FileHits {lineCount = count, lines = ls} = searchFile n maxBound (utf8 file)
          expected = naive q file
      [(h.line, cols h) | h <- ls]
        === [(i, take spansPerLine ms) | (i, ms, _) <- expected]
      [(h.lineCol, h.text) | h <- ls] === [(1, l) | (_, _, l) <- expected]
      count === length expected

limitKeepsFirst :: Property
limitKeepsFirst = property $ do
  q <- forAll (Gen.element ["a", "ab", "λ", " "])
  fs <- forAll (Gen.list (Range.linear 0 5) genFile)
  k <- forAll (Gen.int (Range.linear 0 20))
  let n = needleOf q
      files =
        [ (path (utf8 (Text.pack (show i) <> ".txt")), utf8 f)
        | (i, f) <- zip [1 :: Int ..] fs
        ]
      everything = searchTexts (const True) n maxBound files
      capped = searchTexts (const True) n k files
  capped.results === take k everything.results
  (capped.total, capped.files) === (everything.total, everything.files)

-- | Every line with its hits (non-overlapping, columns in code points),
-- by sourceLines' rules.
naive :: Text -> Text -> [(Int, [(Int, Int)], Text)]
naive q file =
  [ (i, spans, l)
  | (i, l) <- zip [1 ..] (map noCr (Text.splitOn "\n" (noBom file))),
    let spans =
          [ (Text.length before + 1, Text.length before + 1 + Text.length q)
          | (before, _) <- Text.breakOnAll q l
          ],
    not (null spans)
  ]
  where
    noBom t = fromMaybe t (Text.stripPrefix "\xFEFF" t)
    noCr l = fromMaybe l (Text.stripSuffix "\r" l)

genNeedle :: Gen Text
genNeedle = Text.concat <$> Gen.list (Range.linear 1 3) genPiece

genFile :: Gen Text
genFile = do
  bom <- Gen.element ["", "\xFEFF"]
  (bom <>) . Text.concat <$> Gen.list (Range.linear 0 40) genPiece

genPiece :: Gen Text
genPiece = Gen.element ["a", "b", "ab", " ", "\n", "\r", "\r\n", "λ", "ü", "\xFEFF"]

-- * Helpers

single :: (Show a) => [a] -> IO a
single = \case
  [x] -> pure x
  xs -> assertFailure ("expected one, got " <> show xs)

needleOf :: Text -> Needle
needleOf q = fromMaybe (error ("no needle: " <> show q)) (needle q)

full :: Text -> Text -> [LineHit]
full q t = (searchFile (needleOf q) maxBound (utf8 t)).lines

hits :: Text -> Text -> [(Int, [(Int, Int)])]
hits q t = [(h.line, cols h) | h <- full q t]

cols :: LineHit -> [(Int, Int)]
cols h = [(m.start, m.end) | m <- h.matches]

lineCount :: Text -> Text -> Int
lineCount q t = (searchFile (needleOf q) maxBound (utf8 t)).lineCount

utf8 :: Text -> BS.ByteString
utf8 = Text.Encoding.encodeUtf8

path :: BS.ByteString -> RelPath
path p = fromMaybe (error ("bad path " <> show p)) (relPathFromBytes p)

-- * Timing

-- | Report only: some queries over 2,000 files of code-like text (16
-- MiB), limit 8, and a hostile case. Printed, never failed.
timing :: TestTree
timing = testCaseInfo "timing (report only)" $ do
  let fileText i =
        utf8 . Text.unlines $
          [ "module M" <> Text.pack (show i) <> " where",
            "",
            "import qualified Data.Map as Map",
            "import Data.ByteString (ByteString)"
          ]
            <> concat
              ( replicate
                  60
                  [ "-- | Does a thing to the value, with care.",
                    "thing :: Map.Map Int ByteString -> Maybe Int",
                    "thing m = fmap length (Map.lookup 1 m) <|> pure 0 where x = 1"
                  ]
              )
      files =
        [ (path (utf8 ("src/M" <> Text.pack (show i) <> ".hs")), fileText i)
        | i <- [1 :: Int .. 2000]
        ]
      size = sum (map (BS.length . snd) files)
  _ <- evaluate (sum (map (BS.length . snd) files))
  let queries =
        ["e", "where", "import qualified", "ByteString", "<$>", "<|>", "zzqqxx", "ü"]
  let run q fs = timeIt (searchTexts (const True) (needleOf q) 8 fs)
  times <- mapM (\q -> (q,) <$> run q files) queries
  let hostileFile = BS.replicate 1_048_576 0x61
      hostileNeedle = Text.replicate 60 "a" <> " "
  hostile <- run hostileNeedle [(path "a.txt", hostileFile)]
  pure $
    printf "%.1f MiB: " (fromIntegral size / 1048576 :: Double)
      <> unwords [show q <> " " <> ms t | (q, t) <- times]
      <> "; 60 a's and a space over 1 MiB of a: "
      <> ms hostile
  where
    ms :: Double -> String
    ms = printf "%.1f ms" . (* 1000)
    timeIt a = do
      t0 <- getMonotonicTime
      r <- evaluate a
      _ <- evaluate (forceTextAnswer r)
      t1 <- getMonotonicTime
      pure (t1 - t0)
