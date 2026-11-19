-- | The line diff (D-321): properties, the shared line rule, the Myers
-- core against a plain LCS, path coverage, timing. git as an oracle is
-- in "HaskellReviewer.Core.LineDiffOracleTest".
module HaskellReviewer.Core.LineDiffTest (tests, timingTests) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.Unboxed qualified as VU
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getAllocationCounter)
import HaskellReviewer.Core.LineDiff
  ( BomChange (..),
    Counter (..),
    GitRule (..),
    LineDiff (..),
    Op (..),
    Run (..),
    Stats (..),
    Whitespace (..),
    countOf,
    diffLines,
    lineDiff,
    lineDiffStats,
    lineDiffStatsAs,
    warmUpPairs,
  )
import HaskellReviewer.Core.LineDiff.Myers (MyersResult (..), myersCore)
import HaskellReviewer.Core.Sides (Pair (..))
import Hedgehog
  ( Property,
    annotate,
    cover,
    failure,
    forAll,
    property,
    withShrinks,
    withTests,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LineDiffSupport
  ( checkRuns,
    chunks,
    equalPairs,
    genBigPair,
    genLines,
    genPair,
    genPlainPair,
    genRepetitivePair,
    genUnrelatedPair,
    hugePair,
    parseGitVersion,
    renderLf,
  )
import System.Mem (performMinorGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
    testCaseInfo,
    (@?=),
  )
import Test.Tasty.Hedgehog (testProperty)
import Text.Printf (printf)

tests :: TestTree
tests =
  testGroup "LineDiff" $
    [ testCase "lines as fixtures/lines/cases.json" sharedLines,
      testGroup "units" units,
      testGroup "exact" (modeProps KeepWhitespace),
      testGroup "-w" (modeProps IgnoreWhitespace),
      testProperty "CRLF changes no run" crlfIndependence,
      testProperty "a BOM changes no run" bomNote,
      testProperty "a final newline is a change, but not for -w" finalNewline,
      testProperty "-w is exact mode without whitespace" noWhitespace,
      testProperty "Myers core is minimal on small inputs" myersMinimal,
      testProperty
        "coverage: unrelated big texts"
        (withTests 6 (withShrinks 0 bigCoverage)),
      testCase "the heuristic split, both ways (a huge pair)" hugeCoverage,
      testCase "the warm-up takes its paths (Q-RTS-BLACKHOLE)" warmUpPaths,
      testGroup "git version" gitVersions
    ]
      -- In parts that run in parallel: 400 tests.
      <> chunks 2 "coverage: small and repetitive pairs" coverage
      <> [testProperty "coverage: repetitive pairs" repetitiveCoverage]

-- | The timing, on its own: run it after the other line diff tests, or
-- their load shows in the numbers.
timingTests :: TestTree
timingTests = testGroup "LineDiff timing" [timing]

modeProps :: Whitespace -> [TestTree]
modeProps ws =
  [ testProperty "runs are valid" $ property $ do
      p <- forAll (Gen.choice [genPair, genRepetitivePair])
      valid ws p,
    testProperty "runs are valid (big)" $ withTests 6 $ withShrinks 0 $ property $ do
      p <- forAll genBigPair
      valid ws p,
    testProperty "deterministic" $ property $ do
      p <- forAll genPair
      lineDiff ws p === lineDiff ws (fmap Text.copy p),
    testProperty "equal texts are all Equal" $ property $ do
      ls <- forAll (genLines 30)
      let t = renderLf ls
          n = length ls
          crlf = Text.concat (map (<> "\r\n") ls)
      (lineDiff ws Pair {head = t, base = t}).runs === [Run Equal n | n > 0]
      (lineDiff ws Pair {head = crlf, base = t}).runs === [Run Equal n | n > 0]
  ]
  where
    valid w p = case checkRuns w p (lineDiff w p) of
      Right () -> pure ()
      Left err -> annotate err >> failure

-- | Each case's lines are its @lines@, as for the source view (D-276).
sharedLines :: Assertion
sharedLines = do
  bytes <- BS.readFile "../fixtures/lines/cases.json"
  cases :: [LineCase] <-
    either (assertFailure . ("cases.json: " <>)) pure (Aeson.eitherDecodeStrict bytes)
  assertBool "some cases" (length cases > 10)
  for_ cases $ \c -> assertEqual c.name c.lines (diffLines c.text)

-- | One case of fixtures/lines/cases.json.
data LineCase = LineCase {name :: String, text :: Text, lines :: [Text]}

instance Aeson.FromJSON LineCase where
  parseJSON = Aeson.withObject "LineCase" $ \o ->
    LineCase <$> o .: "name" <*> o .: "text" <*> o .: "lines"

-- | Small cases with known runs (each also matches git: see the oracle
-- test's cases).
units :: [TestTree]
units =
  [ testCase "empty" $ runs "" "" @?= [],
    testCase "equal" $ runs "a\nb\n" "a\nb\n" @?= [Run Equal 2],
    testCase "empty to text" $ runs "" "a\nb\n" @?= [Run Insert 2],
    testCase "one change" $
      runs "a\nb\nc\n" "a\nx\nc\n"
        @?= [Run Equal 1, Run Delete 1, Run Insert 1, Run Equal 1],
    testCase "final newline" $
      runs "a\nb" "a\nb\n" @?= [Run Equal 1, Run Delete 1, Run Insert 1],
    testCase "final newline, -w" $
      (lineDiff IgnoreWhitespace (pair "a\nb" "a\nb\n")).runs @?= [Run Equal 2],
    testCase "CRLF and LF are equal" $ do
      let d = lineDiff KeepWhitespace (pair "a\r\nb\r\nc\n" "a\nb\nc\n")
      d.runs @?= [Run Equal 3]
      d.crlfLines @?= 2,
    testCase "a lone CR at the end is dropped" $ runs "a\r" "a" @?= [Run Equal 1],
    testCase "BOM" $ do
      let d = lineDiff KeepWhitespace (pair "a\n" "\xFEFF\&a\n")
      d.runs @?= [Run Equal 1]
      d.bom @?= Just BomAdded
      (lineDiff KeepWhitespace (pair "\xFEFF\&a\n" "a\n")).bom @?= Just BomRemoved,
    testCase "-w: git's whitespace only" $ do
      let w b h = (lineDiff IgnoreWhitespace (pair b h)).runs
      w "a b\n" "ab\n" @?= [Run Equal 1]
      w "a\tb\n" "ab \n" @?= [Run Equal 1]
      w "a\vb\n" "ab\n" @?= [Run Delete 1, Run Insert 1]
      w "a\xA0\&b\n" "ab\n" @?= [Run Delete 1, Run Insert 1],
    testCase "git 2.54 diffs a merged group again" $ do
      let p = pair "  x = 1\n\n}\n-- | Docs.\n\n" "\n}\n  x = 1\n\n-- | Docs.\n}\n\n\n"
          as rule = (lineDiffStatsAs rule KeepWhitespace p).diff.runs
      as Git254
        @?= [Run Delete 1, Run Equal 2, Run Insert 2, Run Equal 1, Run Insert 2]
          <> [Run Equal 1]
      as GitBefore254
        @?= [Run Delete 1, Run Equal 2, Run Delete 1, Run Insert 5, Run Equal 1]
      (lineDiff KeepWhitespace p).runs @?= as Git254,
    testCase "slider: the indent heuristic picks the blank line" $
      runs
        "f() {\n  a;\n}\n\ng() {\n  b;\n}\n"
        "f() {\n  a;\n}\n\nh() {\n  c;\n}\n\ng() {\n  b;\n}\n"
        @?= [Run Equal 4, Run Insert 4, Run Equal 3]
  ]
  where
    runs b h = (lineDiff KeepWhitespace (pair b h)).runs

pair :: Text -> Text -> Pair Text
pair b h = Pair {head = h, base = b}

-- | LF to CRLF on random lines of either side: same runs, and the note
-- counts the Equal pairs that differ.
crlfIndependence :: Property
crlfIndependence = property $ do
  p <- forAll genPlainPair
  let bl = diffLines p.base
      hl = diffLines p.head
  bc <- forAll (Gen.list (Range.singleton (length bl)) Gen.bool)
  hc <- forAll (Gen.list (Range.singleton (length hl)) Gen.bool)
  let crlf cs ls = Text.concat (zipWith (\c l -> l <> if c then "\r\n" else "\n") cs ls)
      p' = Pair {head = crlf hc hl, base = crlf bc bl}
  for_ [KeepWhitespace, IgnoreWhitespace] $ \ws -> do
    let d = lineDiff ws p
        d' = lineDiff ws p'
    d'.runs === d.runs
    d'.crlfLines === length [() | (a, b) <- equalPairs d.runs bc hc, a /= b]

bomNote :: Property
bomNote = property $ do
  p <- forAll genPair
  ws <- forAll (Gen.element [KeepWhitespace, IgnoreWhitespace])
  let d = lineDiff ws p
      bom = ("\xFEFF" <>)
  (lineDiff ws p {head = bom p.head}).runs === d.runs
  (lineDiff ws p {head = bom p.head}).bom === Just BomAdded
  (lineDiff ws p {base = bom p.base}).bom === Just BomRemoved
  (lineDiff ws Pair {head = bom p.head, base = bom p.base}).runs === d.runs
  (lineDiff ws Pair {head = bom p.head, base = bom p.base}).bom === Nothing

finalNewline :: Property
finalNewline = property $ do
  ls <- forAll (Gen.list (Range.linear 1 20) (Gen.element ["a", "", "  b", "}"]))
  let t = renderLf ls
      n = length ls
      cut = Text.dropEnd 1 t
  -- Removing the final newline of "...\n\n" leaves one line fewer.
  unless (last ls == "") $ do
    (lineDiff KeepWhitespace (pair t cut)).runs
      === [Run Equal (n - 1) | n > 1] <> [Run Delete 1, Run Insert 1]
    (lineDiff IgnoreWhitespace (pair t cut)).runs === [Run Equal n]

-- | No whitespace and a final newline on both sides: the modes agree.
noWhitespace :: Property
noWhitespace = property $ do
  let line = Gen.element ["a", "b", "c", "}", "x=1", "line1", "é"]
  b <- forAll (Gen.list (Range.linear 0 40) line)
  h <- forAll (Gen.list (Range.linear 0 40) line)
  let p = Pair {head = renderLf h, base = renderLf b}
  (lineDiff IgnoreWhitespace p).runs === (lineDiff KeepWhitespace p).runs

-- | Without trim and discards, and below git's cost limits, the core
-- finds a minimal diff: as many unchanged lines as the LCS.
myersMinimal :: Property
myersMinimal = property $ do
  k <- forAll (Gen.int (Range.linear 1 4))
  a <- forAll (Gen.list (Range.linear 0 40) (Gen.int (Range.constant 0 k)))
  b <- forAll (Gen.list (Range.linear 0 40) (Gen.int (Range.constant 0 k)))
  let r = myersCore (VU.fromList a) (VU.fromList b)
      keptA = [x | (x, c) <- zip a (VU.toList r.changed1), not c]
      keptB = [x | (x, c) <- zip b (VU.toList r.changed2), not c]
  r.heuristics === 0
  keptA === keptB
  length keptA === lcsLength a b

-- | Plain dynamic-programming LCS length, one row per line of @a@.
lcsLength :: [Int] -> [Int] -> Int
lcsLength a b = first (foldr row (replicate (length b + 1) 0) a)
  where
    -- below: the row for the lines after x.
    row x below =
      foldr
        (\(y, down, diag) acc -> cell x y down diag acc : acc)
        [0]
        (zip3 b below (drop 1 below))
    cell x y down diag acc = if x == y then 1 + diag else max down (first acc)
    first = \case
      v : _ -> v
      [] -> 0

-- | Every compaction path runs often enough.
coverage :: Property
coverage = withTests 200 $ property $ do
  p <- forAll (Gen.choice [genPair, genRepetitivePair])
  ws <- forAll (Gen.element [KeepWhitespace, IgnoreWhitespace])
  let c = (lineDiffStats ws p).counts
  cover 5 "slide" (countOf Slides c > 0)
  cover 5 "merge" (countOf Merges c > 0)
  cover 5 "alignment" (countOf Alignments c > 0)
  cover 5 "indent shift" (countOf IndentShifts c > 0)

-- | The fallback, the discard pass and git 2.54's re-diff.
repetitiveCoverage :: Property
repetitiveCoverage = withTests 300 $ property $ do
  p <- forAll genRepetitivePair
  ws <- forAll (Gen.element [KeepWhitespace, IgnoreWhitespace])
  let c = (lineDiffStats ws p).counts
  cover 5 "fallback" (countOf Fallbacks c > 0)
  cover 5 "discard" (countOf Discards c > 0)
  cover 5 "re-diff" (countOf Rediffs c > 0)

-- | Myers' cost limit, on random unrelated texts. Which path it takes
-- varies (a cover per path over a few tests was flaky): 'hugeCoverage'
-- and 'warmUpPaths' pin both paths on fixed pairs.
bigCoverage :: Property
bigCoverage = property $ do
  p <- forAll genUnrelatedPair
  let c = (lineDiffStats KeepWhitespace p).counts
  let cutoffs = countOf CostCutoffsForward c + countOf CostCutoffsBackward c
  cover 50 "cost cutoff" (cutoffs > 0)

-- | The heuristic split needs a huge fallback region (see
-- 'LineDiffSupport.genHugePair'). This pair takes both directions, and
-- both cost cutoff paths too.
hugeCoverage :: Assertion
hugeCoverage = do
  let p = hugePair 1 4
      s = lineDiffStats KeepWhitespace p
  let branches =
        [ HeuristicSplitsForward,
          HeuristicSplitsBackward,
          CostCutoffsForward,
          CostCutoffsBackward
        ]
  for_ branches $
    \c -> assertBool (show c) (countOf c s.counts > 0)
  either assertFailure pure (checkRuns KeepWhitespace p s.diff)

-- | Each path 'HaskellReviewer.Core.LineDiff.warmUpLineDiff' promises
-- runs on its pairs.
warmUpPaths :: Assertion
warmUpPaths = do
  let stats =
        [ lineDiffStats ws p
        | ws <- [KeepWhitespace, IgnoreWhitespace],
          p <- warmUpPairs ()
        ]
      total c = sum [countOf c s.counts | s <- stats]
  for_
    [ Fallbacks,
      Discards,
      CostCutoffsForward,
      CostCutoffsBackward,
      Slides,
      Merges,
      Alignments,
      IndentShifts,
      Rediffs
    ]
    $ \c -> assertBool (show c) (total c > 0)
  assertBool "CRLF" (any (\s -> s.diff.crlfLines > 0) stats)
  assertBool "BOM" (any (\s -> s.diff.bom /= Nothing) stats)

-- | The version in @git --version@.
gitVersions :: [TestTree]
gitVersions =
  [ testCase name (parseGitVersion out @?= Right rule)
  | (name, out, rule) <-
      [ ("plain", "git version 2.56.0\n", Git254),
        ("Apple", "git version 2.50.1 (Apple Git-155)\n", GitBefore254),
        ("Windows", "git version 2.54.0.windows.1\n", Git254),
        ("rc", "git version 2.54.0-rc0\n", Git254),
        ("old", "git version 2.44.1\n", GitBefore254)
      ]
  ]
    <> [ testCase "no version fails" $
           assertBool "Left" (isLeft (parseGitVersion "")),
         testCase "garbage fails" $
           assertBool "Left" (isLeft (parseGitVersion "oops\n"))
       ]

-- * Timing

-- | Report only: 20,000 lines, both modes, median of 5, with bytes
-- allocated. Never fails.
timing :: TestTree
timing = testCaseInfo "timing (report only)" $ do
  let base = haskellLike 0 20000
      typical =
        edits [(i * 997, 3, ["  changed " <> tshow i, "", "  new"]) | i <- [1 .. 20]] base
      tenPercent =
        [ if i `mod` 10 == 0 then l <> " -- changed" else l
        | (i, l) <- zip [0 :: Int ..] base
        ]
      moved =
        let (a, rest) = splitAt 5000 base
            (blk, c) = splitAt 2000 rest
            (c1, c2) = splitAt 8000 c
         in a <> c1 <> map ("  " <>) blk <> c2
      repetitive s = take 20000 (cycleLines s ["", "}", "  x = 1", "end"])
      unrelated = haskellLike 7 20000
      shuffle s = take 20000 (cycleLines s ["a", "b", "c"])
      cases =
        [ ("typical", base, typical),
          ("10% changed", base, tenPercent),
          ("moved and reindented 2k block", base, moved),
          ("repetitive (fallback)", repetitive 1, repetitive 2),
          ("unrelated", base, unrelated),
          ("small-alphabet shuffle", shuffle 3, shuffle 4)
        ]
  rows <- mapM (\(name, b, h) -> row name (renderLf b) (renderLf h)) cases
  pure (unwords rows)
  where
    tshow = Text.pack . show
    row :: String -> Text -> Text -> IO String
    row name b h = do
      _ <- evaluate (Text.length b + Text.length h)
      let p = Pair {head = h, base = b}
      k <- measure KeepWhitespace p
      w <- measure IgnoreWhitespace p
      pure (name <> ": " <> k <> ", -w " <> w <> ";")
    measure ws p = do
      rs <- mapM (const (once ws p)) [1 :: Int .. 5]
      let (t, a) = sort rs !! 2
      pure (printf "%.1f ms (%.0f MB)" (t * 1000) (fromIntegral a / 1e6 :: Double))
    once :: Whitespace -> Pair Text -> IO (Double, Int64)
    once ws p0 = do
      -- A fresh copy each time, so no run reuses another's result.
      p <- evaluate (fmap Text.copy p0)
      _ <- evaluate (Text.length p.base + Text.length p.head)
      performMinorGC
      a0 <- getAllocationCounter
      t0 <- getMonotonicTime
      d <- evaluate (lineDiff ws p)
      _ <- evaluate (sum (map (.count) d.runs))
      t1 <- getMonotonicTime
      a1 <- getAllocationCounter
      pure (t1 - t0, a0 - a1)

-- | Code-like lines, deterministic.
haskellLike :: Int -> Int -> [Text]
haskellLike seed n = take n (concatMap block [seed * 100000 ..])
  where
    block :: Int -> [Text]
    block i =
      let s = Text.pack (show i)
       in [ "-- | Does thing " <> s <> ".",
            "thing" <> s <> " :: Int -> Maybe Int",
            "thing" <> s <> " x",
            "  | x > " <> s <> " = Just x",
            "  | otherwise = Nothing",
            "  where",
            "    y = x + 1",
            ""
          ]

-- | Replace @len@ lines at @i@ with new ones, for each edit. Edits go
-- by position, and the last one is applied first, so positions hold.
edits :: [(Int, Int, [Text])] -> [Text] -> [Text]
edits es ls = foldr apply ls es
  where
    apply (i, len, new) xs = let (a, b) = splitAt i xs in a <> new <> drop len b

-- | Lines picked from an alphabet by a small LCG.
cycleLines :: Int -> [Text] -> [Text]
cycleLines seed alphabet =
  [ alphabet !! ((v `div` 65536) `mod` length alphabet)
  | v <- iterate (\x -> (x * 1103515245 + 12345) `mod` 2147483648) (seed * 7919 + 1)
  ]
