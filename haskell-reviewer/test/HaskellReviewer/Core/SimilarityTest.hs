-- | git's similarity score (D-324): span tables by hand, and the score
-- against git's (@-M1%@, so scores from 1% show).
module HaskellReviewer.Core.SimilarityTest (tests, oracleTests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.List (sort)
import Data.Vector.Unboxed qualified as VU
import HaskellReviewer.Core.Similarity
  ( Score (..),
    Spans (..),
    estimateSimilarity,
    hashChars,
    passesSizeTest,
    renameScore,
    scorePercent,
  )
import HaskellReviewer.Core.WorkTree (Exec (..))
import Hedgehog (Gen, Property, annotate, evalIO, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import RenamesSupport (Case (..), Item (..), Row (..), genContent, gitCaseRows)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Similarity"
    [ testCase "a line: git's hash, by hand" $
        -- 'a' (97): A1 = 97. LF (10): A1 = (97 << 7) + 10 = 12426, A2 = 0.
        table "a\n" @?= [(12426, 2)],
      testCase "a last chunk without LF counts" $
        table "a" @?= [(97, 1)],
      testCase "CR before LF is skipped in text" $ do
        table "a\r\nb\r\n" @?= table "a\nb\n"
        (hashChars "a\r\nb\r\n").size @?= 6,
      testCase "a lone CR is kept" $
        sum (map snd (table "a\rb\n")) @?= 4,
      testCase "binary (a NUL in the first 8000 bytes) keeps CRs" $ do
        sum (map snd (table "\0a\r\n")) @?= 4
        -- A NUL after the first 8000 bytes: text, so the CR goes.
        sum (map snd (table (Char8.replicate 8000 'x' <> "\0a\r\n"))) @?= 8003,
      testCase "chunks end at 64 bytes" $ do
        let t = table (Char8.replicate 100 'x')
        sum (map snd t) @?= 100
        -- 64 and 36 bytes: two chunks, two buckets.
        sort (map snd t) @?= [36, 64],
      testCase "equal chunks add up in one bucket" $
        table "ab\nab\nab\n" @?= [(bucket "ab\n", 9)],
      testCase "size test" $ do
        passesSizeTest renameScore 100 50 @?= True
        passesSizeTest renameScore 100 49 @?= False
        passesSizeTest (Score 45000) 100 75 @?= True
        passesSizeTest (Score 45000) 100 74 @?= False,
      testCase "scores" $ do
        let s a b = estimateSimilarity renameScore (hashChars a) (hashChars b)
        s "a\nb\n" "a\nb\n" @?= Score 60000
        s "a\nb\nc\nd\n" "a\nb\nc\ne\n" @?= Score 45000
        s "" "" @?= Score 0
        s "a\n" "" @?= Score 0
        scorePercent (Score 35999) @?= 59
    ]
  where
    table :: ByteString -> [(Int, Int)]
    table b =
      let sp = hashChars b
       in zip (map fromIntegral (VU.toList sp.hashes)) (VU.toList sp.counts)
    bucket b = case table b of
      [(h, _)] -> h
      _ -> error "one chunk"

-- | Random pairs against git: base has @a@, head has @b@ (no basename
-- step), and git runs with @-M1%@. A score from 1% shows as R; below,
-- D and A. 30 pairs per git run.
oracleTests :: TestTree
oracleTests =
  testGroup
    "Similarity oracle"
    [testProperty ("pairs " <> show i) (withTests 5 oracle) | i <- [1 :: Int .. 4]]

oracle :: Property
oracle = property $ do
  pairs <- forAll (Gen.list (Range.singleton 30) genPair)
  let cases = [Case {base = [("a", file a)], head = [("b", file b)]} | (a, b) <- pairs]
  rows <- evalIO (gitCaseRows ["-M1%"] cases)
  let ours = map (uncurry expected) pairs
  annotate (show (length (filter isRename ours)) <> " of 30 renamed")
  ours === rows
  where
    file = IFile NotExecutable
    isRename = any (\r -> r.status == 'R')
    expected a b =
      let Score sc = estimateSimilarity (Score 600) (hashChars a) (hashChars b)
       in if a == b || sc >= 600
            then
              let pct = if a == b then 100 else scorePercent (Score sc)
               in [Row "100644" "100644" 'R' (Just pct) (Just "a") "b"]
            else
              [ Row "000000" "100644" 'A' Nothing Nothing "b",
                Row "100644" "000000" 'D' Nothing Nothing "a"
              ]
    genPair = do
      a <- genContent
      b <- Gen.frequency [(1, genContent), (3, edit a)]
      pure (a, b)
    edit :: ByteString -> Gen ByteString
    edit a = do
      extra <- genContent
      cut <- Gen.int (Range.constant 0 (Char8.length a))
      Gen.element [Char8.take cut a <> extra, extra <> Char8.drop cut a, a <> extra]
