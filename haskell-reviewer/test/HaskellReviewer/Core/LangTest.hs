-- | The rule scanner: units, properties on random rules and text, and
-- linear time. The rule files are in LangGoldenTest.
module HaskellReviewer.Core.LangTest (tests, linearTests) where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Highlight (Run (..))
import HaskellReviewer.Core.Lang
  ( BlockComment (..),
    Lang (..),
    StringRule (..),
    buildLangs,
    langCount,
    langFor,
    noLangs,
    scanLang,
  )
import HaskellReviewer.Core.RelPath (relPathFromSegments)
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (DisplayClass (..))
import Hedgehog (Gen, Property, annotateShow, assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem (getAllocationCounter)
import System.Timeout (timeout)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Lang"
    [ testCase "comments, strings, words, numbers" $
        texts lisp "(define x 12) ; c\n#| a #| b |# c |# \"s\\\"t\" y #t"
          @?= [ ("define", KeywordClass),
                ("12", LiteralClass),
                ("; c", CommentClass),
                ("#| a #| b |# c |#", CommentClass),
                ("\"s\\\"t\"", LiteralClass),
                ("#t", LiteralClass)
              ],
      testCase "not nested: the first close ends it" $
        texts lisp {blockComments = [BlockComment "#|" "|#" False]} "#| a #| b |# c |#"
          @?= [("#| a #| b |#", CommentClass)],
      testCase "no end: to the end of the file" $ do
        scanLang lisp "x \"a\nb"
          @?= [Run (Span (Line 1) (Col 3) (Line 2) (Col 2)) LiteralClass]
        scanLang lisp "#| a\n"
          @?= [Run (Span (Line 1) (Col 1) (Line 2) (Col 1)) CommentClass],
      testCase "a comment starts inside a word, a word char doesn't hide it" $
        texts lisp "ab;c #|d|#" @?= [(";c #|d|#", CommentClass)],
      testCase "numbers off: digits are plain" $
        scanLang lisp {numbers = False} "12 3" @?= [],
      testCase "CRLF and a last CR stay out of a line comment" $
        map (.span) (scanLang lisp "; a\r\n; b\r")
          @?= [ Span (Line 1) (Col 1) (Line 1) (Col 4),
                Span (Line 2) (Col 1) (Line 2) (Col 4)
              ],
      testCase "a BOM is no column" $
        map (.span) (scanLang lisp "\xFEFFif") @?= [Span (Line 1) (Col 1) (Line 1) (Col 3)],
      testCase "empty delimiters match nothing" $ do
        let l =
              lisp
                { lineComments = [""],
                  blockComments = [BlockComment "" "x" False, BlockComment "x" "" False],
                  strings = [StringRule "" "" Nothing]
                }
        scanLang l "ab x" @?= [],
      testCase "langFor: by extension, any case" $ do
        let (langs, lost) = buildLangs [("a" :: Text, lisp), ("b", lisp {numbers = False})]
        lost @?= [("b", "scm"), ("b", "ss")]
        langCount langs @?= 2
        langFor langs (rel ["src", "A.SCM"]) @?= Just lisp
        langFor langs (rel ["a.ss"]) @?= Just lisp
        langFor langs (rel ["scm"]) @?= Nothing
        langFor langs (rel ["a.scm.txt"]) @?= Nothing
        langFor noLangs (rel ["a.scm"]) @?= Nothing,
      testProperty "good rules: runs sorted, apart, not empty, in the text" runsInText,
      testProperty "ends, whatever the rules" alwaysEnds
    ]
  where
    rel segs = maybe (error "bad path") id (relPathFromSegments segs)

-- | Each run's text and class.
texts :: Lang -> Text -> [(Text, DisplayClass)]
texts l t = [(sliceSpan src r.span, r.display) | r <- scanLang l t]
  where
    src = sourceLines t

lisp :: Lang
lisp =
  Lang
    { extensions = ["scm", "ss"],
      wordChars = Set.fromList ("#-" :: String),
      lineComments = [";"],
      blockComments = [BlockComment "#|" "|#" True],
      strings = [StringRule "\"" "\"" (Just '\\')],
      numbers = True,
      wordClasses =
        Map.fromList [("define", KeywordClass), ("if", KeywordClass), ("#t", LiteralClass)]
    }

-- * Properties

-- | Text from a small alphabet, so delimiters show up often.
genText :: Gen Text
genText = Gen.text (Range.linear 0 200) genChar

genChar :: Gen Char
genChar = Gen.element ("ab1 ;#|\"\\\n\r\t(é😀" :: String)

-- | No space or control character.
genMark :: Gen Char
genMark = Gen.filter (not . isSpace) genChar

-- | Rules with delimiters and word characters from these. With
-- 'genChar': bad rules too (empty, or with a space: what the parser
-- refuses).
genLang :: Gen Char -> Gen Lang
genLang char = do
  let delim = Gen.text (Range.linear 0 2) char
      some = Gen.list (Range.linear 0 2)
  wordChars <- Set.fromList <$> some char
  lineComments <- some delim
  blockComments <- some (BlockComment <$> delim <*> delim <*> Gen.bool)
  strings <- some (StringRule <$> delim <*> delim <*> Gen.maybe genChar)
  numbers <- Gen.bool
  ws <- some ((,) <$> Gen.text (Range.linear 1 2) genChar <*> Gen.enumBounded)
  pure
    Lang
      { extensions = ["x"],
        wordChars = wordChars,
        lineComments = lineComments,
        blockComments = blockComments,
        strings = strings,
        numbers = numbers,
        wordClasses = Map.fromList ws
      }

runsInText :: Property
runsInText = property $ do
  l <- forAll (genLang genMark)
  t <- forAll genText
  let runs = scanLang l t
      spans = map (.span) runs
      -- The text as the client cuts it: no BOM, no \r at a line end.
      ls = map noCr (Text.splitOn "\n" (maybe t id (Text.stripPrefix "\xFEFF" t)))
      noCr x = maybe x id (Text.stripSuffix "\r" x)
      inText (Line n) (Col c) = case drop (fromIntegral n - 1) ls of
        x : _ -> c >= 1 && fromIntegral c <= Text.length x + 1
        [] -> False
  annotateShow runs
  assert (all (\s -> (s.startLine, s.startCol) < (s.endLine, s.endCol)) spans)
  assert (all (\s -> inText s.startLine s.startCol && inText s.endLine s.endCol) spans)
  assert . and $
    zipWith (\a b -> (a.endLine, a.endCol) <= (b.startLine, b.startCol)) spans (drop 1 spans)

alwaysEnds :: Property
alwaysEnds = property $ do
  l <- forAll (genLang genChar)
  t <- forAll genText
  r <- evalIO (timeout 2_000_000 (evaluate (length (scanLang l t))))
  (r /= Nothing) === True

-- * Linear time

linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: lang"
      [ testCase "words, strings and comments" $
          linear (\k -> Text.replicate k "(define x \"s\" 12) ; c\n") 5_000,
        testCase "one block comment, nested many times" $
          linear (\k -> Text.replicate k "#| a " <> Text.replicate k " |#") 20_000,
        testCase "a string with no end, on one line" $
          linear (\k -> "\"" <> Text.replicate k "ab\\\"") 20_000,
        testCase "many starts that open nothing" $
          linear (\k -> Text.replicate k "# | ") 20_000
      ]

linear :: (Int -> Text) -> Int -> Assertion
linear input small = do
  a <- allocated small
  b <- allocated (4 * small)
  let ratio = fromIntegral b / fromIntegral a :: Double
  assertBool ("bytes " <> show (a, b) <> ", ratio " <> show ratio) (ratio < 6)
  where
    allocated k = do
      t <- evaluate (input k)
      _ <- evaluate (Text.length t)
      a0 <- getAllocationCounter
      _ <- evaluate (length (scanLang lisp t))
      a1 <- getAllocationCounter
      pure (a0 - a1)
