module HaskellReviewer.Core.RevspecTest (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.Revspec
import HaskellReviewer.Core.Sides (Pair (..))
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Revspec"
    [ testCase "two dots" $ do
        parseRevspec "main..feature" @?= range TwoDot "main" "feature"
        parseRevspec "HEAD~1..HEAD" @?= range TwoDot "HEAD~1" "HEAD"
        parseRevspec "v1.2..v1.3" @?= range TwoDot "v1.2" "v1.3",
      testCase "three dots" $ do
        parseRevspec "main...feature" @?= range ThreeDot "main" "feature"
        parseRevspec "a.b...c" @?= range ThreeDot "a.b" "c",
      testCase "an empty end is HEAD" $ do
        parseRevspec "main.." @?= range TwoDot "main" "HEAD"
        parseRevspec "..main" @?= range TwoDot "HEAD" "main"
        parseRevspec "main..." @?= range ThreeDot "main" "HEAD"
        parseRevspec "...main" @?= range ThreeDot "HEAD" "main",
      testCase "no dots: against the working tree" $ do
        parseRevspec "HEAD" @?= Right (AgainstWorkTree "HEAD")
        parseRevspec "v1.2" @?= Right (AgainstWorkTree "v1.2"),
      testCase "dashes are kept as names" $
        parseRevspec "--output=x..-y" @?= range TwoDot "--output=x" "-y",
      testCase "errors" $ do
        parseRevspec "" @?= Left RevspecEmpty
        parseRevspec ".." @?= Left RevspecNoEnds
        parseRevspec "..." @?= Left RevspecNoEnds
        mapM_
          (\t -> (t, parseRevspec t) @?= (t, Left RevspecBadDots))
          ["a....b", "a..b..c", "a...b...c", "a.....b", "a....", "....b"],
      testCase "render" $ do
        renderRevspec (RangeSpec (Range ThreeDot "a" "b")) @?= "a...b"
        renderRevspec (RangeSpec (Range TwoDot "a" "b")) @?= "a..b"
        renderRevspec (AgainstWorkTree "a") @?= "a",
      testCase "the log lines" $ do
        let oid c = ObjectId (Text.replicate 40 c)
            info =
              CommitsInfo
                { range = Range ThreeDot "main" "feature",
                  sides = Pair {head = oid "1", base = oid "2"},
                  baseFrom = MergeBase 2
                }
        diffInfoLines (CommitsDiff info)
          @?= [ "diff: main...feature",
                "head: feature = 1111111",
                "base: merge base of main and feature = 2222222 (1 of 2)"
              ]
        diffInfoLines
          (CommitsDiff info {range = Range TwoDot "main" "feature", baseFrom = BaseTyped})
          @?= [ "diff: main..feature",
                "head: feature = 1111111",
                "base: main = 2222222"
              ]
        diffInfoLines (WorkTreeDiff WorkTreeInfo {rev = "v1\n", base = oid "3"})
          @?= [ "diff: v1\\n (against the working tree)",
                "head: working tree",
                "base: v1\\n = 3333333"
              ],
      testCase "printable: control characters escaped" $ do
        printable "a\nb\ESC[31mc\td" @?= "a\\nb\\ESC[31mc\\td"
        printable "caf\233 ok" @?= "caf\233 ok"
        -- The escape must not run into the next character.
        printable "\SOH" @?= "\\SOH"
        printable "\SO\&H" @?= "\\SO\\&H"
        printable "\8238\&1" @?= "\\8238\\&1"
        printable "\8238x" @?= "\\8238x",
      testProperty "a good end round-trips through .. and ..." propRoundTrip,
      testProperty "... is never TwoDot" propThreeDot,
      testProperty "no end has .. in it, none is empty" propEnds,
      testProperty "never throws" propTotal
    ]
  where
    range f b h = Right (RangeSpec (Range f b h))

-- | A name with no "..", not starting or ending with ".".
genEnd :: Gen Text
genEnd = Gen.filter good (Gen.text (Range.linear 1 12) (Gen.element chars))
  where
    chars = "ab.~^-/:@{}1" :: String
    good t =
      not (".." `Text.isInfixOf` t)
        && not ("." `Text.isPrefixOf` t)
        && not ("." `Text.isSuffixOf` t)

-- | Any text with plenty of dots.
genAny :: Gen Text
genAny = Gen.text (Range.linear 0 20) (Gen.frequency [(3, pure '.'), (2, Gen.unicode)])

propRoundTrip :: Property
propRoundTrip = property $ do
  b <- forAll genEnd
  h <- forAll genEnd
  parseRevspec (b <> ".." <> h) === Right (RangeSpec (Range TwoDot b h))
  parseRevspec (b <> "..." <> h) === Right (RangeSpec (Range ThreeDot b h))

propThreeDot :: Property
propThreeDot = property $ do
  t <- forAll genAny
  case parseRevspec t of
    Right (RangeSpec r) | "..." `Text.isInfixOf` t -> r.form === ThreeDot
    _ -> pure ()

propEnds :: Property
propEnds = property $ do
  t <- forAll genAny
  case parseRevspec t of
    Right (RangeSpec r) -> do
      assert (not (any (".." `Text.isInfixOf`) [r.base, r.head]))
      assert (not (any Text.null [r.base, r.head]))
    Right (AgainstWorkTree rev) -> assert (not (".." `Text.isInfixOf` rev))
    Left _ -> pure ()

propTotal :: Property
propTotal = property $ do
  t <- forAll genAny
  -- Forcing the answer: a throw fails the property.
  assert (length (show (parseRevspec t)) > 0)
