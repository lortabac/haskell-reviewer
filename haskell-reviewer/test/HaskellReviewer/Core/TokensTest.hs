-- | The packed token stream.
module HaskellReviewer.Core.TokensTest (tests) where

import Data.List (sortOn)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens
  ( Token (..),
    tokenAt,
    tokenCount,
    tokenList,
    tokensFromList,
    tokensIn,
  )
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Tokens"
    [ testProperty "pack, then read back: the same tokens" roundTrip,
      testProperty "tokensIn: the tokens that start inside" inside
    ]

roundTrip :: Property
roundTrip = property $ do
  ts <- forAll (Gen.list (Range.linear 0 200) genToken)
  let packed = tokensFromList ts
  tokenList packed === ts
  tokenCount packed === length ts
  tokenAt packed (length ts) === Nothing
  tokenAt packed (-1) === Nothing

inside :: Property
inside = property $ do
  ts <- sortOn start <$> forAll (Gen.list (Range.linear 0 100) genToken)
  s <- forAll genSpan
  let begin = (s.startLine, s.startCol)
      end = (s.endLine, s.endCol)
  tokensIn s (tokensFromList ts)
    === [t | t <- ts, start t >= begin, start t < end]
  where
    start t = (t.span.startLine, t.span.startCol)

genToken :: Gen Token
genToken = Token <$> genSpan <*> Gen.enumBounded

genSpan :: Gen Span
genSpan = do
  l1 <- Gen.word32 (Range.linear 1 50)
  c1 <- Gen.word32 (Range.linear 1 80)
  dl <- Gen.word32 (Range.linear 0 3)
  c2 <- Gen.word32 (Range.linear 1 80)
  let c2' = if dl == 0 then max c1 c2 else c2
  pure (Span (Line l1) (Col c1) (Line (l1 + dl)) (Col c2'))
