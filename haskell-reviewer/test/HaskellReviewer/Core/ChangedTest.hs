module HaskellReviewer.Core.ChangedTest (tests) where

import Data.ByteString.Char8 qualified as Char8
import Data.List (nub)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as Text
import HaskellReviewer.Core.Changed (ModuleKey (..), changedModules)
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.Modules (ModuleId (..))
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import HaskellReviewer.Core.RelPath (parseRelPath)
import HaskellReviewer.Core.Sides (Pair (..))
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Changed"
    [ testCase "same blob and settings: not changed" $
        changed [(a, key 1)] [(a, key 1)] @?= Pair [] [],
      testCase "a blob or a setting differs: changed on both sides" $ do
        changed [(a, key 1), (b, key 2)] [(a, key 3), (b, key 2)] @?= Pair [a] [a]
        let other = (key 1) {settings = defaultParseSettings {ghcOptions = ["-x"]}}
        changed [(a, key 1)] [(a, other)] @?= Pair [a] [a],
      testCase "added and removed: on one side only" $
        changed [(a, key 1), (b, key 2)] [(a, key 1), (c, key 3)] @?= Pair [b] [c],
      testCase "unknown blob: changed" $
        changed [(a, ModuleKey Nothing defaultParseSettings)] [(a, key 1)]
          @?= Pair [a] [a],
      testCase "order is kept" $
        changed [(c, key 1), (a, key 2), (b, key 3)] [] @?= Pair [c, a, b] [],
      testProperty "same sides: nothing changed" propSame,
      testProperty "swapping the sides swaps the result" propSwap,
      testProperty "matches the model" propModel
    ]
  where
    a = loose "a.hs"
    b = loose "b.hs"
    c = loose "c.hs"
    changed h bs = changedModules Pair {head = h, base = bs}

key :: Int -> ModuleKey
key n =
  ModuleKey
    { blob = Just (ObjectId (Text.replicate 40 (Text.pack (show (n `mod` 10))))),
      settings = defaultParseSettings
    }

loose :: String -> ModuleId
loose p = either (error . show) LooseModule (parseRelPath (Char8.pack p))

-- | A side: some of 6 modules, each with one of 3 blobs (or none).
genSide :: Gen [(ModuleId, ModuleKey)]
genSide = do
  ids <- nub <$> Gen.list (Range.linear 0 6) (Gen.element ("abcdef" :: String))
  mapM
    ( \i -> do
        k <- Gen.frequency [(5, key <$> Gen.int (Range.linear 1 3)), (1, pure unknown)]
        pure (loose [i, '.', 'h', 's'], k)
    )
    ids
  where
    unknown = ModuleKey Nothing defaultParseSettings

propSame :: Property
propSame = property $ do
  s <- forAll genSide
  let known = [(i, k) | (i, k) <- s, isJust k.blob]
  changedModules Pair {head = known, base = known} === Pair [] []

propSwap :: Property
propSwap = property $ do
  h <- forAll genSide
  b <- forAll genSide
  let r = changedModules Pair {head = h, base = b}
      s = changedModules Pair {head = b, base = h}
  (s.head, s.base) === (r.base, r.head)

-- | A module of a side is changed iff it is missing on the other side,
-- its keys differ, or its blob is unknown. In the side's order.
propModel :: Property
propModel = property $ do
  h <- forAll genSide
  b <- forAll genSide
  let model these others =
        [ i
        | (i, k) <- these,
          case lookup i others of
            Nothing -> True
            Just o -> k /= o || isNothing k.blob
        ]
  changedModules Pair {head = h, base = b} === Pair (model h b) (model b h)
