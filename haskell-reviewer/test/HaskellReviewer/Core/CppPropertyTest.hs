-- | Properties of the CPP evaluator, on random mixes of directives,
-- macros, quotes, comments and backslashes.
module HaskellReviewer.Core.CppPropertyTest (tests) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp
  ( CppEnv (..),
    CppOutput (..),
    Piece (..),
    Pos (..),
    PositionMap (..),
    Region (..),
    mapSpan,
    runCpp,
    sameLines,
  )
import HaskellReviewer.Core.Project (GhcVersion (..), PackageName (..), Platform (..))
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import Hedgehog (Gen, Property, annotate, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "properties"
    [ testProperty "same line count as the input" lineCount,
      testProperty "with no macros in code, kept lines are the input's" keptLines,
      testProperty "mapped spans are in the file, start before end" spansInside,
      testProperty "copied text maps back to the same text" copiedText,
      testProperty "expanded text maps to a call, at a macro name" expandedAtName,
      testProperty "regions: sorted, apart, blank in the output" regionsOk,
      testProperty "mapSpan sameLines is the identity" identity
    ]

env :: CppEnv
env =
  CppEnv
    { ghc = GhcVersion [9, 14, 1],
      platform = Platform {os = "linux", arch = "x86_64"},
      flags = [],
      buildDepends = [PackageName "base"]
    }

-- * Generators

-- | A file: up to 30 lines.
genFile :: Gen Text -> Gen Text
genFile code = do
  ls <- Gen.list (Range.linear 0 30) (Gen.frequency [(1, genDirective), (3, code)])
  nl <- Gen.bool
  pure (Text.intercalate "\n" ls <> if nl then "\n" else "")

genDirective :: Gen Text
genDirective =
  Gen.element
    [ "#if 0",
      "#if 1",
      "#if FOO",
      "#if defined(F) && MIN_VERSION_base(1,2,3)",
      "#if (1",
      "#ifdef FOO",
      "#ifndef F",
      "#elif FOO",
      "#elifdef F",
      "#else",
      "#endif",
      "#define FOO 1",
      "#define FOO",
      "#define F(x) [x]",
      "#define G(x,y) x/**/y \"x\"",
      "#define H F",
      "#define R(x) R(x)",
      "#undef FOO",
      "#error e",
      "#include \"a.h\"",
      "#bogus FOO",
      "#",
      "#line 3",
      "  #if 1"
    ]

-- | Code with anything in it.
genCode :: Gen Text
genCode = Text.concat <$> Gen.list (Range.linear 0 8) (Gen.element pieces)
  where
    pieces =
      [ "FOO",
        "F",
        "F(",
        "(",
        ")",
        "F(a)",
        "G(a,b)",
        "H(1)",
        "R(1)",
        ",",
        " ",
        "x",
        "'",
        "\"",
        "/*",
        "*/",
        "/",
        "*",
        "-- ",
        "\\",
        "\\ ",
        "\x00e9",
        "\x1D465",
        "\t",
        "it's",
        "0x1F",
        "__LINE__",
        "MIN_VERSION_base(1,2,3)",
        "\r"
      ]

-- | Code with no macro names, no comments and no backslash at the end.
genPlainCode :: Gen Text
genPlainCode = Text.concat <$> Gen.list (Range.linear 0 8) (Gen.element pieces)
  where
    pieces =
      [ "x",
        "y1",
        " ",
        "'",
        "\"",
        "(",
        ")",
        ",",
        "-- ",
        "*",
        "\x00e9",
        "\x1D465",
        "\t",
        "it's",
        "0x1F"
      ]

-- * Properties

lineCount :: Property
lineCount = property $ do
  t <- forAll (genFile genCode)
  let o = runCpp env [] t
  length (Text.splitOn "\n" o.text) === length (Text.splitOn "\n" t)

keptLines :: Property
keptLines = property $ do
  t <- forAll (genFile genPlainCode)
  let o = runCpp env [] t
      inRegion n = any (\r -> r.firstLine <= n && n <= r.lastLine) o.regions
  sequence_
    [ (n, outL) === (n, dropCr inL)
    | (n, inL, outL) <- zip3 [1 :: Int ..] (lines' t) (lines' o.text),
      not (inRegion n),
      not ("#" `Text.isPrefixOf` inL)
    ]
  where
    dropCr l = fromMaybe l (Text.stripSuffix "\r" l)
    lines' = Text.splitOn "\n"

spansInside :: Property
spansInside = property $ do
  t <- forAll (genFile genCode)
  let o = runCpp env [] t
      outLs = Text.splitOn "\n" o.text
      inLs = Text.splitOn "\n" t
  (l1, c1, l2, c2) <- forAll (genSpan outLs)
  let s = mapSpan o.positions (spanOf l1 c1 l2 c2)
      (a, b) = (posOf s.startLine s.startCol, posOf s.endLine s.endCol)
  annotate (show s)
  assert (a <= b)
  assert (inside inLs a && inside inLs b)
  where
    inside ls (l, c) =
      l >= 1 && l <= length ls && c >= 1 && c <= Text.length (ls !! (l - 1)) + 1
    posOf (Line l) (Col c) = (fromIntegral l, fromIntegral c) :: (Int, Int)

copiedText :: Property
copiedText = property $ do
  t <- forAll (genFile genCode)
  let o = runCpp env [] t
      PositionMap m = o.positions
      inLs = Text.splitOn "\n" t
      charAt l c = case drop (l - 1) inLs of
        x : _ | c >= 1, c <= Text.length x -> Just (Text.index x (c - 1))
        _ -> Nothing
  sequence_
    [ (n, k, Just ch) === (n, k, charAt from.line from.col)
    | (n, outL) <- zip [1 :: Int ..] (Text.splitOn "\n" o.text),
      (k, ch) <- zip [1 :: Int ..] (Text.unpack outL),
      Just from <- [copiedFrom (IntMap.lookup n m) n k]
    ]
  where
    -- Where a copied char comes from; Nothing in an expansion.
    copiedFrom entry n k = case entry of
      Nothing -> Just (Pos n k)
      Just cols -> case IntMap.lookupLE k cols of
        Just (k0, Copied p) -> Just p {col = p.col + k - k0}
        _ -> Nothing

expandedAtName :: Property
expandedAtName = property $ do
  t <- forAll (genFile genCode)
  let o = runCpp env [] t
      PositionMap m = o.positions
      inLs = Text.splitOn "\n" t
      nameAt p = case drop (p.line - 1) inLs of
        l : _ -> Text.takeWhile isNameChar (Text.drop (p.col - 1) l)
        [] -> ""
  sequence_
    [ annotate (show (from, nameAt from)) >> assert (nameAt from `elem` names)
    | cols <- IntMap.elems m,
      Expanded from _ <- IntMap.elems cols
    ]
  where
    isNameChar c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c
    names = ["FOO", "F", "G", "H", "R", "__LINE__", "MIN_VERSION_base"]

regionsOk :: Property
regionsOk = property $ do
  t <- forAll (genFile genCode)
  let o = runCpp env [] t
      outLs = Text.splitOn "\n" o.text
  sequence_ [assert (r.firstLine <= r.lastLine && r.firstLine >= 1) | r <- o.regions]
  sequence_
    [assert (a.lastLine < b.firstLine) | (a, b) <- zip o.regions (drop 1 o.regions)]
  sequence_
    [ (n, outLs !! (n - 1)) === (n, "")
    | r <- o.regions,
      n <- [r.firstLine .. r.lastLine]
    ]
  sequence_ [assert (r.lastLine <= length outLs) | r <- o.regions]

identity :: Property
identity = property $ do
  (a, b, c, d) <- forAll (genSpan (replicate 100 (Text.replicate 50 "x")))
  mapSpan sameLines (spanOf a b c d) === spanOf a b c d

-- * Helpers

-- | A span in these lines, start before end.
genSpan :: [Text] -> Gen (Int, Int, Int, Int)
genSpan ls = do
  l1 <- Gen.int (Range.constant 1 (length ls))
  l2 <- Gen.int (Range.constant l1 (length ls))
  c1 <- Gen.int (Range.constant 1 (width l1))
  c2 <- Gen.int (Range.constant (if l1 == l2 then c1 else 1) (width l2))
  pure (l1, c1, l2, c2)
  where
    width l = Text.length (ls !! (l - 1)) + 1

spanOf :: Int -> Int -> Int -> Int -> Span
spanOf a b c d =
  Span (Line (w a)) (Col (w b)) (Line (w c)) (Col (w d))
  where
    w = fromIntegral
