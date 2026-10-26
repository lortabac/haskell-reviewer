-- | Golden tests for CPP: each file in fixtures/cpp is run with GHC
-- 9.14.1 on x86_64 Linux, depending on base, and dumped next to it.
-- A first line @-- cpp-flags: -DX ...@ adds flags (as OPTIONS_GHC
-- would).
--
-- Tests run from the package dir, so fixtures/cpp is one level up.
module HaskellReviewer.Core.CppGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Cpp
  ( CppEnv (..),
    CppOutput (..),
    FlagSource (..),
    MacroFlags (..),
    Piece (..),
    Pos (..),
    PositionMap (..),
    Region (..),
    RegionKind (..),
    macroFlags,
    renderCppWarning,
    runCpp,
  )
import HaskellReviewer.Core.Project (GhcVersion (..), PackageName (..), Platform (..))
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Cpp fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [ goldenVsString f (path f ".golden") (dump (path f ".hs"))
        | f <- cases
        ]
  where
    path f ext = fixtureDir <> f <> ext

cases :: [FilePath]
cases =
  ["basic", "directives", "flags", "macros", "positions", "real-world", "traditional"]

fixtureDir :: FilePath
fixtureDir = "../fixtures/cpp/"

goldenEnv :: CppEnv
goldenEnv =
  CppEnv
    { ghc = GhcVersion [9, 14, 1],
      platform = Platform {os = "linux", arch = "x86_64"},
      flags = [],
      buildDepends = [PackageName "base"]
    }

dump :: FilePath -> IO LBS.ByteString
dump f = do
  t <- Text.Encoding.decodeUtf8Lenient <$> BS.readFile f
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpCpp t)))

-- | Output text, regions, warnings and the position map.
dumpCpp :: Text -> Text
dumpCpp t =
  Text.unlines $
    ["== text"]
      <> Text.splitOn "\n" o.text
      <> ["== regions"]
      <> [kind r.kind <> " " <> num r.firstLine <> "-" <> num r.lastLine | r <- o.regions]
      <> ["== warnings"]
      <> map renderCppWarning o.warnings
      <> ["== positions"]
      <> [ num n <> ":" <> Text.concat (map piece (IntMap.toList cols))
         | (n, cols) <- IntMap.toList m
         ]
  where
    o = runCpp goldenEnv pragmaFlags t
    PositionMap m = o.positions
    pragmaFlags = case Text.stripPrefix "-- cpp-flags:" (Text.takeWhile (/= '\n') t) of
      Just ws -> (macroFlags GhcOptions (Text.words ws)).found
      Nothing -> []
    kind = \case
      DirectiveLines -> "directive"
      Excluded -> "excluded"
    piece (k, p) =
      " " <> num k <> case p of
        Copied a -> " copied " <> pos a
        Expanded a b -> " expanded " <> pos a <> "-" <> pos b
    pos p = num p.line <> ":" <> num p.col
    num = Text.pack . show

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"] @?= sort cases
  sort [dropExtension f | f <- files, takeExtension f == ".hs"] @?= sort cases
