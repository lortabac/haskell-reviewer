{-# LANGUAGE StrictData #-}

-- StrictData: a case's fields are built with the test tree, in one
-- thread. See Note [Shared values in tests] in
-- "HaskellReviewer.Core.PackageGlobTest".

-- | Golden tests for the occurrence table: each case is a file in
-- fixtures/occurrences, parsed with the settings below, its table
-- dumped next to it.
--
-- Tests run from the package dir, so fixtures/occurrences is one level
-- up.
module HaskellReviewer.Core.OccurrencesGoldenTest (tests, cases, readCase, Case (..)) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import HaskellReviewer.Effects.Parser (parseGuarded)
import OccurrencesDump (dumpOccurrences)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Occurrence fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [ goldenVsString c.golden (fixtureDir <> c.golden <> ".golden") (dump c)
        | c <- cases
        ]

-- | One golden file: a fixture parsed with some settings.
data Case = Case
  { -- | The golden file, without .golden.
    golden :: FilePath,
    file :: FilePath,
    settings :: ParseSettings
  }

-- | A case named after its file, with plain settings (GHC2021). The
-- fixtures turn on what they need with LANGUAGE pragmas.
plain :: FilePath -> Case
plain f = Case {golden = dropExtension f, file = f, settings = defaultParseSettings}

-- | Value-level cases: type variables are all free.
cases :: [Case]
cases =
  map
    plain
    [ "shadowing.hs",
      "where-guards.hs",
      "do-mdo.hs",
      "comprehensions.hs",
      "lambda-case.hs",
      "view-patterns.hs",
      "records.hs",
      "operators.hs",
      "classes-instances.hs",
      "th.hs",
      "implicit-params.hs",
      "misc-names.hs",
      "cpp.hs",
      "arrows.hs",
      "exact-names.hs"
    ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/occurrences/"

-- | A fixture's text.
readCase :: Case -> IO Text
readCase c = Text.Encoding.decodeUtf8Lenient <$> BS.readFile (fixtureDir <> c.file)

-- | Parse a fixture and dump its table.
dump :: Case -> IO LBS.ByteString
dump c = do
  text <- readCase c
  r <- parseGuarded c.settings text
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpOccurrences text r)))

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort (nub (map (.file) cases))
