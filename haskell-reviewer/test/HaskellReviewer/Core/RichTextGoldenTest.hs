-- | Golden tests for doc comments as rich text: each case is a file in
-- fixtures/richtext, parsed, its doc comments dumped next to it.
--
-- Tests run from the package dir, so fixtures/richtext is one level up.
module HaskellReviewer.Core.RichTextGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Parser (defaultParseSettings)
import HaskellReviewer.Effects.Parser (parseGuarded)
import RichDump (dumpRich)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Rich text fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [goldenVsString (dropExtension f) (golden f) (dump f) | f <- cases]

cases :: [FilePath]
cases =
  [ -- Every construct, a module header with fields (kept as text).
    "markup.hs",
    -- Raw HTML, images, unsafe and safe links.
    "links.hs",
    -- Names and what they resolve to in the module.
    "identifiers.hs",
    -- Section headings and chunks, in the export list and among the
    -- declarations.
    "headings.hs"
  ]

golden :: FilePath -> FilePath
golden f = fixtureDir <> dropExtension f <> ".golden"

fixtureDir :: FilePath
fixtureDir = "../fixtures/richtext/"

dump :: FilePath -> IO LBS.ByteString
dump f = do
  bytes <- BS.readFile (fixtureDir <> f)
  let text = Text.Encoding.decodeUtf8Lenient bytes
  r <- parseGuarded defaultParseSettings text
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpRich text r)))

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map dropExtension cases)
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort cases
