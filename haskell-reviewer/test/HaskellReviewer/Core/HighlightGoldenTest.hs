{-# LANGUAGE StrictData #-}

-- StrictData: a case's fields are built with the test tree, in one
-- thread. See Note [Shared values in tests] in
-- "HaskellReviewer.Core.PackageGlobTest".

-- | Golden tests for highlighting: each case is a file in
-- fixtures/highlight, parsed with the settings below, its runs dumped
-- next to it.
--
-- Tests run from the package dir, so fixtures/highlight is one level up.
module HaskellReviewer.Core.HighlightGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Highlight (DisplayClass, Run (..), highlightModule, runList)
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import HaskellReviewer.Core.Project (Extension (..))
import HaskellReviewer.Effects.Parser (parseGuarded)
import HighlightDump (dumpHighlight)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Highlight fixtures" $
    [ testCase "every fixture has a test, and the other way round" noStale,
      testCase "every class shows up in some fixture" $ do
        classes <- concat <$> mapM (fmap (map (.display)) . runsOf) cases
        sort (nub classes) @?= ([minBound .. maxBound] :: [DisplayClass])
    ]
      <> [ goldenVsString c.golden (fixtureDir <> c.golden <> ".golden") (dump c)
         | c <- cases
         ]

-- | One golden file: a fixture parsed with some settings.
data Case = Case
  { -- | The golden file, without .golden.
    golden :: FilePath,
    file :: FilePath,
    settings :: ParseSettings
  }

-- | A case named after its file, with plain settings (GHC2021).
plain :: FilePath -> Case
plain f = Case {golden = dropExtension f, file = f, settings = defaultParseSettings}

cases :: [Case]
cases =
  [ -- Every token class, a tab, an astral char before a token.
    plain "basic.hs",
    plain "crlf.hs",
    plain "bom.hs",
    plain "line-pragma.hs",
    -- A comment across bird chunks, cut by the prose.
    (plain "bird.lhs") {settings = with {literate = BirdOrLatex}},
    (plain "latex.lhs") {settings = with {literate = BirdOrLatex}},
    (plain "crlf-bird.lhs") {settings = with {literate = BirdOrLatex}},
    (plain "markdown.lhs") {settings = with {literate = MarkdownUnlit}},
    -- No Haskell block: all prose, with a NoMarkdownCode warning.
    (plain "markdown-no-code.lhs") {settings = with {literate = MarkdownUnlit}},
    -- Text only, no tokens and no regions: we don't lex it (D-225).
    (plain "markdown-moves.lhs") {settings = with {literate = MarkdownUnlit}},
    (plain "loose.lhs") {settings = with {literate = LooseLiterate}},
    -- Directives, excluded branches, a macro, a comment cut by #if lines.
    plain "cpp.hs",
    -- Prose in an excluded branch: CPP wins.
    (plain "bird-cpp.lhs")
      { settings = with {literate = BirdOrLatex, extensions = [ext "CPP"]}
      },
    -- Lexes, fails to parse: still highlighted.
    plain "parse-failure.hs",
    plain "parse-failure-cpp.hs",
    -- Fails to lex: CPP regions only.
    plain "lex-failure.hs",
    -- Text only, no tokens, CPP regions kept.
    plain "pragma-failure.hs",
    -- -pgmF: plain.
    plain "pgmf.hs"
  ]
  where
    with = defaultParseSettings
    ext n = Extension {name = n, enabled = True}

fixtureDir :: FilePath
fixtureDir = "../fixtures/highlight/"

readFixture :: Case -> IO Text
readFixture c = Text.Encoding.decodeUtf8Lenient <$> BS.readFile (fixtureDir <> c.file)

runsOf :: Case -> IO [Run]
runsOf c = do
  text <- readFixture c
  runList . highlightModule <$> parseGuarded c.settings text

-- | Parse a fixture and dump its runs.
dump :: Case -> IO LBS.ByteString
dump c = do
  text <- readFixture c
  r <- parseGuarded c.settings text
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpHighlight text r)))

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort (nub (map (.file) cases))
