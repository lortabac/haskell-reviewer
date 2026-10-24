{-# LANGUAGE StrictData #-}

-- StrictData: a case's fields are built with the test tree, in one
-- thread. See Note [Shared values in tests] in
-- "HaskellReviewer.Core.PackageGlobTest".

-- | Golden tests for the parser: each case is a file in fixtures/parse,
-- parsed with the settings below, dumped next to it.
--
-- Tests run from the package dir, so fixtures/parse is one level up.
module HaskellReviewer.Core.ParseGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
  )
import HaskellReviewer.Core.Project (Extension (..), Language (..))
import HaskellReviewer.Effects.Parser (parseGuarded)
import ParseDump (dumpParse)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Parse fixtures" $
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

-- | A case named after its file, with plain settings (GHC2021).
plain :: FilePath -> Case
plain f = Case {golden = dropExtension f, file = f, settings = defaultParseSettings}

cases :: [Case]
cases =
  [ -- Haddock, deprecation, every export form.
    (plain "header-full.hs") {settings = with {language = GHC2024}},
    -- Every import form, and a WARNING with a category.
    plain "imports.hs",
    plain "no-module-line.hs",
    -- Component flags, then pragmas: NoX, unknown names, ignored flags.
    (plain "extensions.hs")
      { settings =
          with
            { language = Haskell2010,
              extensions =
                [ext "OverloadedStrings", off "ImplicitPrelude", ext "NoSuchThing"],
              ghcOptions = ["-Wall", "-XRecordWildCards", "-XAlsoUnknown"]
            }
      },
    (plain "extensions.hs")
      { golden = "unknown-language",
        settings = with {language = UnknownLanguage "Haskell2030"}
      },
    -- Parses only with LambdaCase.
    (plain "needs-extension.hs")
      { settings = with {language = Haskell2010, extensions = [ext "LambdaCase"]}
      },
    (plain "needs-extension.hs")
      { golden = "needs-extension-missing",
        settings = with {language = Haskell2010}
      },
    -- The error's column counts the tab as one.
    plain "tab-error.hs",
    plain "bom.hs",
    plain "crlf.hs",
    plain "unicode.hs",
    (plain "bird.lhs") {settings = with {literate = BirdOrLatex}},
    (plain "latex.lhs") {settings = with {literate = BirdOrLatex}},
    -- #if outside the code: kept by unlit, so cpp sees it.
    (plain "bird-cpp.lhs")
      { settings = with {literate = BirdOrLatex, extensions = [ext "CPP"]}
      },
    (plain "markdown.lhs") {settings = with {literate = MarkdownUnlit}},
    (plain "markdown-no-code.lhs") {settings = with {literate = MarkdownUnlit}},
    (plain "sig.hsig") {settings = with {kind = SignatureFile}},
    plain "hspec-discover.hs",
    -- -pgmF without -F: parsed.
    plain "pgmf-no-f.hs",
    -- -F from ghc-options, -pgmF from the pragma.
    (plain "pgmf-pragma.hs") {settings = with {ghcOptions = ["-F"]}},
    plain "cpp-directives.hs",
    -- CPP on, no directive: parsed as-is.
    plain "cpp-plain.hs",
    -- -cpp in ghc-options, with directives.
    (plain "cpp-option.hs") {settings = with {ghcOptions = ["-cpp"]}},
    (plain "cpp-option.hs") {golden = "cpp-option-off"},
    plain "unknown-language-pragma.hs",
    -- GHC with -haddock rejects "{- #" too; without, it parses.
    plain "haddock-quirk.hs"
  ]
  where
    with = defaultParseSettings
    ext n = Extension {name = n, enabled = True}
    off n = Extension {name = n, enabled = False}

fixtureDir :: FilePath
fixtureDir = "../fixtures/parse/"

-- | Parse a fixture and dump the result.
dump :: Case -> IO LBS.ByteString
dump c = do
  bytes <- BS.readFile (fixtureDir <> c.file)
  r <- parseGuarded c.settings (decode bytes)
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpParse r)))
  where
    decode :: BS.ByteString -> Text
    decode = Text.Encoding.decodeUtf8Lenient

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort (nub (map (.file) cases))
