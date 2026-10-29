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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Cpp (CppEnv (..))
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Modules qualified as Modules
  ( ComponentSettings (..),
    HaskellFile (..),
  )
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
    moduleParseSettings,
  )
import HaskellReviewer.Core.Project
  ( Extension (..),
    GhcVersion (..),
    Language (..),
    PackageName (..),
    Platform (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.RelPath (relPathFromSegments)
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
    -- -cpp in ghc-options, with indented directives: text to cpp.
    (plain "cpp-option.hs") {settings = with {ghcOptions = ["-cpp"]}},
    (plain "cpp-option.hs") {golden = "cpp-option-off"},
    -- MIN_VERSION_pkg is 1 for any package, defined only for
    -- build-depends.
    (plain "cpp-min-version.hs")
      { settings = withCpp (cppWith {buildDepends = [PackageName "text-short"]})
      },
    (plain "cpp-min-version.hs") {golden = "cpp-min-version-no-dep"},
    (plain "cpp-os-arch.hs")
      { golden = "cpp-os-arch-linux",
        settings = withCpp (cppWith {platform = Platform "linux" "x86_64"})
      },
    (plain "cpp-os-arch.hs")
      { golden = "cpp-os-arch-darwin",
        settings = withCpp (cppWith {platform = Platform "darwin" "aarch64"})
      },
    -- The pragmas are read again after CPP.
    plain "cpp-language-if0.hs",
    -- -D and -U from cpp-options, ghc-options and OPTIONS_GHC, in order.
    (plain "cpp-define-flags.hs")
      { settings =
          withFlags
            ["-DFROM_CPP=fromCpp", "-D", "SPACED", "-Iinclude", "-I", "inc", "-bogus"]
            ["-Wall", "-DFROM_GHC_OPTIONS", "-optP-DOPT_P=2", "-DUNDONE"]
      },
    plain "cpp-include.hs",
    -- -F -pgmF inside #if 1: preprocessed, as GHC.
    plain "cpp-pgmf.hs",
    -- A BOM, then CPP: columns on line 1 don't count it.
    plain "cpp-bom.hs",
    plain "cpp-error.hs",
    -- Names from a macro span the whole call; a tab before one.
    plain "cpp-macro-export.hs",
    -- The error's place is on the file, after a macro and joined lines.
    plain "cpp-parse-error.hs",
    -- A built-in macro and no directive: CPP still runs.
    plain "cpp-ghc-version.hs",
    (plain "cpp-ghc-version.hs")
      { golden = "cpp-ghc-version-910",
        settings = withCpp (cppWith {ghc = GhcVersion [9, 10]})
      },
    plain "unknown-language-pragma.hs",
    -- GHC with -haddock rejects "{- #" too; without, it parses.
    plain "haddock-quirk.hs"
  ]
  where
    with = defaultParseSettings
    cppWith = defaultParseSettings.cpp
    withCpp env = with {cpp = env}
    -- Through moduleParseSettings, as the server does.
    withFlags cppOpts ghcOpts =
      moduleParseSettings
        parserGhcVersion
        Modules.ComponentSettings
          { Modules.language = GHC2021,
            Modules.extensions = [],
            Modules.ghcOptions = ghcOpts,
            Modules.cppOptions = cppOpts,
            Modules.buildDepends = []
          }
        Modules.HaskellFile {Modules.path = file, Modules.literate = NotLiterate}
    file = fromMaybe (error "bad path") (relPathFromSegments ["M.hs"])
    ext n = Extension {name = n, enabled = True}
    off n = Extension {name = n, enabled = False}

fixtureDir :: FilePath
fixtureDir = "../fixtures/parse/"

-- | Parse a fixture and dump the result.
dump :: Case -> IO LBS.ByteString
dump c = do
  bytes <- BS.readFile (fixtureDir <> c.file)
  let text = decode bytes
  r <- parseGuarded c.settings text
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpParse text r)))
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
