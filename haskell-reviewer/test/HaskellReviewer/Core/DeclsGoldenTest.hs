{-# LANGUAGE StrictData #-}

-- StrictData: a case's fields are built with the test tree, in one
-- thread. See Note [Shared values in tests] in
-- "HaskellReviewer.Core.PackageGlobTest".

-- | Golden tests for declarations: each case is a file in
-- fixtures/decls, parsed, its declarations dumped next to it.
--
-- Tests run from the package dir, so fixtures/decls is one level up.
module HaskellReviewer.Core.DeclsGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import DeclsDump (dumpDecls, dumpTokens)
import HaskellReviewer.Core.ModuleModel (ModuleParse)
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
  )
import HaskellReviewer.Effects.Parser (parseGuarded)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Declaration fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [ goldenVsString c.golden (fixtureDir <> c.golden <> ".golden") (dump c)
        | c <- cases
        ]

-- | One golden file: a fixture parsed with some settings, dumped.
data Case = Case
  { golden :: FilePath,
    file :: FilePath,
    settings :: ParseSettings,
    dumper :: Text -> ModuleParse -> Text
  }

-- | A case named after its file, with plain settings (GHC2021).
plain :: FilePath -> Case
plain f =
  Case
    { golden = dropExtension f,
      file = f,
      settings = defaultParseSettings,
      dumper = dumpDecls
    }

cases :: [Case]
cases =
  [ -- Every constructor form, strictness, unpack, deriving strategies.
    plain "data.hs",
    plain "newtype.hs",
    plain "gadt.hs",
    plain "synonyms-families.hs",
    plain "classes.hs",
    -- Merged equations, shared signatures, pragmas, fixities.
    plain "values.hs",
    plain "patsyn.hs",
    plain "exports-subs.hs",
    plain "exports-self.hs",
    -- No module line: only main is exported.
    plain "exports-none.hs",
    plain "exports-all.hs",
    plain "extents.hs",
    -- A macro in a head: one Macro token, the call's span.
    plain "cpp-decls.hs",
    (plain "cpp-decls.hs") {golden = "cpp-decls-tokens", dumper = dumpTokens},
    plain "tabs-unicode.hs",
    (plain "bird.lhs") {settings = defaultParseSettings {literate = BirdOrLatex}},
    (plain "sig.hsig") {settings = defaultParseSettings {kind = SignatureFile}},
    -- One of each other kind, with docs: each keeps its own, or GHC
    -- drops them, but they never go to a neighbour.
    plain "other-kinds.hs",
    -- Keys (contexts, multi-param, operators, lists, promoted,
    -- qualifiers, kinds), methods, associated instances.
    plain "instances.hs",
    -- Clause keys with the arity table, standalone deriving.
    plain "deriving.hs",
    plain "orphans.hs",
    -- Local and external families, data instance field parents.
    plain "family-instances.hs",
    -- RULES, COMPLETE, warnings, ANN, default, splices, foreign, roles.
    plain "pragmas.hs",
    (plain "tokens.hs") {dumper = dumpTokens},
    -- Checked against ghc -haddock --show-iface (9.14).
    plain "haddock-pairing.hs",
    -- Docs after a signature or constructor are in the extent.
    plain "arg-docs.hs",
    -- Spans stay on the file after LINE and COLUMN pragmas.
    plain "line-pragma.hs",
    (plain "line-pragma.hs") {golden = "line-pragma-tokens", dumper = dumpTokens},
    -- The old -} end, pragma text in comments and strings, lower case.
    plain "line-pragma-ends.hs",
    (plain "line-pragma-ends.hs")
      { golden = "line-pragma-ends-tokens",
        dumper = dumpTokens
      },
    -- Known limits (Note [LINE pragmas]): a COLUMN pragma before code
    -- changes the layout once blanked (text only); #line lines move
    -- positions, as in GHC.
    plain "line-pragma-column.hs",
    plain "hash-line.hs"
  ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/decls/"

dump :: Case -> IO LBS.ByteString
dump c = do
  bytes <- BS.readFile (fixtureDir <> c.file)
  let text = Text.Encoding.decodeUtf8Lenient bytes
  r <- parseGuarded c.settings text
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (c.dumper text r)))

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort (map (.golden) cases)
  sort [f | f <- files, takeExtension f /= ".golden"] @?= sort (nub (map (.file) cases))
