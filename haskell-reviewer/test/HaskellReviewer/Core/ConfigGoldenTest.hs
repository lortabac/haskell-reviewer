-- | Golden tests for config.toml: each file in fixtures/config is
-- parsed, and the defaults and warnings dumped next to it.
--
-- Tests run from the package dir, so fixtures/config is one level up.
module HaskellReviewer.Core.ConfigGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Config
  ( ParsedConfig (..),
    UiDefaults (..),
    parseConfig,
    renderConfigWarning,
    sidebarName,
    themeName,
  )
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Config fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [ goldenVsString f (path f ".golden") (dump (path f ".toml"))
        | f <- cases
        ]
  where
    path f ext = fixtureDir <> f <> ext

cases :: [FilePath]
cases =
  [ "bad-enum",
    "bom",
    "comments-only",
    "control-chars",
    "dotted-keys",
    "duplicate-key",
    "duplicate-table",
    "empty",
    "full-valid",
    "one-bad-key",
    "syntax-error",
    "ui-not-table",
    "ui-subtable",
    "unknown-key",
    "unknown-table",
    "wrong-type"
  ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/config/"

-- | The defaults we get, then the warnings, as the user sees them.
dump :: FilePath -> IO LBS.ByteString
dump f = do
  bytes <- BS.readFile f
  let parsed = parseConfig (Text.Encoding.decodeUtf8 bytes)
      d = parsed.defaults
  pure . LBS.fromStrict . Text.Encoding.encodeUtf8 . Text.unlines $
    [ "theme: " <> themeName d.theme,
      "exported-only: " <> bool d.exportedOnly,
      "sidebar: " <> sidebarName d.sidebar,
      "== warnings"
    ]
      <> map (renderConfigWarning "config.toml") parsed.warnings
  where
    bool :: Bool -> Text
    bool b = if b then "true" else "false"

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"] @?= sort cases
  sort [dropExtension f | f <- files, takeExtension f == ".toml"] @?= sort cases
