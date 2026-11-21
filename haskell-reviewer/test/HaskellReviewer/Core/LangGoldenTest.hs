-- | Golden tests for the highlighting rules (Note [Language rules] in
-- "HaskellReviewer.Core.Lang"). Each file in fixtures/lang is parsed,
-- and the rules and warnings dumped next to it. The rules we ship
-- (highlight/ in the repo) must have no warnings, and the @.scm@ files
-- of fixtures/highlight are scanned with them.
--
-- Tests run from the package dir, so the fixtures are one level up.
module HaskellReviewer.Core.LangGoldenTest (tests, langCases) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Config (renderConfigWarning)
import HaskellReviewer.Core.Lang
  ( BlockComment (..),
    Lang (..),
    ParsedLang (..),
    StringRule (..),
    parseLang,
    scanLang,
  )
import HighlightDump (className, dumpLang)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Lang fixtures" $
    [ testCase "every fixture has a test, and the other way round" noStale,
      testCase "the rules we ship have no warnings" $ do
        files <- filter ((== ".toml") . takeExtension) <$> Dir.listDirectory shippedDir
        (files /= []) @?= True
        forM_ files $ \f -> do
          parsed <- parseLang <$> readText (shippedDir <> f)
          map (renderConfigWarning f) parsed.warnings @?= []
          (f, fmap (const ()) parsed.lang) @?= (f, Just ())
    ]
      <> [ goldenVsString f (path f ".golden") (dump (path f ".toml"))
         | f <- cases
         ]
      <> [ goldenVsString f (highlightDir <> dropExtension f <> ".golden") (scan rules f)
         | (f, rules) <- langCases
         ]
  where
    path f ext = fixtureDir <> f <> ext

cases :: [FilePath]
cases =
  [ "bad-values",
    "bom",
    "duplicate-key",
    "full-valid",
    "minimal",
    "no-extensions",
    "syntax-error",
    "unknown-keys",
    "words-not-table",
    "wrong-types"
  ]

-- | Files of fixtures/highlight, and the shipped rules to scan them
-- with. HighlightGoldenTest counts them in its stale check.
langCases :: [(FilePath, FilePath)]
langCases =
  [ -- Every kind of run, a tab, astral chars, a string with no end.
    ("scheme-basic.scm", "scheme.toml"),
    -- A BOM, CRLF, a lone CR at the end.
    ("scheme-crlf.scm", "scheme.toml"),
    -- Hashbang, the three string kinds, numbers, `this.x`.
    ("javascript-basic.js", "javascript.toml"),
    -- TS keywords and types.
    ("typescript-basic.ts", "typescript.toml")
  ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/lang/"

highlightDir :: FilePath
highlightDir = "../fixtures/highlight/"

shippedDir :: FilePath
shippedDir = "../highlight/"

readText :: FilePath -> IO Text
readText f = Text.Encoding.decodeUtf8 <$> BS.readFile f

-- | The rules we get, then the warnings, as the user sees them.
dump :: FilePath -> IO LBS.ByteString
dump f = do
  parsed <- parseLang <$> readText f
  pure . LBS.fromStrict . Text.Encoding.encodeUtf8 . Text.unlines $
    maybe ["ignored"] rules parsed.lang
      <> ["== warnings"]
      <> map (renderConfigWarning "x.toml") parsed.warnings
  where
    rules l =
      [ "extensions: " <> Text.unwords l.extensions,
        "word-chars: " <> Text.pack (Set.toList l.wordChars),
        "line-comments: " <> Text.unwords l.lineComments,
        "char-prefixes: " <> Text.unwords l.charPrefixes,
        "numbers: " <> if l.numbers then "true" else "false"
      ]
        <> map block l.blockComments
        <> map string l.strings
        <> [ "word: " <> w <> " " <> className c
           | (w, c) <- Map.toList l.wordClasses
           ]
    block b =
      Text.unwords $
        ["block-comment:", b.open, b.close] <> ["nested" | b.nested]
    string s =
      Text.unwords $
        ["string:", s.open, s.close]
          <> maybe [] (\c -> ["escape " <> Text.singleton c]) s.escape

-- | Scan a file of fixtures/highlight with shipped rules, dump its runs.
scan :: FilePath -> FilePath -> IO LBS.ByteString
scan rules f = do
  parsed <- parseLang <$> readText (shippedDir <> rules)
  l <- maybe (assertFailure ("no rules in " <> rules)) pure parsed.lang
  text <- readText (highlightDir <> f)
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpLang text (scanLang l text))))

-- | Fail if a fixture or golden file has no case.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"] @?= sort cases
  sort [dropExtension f | f <- files, takeExtension f == ".toml"] @?= sort cases
