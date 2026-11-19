-- | Golden tests for the line diff: each pair @<name>.base.<ext>@ and
-- @<name>.head.<ext>@ in fixtures/linediff is diffed in both modes
-- (@<name>.golden@ and @<name>.w.golden@), and each result is checked
-- against git. @sources.txt@ says where the real pairs come from.
--
-- Tests run from the package dir, so fixtures/linediff is one level up.
module HaskellReviewer.Core.LineDiffGoldenTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.LineDiff
  ( GitRule,
    LineDiff (..),
    Stats (..),
    Whitespace (..),
    lineDiffStatsAs,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import LineDiffDump (dumpLineDiff)
import LineDiffSupport (gitRule, gitRunsMany)
import System.Directory qualified as Dir
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | A pair: the name, and the extension of its files.
data Case = Case {name :: String, ext :: String}

tests :: TestTree
tests =
  testGroup "LineDiff golden" $
    [ testCase "every pair has a golden, and the other way round" noStale,
      withResource loadAll (const (pure ())) $ \getPairs ->
        withResource gitRule (const (pure ())) $ \getRule ->
          testGroup
            "oracle"
            [ testCase (modeName ws) (oracle getRule getPairs ws)
            | ws <- modes
            ]
    ]
      <> [ goldenVsString
             (c.name <> " (" <> modeName ws <> ")")
             (fixtureDir </> c.name <> goldenExt ws)
             (dump ws <$> load c)
         | c <- cases,
           ws <- modes
         ]
  where
    dump ws p = LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpLineDiff ws p))
    loadAll = traverse (\c -> (c.name,) <$> load c) cases

modes :: [Whitespace]
modes = [KeepWhitespace, IgnoreWhitespace]

modeName :: Whitespace -> String
modeName = \case
  KeepWhitespace -> "exact"
  IgnoreWhitespace -> "-w"

goldenExt :: Whitespace -> String
goldenExt = \case
  KeepWhitespace -> ".golden"
  IgnoreWhitespace -> ".w.golden"

cases :: [Case]
cases =
  [ Case "apigolden" "hs",
    Case "app" "tsx",
    Case "blanks-braces" "txt",
    Case "bom" "txt",
    Case "bom-removed" "txt",
    Case "cabal" "cabal",
    Case "client" "ts",
    Case "convert" "hs",
    Case "crlf-both" "txt",
    Case "crlf-real" "hs",
    Case "empty-to-text" "txt",
    Case "mixed-eol" "txt",
    Case "modulemodel" "hs",
    Case "newline-added" "txt",
    Case "newline-dropped" "txt",
    Case "open-ctx" "txt",
    Case "open-w" "txt",
    Case "reindent" "hs",
    Case "router" "hs",
    Case "slider" "txt",
    Case "snapshot" "hs",
    Case "spec" "md",
    Case "testmain" "hs",
    Case "testsupport" "hs",
    Case "text-to-empty" "txt"
  ]

fixtureDir :: FilePath
fixtureDir = "../fixtures/linediff"

-- | Read a pair. Bytes are decoded as UTF-8 and nothing else changes
-- (a BOM stays, CRLF stays).
load :: Case -> IO (Pair Text)
load c = do
  b <- side "base"
  h <- side "head"
  pure Pair {head = h, base = b}
  where
    side s =
      Text.Encoding.decodeUtf8Lenient
        <$> BS.readFile (fixtureDir </> c.name <> "." <> s <> "." <> c.ext)

-- | Our runs match git's on every pair, per mode. git can't see a BOM
-- (we drop it and make a note), so it is cut off first.
oracle :: IO GitRule -> IO [(String, Pair Text)] -> Whitespace -> IO ()
oracle getRule getPairs ws = do
  rule <- getRule
  named <- getPairs
  let noBom t = fromMaybe t (Text.stripPrefix "\xFEFF" t)
      git = [Pair {head = noBom p.head, base = noBom p.base} | (_, p) <- named]
  gitRunsMany ws git >>= \case
    Left err -> assertFailure err
    Right rs -> do
      let ours p = (lineDiffStatsAs rule ws p).diff.runs
          bad = [n | ((n, p), r) <- zip named rs, ours p /= r]
      bad @?= []

-- | Fail if a fixture, a golden file or a source has no case. A pair
-- whose two files have different extensions fails here too.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  let other = ["sources.txt", ".gitattributes"]
      fixtures = [f | f <- files, takeExtension f /= ".golden", f `notElem` other]
      names = map (.name) cases
      sides c = [c.name <> ".base." <> c.ext, c.name <> ".head." <> c.ext]
  sort fixtures @?= sort (concatMap sides cases)
  sort [f | f <- files, takeExtension f == ".golden"]
    @?= sort (concat [[n <> ".golden", n <> ".w.golden"] | n <- names])
  sources <- readFile (fixtureDir </> "sources.txt")
  let real = [n | l <- lines sources, n : _ <- [words l]]
  sort [n | n <- real, n `notElem` names] @?= []
