-- | Shared by the line diff tests: generators, a check of the runs,
-- and git as an oracle.
module LineDiffSupport
  ( -- * Generators
    genPair,
    genPlainPair,
    genRepetitivePair,
    genBigPair,
    genUnrelatedPair,
    genHugePair,
    hugePair,
    genLines,
    renderLf,

    -- * Checks
    checkRuns,
    equalPairs,

    -- * Running
    chunks,

    -- * Oracle
    gitRuns,
    gitRunsMany,
    gitRule,
    parseGitVersion,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.LineDiff
  ( GitRule (..),
    LineDiff (..),
    Op (..),
    Run (..),
    Whitespace (..),
    diffLines,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import Hedgehog (Gen, Property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.Hedgehog (testProperty)
import TestSupport (GitRun (..), gitBytesEnv, withCanonicalTemp)

-- * Generators

-- | One line, weighted towards what makes diffs interesting: blank
-- lines, closers, indented lines, whitespace variants.
genLine :: Gen Text
genLine =
  Gen.frequency
    [ (4, pure ""),
      (3, Gen.element ["}", "end", "where"]),
      (4, Gen.element ["  x = 1", "    x = 1", "\tx = 1"]),
      (2, Gen.element ["-- a comment", "-- | Docs."]),
      (4, ("line " <>) . Text.pack . show <$> Gen.int (Range.linear 0 300)),
      ( 2,
        Gen.element
          ["x  = 1", "x = 1 ", " x = 1", "x\v= 1", "x\f= 1", "x\xA0= 1", "é", "x = 1"]
      )
    ]

-- | Some lines.
genLines :: Int -> Gen [Text]
genLines n = Gen.list (Range.linear 0 n) genLine

-- | Head from base, by up to 5 edits.
genEdits :: Gen Text -> [Text] -> Gen [Text]
genEdits = genEditsN 0 5

-- | Head from base, by @lo@ to @hi@ edits.
genEditsN :: Int -> Int -> Gen Text -> [Text] -> Gen [Text]
genEditsN lo hi line base = do
  k <- Gen.int (Range.linear lo hi)
  go k base
  where
    go :: Int -> [Text] -> Gen [Text]
    go 0 ls = pure ls
    go k ls = edit ls >>= go (k - 1)
    block ls = do
      i <- Gen.int (Range.constant 0 (length ls))
      len <- Gen.int (Range.constant 1 6)
      pure (i, len)
    newBlock = Gen.list (Range.linear 1 5) line
    edit ls = do
      (i, len) <- block ls
      let (before, rest) = splitAt i ls
          (mid, after) = splitAt len rest
      Gen.choice
        [ (\b -> before <> b <> rest) <$> newBlock,
          pure (before <> after),
          (\b -> before <> b <> after) <$> newBlock,
          do
            j <- Gen.int (Range.constant 0 (length ls))
            let (x, y) = splitAt j ls in pure (x <> mid <> y),
          do
            let rest' = before <> after
            j <- Gen.int (Range.constant 0 (length rest'))
            let (x, y) = splitAt j rest' in pure (x <> mid <> y),
          do
            f <- Gen.element [("  " <>), ("\t" <>), Text.stripStart]
            pure (before <> map f mid <> after),
          pure (before <> [""] <> mid <> [""] <> after)
        ]

-- | Lines to text: LF, CRLF or mixed, with or without a final newline.
render :: [Text] -> Gen Text
render ls = do
  style <- Gen.element [Lf, Crlf, Mixed]
  ends <- mapM (const (eol style)) ls
  final <- Gen.frequency [(4, pure True), (1, pure False)]
  let parts = zipWith (<>) ls ends
  pure $ case (final, parts) of
    (False, _ : _) -> Text.concat (init parts) <> last ls
    _ -> Text.concat parts
  where
    eol = \case
      Lf -> pure "\n"
      Crlf -> pure "\r\n"
      Mixed -> Gen.element ["\n", "\r\n"]

data Eol = Lf | Crlf | Mixed

-- | LF only, final newline.
renderLf :: [Text] -> Text
renderLf = Text.concat . map (<> "\n")

-- | A pair from 'genLine' lines. One in ten is two unrelated texts.
genPair :: Gen (Pair Text)
genPair = pairFrom genLine 40

-- | Like 'genPair', LF only, with a final newline.
genPlainPair :: Gen (Pair Text)
genPlainPair = do
  b <- genLines 40
  h <- genEdits genLine b
  pure Pair {head = renderLf h, base = renderLf b}

-- | Lines from a tiny alphabet, many copies of each: forces Myers
-- fallbacks. Edits bring in unique lines too, for the discard pass.
genRepetitivePair :: Gen (Pair Text)
genRepetitivePair = do
  let common = Gen.element ["x", "}", "", "  y = 2"]
      line =
        Gen.frequency
          [ (3, common),
            (1, ("u " <>) . Text.pack . show <$> Gen.int (Range.linear 0 50))
          ]
  b <- Gen.list (Range.linear 270 600) common
  h <- genEditsN 1 10 line b
  renderPair b h

-- | 1,200 to 3,000 lines of an 8-letter alphabet: big fallback
-- regions. Unrelated texts with a few long shared runs hit Myers' cost
-- limit; dense small edits give long snakes (the heuristic split itself
-- needs 'genHugePair').
genBigPair :: Gen (Pair Text)
genBigPair = Gen.choice [genUnrelatedPair, dense]
  where
    dense = do
      b <- Gen.list (Range.constant 1200 3000) bigLetter
      h <-
        concat
          <$> mapM
            ( \l ->
                Gen.frequency
                  [ (12, pure [l]),
                    (1, pure []),
                    (1, (: []) <$> bigLetter),
                    (1, (\x -> [l, x]) <$> bigLetter)
                  ]
            )
            b
      pure Pair {head = renderLf h, base = renderLf b}

-- | The unrelated half of 'genBigPair': it reaches Myers' cost limit,
-- taking the forward and the backward path.
genUnrelatedPair :: Gen (Pair Text)
genUnrelatedPair = do
  let noise = Gen.list (Range.constant 300 500) bigLetter
  shared <- Gen.list (Range.constant 3 5) (Gen.list (Range.constant 25 60) bigLetter)
  let side = do
        ns <- mapM (const noise) shared
        end <- noise
        pure (concat (concat (zipWith (\n s -> [n, s]) ns shared)) <> end)
  b <- side
  h <- side
  pure Pair {head = renderLf h, base = renderLf b}

bigLetter :: Gen Text
bigLetter = Gen.element ["a", "b", "c", "d", "e", "f", "g", "h"]

-- | About 34,000 lines a side of an 8-letter alphabet, with an edit
-- every 4 to 16 lines. git's Myers heuristic split can only fire when a
-- fallback region keeps more than 65,532 lines: below that its cost
-- limit (256) stops the search first. Built from a seed (an LCG), so
-- Hedgehog stays cheap; don't shrink it.
genHugePair :: Gen (Pair Text)
genHugePair =
  hugePair
    <$> Gen.int (Range.constant 1 1000000)
    <*> Gen.int (Range.constant 4 16)

-- | 'genHugePair' for a seed and an edit every @every@ lines (about).
hugePair :: Int -> Int -> Pair Text
hugePair seed every = Pair {head = renderLf h, base = renderLf b}
  where
    letter v = Text.singleton (toEnum (fromEnum 'a' + v `mod` 8))
    b = map letter (take 34000 (lcg seed))
    edit (l : ls) (r : r2 : rs) = case r `mod` (3 * every) of
      0 -> edit ls rs
      1 -> letter r2 : edit ls rs
      2 -> l : letter r2 : edit ls rs
      _ -> l : edit ls (r2 : rs)
    edit ls _ = ls
    h = edit b (lcg (seed + 1))
    lcg :: Int -> [Int]
    lcg s0 = map (`div` 65536) (drop 1 (iterate step s0))
    step x = (x * 1103515245 + 12345) `mod` 2147483648

pairFrom :: Gen Text -> Int -> Gen (Pair Text)
pairFrom line n = do
  b <- Gen.list (Range.linear 0 n) line
  h <-
    Gen.frequency
      [ (9, genEdits line b),
        (1, Gen.list (Range.linear 0 n) line)
      ]
  renderPair b h

-- | Base and head lines to a pair, each side rendered on its own.
renderPair :: [Text] -> [Text] -> Gen (Pair Text)
renderPair b h = do
  rb <- render b
  rh <- render h
  pure Pair {head = rh, base = rb}

-- * Checks

-- | The rules on 'runs' (normal form, totals), and every Equal pair
-- equal under the mode's key. Gives the first problem.
checkRuns :: Whitespace -> Pair Text -> LineDiff -> Either String ()
checkRuns ws p d = do
  let rs = d.runs
  check "a count below 1" (all ((>= 1) . (.count)) rs)
  let neighbours = zip rs (drop 1 rs)
  check "two neighbours with the same op" (all (\(a, b) -> a.op /= b.op) neighbours)
  check
    "Insert before Delete"
    (not (any (\(a, b) -> a.op == Insert && b.op == Delete) neighbours))
  let total o = sum [r.count | r <- rs, r.op == o]
      b = keyed p.base
      h = keyed p.head
  check "base total" (total Equal + total Delete == length b)
  check "head total" (total Equal + total Insert == length h)
  let bad = filter (not . uncurry (sameKey ws)) (equalPairs rs b h)
  check ("an Equal pair that differs: " <> show (take 1 bad)) (null bad)
  where
    check what ok = if ok then Right () else Left what

-- | A line, and whether it ends with a newline (only the last line
-- can lack one).
data Keyed = Keyed {text :: Text, newline :: Newline}
  deriving (Show)

data Newline = HasNewline | NoNewline
  deriving (Eq, Show)

keyed :: Text -> [Keyed]
keyed t =
  let ls = diffLines t
      -- Without a leading BOM (D-276).
      noBom = case Text.uncons t of
        Just ('\xFEFF', rest) -> rest
        _ -> t
      lastNewline = case Text.unsnoc noBom of
        Just (_, '\n') -> HasNewline
        _ -> NoNewline
      newlines = replicate (length ls - 1) HasNewline <> [lastNewline | not (null ls)]
   in zipWith Keyed ls newlines

-- | Equal under the mode: exact text and newline, or text without git's
-- whitespace.
sameKey :: Whitespace -> Keyed -> Keyed -> Bool
sameKey ws a b = case ws of
  KeepWhitespace -> a.text == b.text && a.newline == b.newline
  IgnoreWhitespace -> strip a.text == strip b.text
  where
    strip = Text.filter (`notElem` [' ', '\t', '\r', '\n'])

equalPairs :: [Run] -> [x] -> [x] -> [(x, x)]
equalPairs [] _ _ = []
equalPairs (r : rs) b h = case r.op of
  Equal ->
    zip (take r.count b) (take r.count h)
      <> equalPairs rs (drop r.count b) (drop r.count h)
  Delete -> equalPairs rs (drop r.count b) h
  Insert -> equalPairs rs b (drop r.count h)

-- * Running

-- | A property as @k@ tests (each with the property's own test count):
-- the same total, but the parts run in parallel, so the suite's tail
-- is shorter.
chunks :: Int -> TestName -> Property -> [TestTree]
chunks k name p =
  [testProperty (name <> " (" <> show i <> "/" <> show k <> ")") p | i <- [1 .. k]]

-- * Oracle

-- | git's runs for a pair: @git diff --no-index --histogram@ with full
-- context and pinned config, on copies of the texts with CRLF turned
-- into LF. Inputs must have no BOM, no lone final CR and no NUL.
gitRuns :: Whitespace -> Pair Text -> IO (Either String [Run])
gitRuns ws p =
  gitRunsMany ws [p] >>= \case
    Right [rs] -> pure (Right rs)
    Right other -> pure (Left ("gitRuns: " <> show (length other) <> " results"))
    Left err -> pure (Left err)

-- | 'gitRuns' for many pairs with one git run: pair i is @base/i@ and
-- @head/i@, and git diffs the two dirs file by file. One process per
-- pair made the oracle tests the suite's tail (git runs are slow to
-- start from a big test process).
gitRunsMany :: Whitespace -> [Pair Text] -> IO (Either String [[Run]])
gitRunsMany ws ps = withCanonicalTemp $ \dir -> do
  Dir.createDirectory (dir </> "base")
  Dir.createDirectory (dir </> "head")
  for_ (zip [0 :: Int ..] ps) $ \(i, p) -> do
    BS.writeFile (dir </> "base" </> show i) (toLf p.base)
    BS.writeFile (dir </> "head" </> show i) (toLf p.head)
  let sizes = [(length (diffLines p.base), length (diffLines p.head)) | p <- ps]
      context = maximum (1 : [n1 + n2 + 1 | (n1, n2) <- sizes])
      args =
        [ "-c",
          "core.autocrlf=false",
          "-c",
          "diff.algorithm=histogram",
          "-c",
          "diff.indentHeuristic=true",
          "-c",
          "diff.suppressBlankEmpty=false",
          "diff",
          "--no-index",
          "--no-renames",
          "--histogram",
          "--indent-heuristic",
          "--no-color",
          "--no-ext-diff",
          "--no-textconv"
        ]
          <> ["-w" | ws == IgnoreWhitespace]
          <> ["--unified=" <> show context, "base", "head"]
      env = [("GIT_CEILING_DIRECTORIES", takeDirectory dir)]
  run <- gitBytesEnv env dir args ""
  case run.code of
    ExitFailure c | c /= 1 -> do
      v <- gitBytesEnv env dir ["--version"] ""
      pure (Left ("git failed: " <> Char8.unpack run.err <> "; " <> Char8.unpack v.out))
    _ -> pure $ do
      found <- sections (Char8.lines run.out)
      let indexes = map fst found
          n = length ps
      unless (all (\i -> 0 <= i && i < n) indexes) $
        Left ("a file number out of range in " <> show indexes)
      unless (length (nubOrd indexes) == length indexes) $
        Left ("a file twice in " <> show indexes)
      let runsOf i (n1, _) = case lookup i found of
            Nothing -> Right [Run Equal n1 | n1 > 0]
            Just body -> parseRuns body
      sequence (zipWith runsOf [0 ..] sizes)

-- | git's output split by file: the pair number (from the
-- @diff --git a/base/i b/head/i@ line) and the lines after it. Any
-- other first line, or a header we can't read, is an error.
sections :: [ByteString] -> Either String [(Int, [ByteString])]
sections = \case
  [] -> Right []
  l : rest
    | isHeader l,
      Just (i, "") <- Char8.readInt (Char8.takeWhileEnd (/= '/') l) ->
        let (body, more) = break isHeader rest
         in ((i, body) :) <$> sections more
    | otherwise -> Left ("not a file header we can read: " <> show l)
  where
    isHeader = ("diff --git " `BS.isPrefixOf`)

-- | Which rule the git on PATH follows (see 'GitRule'). Fails if the
-- version can't be read.
gitRule :: IO GitRule
gitRule = withCanonicalTemp $ \dir -> do
  v <- gitBytesEnv [] dir ["--version"] ""
  either assertFailure pure (parseGitVersion v.out)

-- | The rule for a @git --version@ output: its first word that starts
-- with a digit is the version ("git version 2.50.1 (Apple Git-155)",
-- "2.54.0.windows.1", "2.54.0-rc0").
parseGitVersion :: ByteString -> Either String GitRule
parseGitVersion out =
  case filter startsWithDigit (Char8.words out) of
    version : _
      | Just (major, rest) <- Char8.readInt version,
        Just ('.', rest') <- Char8.uncons rest,
        Just (minor, _) <- Char8.readInt rest' ->
          Right (if (major, minor) >= (2, 54) then Git254 else GitBefore254)
    _ -> Left ("can't read the git version in " <> show out)
  where
    startsWithDigit w = maybe False (isDigit . fst) (Char8.uncons w)

-- | CRLF to LF.
toLf :: Text -> ByteString
toLf = go . Text.Encoding.encodeUtf8
  where
    go b = case BS.breakSubstring "\r\n" b of
      (x, y)
        | BS.null y -> x
        | otherwise -> x <> "\n" <> go (BS.drop 2 y)

-- | Runs from one file's full-context hunk (a file's lines in git's
-- output; git prints no hunk only for equal files, handled by the
-- caller).
parseRuns :: [ByteString] -> Either String [Run]
parseRuns ls = case dropWhile (not . ("@@ " `BS.isPrefixOf`)) ls of
  [] -> Left "no hunk in a file git says changed"
  _ : body -> rle <$> traverse opOf (filter (not . ("\\" `BS.isPrefixOf`)) body)
  where
    opOf l = case Char8.uncons l of
      Nothing -> Right Equal
      Just (' ', _) -> Right Equal
      Just ('-', _) -> Right Delete
      Just ('+', _) -> Right Insert
      _ -> Left ("unexpected git output line: " <> show l)
    rle = reverse . foldl' step []
    step (Run o k : acc) o' | o == o' = Run o (k + 1) : acc
    step acc o' = Run o' 1 : acc
