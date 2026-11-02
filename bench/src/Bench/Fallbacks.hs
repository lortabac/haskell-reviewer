-- | The fallback census (plan §3): every listed module, bucketed by why
-- it's only shown as text, checked against the counts we expect in
-- @bench\/fallbacks.txt@.
module Bench.Fallbacks
  ( -- * Categories
    Category (..),
    categoryText,
    deterministic,
    isTextOnly,

    -- * Census
    Hit (..),
    Census,
    Slot (..),
    PageError (..),
    classifyPage,
    countCensus,

    -- * Expected counts
    Expected (..),
    FallbacksError (..),
    renderFallbacksError,
    parseFallbacksFile,
    Lookup (..),
    lookupExpected,
    expectedTextOnly,

    -- * Comparing
    Diff (..),
    compareCensus,
    renderDiff,

    -- * Verdict
    FallbackVerdict (..),
    NotChecked (..),
    Strictness (..),
    fallbackVerdict,
    failsBench,
  )
where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Char (isDigit)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)

-- * Categories

-- | Why a module is text only, or has no page body at all. One per
-- @reason.kind@ of the module page (D-124), with two splits: no-source
-- by its reason, and preprocessed by where the preprocessor came from.
data Category
  = ParseError
  | PragmaError
  | ParserCrashed
  | TooSlow
  | -- | An @OPTIONS_GHC -F@ pragma in the file. The file has a slot.
    Preprocessed
  | -- | @-pgmF@\/@-pgmL@ in the component's flags. No slot (D-157).
    PreprocessedByFlags
  | Generated
  | NoSourceAutogen
  | NoSourceMissing
  | NoSourceOutside
  | NoSourceSymlink
  | NotText
  | TooLarge
  | -- | No answer: the module page was a 404 or a 500 (read error,
    -- worker crash, D-156).
    Failed
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The name in @bench\/fallbacks.txt@ and the report.
categoryText :: Category -> Text
categoryText = \case
  ParseError -> "parse-error"
  PragmaError -> "pragma-error"
  ParserCrashed -> "parser-crashed"
  TooSlow -> "too-slow"
  Preprocessed -> "preprocessed"
  PreprocessedByFlags -> "preprocessed-by-flags"
  Generated -> "generated"
  NoSourceAutogen -> "no-source/autogen"
  NoSourceMissing -> "no-source/missing"
  NoSourceOutside -> "no-source/outside"
  NoSourceSymlink -> "no-source/symlink"
  NotText -> "not-text"
  TooLarge -> "too-large"
  Failed -> "failed"

-- | Same project, same commit: same count, whatever the machine. Only
-- 'TooSlow' depends on the machine and its load (a time limit).
deterministic :: Category -> Bool
deterministic c = c /= TooSlow

-- | Counted as "text only" in the done line (D-164): a slot that ended
-- Ready, but with no parse. Failed slots count as failed there.
isTextOnly :: Category -> Bool
isTextOnly c =
  c `elem` [ParseError, PragmaError, ParserCrashed, TooSlow, Preprocessed, NotText, TooLarge]

-- * Census

-- | One module in a category.
data Hit = Hit
  { ref :: Text,
    -- | The reason's message, tool or size, for the report. May be empty.
    detail :: Text
  }
  deriving (Eq, Show)

-- | The census of one project.
type Census = Map Category [Hit]

-- | Does the module have an index slot (D-157)? It splits
-- 'Preprocessed' from 'PreprocessedByFlags'.
data Slot = HasSlot | NoSlot
  deriving (Eq, Show)

-- | The body is not a module page we know (D-124): aeson's message.
newtype PageError = PageError String
  deriving (Eq, Show)

-- | The category of one module page (decoded to JSON), or 'Nothing' if
-- it was parsed.
classifyPage :: Text -> Slot -> Aeson.Value -> Either PageError (Maybe (Category, Hit))
classifyPage ref slot =
  either (Left . PageError) Right . Aeson.Types.parseEither page
  where
    page = Aeson.withObject "module page" $ \o -> do
      parse <- o .: "parse"
      kind <- parse .: "kind" :: Aeson.Types.Parser Text
      case kind of
        "parsed" -> pure Nothing
        "text-only" -> do
          reason <- parse .: "reason"
          (c, d) <- fromReason reason
          pure (Just (c, Hit {ref = ref, detail = d}))
        _ -> fail ("unknown parse kind: " <> Text.unpack kind)
    fromReason = Aeson.withObject "reason" $ \r -> do
      kind <- r .: "kind" :: Aeson.Types.Parser Text
      message <- fromMaybe "" <$> r .:? "message"
      tool <- fromMaybe "" <$> r .:? "tool"
      case kind of
        "parse-error" -> pure (ParseError, message)
        "pragma-error" -> pure (PragmaError, message)
        "parser-crashed" -> pure (ParserCrashed, message)
        "too-slow" -> pure (TooSlow, "")
        "preprocessed" -> pure (preprocessed, tool)
        "generated" -> pure (Generated, tool)
        "not-text" -> pure (NotText, "")
        "too-large" -> pure (TooLarge, "")
        "no-source" -> do
          why <- r .: "reason" :: Aeson.Types.Parser Text
          case why of
            "autogen" -> pure (NoSourceAutogen, "")
            "missing" -> pure (NoSourceMissing, "")
            "outside" -> pure (NoSourceOutside, "")
            "symlink" -> pure (NoSourceSymlink, "")
            _ -> fail ("unknown no-source reason: " <> Text.unpack why)
        _ -> fail ("unknown text-only reason: " <> Text.unpack kind)
    preprocessed = case slot of
      HasSlot -> Preprocessed
      NoSlot -> PreprocessedByFlags

-- | Hits per category.
countCensus :: Census -> Map Category Int
countCensus = Map.map length

-- * Expected counts

-- | A line of @bench\/fallbacks.txt@:
--
-- > name sha category=count ...
--
-- Categories not on the line are expected to be 0.
data Expected = Expected
  { name :: Text,
    sha :: Text,
    counts :: Map Category Int
  }
  deriving (Eq, Show)

-- | What's wrong with the file.
data FallbacksError
  = -- | 1-based line number, and why.
    BadFallbacksLine Int Text
  | -- | Name and sha.
    DuplicateRow Text Text
  deriving (Eq, Show)

-- | A message for the user, with the file's path.
renderFallbacksError :: FilePath -> FallbacksError -> Text
renderFallbacksError file (BadFallbacksLine n why) =
  Text.pack file <> " line " <> Text.pack (show n) <> ": " <> why
renderFallbacksError file (DuplicateRow n s) =
  Text.pack file <> ": two lines for " <> n <> " at " <> s

-- | Parse the whole file. Blank lines and @#@ comments are skipped.
parseFallbacksFile :: Text -> Either FallbacksError [Expected]
parseFallbacksFile text = do
  rows <- traverse parseLine (filter (not . skip . snd) (zip [1 ..] (Text.lines text)))
  noDuplicates rows
  Right rows
  where
    skip l = Text.null s || "#" `Text.isPrefixOf` s
      where
        s = Text.strip l
    noDuplicates [] = Right ()
    noDuplicates (r : rest)
      | any (\x -> x.name == r.name && x.sha == r.sha) rest =
          Left (DuplicateRow r.name r.sha)
      | otherwise = noDuplicates rest

parseLine :: (Int, Text) -> Either FallbacksError Expected
parseLine (n, l) = case Text.words l of
  (nm : s : pairs)
    | Text.length s == 40 && Text.all isHex s -> do
        cs <- traverse pair pairs
        let m = Map.fromList cs
        if Map.size m /= length cs
          then bad "a category twice"
          else Right Expected {name = nm, sha = s, counts = Map.filter (/= 0) m}
    | otherwise -> bad ("bad sha: " <> s)
  _ -> bad "expected: name sha category=count ..."
  where
    bad :: Text -> Either FallbacksError a
    bad = Left . BadFallbacksLine n
    isHex c = isDigit c || (c >= 'a' && c <= 'f')
    pair p = case Text.breakOn "=" p of
      (c, eqCount)
        | Just cat <- find ((== c) . categoryText) [minBound .. maxBound],
          Just k <- readMaybe (Text.unpack (Text.drop 1 eqCount)),
          k >= 0 ->
            Right (cat, k)
      _ -> bad ("bad category=count: " <> p)

-- | Is there a row for this project at this commit?
data Lookup
  = Found Expected
  | -- | Rows for the name, but none at this sha: the pin moved and the
    -- file wasn't updated. The shas we have.
    OtherShas [Text]
  | NoRow
  deriving (Eq, Show)

-- | The row for this name and sha.
lookupExpected :: Text -> Text -> [Expected] -> Lookup
lookupExpected nm s rows = case filter ((== nm) . (.name)) rows of
  [] -> NoRow
  named -> case find ((== s) . (.sha)) named of
    Just e -> Found e
    Nothing -> OtherShas (map (.sha) named)

-- | What the done line's "text only" should be, from the expected
-- counts.
expectedTextOnly :: Map Category Int -> Int
expectedTextOnly m = sum [k | (c, k) <- Map.toList m, isTextOnly c]

-- * Comparing

-- | A category whose count is not the expected one.
data Diff = Diff
  { category :: Category,
    expected :: Int,
    got :: Int,
    -- | The modules found (all of them; the table shows a few).
    hits :: [Hit]
  }
  deriving (Eq, Show)

-- | Every category whose count differs, in 'Category' order.
compareCensus :: Map Category Int -> Census -> [Diff]
compareCensus expected census =
  [ Diff {category = c, expected = e, got = length hs, hits = hs}
  | c <- [minBound .. maxBound],
    let e = Map.findWithDefault 0 c expected
        hs = Map.findWithDefault [] c census,
    e /= length hs
  ]

-- | A few lines for the table: the count, then up to 5 modules with
-- their detail.
renderDiff :: Diff -> [Text]
renderDiff d =
  ( categoryText d.category
      <> ": expected "
      <> showT d.expected
      <> ", got "
      <> showT d.got
      <> (if deterministic d.category then "" else " (depends on load)")
  )
    : map hitLine (take 5 d.hits)
      <> ["  ... " <> showT (length d.hits - 5) <> " more" | length d.hits > 5]
  where
    hitLine h
      | Text.null h.detail = "  " <> h.ref
      | otherwise = "  " <> h.ref <> ": " <> firstLine h.detail
    firstLine t = Text.take 160 (Text.takeWhile (/= '\n') t)
    showT = Text.pack . show

-- * Verdict

-- | What the check says about a project, for the table, the JSON and
-- the exit code.
data FallbackVerdict
  = NotCheckedBecause NotChecked
  | AsExpected
  | -- | Only 'TooSlow' differs: depends on the machine's load.
    TooSlowDiffers
  | -- | A count other than 'TooSlow' differs.
    Mismatch
  deriving (Eq, Show)

-- | Why there's nothing to compare.
data NotChecked
  = -- | No row for the project.
    NoRowForProject
  | -- | Rows only at these other shas: the pin moved, the file didn't.
    OtherShasOnly [Text]
  | -- | No run did the census (@--steady-runs 0@).
    NoCensusRun
  deriving (Eq, Show)

-- | How hard a missing or stale row fails the bench.
data Strictness
  = -- | A reference project in a normal run: the row must be there.
    RowRequired
  | -- | An extra project, or @--census-only@ (which is how a new row
    -- gets its counts): only a warning.
    RowOptional
  deriving (Eq, Show)

-- | The verdict from the census (if any), the row lookup and the diffs.
fallbackVerdict :: Maybe Census -> Lookup -> [Diff] -> FallbackVerdict
fallbackVerdict census expected diffs = case (expected, census) of
  (NoRow, _) -> NotCheckedBecause NoRowForProject
  (OtherShas shas, _) -> NotCheckedBecause (OtherShasOnly shas)
  (Found _, Nothing) -> NotCheckedBecause NoCensusRun
  (Found _, Just _)
    | any (deterministic . (.category)) diffs -> Mismatch
    | null diffs -> AsExpected
    | otherwise -> TooSlowDiffers

-- | Does this verdict make the bench exit 1?
failsBench :: Strictness -> FallbackVerdict -> Bool
failsBench strictness = \case
  Mismatch -> True
  NotCheckedBecause NoRowForProject -> strictness == RowRequired
  NotCheckedBecause (OtherShasOnly _) -> strictness == RowRequired
  NotCheckedBecause NoCensusRun -> False
  AsExpected -> False
  TooSlowDiffers -> False
