{-# LANGUAGE StrictData #-}

-- | The @--diff@ argument (SPEC §2, D-316): parsing it, and what it
-- resolved to. Pure. See Note [Revspec].
module HaskellReviewer.Core.Revspec
  ( -- * Parsing
    Revspec (..),
    Range (..),
    RangeForm (..),
    RevspecError (..),
    parseRevspec,
    renderRevspec,
    renderRange,
    renderRevspecError,

    -- * Resolved
    DiffInfo (..),
    CommitsInfo (..),
    WorkTreeInfo (..),
    BaseFrom (..),
    shortId,
    diffInfoLines,

    -- * Messages
    printable,
  )
where

import Data.Char (isDigit, isPrint)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Revspec]

SPEC §2, D-316. @--diff@ takes:

- @BASE..HEAD@: two commits;
- @BASE...HEAD@: the merge base of the two, and HEAD;
- @REV@ (no @..@ at all): REV's commit as base, the working tree as
  head.

We split at the first @...@, else at the first @..@. An empty end is
HEAD, as in git. Both ends empty is an error, and so is an empty text.

An end that still has @..@ in it, a base that ends with @.@ or a head
that starts with @.@ is an error. So @a....b@ and @a..b..c@ are
rejected. git would read @.b@ as a name: we are stricter, on purpose.

Each end (or REV) is then resolved by git, as any rev (Note [Git
hardening] in "HaskellReviewer.Effects.Git"). We never pass the range
itself to git. What it resolved to is a 'DiffInfo': two commits, or one
commit and the working tree.
-}

-- | What @--diff@ asked for. See Note [Revspec].
data Revspec
  = -- | @BASE..HEAD@ or @BASE...HEAD@.
    RangeSpec Range
  | -- | @REV@: against the working tree.
    AgainstWorkTree Text
  deriving (Eq, Show)

-- | Two ends, as typed (an empty end is already @HEAD@).
data Range = Range
  { form :: RangeForm,
    base :: Text,
    head :: Text
  }
  deriving (Eq, Show)

-- | Two dots or three.
data RangeForm
  = -- | @..@: base as typed.
    TwoDot
  | -- | @...@: base is the merge base.
    ThreeDot
  deriving (Eq, Show)

-- | Why @--diff@ can't be read.
data RevspecError
  = -- | @--diff ''@.
    RevspecEmpty
  | -- | @..@ or @...@ alone.
    RevspecNoEnds
  | -- | Dots where an end should be, like @a....b@.
    RevspecBadDots
  deriving (Eq, Show)

-- | Read the @--diff@ argument. See Note [Revspec].
parseRevspec :: Text -> Either RevspecError Revspec
parseRevspec t
  | Text.null t = Left RevspecEmpty
  | Just (b, h) <- splitAt' "..." = range ThreeDot b h
  | Just (b, h) <- splitAt' ".." = range TwoDot b h
  | otherwise = Right (AgainstWorkTree t)
  where
    splitAt' sep = case Text.breakOn sep t of
      (_, rest) | Text.null rest -> Nothing
      (b, rest) -> Just (b, Text.drop (Text.length sep) rest)
    range form b h
      | Text.null b && Text.null h = Left RevspecNoEnds
      | ".." `Text.isInfixOf` b || ".." `Text.isInfixOf` h = Left RevspecBadDots
      | "." `Text.isSuffixOf` b || "." `Text.isPrefixOf` h = Left RevspecBadDots
      | otherwise =
          Right (RangeSpec Range {form = form, base = orHead b, head = orHead h})
    orHead e = if Text.null e then "HEAD" else e

-- | Back to text, empty ends as @HEAD@.
renderRevspec :: Revspec -> Text
renderRevspec = \case
  RangeSpec r -> renderRange r
  AgainstWorkTree rev -> rev

-- | @base..head@ or @base...head@.
renderRange :: Range -> Text
renderRange r = r.base <> dots <> r.head
  where
    dots = case r.form of
      TwoDot -> ".."
      ThreeDot -> "..."

-- | For the usage error, after optparse's @option --diff:@.
renderRevspecError :: RevspecError -> Text
renderRevspecError = \case
  RevspecEmpty -> "empty revision; use BASE..HEAD, BASE...HEAD or REV"
  RevspecNoEnds -> "a range needs BASE or HEAD; use BASE..HEAD or BASE...HEAD"
  RevspecBadDots -> "too many dots; use BASE..HEAD or BASE...HEAD"

-- * Resolved

-- | What @--diff@ resolved to, before the URL line. Logged at verbose.
data DiffInfo
  = -- | @BASE..HEAD@ or @BASE...HEAD@.
    CommitsDiff CommitsInfo
  | -- | @REV@: head is the working tree.
    WorkTreeDiff WorkTreeInfo
  deriving (Eq, Show)

-- | Two commits.
data CommitsInfo = CommitsInfo
  { range :: Range,
    -- | Each side's commit. For @...@, base's is the merge base, not
    -- the commit of the base as typed.
    sides :: Pair ObjectId,
    baseFrom :: BaseFrom
  }
  deriving (Eq, Show)

-- | A commit and the working tree.
data WorkTreeInfo = WorkTreeInfo
  { -- | As typed.
    rev :: Text,
    -- | Base's commit.
    base :: ObjectId
  }
  deriving (Eq, Show)

-- | Where base's commit comes from.
data BaseFrom
  = -- | @BASE..HEAD@: the base as typed.
    BaseTyped
  | -- | @BASE...HEAD@: the first of this many merge bases (at least 1).
    MergeBase Int
  deriving (Eq, Show)

-- | The first 7 hex digits.
shortId :: ObjectId -> Text
shortId (ObjectId o) = Text.take 7 o

-- | The verbose lines about it:
--
-- > diff: main...feature
-- > head: feature = 1a2b3c4
-- > base: merge base of main and feature = 5d6e7f8 (1 of 2)
--
-- or, for @REV@:
--
-- > diff: HEAD (against the working tree)
-- > head: working tree
-- > base: HEAD = 1a2b3c4
diffInfoLines :: DiffInfo -> [Text]
diffInfoLines =
  map printable . \case
    CommitsDiff info ->
      let (base, count) = case info.baseFrom of
            BaseTyped -> (info.range.base, "")
            MergeBase n ->
              ( "merge base of " <> info.range.base <> " and " <> info.range.head,
                if n > 1 then " (1 of " <> Text.pack (show n) <> ")" else ""
              )
       in [ "diff: " <> renderRange info.range,
            "head: " <> info.range.head <> " = " <> shortId info.sides.head,
            "base: " <> base <> " = " <> shortId info.sides.base <> count
          ]
    WorkTreeDiff info ->
      [ "diff: " <> info.rev <> " (against the working tree)",
        "head: working tree",
        "base: " <> info.rev <> " = " <> shortId info.base
      ]

-- | The text with each character that isn't printable (a newline, ESC,
-- ...) written as a Haskell escape, like @\\n@. So a message with a
-- name the user typed stays on one line, and sends no control codes.
-- As in 'show', @\\&@ ends an escape that the next character would
-- extend (@\\SO@ then @H@, a number then a digit).
printable :: Text -> Text
printable = Text.pack . go . Text.unpack
  where
    go = \case
      [] -> []
      c : rest
        | isPrint c -> c : go rest
        | otherwise -> escape c <> sep c rest <> go rest
    -- show gives 'c' with quotes, escaped.
    escape c = drop 1 (init (show c))
    sep c = \case
      n : _
        | c == '\SO' && n == 'H' -> "\\&"
        | isDigit (last (escape c)) && isDigit n -> "\\&"
      _ -> ""
