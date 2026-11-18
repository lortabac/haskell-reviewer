-- | The sides of a session (SPEC §2.2, D-313). See Note [Sides].
module HaskellReviewer.Core.Sides
  ( -- * Types
    Side (..),
    Sides (..),
    Pair (..),

    -- * Names
    sideName,
    parseSide,
    sideTag,

    -- * Operations
    lookupSide,
    sideList,
    withSide,
  )
where

import Data.Text (Text)

{- Note [Sides]

A session has sides, and each side has its own snapshot. Browse mode
has one side, tree (the working tree). Diff mode has two, base and
head, and no tree. 'Sides' only holds the sides a mode can have, so
"tree in diff mode" can't be built at all.

Order: head comes first wherever sides are walked (the derived
Foldable and Traversable, 'sideList'). Head is indexed first (G19).
-}

-- | One side of a session. See Note [Sides].
data Side = TreeSide | BaseSide | HeadSide
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The sides of a session, each with a value. See Note [Sides].
data Sides a
  = -- | Browse mode: tree only.
    BrowseSides a
  | -- | Diff mode: base and head only.
    DiffSides (Pair a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Head and base. Head first: the derived instances walk it first.
data Pair a = Pair
  { head :: a,
    base :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The name in URLs and on the wire.
sideName :: Side -> Text
sideName = \case
  TreeSide -> "tree"
  BaseSide -> "base"
  HeadSide -> "head"

-- | The side with this name, if any.
parseSide :: Text -> Maybe Side
parseSide = \case
  "tree" -> Just TreeSide
  "base" -> Just BaseSide
  "head" -> Just HeadSide
  _ -> Nothing

-- | What goes in front of a log line about one side: nothing for tree
-- (browse mode lines have no tag), else @head: @ or @base: @.
sideTag :: Side -> Text
sideTag = \case
  TreeSide -> ""
  s -> sideName s <> ": "

-- | The value for a side, if the session has that side.
lookupSide :: Side -> Sides a -> Maybe a
lookupSide side sides = case (side, sides) of
  (TreeSide, BrowseSides a) -> Just a
  (HeadSide, DiffSides p) -> Just p.head
  (BaseSide, DiffSides p) -> Just p.base
  _ -> Nothing

-- | Every side with its value, head first.
sideList :: Sides a -> [(Side, a)]
sideList = \case
  BrowseSides a -> [(TreeSide, a)]
  DiffSides p -> [(HeadSide, p.head), (BaseSide, p.base)]

-- | Give each value its side.
withSide :: (Side -> a -> b) -> Sides a -> Sides b
withSide f = \case
  BrowseSides a -> BrowseSides (f TreeSide a)
  DiffSides p -> DiffSides Pair {head = f HeadSide p.head, base = f BaseSide p.base}
