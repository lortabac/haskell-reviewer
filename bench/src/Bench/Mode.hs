-- | The bench's modes (D-357): browse, or diff mode against the
-- project's base commit, as a range or against the working tree; and
-- the sides of each.
module Bench.Mode
  ( -- * Types
    Mode (..),
    Side (..),
    ModeRuns (..),

    -- * Modes
    allModes,
    parseModes,
    modeText,
    modeNames,
    modeSides,
    modeRuns,
    projectModes,
    diffSpec,

    -- * Sides
    sideText,
    diffSide,
    onSide,
  )
where

import Bench.Projects (Project (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- | How the product is started.
data Mode
  = -- | No @--diff@.
    Browse
  | -- | @--diff BASE..HEAD@: two commits.
    Range
  | -- | @--diff BASE@ on the clean checkout (at HEAD): the working
    -- tree against a commit.
    WorkTree
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Which side of the session a route asks about. Browse mode has one,
-- @tree@; diff mode two.
data Side = Tree | Head | Base
  deriving (Eq, Ord, Show)

-- | Every mode, in run order.
allModes :: [Mode]
allModes = [minBound .. maxBound]

-- | @--modes@: a comma list, e.g. @browse,range@. Run in 'allModes'
-- order whatever the order given; repeats are fine.
parseModes :: String -> Either String [Mode]
parseModes s = do
  ms <- traverse one (Text.splitOn "," (Text.pack s))
  Right [m | m <- allModes, m `elem` ms]
  where
    one w = case lookup (Text.strip w) [(modeText m, m) | m <- allModes] of
      Just m -> Right m
      Nothing ->
        Left ("unknown mode " <> show w <> " (" <> Text.unpack modeNames <> ")")

-- | The name in the report and on the command line.
modeText :: Mode -> Text
modeText Browse = "browse"
modeText Range = "range"
modeText WorkTree = "worktree"

-- | Every mode's name, comma separated: @--modes@'s default.
modeNames :: Text
modeNames = Text.intercalate "," (map modeText allModes)

-- | What a mode runs (D-357).
data ModeRuns = ModeRuns
  { -- | The fallback census and the text-only check: their rows are
    -- for browse mode.
    census :: Bool,
    -- | Worktree: one steady run, as its steady work is range's.
    maxSteadyRuns :: Maybe Int,
    -- | Search and refs while indexing.
    duringRuns :: Bool,
    -- | Needs the base commit in the checkout.
    needsBase :: Bool,
    -- | Needs a clean checkout: worktree mode compares the working tree.
    needsClean :: Bool
  }
  deriving (Eq, Show)

-- | The rules of each mode.
modeRuns :: Mode -> ModeRuns
modeRuns = \case
  Browse ->
    ModeRuns
      { census = True,
        maxSteadyRuns = Nothing,
        duringRuns = True,
        needsBase = False,
        needsClean = False
      }
  Range ->
    ModeRuns
      { census = False,
        maxSteadyRuns = Nothing,
        duringRuns = True,
        needsBase = True,
        needsClean = False
      }
  WorkTree ->
    ModeRuns
      { census = False,
        maxSteadyRuns = Just 1,
        duringRuns = False,
        needsBase = True,
        needsClean = True
      }

-- | The sides of a mode, head first.
modeSides :: Mode -> NonEmpty Side
modeSides Browse = Tree :| []
modeSides _ = Head :| [Base]

-- | The modes asked for that a project can run: the diff modes need a
-- base commit.
projectModes :: [Mode] -> Project -> [Mode]
projectModes asked p = case p.base of
  Just _ -> asked
  Nothing -> filter (== Browse) asked

-- | What @--diff@ gets: 'Nothing' in browse mode (or with no base).
diffSpec :: Mode -> Project -> Maybe Text
diffSpec mode p = case (mode, p.base) of
  (Range, Just b) -> Just (b <> ".." <> p.sha)
  (WorkTree, Just b) -> Just b
  _ -> Nothing

-- | The side in a URL, the report and the JSON.
sideText :: Side -> Text
sideText Tree = "tree"
sideText Head = "head"
sideText Base = "base"

-- | The side, if it's a diff side: browse mode's 'Tree' says nothing in
-- the report.
diffSide :: Side -> Maybe Side
diffSide Tree = Nothing
diffSide s = Just s

-- | " on base", say, for messages; nothing in browse mode.
onSide :: Side -> Text
onSide = maybe "" ((" on " <>) . sideText) . diffSide
