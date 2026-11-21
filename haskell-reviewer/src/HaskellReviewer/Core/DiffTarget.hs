-- | Which pair of files a diff URL means (G13, D-331). Pure. See Note
-- [Diff target].
module HaskellReviewer.Core.DiffTarget
  ( -- * Types
    ChangeIndex (..),
    Target (..),

    -- * Building
    changeIndex,
    forceChangeIndex,

    -- * Lookup
    findTarget,
    targetPath,
    targetPaths,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ModifiedFile (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Diff target]

api/diff/<path…> takes one path. It may be a head path or a base path:

- on head: the change with that head path. None: the change with that
  base path (removed, or the source of a rename). None, and the path is
  on base too: the same path on both sides ('SamePath'; the route tells
  unchanged from modified by the contents). None, and not on base:
  added.
- on base only: the change with that base path. None: removed.
- on neither: not found.

So the head path and the base path of a rename give the same answer.

A path can be on both lists and still be removed, or a rename's
source: the pairing skips nested repos (D-324), so a file on base that
is an untracked nested repo on head is removed. D-324's rule (a path on
both sides is never a rename's end) holds for the pairing's entries,
not for the lists. Head is looked up first, so a path is never two
changes at once.

The fallbacks cover what the pairing never saw: paths past the file
cap (no renames there), and files of a disk side in tests.
-}

-- | The pairing's changes by head path and by base path. Strict maps of
-- strict values: built and forced before request threads read it
-- (Q-RTS-BLACKHOLE).
data ChangeIndex = ChangeIndex
  { byHead :: !(Map RelPath Change),
    byBase :: !(Map RelPath Change)
  }
  deriving (Eq, Show)

-- | What a diff URL points at.
data Target
  = -- | A change of the pairing, or added / removed past the cap.
    Changed Change
  | -- | On both sides, not in the pairing.
    SamePath RelPath
  deriving (Eq, Show)

-- | Index the pairing's changes. Added: by head. Removed: by base.
-- Modified: both, same path. Renamed: head by @to@, base by @from@.
changeIndex :: [Change] -> ChangeIndex
changeIndex cs =
  ChangeIndex
    { byHead = Map.fromList [(p, c) | c <- cs, Just p <- [headPath c]],
      byBase = Map.fromList [(p, c) | c <- cs, Just p <- [basePath c]]
    }
  where
    headPath = \case
      Added p -> Just p
      Removed _ -> Nothing
      Modified m -> Just m.path
      Renamed r -> Just r.to
    basePath = \case
      Added _ -> Nothing
      Removed p -> Just p
      Modified m -> Just m.path
      Renamed r -> Just r.from

-- | The sizes: enough, as both maps are strict in keys and values and
-- 'Change' is strict.
forceChangeIndex :: ChangeIndex -> Int
forceChangeIndex i = Map.size i.byHead + Map.size i.byBase

-- | The target of a path, given which paths each side has. See Note
-- [Diff target].
findTarget :: ChangeIndex -> Pair (RelPath -> Bool) -> RelPath -> Maybe Target
findTarget idx on p
  | on.head p = Just $ case Map.lookup p idx.byHead of
      Just c -> Changed c
      Nothing
        | Just c <- Map.lookup p idx.byBase -> Changed c
        | on.base p -> SamePath p
        | otherwise -> Changed (Added p)
  | on.base p = Just . Changed $ Map.findWithDefault (Removed p) p idx.byBase
  | otherwise = Nothing

-- | The head path, or the base path for removed: the path on the wire.
targetPath :: Target -> RelPath
targetPath = \case
  SamePath p -> p
  Changed (Added p) -> p
  Changed (Removed p) -> p
  Changed (Modified m) -> m.path
  Changed (Renamed r) -> r.to

-- | The path to read on each side; 'Nothing' where the file isn't.
targetPaths :: Target -> Pair (Maybe RelPath)
targetPaths = \case
  SamePath p -> both p
  Changed (Modified m) -> both m.path
  Changed (Added p) -> Pair {head = Just p, base = Nothing}
  Changed (Removed p) -> Pair {head = Nothing, base = Just p}
  Changed (Renamed r) -> Pair {head = Just r.to, base = Just r.from}
  where
    both p = Pair {head = Just p, base = Just p}
