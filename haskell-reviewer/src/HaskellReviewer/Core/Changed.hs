{-# LANGUAGE StrictData #-}

-- | Which modules changed between the two sides of a diff, so they are
-- indexed first (SPEC §10.2, D-317). Pure. See Note [Changed modules].
module HaskellReviewer.Core.Changed
  ( -- * Keys
    ModuleKey (..),
    moduleKeys,

    -- * Changed
    changedModules,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import HaskellReviewer.Core.Git (ObjectId)
import HaskellReviewer.Core.IndexQueue (Job (..), indexJobs)
import HaskellReviewer.Core.Modules (HaskellFile (..), ModuleId, ModuleIndex (..))
import HaskellReviewer.Core.Parser (ParseSettings, moduleParseSettings)
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Changed modules]

SPEC §10.2, D-317. Modules are paired by 'ModuleId' across the sides.
A module is changed if:

- it is on one side only (added or removed), or
- its blob differs, or its 'ParseSettings' differ (extensions, CPP
  flags, ...: a .cabal change).

A blob we don't know ('Nothing', a disk side) counts as changed: the
safe side. A module moved by hs-source-dirs with the same blob and
settings is not changed: its parse is the same.

Limits: a module that is a symlink is keyed by the link's own blob,
so a change in its target alone doesn't mark it. Nor does a change in
a file it #includes. It is only a priority, so both are fine.
-}

-- | What a module's parse depends on.
data ModuleKey = ModuleKey
  { -- | 'Nothing': not known (a disk side).
    blob :: Maybe ObjectId,
    settings :: ParseSettings
  }
  deriving (Eq, Show)

-- | Every module of the index that needs a parse, with its key, in
-- background order.
moduleKeys :: (RelPath -> Maybe ObjectId) -> ModuleIndex -> [(ModuleId, ModuleKey)]
moduleKeys blobOf idx =
  [ ( job.id,
      ModuleKey
        { blob = blobOf job.file.path,
          settings = moduleParseSettings idx.ghc job.settings job.file
        }
    )
  | job <- indexJobs idx
  ]

-- | The changed modules of each side, in the order given. See Note
-- [Changed modules].
changedModules :: Pair [(ModuleId, ModuleKey)] -> Pair [ModuleId]
changedModules keys =
  Pair {head = changedIn keys.head keys.base, base = changedIn keys.base keys.head}
  where
    changedIn these others =
      let other = Map.fromList others
       in [i | (i, k) <- these, not (same k (Map.lookup i other))]
    same k = \case
      Just o -> isJust k.blob && k.blob == o.blob && k.settings == o.settings
      Nothing -> False
