{-# LANGUAGE StrictData #-}

-- | Per-module change markers in diff mode (G10 revised, D-324):
-- added, removed, modified, renamed, and whether the module's parse
-- settings changed. Pure. See Note [Module markers].
module HaskellReviewer.Core.Markers
  ( -- * Input
    ModuleFacts (..),
    moduleFacts,

    -- * Markers
    ModuleChange (..),
    SettingsChange (..),
    Marker (..),
    noMarker,
    moduleMarkers,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import HaskellReviewer.Core.Changed (ModuleKey (..))
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleRole,
    PackageModules (..),
  )
import HaskellReviewer.Core.Parser (ParseSettings)
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ModifiedFile (..),
    Pairing (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Sides (Pair (..))

{- Note [Module markers]

Modules are paired by 'ModuleId' across the sides (D-44).

- On head only: added. On base only: removed.
- On both, with files pB (base) and pH (head):
  - the file pairing renamed pB to pH: renamed;
  - pB = pH and the file is modified: modified;
  - pB /= pH and the files were not paired (or one side has no file):
    modified;
  - else none.
- Settings changed (on both sides only): the 'ParseSettings' or the
  role (exposed, other, ...) differ. A component change already
  changes the 'ModuleId'.

Every module of a side gets a 'Marker', 'noMarker' if nothing changed.
-}

-- | What markers need to know of a module on one side.
data ModuleFacts = ModuleFacts
  { -- | Its file, if it has one.
    path :: Maybe RelPath,
    -- | 'Nothing' for a loose module.
    role :: Maybe ModuleRole,
    -- | 'Nothing' if it isn't parsed (generated, no source, ...).
    settings :: Maybe ParseSettings
  }
  deriving (Eq, Show)

-- | The facts of every module of an index. The keys ('moduleKeys')
-- give the settings; the caller has them already.
moduleFacts :: [(ModuleId, ModuleKey)] -> ModuleIndex -> Map ModuleId ModuleFacts
moduleFacts keys idx =
  Map.fromSet
    ( \i ->
        ModuleFacts
          { path = Map.lookup i idx.paths,
            role = Map.lookup i roles,
            settings = Map.lookup i settingsOf
          }
    )
    idx.ids
  where
    roles =
      Map.fromList
        [ (ComponentModule p.name c.name m.name, m.role)
        | p <- idx.packages,
          c <- p.components,
          m <- c.modules
        ]
    settingsOf = Map.fromList [(i, k.settings) | (i, k) <- keys]

-- | How a module changed.
data ModuleChange = ModuleAdded | ModuleRemoved | ModuleModified | ModuleRenamed
  deriving (Eq, Show)

-- | Whether a module's settings changed.
data SettingsChange = SettingsSame | SettingsChanged
  deriving (Eq, Show)

-- | One module's marker.
data Marker = Marker
  { change :: Maybe ModuleChange,
    settingsChange :: SettingsChange
  }
  deriving (Eq, Show)

-- | Nothing changed (and every module in browse mode).
noMarker :: Marker
noMarker = Marker {change = Nothing, settingsChange = SettingsSame}

-- | The markers of every module of each side. See Note [Module
-- markers].
moduleMarkers :: Pairing -> Pair (Map ModuleId ModuleFacts) -> Pair (Map ModuleId Marker)
moduleMarkers pairing facts =
  Pair
    { head = Map.union both (alone ModuleAdded <$ facts.head),
      base = Map.union both (alone ModuleRemoved <$ facts.base)
    }
  where
    -- Once per module on both sides: the settings compare is the cost.
    both = Map.intersectionWith onBoth facts.base facts.head
    alone c = Marker {change = Just c, settingsChange = SettingsSame}
    onBoth b h =
      Marker
        { change = fileChange b.path h.path,
          settingsChange =
            if b.settings /= h.settings || b.role /= h.role
              then SettingsChanged
              else SettingsSame
        }
    fileChange pb ph = case (pb, ph) of
      (Just x, Just y)
        | Map.lookup x renames == Just y -> Just ModuleRenamed
        | x == y -> if Set.member x modified then Just ModuleModified else Nothing
        | otherwise -> Just ModuleModified
      (Nothing, Nothing) -> Nothing
      _ -> Just ModuleModified
    renames = Map.fromList [(r.from, r.to) | Renamed r <- pairing.changes]
    modified = Set.fromList [m.path | Modified m <- pairing.changes]
