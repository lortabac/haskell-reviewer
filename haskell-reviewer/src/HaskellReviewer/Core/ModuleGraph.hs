{-# LANGUAGE StrictData #-}

-- StrictData, and 'forceModuleGraph': the graph is shared by request
-- threads (Q-RTS-BLACKHOLE).

-- | Which module an import names: one of ours, or one of a dependency.
-- Pure, built once per snapshot from the module index. See Note [Import
-- mapping].
module HaskellReviewer.Core.ModuleGraph
  ( -- * Types
    ModuleGraph,
    ImportTarget (..),
    MappingGuess (..),

    -- * Building
    buildModuleGraph,
    forceModuleGraph,

    -- * Lookup
    mapImport,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import HaskellReviewer.Core.Entities (groupOn)
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ComponentSettings (..),
    LooseFile (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleRole (..),
    OtherFile (..),
    PackageModules (..),
  )
import HaskellReviewer.Core.Names (ExternalModule (..))
import HaskellReviewer.Core.Project
  ( ComponentName (..),
    LibraryDep (..),
    LibraryName (..),
    ModuleName (..),
    PackageName (..),
  )

{- Note [Import mapping]

An import of module N, from a module of component C:

1. N is a module of C itself (any role): that one.
2. Else the exposed modules (and signatures) of the home libraries in
   C's build-depends, in that order: @pkg@ is pkg's main library,
   @pkg:sub@ its sub-library (Cabal already turns an old-style internal
   library name into @pkg:sub@). Two hits: the first, flagged
   'AmbiguousModule'.
3. Else a loose file with that module name (a module missing from the
   .cabal file): flagged 'UnlistedModule'.
4. Else a module of a dependency we don't have: 'ExternalImport'.

A package import (@import "pkg" N@) looks only at pkg's libraries in
build-depends; @"this"@ only at C. A miss is external, with the
package. As in GHC, a package name never means the home unit, not even
inside that package's own library: only @"this"@ does.

From a loose module (no component, so no build-depends): other loose
files with that name first (one, and no owned module of that name:
exact; else the first, flagged 'AmbiguousModule'), then owned modules,
exposed library modules first, flagged 'LooseImport' (and
'AmbiguousModule' when there are several).

@reexported-modules@ and @mixins@ are not read: such a module comes out
external.
-}

-- * Types

-- | Built by 'buildModuleGraph'. See Note [Import mapping].
data ModuleGraph = ModuleGraph
  { components :: Map ComponentKey Component,
    -- | Loose modules by name, in path order.
    loose :: Map ModuleName [ModuleId],
    -- | Owned modules by name, exposed library modules first. For
    -- loose importers.
    owned :: Map ModuleName [ModuleId]
  }

-- | A component of the project.
data ComponentKey = ComponentKey PackageName ComponentName
  deriving (Eq, Ord)

-- | What an import from a component can see.
data Component = Component
  { package :: PackageName,
    own :: Map ModuleName ModuleId,
    -- | The home libraries in build-depends, in order.
    deps :: [Library]
  }

-- | A home library's exposed modules (and signatures).
data Library = Library
  { package :: PackageName,
    exposed :: Map ModuleName ModuleId
  }

-- | What an import names.
data ImportTarget
  = -- | One of ours, with how sure the mapping is: no guess is exact.
    HomeModule ModuleId [MappingGuess]
  | ExternalImport ExternalModule
  deriving (Eq, Show)

-- | Why the module an import maps to is a guess.
data MappingGuess
  = -- | Several modules have that name: the first one.
    AmbiguousModule
  | -- | A loose file: not in the component, nor in its dependencies.
    UnlistedModule
  | -- | Imported from a loose module: no build-depends to go by.
    LooseImport
  deriving (Eq, Ord, Show, Enum, Bounded)

-- * Building

-- | The graph of a module index. Force it with 'forceModuleGraph'
-- before sharing it.
buildModuleGraph :: ModuleIndex -> ModuleGraph
buildModuleGraph idx =
  ModuleGraph
    { components =
        Map.fromList
          [ (ComponentKey p.name c.name, component p c)
          | p <- idx.packages,
            c <- p.components
          ],
      loose = groupOn [(n, LooseModule f.path) | (n, f) <- looseFiles],
      owned = groupOn (exposedFirst <> others)
    }
  where
    looseFiles = [(n, f) | OtherModule f <- idx.other, Just n <- [f.name]]
    -- The exposed modules of every library.
    libraries =
      Map.fromList
        [ ( ComponentKey p.name c.name,
            Library
              { package = p.name,
                exposed =
                  Map.fromList
                    [ (m.name, ComponentModule p.name c.name m.name)
                    | m <- c.modules,
                      m.role `elem` [RoleExposed, RoleSignature]
                    ]
              }
          )
        | p <- idx.packages,
          c <- p.components,
          isLibrary c.name
        ]
    component p c =
      Component
        { package = p.name,
          own =
            Map.fromList
              [ (m.name, ComponentModule p.name c.name m.name)
              | m <- c.modules
              ],
          deps =
            mapMaybe
              (\k -> Map.lookup k libraries)
              (nubOrd (map depKey c.settings.libraryDepends))
        }
    depKey d = ComponentKey d.package $ case d.library of
      MainLibrary -> Lib
      SubLibrary n -> SubLib n
    isLibrary = \case
      Lib -> True
      SubLib _ -> True
      _ -> False
    exposedFirst =
      [ (n, i)
      | lib <- Map.elems libraries,
        (n, i) <- Map.toList lib.exposed
      ]
    others =
      [ (m.name, i)
      | p <- idx.packages,
        c <- p.components,
        m <- c.modules,
        let i = ComponentModule p.name c.name m.name,
        not (isLibrary c.name && m.role `elem` [RoleExposed, RoleSignature])
      ]

-- | Force every map and list, so request threads find no thunk.
forceModuleGraph :: ModuleGraph -> ()
forceModuleGraph g =
  Map.foldr component () g.components `seq`
    Map.foldr ids () g.loose `seq`
      Map.foldr ids () g.owned
  where
    component c r =
      Map.foldr seq () c.own `seq`
        foldr (\l r' -> Map.foldr seq () l.exposed `seq` r') () c.deps `seq`
          r
    ids is r = foldr seq () is `seq` r

-- * Lookup

-- | The module an import names, from the importer's point of view. The
-- package is the import's package name, if any. See Note [Import
-- mapping].
mapImport :: ModuleGraph -> ModuleId -> Maybe Text -> ModuleName -> ImportTarget
mapImport g importer pkg n = case importer of
  ComponentModule p c _
    | Just comp <- Map.lookup (ComponentKey p c) g.components -> fromComponent comp
  _ -> fromLoose
  where
    external = ExternalImport ExternalModule {package = PackageName <$> pkg, name = n}
    fromComponent comp = case pkg of
      Just "this" -> maybe external (`HomeModule` []) (Map.lookup n comp.own)
      Just p -> fromDeps [l | l <- comp.deps, l.package == PackageName p] external
      Nothing -> case Map.lookup n comp.own of
        Just i -> HomeModule i []
        Nothing -> fromDeps comp.deps fromLooseFiles
    fromDeps libs orElse = case [i | l <- libs, Just i <- [Map.lookup n l.exposed]] of
      [] -> orElse
      i : rest -> HomeModule i [AmbiguousModule | any (/= i) rest]
    fromLooseFiles = case Map.findWithDefault [] n g.loose of
      i : rest -> HomeModule i (UnlistedModule : [AmbiguousModule | not (null rest)])
      [] -> external
    fromLoose
      | Just _ <- pkg = external
      | otherwise = case Map.findWithDefault [] n g.loose of
          [i]
            | Map.member n g.owned -> HomeModule i [AmbiguousModule]
            | otherwise -> HomeModule i []
          i : _ -> HomeModule i [AmbiguousModule]
          [] -> case Map.findWithDefault [] n g.owned of
            i : rest -> HomeModule i (LooseImport : [AmbiguousModule | not (null rest)])
            [] -> external
