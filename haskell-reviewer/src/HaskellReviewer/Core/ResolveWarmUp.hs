-- | A small in-memory project that takes every path of resolution:
-- every origin, open part, reason, result kind, guess kind, filter kind
-- and context. Run once before request threads resolve in parallel, so
-- the shared values are built (Q-RTS-BLACKHOLE). A test checks the
-- coverage.
module HaskellReviewer.Core.ResolveWarmUp
  ( resolveWarmUpInput,
    resolveWarmUpModules,
    resolveWarmUpIndex,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.ModuleGraph (buildModuleGraph)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..))
import HaskellReviewer.Core.Modules
  ( FoundModule (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleSource (..),
    buildModuleIndex,
    findModule,
  )
import HaskellReviewer.Core.Parser (moduleParseSettings, parseSource)
import HaskellReviewer.Core.Project
  ( Component (..),
    ComponentName (..),
    DetectedGhc (..),
    GhcSource (..),
    Language (..),
    LibraryDep (..),
    LibraryName (..),
    ListedModule (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.RelPath (RelPath, relDirFromSegments, relPathFromSegments)
import HaskellReviewer.Core.Resolve
  ( ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
  )

-- | The modules to resolve: all of them.
resolveWarmUpModules :: [ModuleId]
resolveWarmUpModules = Map.keys (Map.fromList states)

-- | The warm-up project: parsed, with one module not ready yet.
resolveWarmUpInput :: ResolveInput
resolveWarmUpInput =
  ResolveInput
    { graph = buildModuleGraph index,
      state = \i -> fromMaybe ModuleNotParsed (lookup i states),
      memo = Map.empty
    }

-- | The warm-up project's module index.
resolveWarmUpIndex :: ModuleIndex
resolveWarmUpIndex = index

index :: ModuleIndex
index =
  buildModuleIndex project (mkFileList FromDisk 0 [(path p, EntryFile) | (p, _) <- files])

states :: [(ModuleId, ModuleState)]
states =
  [ (i, st)
  | (p, src) <- files,
    let i = idOf p,
    let st = stateOf i src
  ]
  where
    stateOf i src
      | i == pending = ModuleNotReady
      | otherwise = case findModule index i of
          Just found
            | HaskellSource f <- found.source ->
                case parseSource (moduleParseSettings index.ghc found.settings f) src of
                  Parsed m -> ModuleParsed ParsedInput {text = src, parsed = m}
                  TextOnly _ -> ModuleNotParsed
          _ -> ModuleNotParsed
    pending = ComponentModule pkg Lib (ModuleName "Pending")

-- | A file's module id: by its dir.
idOf :: Text -> ModuleId
idOf p = case Text.splitOn "/" p of
  ["w", "src", "Unlisted.hs"] -> LooseModule (path p)
  ["w", "src", f] -> ComponentModule pkg Lib (modName f)
  ["w", "s", f] -> ComponentModule pkg (SubLib "s") (modName f)
  ["w", "app", _] -> ComponentModule pkg (Exe "e") (ModuleName "Main")
  _ -> LooseModule (path p)
  where
    modName f = ModuleName (Text.dropEnd 3 f)

pkg :: PackageName
pkg = PackageName "w"

path :: Text -> RelPath
path p = fromMaybe (error "warm-up: bad path") (relPathFromSegments (segments p))

segments :: Text -> [Text]
segments = Text.splitOn "/"

project :: Project
project =
  Project
    { source = ProjectFile [],
      withCompiler = Nothing,
      ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
      packages =
        [ Package
            { name = pkg,
              version = "1",
              cabalFile = path "w/w.cabal",
              flags = [],
              components =
                [ component Lib "w/src" [] [m | ("w" : "src" : _, m) <- listed],
                  component (SubLib "s") "w/s" [] [m | ("w" : "s" : _, m) <- listed],
                  ( component
                      (Exe "e")
                      "w/app"
                      [main', LibraryDep pkg (SubLibrary "s")]
                      []
                  )
                    { mainIs = Just "Main.hs"
                    }
                ]
            }
        ],
      brokenPackages = [],
      warnings = []
    }
  where
    listed =
      [ (init segs, exposed (Text.dropEnd 3 (last segs)))
      | (p, _) <- files,
        let segs = Text.splitOn "/" p,
        take 1 segs == ["w"],
        segs !! 1 /= "app",
        last segs /= "Unlisted.hs"
      ]
    main' = LibraryDep pkg MainLibrary
    exposed n = ListedModule {name = ModuleName n, kind = Exposed, autogen = False}
    component name dir deps ms =
      Component
        { name = name,
          buildable = True,
          sourceDirs =
            [fromMaybe (error "warm-up: bad dir") (relDirFromSegments (segments dir))],
          language = GHC2021,
          extensions = [],
          buildDepends = map (.package) deps,
          libraryDepends = deps,
          mainIs = Nothing,
          modules = ms,
          ghcOptions = [],
          cppOptions = [],
          includeDirs = []
        }

-- | Paths and sources.
files :: [(Text, Text)]
files =
  [ ( "w/src/Defs.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms, TypeData, TypeFamilies #-}",
          "module Defs (Rec (..), Cls (cm), Nat (..), pattern P, type (:+),"
            <> " Kind (..), Fam,",
          "  val, amb, qq) where",
          "data Rec = Rec {ra :: Int, rb :: Int}",
          "class Cls a where { cm :: a -> Int; cm2 :: a -> Int }",
          "data Nat = Z | S Nat",
          "pattern P :: Int",
          "pattern P = 0",
          "type data Kind = KA | KB",
          "type a :+ b = Either a b",
          "type family Fam a",
          "val, amb, qq :: Int",
          "val = 1",
          "amb = 2",
          "qq = 3"
        ]
    ),
    ( "w/src/Other.hs",
      "module Other (amb) where\namb :: Int\namb = 4\n"
    ),
    ( "w/src/Re.hs",
      Text.unlines
        [ "module Re (module Defs, module X, Map, gen, ghost, Rec (..)) where",
          "import Defs hiding (val, Kind (..))",
          "import Data.Char as X",
          "import Data.Map (Map)",
          "import Gen"
        ]
    ),
    ( "w/src/Gen.hs",
      "{-# LANGUAGE TemplateHaskell #-}\nmodule Gen (gen) where\n$(pure [])\n"
    ),
    ( "w/src/NoList.hs",
      Text.unlines
        [ "{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}",
          "module NoList where",
          "$(pure [])",
          "nl = mystery"
        ]
    ),
    ( "w/src/CycA.hs",
      "module CycA (module CycB, a) where\nimport {-# SOURCE #-} CycB\na = 1\n"
    ),
    ( "w/src/CycB.hs",
      "module CycB (module CycA, b) where\nimport CycA\nb = 2\n"
    ),
    ( "w/src/CycUse.hs",
      Text.unlines
        [ "{-# LANGUAGE NoImplicitPrelude #-}",
          "module CycUse where",
          "import CycA",
          "import CycB",
          "c = b unknown"
        ]
    ),
    -- A pattern synonym bundled with an imported type, then imported
    -- and exported alone, hidden, and put back under its type.
    ( "w/src/Bun.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms #-}",
          "module Bun (Nat (.., PZ)) where",
          "import Defs (Nat (..))",
          "pattern PZ :: Nat",
          "pattern PZ = Z"
        ]
    ),
    ( "w/src/Bun2.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms #-}",
          "module Bun2 (module Bun, Nat (..), pattern PZ) where",
          "import Bun (Nat)",
          "import Bun (pattern PZ)",
          "import Bun hiding (pattern PZ)"
        ]
    ),
    ( "w/src/Bun3.hs",
      "{-# LANGUAGE PatternSynonyms #-}\nmodule Bun3 (pattern PZ) where\nimport Bun\n"
    ),
    -- PZ found through a hiding import before a whole one.
    ( "w/src/Bun4.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms #-}",
          "module Bun4 (pattern PZ) where",
          "import Bun hiding (S)",
          "import Bun"
        ]
    ),
    -- A qualified-only module export: only PZ is in scope unqualified.
    ( "w/src/Bun5.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms #-}",
          "module Bun5 (module Bun) where",
          "import qualified Bun",
          "import Bun (pattern PZ)"
        ]
    ),
    ("w/src/Pending.hs", "module Pending (pend) where\npend = 1\n"),
    ("w/src/Broken.hs", "module Broken (x) where\nx = = 1\n"),
    ("w/src/Unlisted.hs", "module Unlisted (unl) where\nunl = 1\n"),
    ( "w/src/Use.hs",
      Text.unlines
        [ "{-# LANGUAGE DataKinds, OverloadedRecordDot, QuasiQuotes, RecordWildCards #-}",
          "{-# LANGUAGE TemplateHaskell #-}",
          "{-# LANGUAGE ImplicitParams, PatternSynonyms, TypeOperators #-}",
          "module Use (Rec (..), use, gen, Kind (KA)) where",
          "import Re",
          "import qualified Defs as D (Nat (..))",
          "import Defs (Cls (..), pattern P, type (:+), Kind (..), notThere)",
          "import Defs hiding (val)",
          "import Other",
          "import Pending (pend)",
          "import Broken",
          "import Unlisted (unl)",
          "import Data.List (sort)",
          "import Data.Maybe",
          "import Data.Either hiding (lefts)",
          "import qualified Bare as B (R (..))",
          "use :: D.Nat -> Int :+ Bool",
          "use n = Left (ra (Rec {ra = 1, rb = P}) + amb + pend + x + unl"
            <> " + ord 'a' + gen)",
          "instance Cls Bool where { cm _ = 1; cm2 _ = 2 }",
          "instance Show Rec where show r = show r.rb",
          "w Rec {..} = ra",
          "w2 Foreign {..} = fld",
          "dk :: Proxy (S Z)",
          "dk = undefined",
          "q = [qq|x|]",
          "vs = $(local)",
          "local = undefined",
          "ip = ?imp",
          "s = sort (fromMaybe [] Nothing)",
          "m :: Map Int Int",
          "m = undefined",
          "k :: KA",
          "k = undefined",
          "bare r = (B.get, r {B.get = 1})"
        ]
    ),
    ( "w/src/Cpp.hs",
      Text.unlines
        [ "{-# LANGUAGE CPP, NoImplicitPrelude #-}",
          "module Cpp where",
          "#if 0",
          "import Gone",
          "#endif",
          "#if 1",
          "import Defs (val)",
          "#endif",
          "c = val gone"
        ]
    ),
    ( "w/src/Bare2.hs",
      Text.unlines
        [ "{-# LANGUAGE NoImplicitPrelude, OverloadedRecordDot #-}",
          "module Bare2 where",
          "import Ext",
          "b = nothing",
          "d r = r.nofield"
        ]
    ),
    ("w/src/Shared.hs", "module Shared (sh) where\nsh = 1\n"),
    -- Every hover sig shape, plus a long one and a missing one. u and
    -- U use them so they get links. See Note [Hover signatures] in
    -- "HaskellReviewer.Core.SourceLinks".
    ( "w/src/Sigs.hs",
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms, TypeFamilies #-}",
          "module Sigs where",
          "v :: Int",
          "  -> Int",
          "v = id",
          "w = 1",
          "pattern P :: Int",
          "pattern P = 1",
          "pattern Q {q} = Just q",
          "class C a where",
          "  type E a",
          "  m :: a -> Int",
          "data T a = K a | L {fld :: Int}",
          "data GA a where",
          "  GK :: a -> GA a",
          "newtype N = N Int",
          "type S = Int",
          "type family F a",
          "type family G a where",
          "  G a = a",
          "data family D a",
          "data instance D Int = DI Int",
          "long :: " <> Text.intercalate " -> " (replicate 80 "Int"),
          "long = undefined",
          "u = (v, w, P, Q, q, m, K, L, fld, N, DI, GK, long)",
          "type U = (T S, N, F Int, G Int, D Int, E Int, GA Int)",
          "f :: C a => a -> Int",
          "f = m"
        ]
    ),
    -- Doc names of every kind. See Note [Names in docs] in
    -- "HaskellReviewer.Core.DocLinks".
    ("w/src/Ops.hs", "module Ops where\ntype a +++ b = Either a b\no = 1\n"),
    ( "w/src/Docs.hs",
      Text.unlines
        [ "module Docs (f) where",
          "import qualified Defs as D",
          "import Defs (Rec (..), Nat (..))",
          "import Data.List (sort)",
          "import Data.Char",
          "import Ops",
          "-- | Alias 'D.val', module 'Defs.amb', t'Rec', v'Rec', 'Nat' (a type),",
          "-- 'Z' (a constructor), '+++' (a type), 'o' (no sig), 'sort' (listed),",
          "-- 'mystery' (one of the open imports), 'Nope.x' (nothing), 'f' (local).",
          "f :: D.Nat -> Rec -> Int +++ Bool",
          "f = undefined"
        ]
    ),
    -- A package import.
    ( "w/src/Pkg.hs",
      Text.unlines
        [ "{-# LANGUAGE PackageImports #-}",
          "module Pkg where",
          "import \"base\" Data.Word (Word8)",
          "w :: Word8",
          "w = 1"
        ]
    ),
    ( "w/src/Bare.hs",
      Text.unlines
        [ "{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors, OverloadedRecordDot #-}",
          "module Bare (R (..), get, twice) where",
          "data R = R {get :: Int}",
          "data S = S {twice :: Int}",
          "data T = T {twice :: Int}",
          "f r = r.twice"
        ]
    ),
    ("w/s/Shared.hs", "module Shared (sh) where\nsh = 2\n"),
    ( "w/app/Main.hs",
      "module Main (main) where\nimport Shared\nimport Defs\nmain = print (sh + val)\n"
    ),
    ("loose/L.hs", "module L where\nimport Defs (val)\nimport Dup\nl = val + dup\n"),
    ("loose/a/Dup.hs", "module Dup where\ndup = 1\n"),
    ("loose/b/Dup.hs", "module Dup where\ndup = 2\n")
  ]
