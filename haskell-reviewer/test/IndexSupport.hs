-- | Helpers for tests of resolution and indexing: a one-library
-- project, and waits on an index's slots.
module IndexSupport
  ( -- * A one-library project
    libProject,
    libFile,
    modId,
    rel,
    MemProject (..),
    memProject,
    memInput,

    -- * Waiting on slots
    within,
    isParsing,
    isReady,
    waitState,
    waitDone,
  )
where

import Control.Concurrent.STM (atomically, check)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
import HaskellReviewer.Session.Index
  ( Index,
    Progress (..),
    SlotState (..),
    readProgress,
    slotState,
  )
import System.Timeout (timeout)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- * A one-library project

-- | Package @p@: one library in @src@ with these modules, all exposed.
libProject :: [Text] -> Project
libProject names =
  Project
    { source = ProjectFile [],
      withCompiler = Nothing,
      ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
      packages =
        [ Package
            { name = PackageName "p",
              version = "1",
              cabalFile = rel "p.cabal",
              flags = [],
              components =
                [ Component
                    { name = Lib,
                      buildable = True,
                      sourceDirs =
                        [fromMaybe (error "bad dir") (relDirFromSegments ["src"])],
                      language = GHC2021,
                      extensions = [],
                      buildDepends = [],
                      libraryDepends = [],
                      mainIs = Nothing,
                      modules = map exposed names,
                      ghcOptions = [],
                      cppOptions = [],
                      includeDirs = []
                    }
                ]
            }
        ],
      brokenPackages = [],
      warnings = []
    }
  where
    exposed n = ListedModule {name = ModuleName n, kind = Exposed, autogen = False}

-- | A module's file in 'libProject': @A.B@ is @src/A/B.hs@.
libFile :: Text -> RelPath
libFile n = rel ("src/" <> Text.replace "." "/" n <> ".hs")

-- | A module of 'libProject'.
modId :: Text -> ModuleId
modId n = ComponentModule (PackageName "p") Lib (ModuleName n)

-- | A path, from @a/b@.
rel :: Text -> RelPath
rel p =
  fromMaybe (error ("bad path " <> show p)) (relPathFromSegments (Text.splitOn "/" p))

-- | A 'libProject' in memory: its index, and what resolution reads.
data MemProject = MemProject
  { index :: ModuleIndex,
    input :: ResolveInput
  }

-- | A 'libProject' of these modules (name, text), every module parsed.
-- The ones in the set are not ready yet.
memProject :: Set Text -> [(Text, Text)] -> MemProject
memProject pending mods = MemProject {index = idx, input = input}
  where
    fl = mkFileList FromDisk 0 [(libFile n, EntryFile) | (n, _) <- mods]
    idx = buildModuleIndex (libProject (map fst mods)) fl
    input =
      ResolveInput
        { graph = buildModuleGraph idx,
          state = \i -> Map.findWithDefault ModuleNotParsed i states,
          memo = Map.empty
        }
    states =
      Map.fromList
        [ (modId n, if Set.member n pending then ModuleNotReady else parsed (modId n) src)
        | (n, src) <- mods
        ]
    parsed i src = case findModule idx i of
      Just found
        | HaskellSource f <- found.source ->
            case parseSource (moduleParseSettings idx.ghc found.settings f) src of
              Parsed m -> ModuleParsed ParsedInput {text = src, parsed = m}
              TextOnly _ -> ModuleNotParsed
      _ -> ModuleNotParsed

-- | The resolution input of 'memProject'.
memInput :: Set Text -> [(Text, Text)] -> ResolveInput
memInput pending mods = (memProject pending mods).input

-- * Waiting on slots

-- | 5 s, a safety net.
within :: IO a -> IO a
within action =
  timeout 5_000_000 action >>= \case
    Just a -> pure a
    Nothing -> assertFailure "not within 5 s"

isParsing :: SlotState -> Bool
isParsing = \case
  Parsing -> True
  _ -> False

isReady :: SlotState -> Bool
isReady = \case
  Ready _ -> True
  _ -> False

-- | Wait until the module's slot is in this state.
waitState :: Index -> ModuleId -> (SlotState -> Bool) -> Assertion
waitState index i ok =
  within . atomically $
    slotState index i >>= check . maybe False ok

-- | Wait until no slot is Pending or Parsing.
waitDone :: Index -> Assertion
waitDone index =
  within . atomically $ do
    p <- readProgress index
    check (p.ready + p.failed == p.total)
