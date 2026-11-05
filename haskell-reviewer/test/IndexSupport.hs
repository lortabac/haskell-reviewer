-- | Helpers for tests of resolution and indexing: a one-library
-- project, and waits on an index's slots.
module IndexSupport
  ( -- * A one-library project
    libProject,
    libFile,
    modId,
    rel,

    -- * Waiting on slots
    within,
    isParsing,
    isReady,
    waitState,
    waitDone,
  )
where

import Control.Concurrent.STM (atomically, check)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Modules (ModuleId (..))
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
