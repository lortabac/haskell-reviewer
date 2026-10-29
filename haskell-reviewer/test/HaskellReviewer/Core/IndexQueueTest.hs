-- | The parse queue and the jobs that go in it.
module HaskellReviewer.Core.IndexQueueTest (tests) where

import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.IndexQueue
  ( Job (..),
    Priority (..),
    Queue,
    fromList,
    indexJobs,
    pop,
    push,
  )
import HaskellReviewer.Core.Modules (ModuleId (..), buildModuleIndex, renderModuleId)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "IndexQueue"
    [ testCase "requested before background, first in first out within one" $ do
        let q = fromList (map m ["A", "B"])
        drain (push Requested (m "D") (push Requested (m "C") q))
          @?= map m ["C", "D", "A", "B"],
      testCase "changed between requested and background" $ do
        let q = push Changed (m "C") (push Requested (m "R") (fromList [m "A"]))
        drain q @?= map m ["R", "C", "A"],
      testCase "empty: nothing to pop" $ do
        drain (fromList []) @?= []
        drain (push Requested (m "A") (fromList [])) @?= [m "A"],
      testCase "jobs: module tree order, only what needs a parse" $ do
        let fl =
              mkFileList
                FromDisk
                0
                [ (path p, EntryFile)
                | p <-
                    [ "p/p.cabal",
                      "p/src/B.hs",
                      "p/src/A.lhs",
                      "p/src/Lexer.x",
                      "p/app/Main.hs",
                      "z/Loose.hs",
                      "a/Loose.hs",
                      "a/Loose.hs-boot"
                    ]
                ]
            lib = component Lib "p/src" ["B", "A", "Lexer", "Gone"]
            idx = buildModuleIndex (project [lib, exe]) fl
        map (renderModuleId . (.id)) (indexJobs idx)
          @?= ["p:lib:A", "p:lib:B", "p:exe:app:Main", "a/Loose.hs", "z/Loose.hs"]
    ]
  where
    exe = (component (Exe "app") "p/app" []) {mainIs = Just "Main.hs"}

drain :: Queue -> [ModuleId]
drain = unfoldr pop

m :: Text -> ModuleId
m n = LooseModule (path (n <> ".hs"))

project :: [Component] -> Project
project cs =
  Project
    { source = ProjectFile [],
      withCompiler = Nothing,
      ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
      packages =
        [ Package
            { name = PackageName "p",
              version = "1.0",
              cabalFile = path "p/p.cabal",
              flags = [],
              components = cs
            }
        ],
      brokenPackages = [],
      warnings = []
    }

component :: ComponentName -> Text -> [Text] -> Component
component name src ms =
  Component
    { name = name,
      buildable = True,
      sourceDirs = [fromMaybe (error "bad dir") (relDirFromSegments (segments src))],
      language = Haskell2010,
      extensions = [],
      buildDepends = [],
      mainIs = Nothing,
      modules = map exposed ms,
      ghcOptions = [],
      cppOptions = [],
      includeDirs = []
    }

path :: Text -> RelPath
path p = fromMaybe (error ("bad path " <> show p)) (relPathFromSegments (segments p))

segments :: Text -> [Text]
segments = Text.splitOn "/"

exposed :: Text -> ListedModule
exposed n = ListedModule {name = ModuleName n, kind = Exposed, autogen = False}
