-- | The two phases, with files in memory.
module HaskellReviewer.Core.ProjectLoadTest (tests) where

import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList (..),
    ListSource (..),
    mkFileList,
  )
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    CabalProblem (..),
    Component (..),
    DetectedGhc (..),
    FileProblem (..),
    FlagName (..),
    FlagSource (..),
    FlagValue (..),
    GhcSource (..),
    GhcVersion (..),
    ListedModule (..),
    Loc (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Platform (..),
    Project (..),
    ProjectSource (..),
    ProjectWarning (..),
    WithCompiler (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.ProjectLoad (loadProjectConfig, resolveProject)
import HaskellReviewer.Core.RelPath
  ( PathProblem (..),
    RelPath,
    relPathFromSegments,
    relPathText,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "ProjectLoad"
    [ testCase "imports: spliced in place, relative to the importer" $ do
        let p =
              load
                [ file
                    "cabal.project"
                    [ "packages: a/",
                      "import: cfg/one.project",
                      "flags: +x"
                    ],
                  file "cfg/one.project" ["flags: -x +y", "import: two.project"],
                  file "cfg/two.project" ["flags: -y"],
                  pkg "a/a.cabal" "a" ["x", "y"]
                ]
        p.source @?= ProjectFile [path "cfg/one.project", path "cfg/two.project"]
        p.warnings @?= []
        flagsOf p
          @?= [ (FlagName "x", True, FlagFromProject (path "cabal.project")),
                (FlagName "y", False, FlagFromProject (path "cfg/two.project"))
              ],
      testCase "import cycle, missing, remote, outside ROOT, symlink" $ do
        let p =
              load
                [ file
                    "cabal.project"
                    [ "import: a.project",
                      "import: nope.project",
                      "import: ../up"
                    ],
                  file
                    "a.project"
                    [ "import: cabal.project",
                      "import: http://x/y",
                      "import: link.project"
                    ],
                  link "link.project"
                ]
        p.warnings
          @?= [ ImportCycle (at "a.project" 1) "cabal.project",
                ImportRemote (at "a.project" 2) "http://x/y",
                ProjectUnreadable (path "link.project") IsSymlink,
                ImportMissing (at "cabal.project" 2) "nope.project",
                ImportBadPath (at "cabal.project" 3) "../up" AboveRoot
              ],
      testCase "with-compiler: the last one; in a conditional: ignored" $ do
        let p =
              load
                [ file "cabal.project" ["with-compiler: ghc-9.10.1", "import: b.project"],
                  file
                    "b.project"
                    [ "with-compiler: ghc-9.12.2",
                      "if os(linux)",
                      "  with-compiler: ghc-1"
                    ]
                ]
        p.withCompiler
          @?= Just
            WithCompiler
              { written = "ghc-9.12.2",
                version = Just (GhcVersion [9, 12, 2]),
                file = path "b.project"
              }
        p.warnings @?= [WithCompilerInConditional (at "b.project" 3)],
      testCase "impl(ghc >= 9.12.1): a two-part --ghc 9.12 compares as less (D-98)" $ do
        let cabalFile =
              file
                "p.cabal"
                [ "cabal-version: 3.0",
                  "name: p",
                  "version: 1",
                  "library",
                  "  if impl(ghc >= 9.12.1)",
                  "    exposed-modules: New",
                  "  else",
                  "    exposed-modules: Old"
                ]
            detected = DetectedGhc {version = GhcVersion [9, 12], source = GhcFromCli}
            p = runGhc detected [cabalFile] (fileList [cabalFile])
        modulesOf p @?= ["Old"],
      testCase "conditionals come after the plain fields" $ do
        let p =
              load
                [ file
                    "cabal.project"
                    [ "if os(linux)",
                      "  flags: +x",
                      "flags: -x",
                      "packages: a"
                    ],
                  pkg "a/a.cabal" "a" ["x"]
                ]
        flagsOf p @?= [(FlagName "x", True, FlagFromProject (path "cabal.project"))],
      testCase "cabal.project that doesn't lex: *.cabal in ROOT" $ do
        let p =
              load [file "cabal.project" ["packages: a", "\"oops"], pkg "r.cabal" "r" []]
        p.source @?= ImplicitRootCabalFiles
        map (.name) p.packages @?= [PackageName "r"]
        length p.warnings @?= 1,
      testCase "symlinked cabal.project: *.cabal in ROOT" $ do
        let p = load [link "cabal.project", pkg "r.cabal" "r" []]
        p.source @?= ImplicitRootCabalFiles
        p.warnings @?= [ProjectUnreadable (path "cabal.project") IsSymlink],
      testCase "symlinked, binary and bad .cabal files: broken" $ do
        let p =
              load
                [ link "a.cabal",
                  File {path = path "b.cabal", kind = EntryFile, content = Left NotText},
                  file "c.cabal" ["name: c", "library", "  build-depends: base >="],
                  pkg "d.cabal" "d" []
                ]
        map (.name) p.packages @?= [PackageName "d"]
        [(relPathText b.cabalFile, isSyntax b.problem) | b <- p.brokenPackages]
          @?= [("a.cabal", False), ("b.cabal", False), ("c.cabal", True)]
        take 2 (map (.problem) p.brokenPackages)
          @?= [CabalUnreadable IsSymlink, CabalUnreadable NotText],
      testCase "same package name twice: the first by path" $ do
        let p =
              load
                [ file "cabal.project" ["packages: x y"],
                  pkg "y/n.cabal" "n" [],
                  pkg "x/n.cabal" "n" []
                ]
        map (.cabalFile) p.packages @?= [path "x/n.cabal"]
        p.warnings
          @?= [DuplicatePackage (PackageName "n") [path "x/n.cabal", path "y/n.cabal"]],
      testCase "truncated file list: warning" $ do
        let fl = (fileList [pkg "r.cabal" "r" []]) {truncated = True}
            p = run [pkg "r.cabal" "r" []] fl
        p.warnings @?= [FileListTruncated]
    ]
  where
    isSyntax = \case
      CabalSyntax _ -> True
      CabalUnreadable _ -> False

-- | A file of the snapshot: path, kind, what reading gives.
data File = File
  { path :: RelPath,
    kind :: EntryKind,
    -- | What reading it gives.
    content :: Either FileProblem Text
  }

-- | A text file, by lines.
file :: Text -> [Text] -> File
file p ls = File {path = path p, kind = EntryFile, content = Right (Text.unlines ls)}

link :: Text -> File
link p = File {path = path p, kind = EntrySymlink, content = Left IsSymlink}

-- | A .cabal file with a library and these manual flags (default off).
pkg :: Text -> Text -> [Text] -> File
pkg p name flags =
  file p $
    ["cabal-version: 3.0", "name: " <> name, "version: 1"]
      <> concat [["flag " <> f, "  default: False", "  manual: True"] | f <- flags]
      <> ["library", "  exposed-modules: M"]

fileList :: [File] -> FileList
fileList fs = mkFileList FromGit 0 [(f.path, f.kind) | f <- fs]

load :: [File] -> Project
load fs = run fs (fileList fs)

run :: [File] -> FileList -> Project
run = runGhc (DetectedGhc {version = parserGhcVersion, source = GhcFromParser})

-- | Like 'run', with the given detected GHC version instead of the
-- parser's own.
runGhc :: DetectedGhc -> [File] -> FileList -> Project
runGhc detected fs fl = runIdentity $ do
  config <- loadProjectConfig reader fl
  resolveProject reader platform detected fl config
  where
    contents :: Map RelPath (Either FileProblem Text)
    contents = Map.fromList [(f.path, f.content) | f <- fs]
    reader p = Identity (Map.findWithDefault (Left (CantRead "no such file")) p contents)
    platform = Platform {os = "linux", arch = "x86_64"}

flagsOf :: Project -> [(FlagName, Bool, FlagSource)]
flagsOf p = [(f.name, f.value, f.source) | pk <- p.packages, f <- pk.flags]

modulesOf :: Project -> [Text]
modulesOf p =
  [n | pk <- p.packages, c <- pk.components, m <- c.modules, let ModuleName n = m.name]

path :: Text -> RelPath
path t =
  fromMaybe (error ("bad path: " <> show t)) (relPathFromSegments (Text.splitOn "/" t))

at :: Text -> Int -> Loc
at f l = Loc {file = path f, line = l}
