-- | Module discovery on in-memory projects. See Note [Module discovery]
-- in "HaskellReviewer.Core.Modules".
module HaskellReviewer.Core.ModulesTest (tests) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList,
    ListSource (..),
    mkFileList,
  )
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    GeneratedFile (..),
    HaskellFile (..),
    Literate (..),
    LooseFile (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleRole (..),
    ModuleSource (..),
    NoSourceReason (..),
    Notice (..),
    OtherFile (..),
    PackageModules (..),
    PreprocessedFile (..),
    Preprocessor (..),
    buildModuleIndex,
    moduleNameFromPath,
    moduleRef,
    renderModuleId,
    resolveModuleRef,
  )
import HaskellReviewer.Core.Project
  ( Component (..),
    ComponentName (..),
    DetectedGhc (..),
    FileProblem (..),
    GhcSource (..),
    Language (..),
    ListedModule (..),
    Loc (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    ProjectWarning (..),
    parserGhcVersion,
  )
-- Qualified too, for record updates: Core.Modules has fields of the
-- same names.
import HaskellReviewer.Core.Project qualified as P (Component (..), Project (..))
import HaskellReviewer.Core.RelPath
  ( PathProblem (..),
    RelDir,
    RelPath,
    relDirFromSegments,
    relPathFromSegments,
    rootDir,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Modules"
    [ lookupTests,
      mainTests,
      toolTests,
      otherTests,
      identityTests,
      noticeTests
    ]

-- * Lookup

lookupTests :: TestTree
lookupTests =
  testGroup
    "lookup"
    [ testCase "preprocessor input wins, even in a later dir" $ do
        let idx =
              onePackage
                ["p/a/Lexer.hs", "p/b/Lexer.x", "p/a/P.chs", "p/a/P.hsc"]
                [(lib ["p/a", "p/b"]) {P.modules = [exposed "Lexer", exposed "P"]}]
        sources idx
          @?= [ ("p:lib:Lexer", generated "p/b/Lexer.x" Alex),
                ("p:lib:P", generated "p/a/P.chs" C2hs)
              ]
        -- Lexer.hs is claimed by no one.
        otherPaths idx @?= [path "p/a/Lexer.hs"],
      testCase "Haskell files: dir by dir, then extension" $ do
        let idx =
              onePackage
                ["p/a/H.lhs", "p/b/H.hs", "p/a/Sig.hsig", "p/a/L.lhsig"]
                [ (lib ["p/a", "p/b"])
                    { P.modules = [exposed "H", signature "Sig", signature "L"]
                    }
                ]
        sources idx
          @?= [ ("p:lib:H", haskell "p/a/H.lhs" BirdOrLatex),
                ("p:lib:L", haskell "p/a/L.lhsig" BirdOrLatex),
                ("p:lib:Sig", haskell "p/a/Sig.hsig" NotLiterate)
              ]
        roles idx @?= [RoleExposed, RoleSignature, RoleSignature],
      testCase "nested module names" $
        sources
          ( onePackage
              ["p/src/Data/Foo/Bar.hs"]
              [(lib ["p/src"]) {P.modules = [exposed "Data.Foo.Bar"]}]
          )
          @?= [("p:lib:Data.Foo.Bar", haskell "p/src/Data/Foo/Bar.hs" NotLiterate)],
      testCase "autogen, Paths_, missing" $
        sources
          ( packageNamed
              "my-pkg"
              ["p/src/Gen.hs", "p/src/Paths_my_pkg.hs"]
              [ (lib ["p/src"])
                  { P.modules =
                      [ (other "Gen") {autogen = True},
                        other "Paths_my_pkg",
                        other "PackageInfo_my_pkg",
                        other "Gone"
                      ]
                  }
              ]
          )
          @?= [ ("my-pkg:lib:Gen", NoSource Autogen),
                ("my-pkg:lib:Gone", NoSource Missing),
                ("my-pkg:lib:PackageInfo_my_pkg", NoSource Autogen),
                ("my-pkg:lib:Paths_my_pkg", haskell "p/src/Paths_my_pkg.hs" NotLiterate)
              ],
      testCase "symlink: no source" $ do
        let fl =
              mkFileList
                FromDisk
                0
                [(path "p/src/A.hs", EntrySymlink), (path "p/p.cabal", EntryFile)]
            idx =
              buildModuleIndex
                (project [package "p" [(lib ["p/src"]) {P.modules = [exposed "A"]}]])
                fl
        sources idx @?= [("p:lib:A", NoSource (SymlinkSource (path "p/src/A.hs")))]
        otherPaths idx @?= [],
      testCase "test-module and buildable False are kept" $ do
        let idx =
              onePackage
                ["p/t/T.hs"]
                [ (comp (Test "t") ["p/t"])
                    { P.modules = [ListedModule (ModuleName "T") TestModule False],
                      P.buildable = False
                    }
                ]
        roles idx @?= [RoleTestModule]
        [c.buildable | p <- idx.packages, c <- p.components] @?= [False]
    ]

-- * main-is

mainTests :: TestTree
mainTests =
  testGroup
    "main-is"
    [ testCase "with .., in a later dir" $
        sources
          ( onePackage
              ["p/shared/Main.hs", "p/b/M.hs"]
              [ (exe "x" ["p/app"]) {mainIs = Just "../shared/Main.hs"},
                (exe "y" ["p/a", "p/b"]) {mainIs = Just "M.hs"}
              ]
          )
          @?= [ ("p:exe:x:Main", haskell "p/shared/Main.hs" NotLiterate),
                ("p:exe:y:Main", haskell "p/b/M.hs" NotLiterate)
              ],
      testCase "outside, absolute, missing" $
        sources
          ( onePackage
              []
              [ (exe "up" ["p/app"]) {mainIs = Just "../../../Main.hs"},
                (exe "abs" ["p/app"]) {mainIs = Just "/tmp/Main.hs"},
                (exe "gone" ["p/app"]) {mainIs = Just "Main.hs"}
              ]
          )
          @?= [ ("p:exe:up:Main", NoSource (Outside AboveRoot)),
                ("p:exe:abs:Main", NoSource (Outside AbsolutePath)),
                ("p:exe:gone:Main", NoSource Missing)
              ],
      testCase "role main; a listed Main is dropped; .lhs" $ do
        let idx =
              onePackage
                ["p/app/Main.lhs", "p/app/Main.hs", "p/app/Util.hs"]
                [ (exe "x" ["p/app"])
                    { mainIs = Just "Main.lhs",
                      P.modules = [other "Main", other "Util"]
                    }
                ]
        sources idx
          @?= [ ("p:exe:x:Main", haskell "p/app/Main.lhs" BirdOrLatex),
                ("p:exe:x:Util", haskell "p/app/Util.hs" NotLiterate)
              ]
        roles idx @?= [RoleMain, RoleOther]
        otherPaths idx @?= [path "p/app/Main.hs"],
      testCase "a C main-is is skipped" $
        sources
          ( onePackage
              ["p/app/main.c"]
              [(exe "x" ["p/app"]) {mainIs = Just "main.c", P.modules = [other "Main"]}]
          )
          @?= [("p:exe:x:Main", NoSource Missing)]
    ]

-- * -pgmF / -pgmL

toolTests :: TestTree
toolTests =
  testGroup
    "preprocessors"
    [ testCase "-F -pgmF: split or joined, path dropped, last wins" $ do
        let with opts =
              sources
                ( onePackage
                    ["p/t/Spec.hs", "p/t/Lexer.x"]
                    [ (comp (Test "t") ["p/t"])
                        { mainIs = Just "Spec.hs",
                          P.modules = [other "Lexer"],
                          ghcOptions = opts
                        }
                    ]
                )
        with ["-F", "-pgmF", "hspec-discover"]
          @?= [ ("p:test:t:Lexer", generated "p/t/Lexer.x" Alex),
                ("p:test:t:Main", preprocessed "p/t/Spec.hs" "hspec-discover")
              ]
        with ["-pgmFold", "-F", "-pgmF/usr/bin/tool"]
          @?= [ ("p:test:t:Lexer", generated "p/t/Lexer.x" Alex),
                ("p:test:t:Main", preprocessed "p/t/Spec.hs" "tool")
              ]
        -- No -F: GHC doesn't run it.
        with ["-pgmF", "hspec-discover"]
          @?= [ ("p:test:t:Lexer", generated "p/t/Lexer.x" Alex),
                ("p:test:t:Main", haskell "p/t/Spec.hs" NotLiterate)
              ],
      testCase "-pgmL: literate files only; markdown-unlit is a module" $ do
        let with opts =
              sources
                ( onePackage
                    ["p/s/A.lhs", "p/s/B.hs"]
                    [ (lib ["p/s"])
                        { P.modules = [exposed "A", exposed "B"],
                          ghcOptions = opts
                        }
                    ]
                )
        with ["-pgmL", "tools/unlit2"]
          @?= [ ("p:lib:A", preprocessed "p/s/A.lhs" "unlit2"),
                ("p:lib:B", haskell "p/s/B.hs" NotLiterate)
              ]
        with ["-pgmLmarkdown-unlit"]
          @?= [ ("p:lib:A", haskell "p/s/A.lhs" MarkdownUnlit),
                ("p:lib:B", haskell "p/s/B.hs" NotLiterate)
              ]
    ]

-- * Other files

otherTests :: TestTree
otherTests =
  testGroup
    "other files"
    [ testCase "unclaimed Haskell files and boot files, build output skipped" $ do
        let idx =
              index
                ( mkFileList FromDisk 0 $
                    [ (path p, EntryFile)
                    | p <-
                        [ "Setup.hs",
                          "dist-newstyle/build/Gen.hs",
                          "p/.stack-work/Gen.hs",
                          "p/p.cabal",
                          "p/src/A.hs",
                          "p/src/A.hs-boot",
                          "p/src/B.lhs-boot",
                          "p/src/C.hsig",
                          "p/src/D.lhsig",
                          "p/src/E.lhs",
                          "p/src/cbits.c",
                          "scripts/build-docs.hs"
                        ]
                    ]
                      <> [(path "p/src/Link.hs", EntrySymlink)]
                )
                (project [package "p" [(lib ["p/src"]) {P.modules = [exposed "A"]}]])
        idx.other
          @?= [ loose "Setup.hs" (Just "Setup"),
                BootFile (path "p/src/A.hs-boot"),
                BootFile (path "p/src/B.lhs-boot"),
                loose "p/src/C.hsig" (Just "C"),
                loose "p/src/D.lhsig" (Just "D"),
                loose "p/src/E.lhs" (Just "E"),
                loose "scripts/build-docs.hs" Nothing
              ],
      testCase "module names from paths" $
        map
          (\p -> moduleNameFromPath (path p))
          [ "src/Foo/Bar.hs",
            "Setup.hs",
            "scripts/build-docs.hs",
            "app/Data/Deep/Mod.lhs",
            "a.b/Foo.hs",
            "Foo.Bar.hs",
            "x/Ünï'_1.hs",
            "lower.hs"
          ]
          @?= [ Just (ModuleName "Foo.Bar"),
                Just (ModuleName "Setup"),
                Nothing,
                Just (ModuleName "Data.Deep.Mod"),
                Just (ModuleName "Foo"),
                Nothing,
                Just (ModuleName "Ünï'_1"),
                Nothing
              ]
    ]

-- * Identity

identityTests :: TestTree
identityTests =
  testGroup
    "identity"
    [ -- One test: the fixture is built once, never shared between
      -- parallel tests (Note [Shared values in tests] in
      -- HaskellReviewer.Core.PackageGlobTest).
      testCase "short forms, owned modules win; refs resolve back" $ do
        let idx = identityFixture
            ids = Set.toList idx.ids
        sort [(renderModuleId i, moduleRef idx i) | i <- ids]
          @?= sort
            [ ("p:exe:a:Main", "p:exe:a:Main"),
              ("p:exe:b:Main", "p:exe:b:Main"),
              ("p:lib:Only", "Only"),
              ("p:lib:util:Dup", "p:lib:util:Dup"),
              ("p:lib:Dup", "p:lib:Dup"),
              ("p:test:t:Spec", "Spec"),
              ("p:bench:b:Bench", "Bench"),
              ("w/Only.hs", "w/Only.hs"),
              ("w/Dup.hs", "w/Dup.hs"),
              ("x/Tool.hs", "x/Tool.hs"),
              ("y/Tool.hs", "y/Tool.hs"),
              ("z/Alone.hs", "Alone"),
              ("z/build-docs.hs", "z/build-docs.hs")
            ]
        mapM_
          ( \i -> do
              resolveModuleRef idx (moduleRef idx i) @?= Just i
              resolveModuleRef idx (renderModuleId i) @?= Just i
          )
          ids
        map
          (resolveModuleRef idx)
          [ "Main",
            "Dup",
            "Tool",
            "Nope",
            "p:lib:Nope",
            "p:exe:c:Main",
            "q:lib:Only",
            "p:x:a:Main",
            "p:lib",
            "z/Nope.hs",
            "../z/Alone.hs",
            ""
          ]
          @?= replicate 12 Nothing
    ]

-- | Two exe Mains, a name in two libs, two loose Tool, loose files
-- named like owned modules, unique ones.
identityFixture :: ModuleIndex
identityFixture =
  onePackage
    [ "p/src/Only.hs",
      "p/src/Dup.hs",
      "p/util/Dup.hs",
      "p/a/Main.hs",
      "p/b/Main.hs",
      "p/t/Spec.hs",
      "w/Only.hs",
      "w/Dup.hs",
      "x/Tool.hs",
      "y/Tool.hs",
      "z/Alone.hs",
      "z/build-docs.hs"
    ]
    [ (lib ["p/src"]) {P.modules = [exposed "Only", exposed "Dup"]},
      (comp (SubLib "util") ["p/util"]) {P.modules = [exposed "Dup"]},
      (exe "a" ["p/a"]) {mainIs = Just "Main.hs"},
      (exe "b" ["p/b"]) {mainIs = Just "Main.hs"},
      (comp (Test "t") ["p/t"]) {P.modules = [other "Spec"]},
      (comp (Bench "b") ["p/bench"]) {P.modules = [other "Bench"]}
    ]

-- * Notices

noticeTests :: TestTree
noticeTests =
  testGroup
    "notices"
    [ testCase "stack.yaml, nothing else: stack and implicit" $
        (index (files ["stack.yaml", "Foo.hs"]) (implicitProject [])).notices
          @?= [StackNotice, ImplicitNotice],
      testCase "stack.yaml next to an unreadable cabal.project: no stack" $
        ( index
            (files ["stack.yaml", "cabal.project"])
            (implicitProject [ProjectUnreadable (path "cabal.project") Gone])
        ).notices
          @?= [ImplicitNotice],
      testCase "stack.yaml and a root .cabal: stack only" $
        ( index
            (files ["stack.yaml", "p.cabal"])
            (project [package "p" []]) {P.source = ImplicitRootCabalFiles}
        ).notices
          @?= [StackNotice],
      testCase "hpack: HpackOnly dirs and ROOT, warning moved" $ do
        let idx =
              index
                (files ["package.yaml", "sub/package.yaml"])
                (implicitProject [HpackOnly (dir "sub"), noMatch])
        idx.notices @?= [ImplicitNotice, HpackNotice (dir "sub" :| [rootDir])]
        idx.warnings @?= [noMatch],
      testCase "a cabal.project with no packages: no notice" $
        (index (files ["cabal.project", "stack.yaml"]) (project [])).notices @?= []
    ]
  where
    noMatch = EntryNoMatch Loc {file = path "cabal.project", line = 1} "x/"

-- * Helpers

-- | One package @p@ in dir @p@, with these files.
onePackage :: [Text] -> [Component] -> ModuleIndex
onePackage = packageNamed "p"

packageNamed :: Text -> [Text] -> [Component] -> ModuleIndex
packageNamed name ps cs = index (files ("p/p.cabal" : ps)) (project [package name cs])

index :: FileList -> Project -> ModuleIndex
index fl p = buildModuleIndex p fl

files :: [Text] -> FileList
files ps = mkFileList FromDisk 0 [(path p, EntryFile) | p <- ps]

project :: [Package] -> Project
project ps =
  Project
    { P.source = ProjectFile [],
      withCompiler = Nothing,
      ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
      packages = ps,
      brokenPackages = [],
      P.warnings = []
    }

-- | No cabal.project, no packages. A function, not a shared value:
-- see Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
implicitProject :: [ProjectWarning] -> Project
implicitProject ws = (project []) {P.source = ImplicitRootCabalFiles, P.warnings = ws}

package :: Text -> [Component] -> Package
package name cs =
  Package
    { name = PackageName name,
      version = "1.0",
      cabalFile = path "p/p.cabal",
      flags = [],
      components = cs
    }

lib :: [Text] -> Component
lib = comp Lib

exe :: Text -> [Text] -> Component
exe = comp . Exe

comp :: ComponentName -> [Text] -> Component
comp name dirs =
  Component
    { name = name,
      P.buildable = True,
      sourceDirs = map dir dirs,
      language = Haskell2010,
      extensions = [],
      buildDepends = [],
      mainIs = Nothing,
      P.modules = [],
      ghcOptions = [],
      cppOptions = [],
      includeDirs = []
    }

exposed, other, signature :: Text -> ListedModule
exposed n = ListedModule {name = ModuleName n, kind = Exposed, autogen = False}
other n = (exposed n) {kind = Other}
signature n = (exposed n) {kind = Signature}

-- | Long id and source of every component module.
sources :: ModuleIndex -> [(Text, ModuleSource)]
sources idx =
  [ (renderModuleId (ComponentModule p.name c.name m.name), m.source)
  | p <- idx.packages,
    c <- p.components,
    m <- c.modules
  ]

roles :: ModuleIndex -> [ModuleRole]
roles idx = [m.role | p <- idx.packages, c <- p.components, m <- c.modules]

-- | Paths of the other files.
otherPaths :: ModuleIndex -> [RelPath]
otherPaths idx =
  [ case o of
      OtherModule f -> f.path
      BootFile p -> p
  | o <- idx.other
  ]

haskell :: Text -> Literate -> ModuleSource
haskell p l = HaskellSource HaskellFile {path = path p, literate = l}

generated :: Text -> Preprocessor -> ModuleSource
generated p t = GeneratedSource GeneratedFile {path = path p, tool = t}

preprocessed :: Text -> Text -> ModuleSource
preprocessed p t = PreprocessedSource PreprocessedFile {path = path p, tool = t}

loose :: Text -> Maybe Text -> OtherFile
loose p n = OtherModule LooseFile {path = path p, name = ModuleName <$> n}

path :: Text -> RelPath
path p =
  fromMaybe (error ("bad path " <> show p)) (relPathFromSegments (Text.splitOn "/" p))

dir :: Text -> RelDir
dir d = fromMaybe (error ("bad dir " <> show d)) (relDirFromSegments (Text.splitOn "/" d))
