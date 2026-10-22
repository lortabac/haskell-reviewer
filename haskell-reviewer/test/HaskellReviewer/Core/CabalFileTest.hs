module HaskellReviewer.Core.CabalFileTest (tests) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.CabalFile (readCabalFile)
import HaskellReviewer.Core.Project
  ( BadCabalPath (..),
    CabalProblem (..),
    Component (..),
    ComponentName (..),
    FlagKind (..),
    FlagName (..),
    FlagScope (..),
    FlagSetting (..),
    FlagSource (..),
    FlagValue (..),
    FlagsField (..),
    GhcVersion (..),
    Language (..),
    Loc (..),
    Package (..),
    PackageName (..),
    Platform (..),
    ProjectSettings (..),
    ProjectWarning (..),
  )
import HaskellReviewer.Core.RelPath
  ( PathProblem (..),
    RelPath,
    relDirText,
    relPathFromSegments,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "CabalFile"
    [ testCase "os: darwin and osx are the same" $ do
        cppIf (on "darwin" "x86_64") "os(osx)" @?= ["-DYES"]
        cppIf (on "darwin" "x86_64") "os(darwin)" @?= ["-DYES"]
        cppIf (on "linux" "x86_64") "os(darwin)" @?= []
        cppIf (on "mingw32" "x86_64") "os(windows)" @?= ["-DYES"],
      testCase "arch" $ do
        cppIf (on "linux" "aarch64") "arch(aarch64)" @?= ["-DYES"]
        cppIf (on "linux" "aarch64") "arch(x86_64)" @?= [],
      testCase "impl(ghc ...) uses the GHC version" $ do
        cppIf linux "impl(ghc >= 9.14)" @?= ["-DYES"]
        cppIf linux {ghcVersion = GhcVersion [9, 12, 2]} "impl(ghc >= 9.14)" @?= []
        cppIf linux "impl(ghc >= 9.14.2)" @?= []
        cppIf linux "impl(ghc)" @?= ["-DYES"]
        cppIf linux "impl(ghcjs)" @?= []
        cppIf linux "!impl(ghcjs) && (true || false)" @?= ["-DYES"],
      testCase "main-is at top and in a branch: last one, no crash" $ do
        pkg <-
          ok
            linux
            []
            [ "executable e",
              "  main-is: A.hs",
              "  if os(linux)",
              "    main-is: B.hs",
              "  if os(darwin)",
              "    main-is: C.hs"
            ]
        map (.mainIs) pkg.components @?= [Just "B.hs"],
      testCase "no default-language: Haskell98; source dir: package dir" $ do
        pkg <- ok linux [] ["library", "  exposed-modules: A"]
        map (.language) pkg.components @?= [Haskell98]
        map (map relDirText . (.sourceDirs)) pkg.components @?= [["p"]],
      testCase "flags: default < global < package name" $ do
        let field l scope fs =
              FlagsField
                { at = Loc {file = projectFile, line = l},
                  scope = scope,
                  flags = [FlagSetting {name = FlagName n, value = v} | (n, v) <- fs]
                }
        pkg <-
          ok
            linux
            [ field 5 (OnePackage (PackageName "p")) [("c", True)],
              field 1 GlobalFlags [("a", True), ("b", False), ("c", False)],
              field 6 (OnePackage (PackageName "other")) [("a", False)]
            ]
            [ "flag a",
              "  default: False",
              "flag b",
              "  default: False",
              "  manual: True",
              "flag c",
              "  default: False",
              "flag d",
              "  default: True",
              "library",
              "  exposed-modules: A"
            ]
        [(f.name, f.value, f.kind) | f <- pkg.flags]
          @?= [ (FlagName "a", True, Automatic),
                (FlagName "b", False, Manual),
                (FlagName "c", True, Automatic),
                (FlagName "d", True, Automatic)
              ]
        map (.source) pkg.flags
          @?= replicate 3 (FlagFromProject projectFile) <> [FlagDefault],
      testCase "unknown flag in package <name>: warning" $ do
        let field =
              FlagsField
                { at = Loc {file = projectFile, line = 2},
                  scope = OnePackage (PackageName "p"),
                  flags = [FlagSetting {name = FlagName "nope", value = True}]
                }
        case readCabalFile linux [field] cabalFile (cabal library) of
          Right (_, ws) ->
            ws @?= [UnknownProjectFlag field.at (PackageName "p") (FlagName "nope")]
          Left e -> assertFailure (show e),
      testCase "paths outside ROOT: dropped, warning" $
        case readCabalFile
          linux
          []
          cabalFile
          ( cabal
              [ "library",
                "  exposed-modules: A",
                "  hs-source-dirs: src ../shared ../../up",
                "  include-dirs: /usr/include inc"
              ]
          ) of
          Right (pkg, ws) -> do
            map (map relDirText . (.sourceDirs)) pkg.components @?= [["p/src", "shared"]]
            map (map relDirText . (.includeDirs)) pkg.components @?= [["p/inc"]]
            ws
              @?= [ badPath "hs-source-dirs" "../../up" AboveRoot,
                    badPath "include-dirs" "/usr/include" AbsolutePath
                  ]
          Left e -> assertFailure (show e),
      testCase "two components with one name: the first, warning" $
        case readCabalFile
          linux
          []
          cabalFile
          ( cabal
              ["executable e", "  main-is: A.hs", "executable e", "  main-is: B.hs"]
          ) of
          Right (pkg, ws) -> do
            map (.mainIs) pkg.components @?= [Just "A.hs"]
            ws @?= [DuplicateComponent cabalFile (Exe "e")]
          Left e -> assertFailure (show e),
      testCase "syntax error: CabalSyntax with the position" $
        let txt = cabal (library <> ["  build-depends: base >="])
         in case readCabalFile linux [] cabalFile txt of
              Left (CabalSyntax [e]) -> ("p/p.cabal:6:" `Text.isPrefixOf` e) @?= True
              other -> assertFailure (show other)
    ]

badPath :: Text -> Text -> PathProblem -> ProjectWarning
badPath field value problem =
  CabalBadPath
    BadCabalPath {cabalFile = cabalFile, field = field, value = value, problem = problem}

linux :: ProjectSettings
linux = on "linux" "x86_64"

on :: Text -> Text -> ProjectSettings
on os arch =
  ProjectSettings
    { platform = Platform {os = os, arch = arch},
      ghcVersion = GhcVersion [9, 14, 1]
    }

cabalFile :: RelPath
cabalFile = path ["p", "p.cabal"]

projectFile :: RelPath
projectFile = path ["cabal.project"]

path :: [Text] -> RelPath
path segs = fromMaybe (error "bad path") (relPathFromSegments segs)

-- | A .cabal file named p, with these lines after the header.
cabal :: [Text] -> Text
cabal ls = Text.unlines (["cabal-version: 3.0", "name: p", "version: 1"] <> ls)

-- | The start of a library.
library :: [Text]
library = ["library", "  exposed-modules: A"]

ok :: ProjectSettings -> [FlagsField] -> [Text] -> IO Package
ok settings flags ls = case readCabalFile settings flags cabalFile (cabal ls) of
  Right (pkg, []) -> pure pkg
  Right (_, ws) -> assertFailure ("warnings: " <> show ws)
  Left e -> assertFailure (show e)

-- | The library's cpp-options when the condition holds: @-DYES@.
cppIf :: ProjectSettings -> Text -> [Text]
cppIf settings cond =
  case readCabalFile
    settings
    []
    cabalFile
    (cabal (library <> ["  if " <> cond, "    cpp-options: -DYES"])) of
    Right (pkg, _) -> concatMap (.cppOptions) pkg.components
    Left e -> ["error: " <> Text.pack (show e)]
