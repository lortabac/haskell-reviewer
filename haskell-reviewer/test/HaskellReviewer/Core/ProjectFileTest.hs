module HaskellReviewer.Core.ProjectFileTest (tests) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Project
  ( FlagName (..),
    FlagScope (..),
    FlagSetting (..),
    FlagsField (..),
    GhcVersion (..),
    Loc (..),
    PackageEntry (..),
    PackageName (..),
    ProjectWarning (..),
    Required (..),
  )
import HaskellReviewer.Core.ProjectFile
  ( RawItem (..),
    parseFlags,
    parseProjectFile,
    parseWithCompilerVersion,
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "ProjectFile"
    [ testCase "flags: +a -b c, commas, case" $
        parseFlags "+a -B, c" @?= [on "a", off "b", on "c"],
      testCase "flags: a lone sign is skipped" $
        parseFlags "+ - x" @?= [on "x"],
      testCase "with-compiler versions" $ do
        parseWithCompilerVersion "ghc-9.12.2" @?= Just (GhcVersion [9, 12, 2])
        parseWithCompilerVersion "ghc-9.12" @?= Just (GhcVersion [9, 12])
        parseWithCompilerVersion "/opt/bin/ghc-9.10.1" @?= Just (GhcVersion [9, 10, 1])
        parseWithCompilerVersion "ghc" @?= Nothing
        parseWithCompilerVersion "ghc-9.x" @?= Nothing
        parseWithCompilerVersion "ghc-" @?= Nothing
        parseWithCompilerVersion "/usr/bin/ghc" @?= Nothing
        -- Would wrap around in an Int.
        parseWithCompilerVersion "ghc-99999999999999999999.1" @?= Nothing,
      testCase "fields we read, in order" $ do
        (items, warns) <-
          parsed
            [ "packages: a b,",
              "  c",
              "optional-packages: d",
              "with-compiler: ghc-9.12",
              "import: other.project",
              "flags: +x",
              "constraints: foo ==1",
              "source-repository-package",
              "  type: git",
              "  location: https://example.com",
              "package *",
              "  flags: -y",
              "  ghc-options: -O2",
              "package Foo",
              "  flags: z"
            ]
        warns @?= []
        items
          @?= [ RawPackage (entry 1 "a" Required),
                RawPackage (entry 1 "b" Required),
                RawPackage (entry 2 "c" Required),
                RawPackage (entry 3 "d" Optional),
                RawWithCompiler (loc 4) "ghc-9.12",
                RawImport (loc 5) "other.project",
                RawFlags (flags 6 GlobalFlags [on "x"]),
                RawFlags (flags 15 (OnePackage (PackageName "Foo")) [on "z"])
              ],
      testCase "packages: commas in braces stay" $ do
        (items, _) <- parsed ["packages: c,", "  x/{a,b}/*, d"]
        [e.glob | RawPackage e <- items] @?= ["c", "x/{a,b}/*", "d"],
      -- See Note [Braces in fields].
      testCase "packages: a brace glob first doesn't lex, like in cabal" $ do
        let lexes t = either (const False) (const True) (parseProjectFile file t)
        lexes "packages: {a,b}/*\n" @?= False
        lexes "packages:\n  {a,b}/\n" @?= False
        lexes "packages: c\n  {a,b}/\n" @?= True,
      testCase "packages: quoted entries" $ do
        (items, _) <- parsed ["packages: \"my dir\"/ b, \"c,d\""]
        [e.glob | RawPackage e <- items] @?= ["my dir/", "b", "c,d"],
      testCase "if / elif / else" $ do
        (items, warns) <-
          parsed
            [ "if os(linux)",
              "  packages: l",
              "elif os(darwin)",
              "  packages: d",
              "else",
              "  packages: o"
            ]
        warns @?= []
        case items of
          [RawIf _ [RawPackage l] [RawIf _ [RawPackage d] [RawPackage o]]] ->
            map (.glob) [l, d, o] @?= ["l", "d", "o"]
          _ -> assertFailure ("unexpected: " <> show items),
      testCase "flag() in a condition: warning" $ do
        (_, warns) <- parsed ["if flag(x)", "  packages: a"]
        warns @?= [FlagInProjectCondition (loc 1)],
      testCase "bad condition: block skipped, warning" $ do
        (items, warns) <- parsed ["if os(", "  packages: a", "packages: b"]
        items @?= [RawPackage (entry 3 "b" Required)]
        warns @?= [BadProjectCondition (loc 1)],
      testCase "doesn't lex: Left" $
        case parseProjectFile file "packages: a\n\"oops\n" of
          Left e -> ("line 2" `Text.isInfixOf` e) @?= True
          Right r -> assertFailure ("parsed: " <> show r)
    ]
  where
    on n = FlagSetting {name = FlagName n, value = True}
    off n = FlagSetting {name = FlagName n, value = False}
    entry l g r = PackageEntry {at = loc l, glob = g, required = r}
    flags l s fs = FlagsField {at = loc l, scope = s, flags = fs}

file :: RelPath
file = fromJust (relPathFromSegments ["cabal.project"])

loc :: Int -> Loc
loc l = Loc {file = file, line = l}

parsed :: [Text] -> IO ([RawItem], [ProjectWarning])
parsed ls = case parseProjectFile file (Text.unlines ls) of
  Left e -> assertFailure ("doesn't lex: " <> show e)
  Right r -> pure r
