{-# LANGUAGE StrictData #-}

-- StrictData: fewer thunks to force before the index is shared. See
-- Note [Forced snapshot] in "HaskellReviewer.Server.Api".

-- | Which modules the project has, and where their sources are: the
-- module tree (SPEC §2.3, §4.1). Pure: works on the project model and
-- the file list.
module HaskellReviewer.Core.Modules
  ( -- * Types

    -- | Fields only to read: build it with 'buildModuleIndex' (the name
    -- maps are derived from the rest).
    ModuleIndex (..),
    PackageModules (..),
    ComponentModules (..),
    ModuleEntry (..),
    ModuleRole (..),
    ModuleSource (..),
    HaskellFile (..),
    Literate (..),
    GeneratedFile (..),
    Preprocessor (..),
    PreprocessedFile (..),
    NoSourceReason (..),
    OtherFile (..),
    LooseFile (..),
    Notice (..),

    -- * Building
    buildModuleIndex,

    -- * Identity
    ModuleId (..),
    renderModuleId,
    moduleRef,
    resolveModuleRef,

    -- * Helpers
    moduleNameFromPath,
    renderPreprocessor,
    Tools (..),
    toolsFrom,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isUpper)
import Data.List (nub, sortOn, unsnoc)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..))
import HaskellReviewer.Core.Project
  ( BrokenPackage,
    Component (..),
    ComponentName (..),
    ListedModule (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    ProjectWarning (..),
    renderComponentName,
  )
import HaskellReviewer.Core.RelPath
  ( PathProblem,
    RelDir,
    RelPath,
    relDirFile,
    relDirSegments,
    relPathFromSegments,
    relPathName,
    relPathSegments,
    relPathText,
    resolveUnder,
    rootDir,
  )

{- Note [Module discovery]

How we find each component's modules, without building (SPEC §2.3,
D-84):

1. Lookup order is Cabal's (preprocessFile): first the preprocessor
   inputs, then the Haskell files. Each step goes dir by dir, in
   hs-source-dirs order, then extension by extension:
     .gc .chs .hsc .x .y .ly .cpphs, then .hs .lhs .hsig .lhsig.
   So Lexer.x wins over Lexer.hs, even in a later dir. Lexer.hs is then
   claimed by no one and goes to "Other files".
2. A preprocessor input is a generated module: text only, not indexed.
3. -F -pgmF <tool> in the component's ghc-options: every Haskell file
   of the component is "preprocessed by <tool>", not indexed. -pgmF
   alone does nothing (GHC needs -F). -pgmL <tool>: the same for .lhs
   and .lhsig, except markdown-unlit, which we do ourselves (a module).
   Both "-pgmF tool" and "-pgmFtool"; the last one wins; the tool shown
   is the file name. OPTIONS_GHC pragmas are not read here.
4. main-is: under each source dir in order, read like a path written
   in a .cabal file (may hold ..). The first listed file wins. If it's
   outside ROOT under every dir: no source, "outside". A .git segment
   counts as outside too. Its module is Main (role main); a listed Main
   in the same component is dropped. A main-is that is not .hs or .lhs
   (a C main) is skipped.
5. A found file that is a symlink: no source, "symlink". We never follow
   symlinks (Note [File containment] in "HaskellReviewer.Core.RelPath").
   A symlink in an earlier dir wins over a real file in a later one,
   like the lookup would on disk.
6. No file found: "autogen" if the module is in autogen-modules (we
   don't even look), or named Paths_<pkg> / PackageInfo_<pkg>; else
   "missing".
7. Other files: the .hs, .lhs, .hsig and .lhsig files no component
   claims, and the .hs-boot / .lhs-boot files
   (text only). Paths with a dist-newstyle or .stack-work segment are
   skipped: build output, not code. They stay in the file tree.
   With no project at all, the other files are the implicit package.
8. Identity (D-44): pkg:lib:M, pkg:lib:x:M, pkg:exe:x:M, ... For other
   files it's the path. It never clashes with a module name: it ends in
   a Haskell extension (.hs, .lhs, ...), and a module name can't. An
   other file's module name comes from its path ('moduleNameFromPath').
   Short forms, where owned (listed) modules win:
   - an owned module's short form is its name when no other owned
     module has that name, whatever the other files are;
   - an other file's short form is its name when no owned module and
     no other file has it. Else it's the path.
   'resolveModuleRef' follows the same rules.
9. Notices:
   - stack: stack.yaml in ROOT and no cabal.project at all. An
     unreadable one counts as a cabal.project: no stack notice.
   - implicit: no cabal.project used (none, or unreadable: then we do
     use the implicit package), no packages, no broken packages.
   - hpack: the HpackOnly dirs, plus ROOT when implicit with a
     package.yaml. HpackOnly warnings move to the notice.

Known gap: Cabal also runs preprocessors on main-is. We don't.
-}

-- * Types

-- | The module tree of one snapshot. See Note [Module discovery].
data ModuleIndex = ModuleIndex
  { -- | In project order (by .cabal path).
    packages :: [PackageModules],
    broken :: [BrokenPackage],
    -- | Sorted by path.
    other :: [OtherFile],
    notices :: [Notice],
    -- | The project's warnings, minus the ones a notice shows.
    warnings :: [ProjectWarning],
    -- | Owned (listed) modules by name, for short forms.
    ownedByName :: Map ModuleName [ModuleId],
    -- | Other files by module name, for short forms.
    looseByName :: Map ModuleName [RelPath],
    -- | Every module id.
    ids :: Set ModuleId
  }

-- | The modules of one package.
data PackageModules = PackageModules
  { name :: PackageName,
    version :: Text,
    cabalFile :: RelPath,
    -- | In .cabal order.
    components :: [ComponentModules]
  }

-- | The modules of one component.
data ComponentModules = ComponentModules
  { name :: ComponentName,
    buildable :: Bool,
    -- | Sorted by name.
    modules :: [ModuleEntry]
  }

-- | One module of a component. Its id is a 'ComponentModule'.
data ModuleEntry = ModuleEntry
  { name :: ModuleName,
    role :: ModuleRole,
    source :: ModuleSource
  }

-- | Why the module is in the component.
data ModuleRole
  = RoleExposed
  | RoleOther
  | RoleSignature
  | -- | @test-module@ of a detailed-0.9 test suite.
    RoleTestModule
  | -- | From @main-is@.
    RoleMain
  deriving (Eq, Show)

-- | Where a module's code is.
data ModuleSource
  = -- | We parse it.
    HaskellSource HaskellFile
  | -- | A preprocessor input: text only.
    GeneratedSource GeneratedFile
  | -- | Run through a -pgmF / -pgmL tool: text only.
    PreprocessedSource PreprocessedFile
  | NoSource NoSourceReason
  deriving (Eq, Show)

-- | A Haskell file of a module.
data HaskellFile = HaskellFile
  { path :: RelPath,
    literate :: Literate
  }
  deriving (Eq, Show)

-- | How to unlit the file.
data Literate
  = NotLiterate
  | -- | .lhs or .lhsig, GHC's unlit.
    BirdOrLatex
  | -- | -pgmL markdown-unlit: code blocks only (D-84).
    MarkdownUnlit
  deriving (Eq, Show)

-- | A preprocessor input.
data GeneratedFile = GeneratedFile
  { path :: RelPath,
    tool :: Preprocessor
  }
  deriving (Eq, Show)

-- | The preprocessors Cabal knows, by input extension.
data Preprocessor
  = GreenCard
  | C2hs
  | Hsc2hs
  | Alex
  | Happy
  | Cpphs
  deriving (Eq, Show)

-- | A file run through a -pgmF / -pgmL tool.
data PreprocessedFile = PreprocessedFile
  { path :: RelPath,
    -- | The tool's file name, e.g. @hspec-discover@.
    tool :: Text
  }
  deriving (Eq, Show)

-- | Why a listed module has no source.
data NoSourceReason
  = -- | Made at build time.
    Autogen
  | Missing
  | -- | main-is out of ROOT.
    Outside PathProblem
  | -- | The file found is a symlink: never followed.
    SymlinkSource RelPath
  deriving (Eq, Show)

-- | A file of "Other files".
data OtherFile
  = OtherModule LooseFile
  | -- | .hs-boot or .lhs-boot: text only.
    BootFile RelPath
  deriving (Eq, Show)

-- | A Haskell file no component claims.
data LooseFile = LooseFile
  { path :: RelPath,
    -- | From the path. 'Nothing' if the path isn't a module name.
    name :: Maybe ModuleName
  }
  deriving (Eq, Show)

-- | A project-wide message for the user.
data Notice
  = -- | stack.yaml, no cabal.project (D-66).
    StackNotice
  | -- | No project: the implicit package (SPEC §8).
    ImplicitNotice
  | -- | Dirs with package.yaml but no .cabal (D-41).
    HpackNotice (NonEmpty RelDir)
  deriving (Eq, Show)

-- | A module's identity (D-44). See Note [Module discovery].
data ModuleId
  = ComponentModule PackageName ComponentName ModuleName
  | -- | An other file: the path is the id.
    LooseModule RelPath
  deriving (Eq, Ord, Show)

-- * Building

-- | The module tree of a project. See Note [Module discovery].
buildModuleIndex :: Project -> FileList -> ModuleIndex
buildModuleIndex project fl =
  ModuleIndex
    { packages = packages,
      broken = project.brokenPackages,
      other = other,
      notices = notices,
      warnings = [w | w <- project.warnings, not (isHpackOnly w)],
      ownedByName = groupByName owned,
      looseByName = groupByName [(n, f.path) | f <- loose, Just n <- [f.name]],
      ids = Set.fromList (map snd owned <> [LooseModule f.path | f <- loose])
    }
  where
    packages = map (packageModules fl) project.packages
    claimed =
      Set.fromList
        [ p
        | pkg <- packages,
          c <- pkg.components,
          m <- c.modules,
          Just p <- [sourcePath m.source]
        ]
    other =
      [ o
      | (p, EntryFile) <- Map.toList fl.entries,
        not (isBuildOutput p),
        not (Set.member p claimed),
        Just o <- [otherFile p]
      ]
    loose = [f | OtherModule f <- other]
    owned =
      [ (m.name, ComponentModule pkg.name c.name m.name)
      | pkg <- packages,
        c <- pkg.components,
        m <- c.modules
      ]

    -- Notices. See Note [Module discovery].
    has name = maybe False (`Map.member` fl.entries) (relPathFromSegments [name])
    implicit =
      project.source == ImplicitRootCabalFiles
        && null project.packages
        && null project.brokenPackages
    hpackDirs =
      nub $
        [d | HpackOnly d <- project.warnings]
          <> [rootDir | implicit, has "package.yaml"]
    notices =
      [StackNotice | has "stack.yaml", not (has "cabal.project")]
        <> [ImplicitNotice | implicit]
        <> [HpackNotice ds | Just ds <- [nonEmpty hpackDirs]]
    isHpackOnly = \case
      HpackOnly _ -> True
      _ -> False

-- * Identity

-- | The long form: @pkg:lib:M@, @pkg:exe:x:M@, or the path.
renderModuleId :: ModuleId -> Text
renderModuleId = \case
  ComponentModule (PackageName p) c (ModuleName m) ->
    p <> ":" <> renderComponentName c <> ":" <> m
  LooseModule p -> relPathText p

-- | How URLs name a module: the short form when it's unique, else the
-- long form (D-44). See Note [Module discovery] for who wins.
moduleRef :: ModuleIndex -> ModuleId -> Text
moduleRef idx i = case i of
  ComponentModule _ _ n@(ModuleName t)
    | Map.lookup n idx.ownedByName == Just [i] -> t
  LooseModule p
    | Just n@(ModuleName t) <- moduleNameFromPath p,
      shortFor idx n == Just i ->
        t
  _ -> renderModuleId i

-- | The module a short name stands for, if any.
shortFor :: ModuleIndex -> ModuleName -> Maybe ModuleId
shortFor idx n = case (Map.lookup n idx.ownedByName, Map.lookup n idx.looseByName) of
  (Just [i], _) -> Just i
  (Nothing, Just [p]) -> Just (LooseModule p)
  _ -> Nothing

-- | Back from 'moduleRef' (or a long form). 'Nothing' if no module has
-- it, or a short name is not unique.
resolveModuleRef :: ModuleIndex -> Text -> Maybe ModuleId
resolveModuleRef idx t = short <|> long <|> loose
  where
    short = shortFor idx (ModuleName t)
    long = do
      i <- case Text.splitOn ":" t of
        [p, "lib", m] -> Just (ComponentModule (PackageName p) Lib (ModuleName m))
        [p, kind, n, m] -> do
          c <- case kind of
            "lib" -> Just (SubLib n)
            "exe" -> Just (Exe n)
            "test" -> Just (Test n)
            "bench" -> Just (Bench n)
            _ -> Nothing
          Just (ComponentModule (PackageName p) c (ModuleName m))
        _ -> Nothing
      known i
    loose = known . LooseModule =<< relPathFromSegments (Text.splitOn "/" t)
    known i = if Set.member i idx.ids then Just i else Nothing

-- * Helpers

-- | The module name a path gives: the longest tail of segments that
-- are all module name parts (extension dropped from the last).
-- @src\/Foo\/Bar.hs@ is @Foo.Bar@, @scripts\/build-docs.hs@ is none.
moduleNameFromPath :: RelPath -> Maybe ModuleName
moduleNameFromPath p = case reverse (relPathSegments p) of
  file : dirs
    | isConId stem ->
        Just (ModuleName (Text.intercalate "." (reverse (stem : modDirs))))
    where
      stem = Text.dropEnd (Text.length (extension file)) file
      modDirs = takeWhile isConId dirs
  _ -> Nothing
  where
    isConId s = case Text.uncons s of
      Just (c, rest) -> isUpper c && Text.all isIdChar rest
      Nothing -> False
    isIdChar x = isAlphaNum x || x == '_' || x == '\''

-- | The tool's name, e.g. @alex@.
renderPreprocessor :: Preprocessor -> Text
renderPreprocessor = \case
  GreenCard -> "greencard"
  C2hs -> "c2hs"
  Hsc2hs -> "hsc2hs"
  Alex -> "alex"
  Happy -> "happy"
  Cpphs -> "cpphs"

-- | The -pgmF / -pgmL tools that apply to a component.
data Tools = Tools
  { -- | Only with -F.
    pgmF :: Maybe Text,
    pgmL :: Maybe Text
  }

-- | Read the tools from ghc-options (or ghc-options plus OPTIONS_GHC).
-- See Note [Module discovery].
toolsFrom :: [Text] -> Tools
toolsFrom opts =
  Tools
    { pgmF = if "-F" `elem` opts then lastArg "-pgmF" else Nothing,
      pgmL = lastArg "-pgmL"
    }
  where
    lastArg flag = fileName <$> listToMaybe (reverse (args flag opts))
    args flag = \case
      o : t : rest | o == flag -> t : args flag rest
      o : rest
        | Just t <- Text.stripPrefix flag o, not (Text.null t) -> t : args flag rest
        | otherwise -> args flag rest
      [] -> []
    fileName t = case Text.takeWhileEnd (/= '/') t of
      "" -> t
      n -> n

-- * Internal

packageModules :: FileList -> Package -> PackageModules
packageModules fl pkg =
  PackageModules
    { name = pkg.name,
      version = pkg.version,
      cabalFile = pkg.cabalFile,
      components = map (componentModules fl pkg.name) pkg.components
    }

componentModules :: FileList -> PackageName -> Component -> ComponentModules
componentModules fl pkg c =
  ComponentModules
    { name = c.name,
      buildable = c.buildable,
      modules = sortOn (.name) (mains <> filter notMain listed)
    }
  where
    tools = toolsFrom c.ghcOptions
    listed = map (listedEntry fl pkg tools c.sourceDirs) c.modules
    mains = maybe [] (mainEntry fl tools c.sourceDirs) c.mainIs
    notMain m = null mains || m.name /= ModuleName "Main"

-- | A module listed in the .cabal file.
listedEntry ::
  FileList -> PackageName -> Tools -> [RelDir] -> ListedModule -> ModuleEntry
listedEntry fl (PackageName pkg) tools dirs m =
  ModuleEntry {name = m.name, role = role m.kind, source = source}
  where
    ModuleName name = m.name
    parts = Text.splitOn "." name
    source
      | m.autogen = NoSource Autogen
      | otherwise = case firstListed fl (preprocessed <> haskell) of
          Just hit -> found tools hit
          Nothing
            | isPackageAutogen -> NoSource Autogen
            | otherwise -> NoSource Missing
    preprocessed =
      [f | d <- dirs, (ext, _) <- preprocessorExts, Just f <- [file d ext]]
    haskell = [f | d <- dirs, ext <- haskellExts, Just f <- [file d ext]]
    file d ext = do
      (up, lastPart) <- unsnoc parts
      relPathFromSegments (relDirSegments d <> up <> [lastPart <> ext])
    pkgPart = Text.replace "-" "_" pkg
    isPackageAutogen = name `elem` ["Paths_" <> pkgPart, "PackageInfo_" <> pkgPart]
    role = \case
      Exposed -> RoleExposed
      Other -> RoleOther
      Signature -> RoleSignature
      TestModule -> RoleTestModule

-- | The Main module from main-is, if it's a Haskell file.
mainEntry :: FileList -> Tools -> [RelDir] -> Text -> [ModuleEntry]
mainEntry fl tools dirs mainIs
  | not (any (`Text.isSuffixOf` mainIs) [".hs", ".lhs"]) = []
  | otherwise = [ModuleEntry {name = ModuleName "Main", role = RoleMain, source = source}]
  where
    resolved = [resolveUnder d mainIs | d <- dirs]
    candidates = [p | Right d <- resolved, Just p <- [relDirFile d]]
    source = case firstListed fl candidates of
      Just hit -> found tools hit
      Nothing -> case [e | Left e <- resolved] of
        e : _ | null candidates -> NoSource (Outside e)
        _ -> NoSource Missing

-- | A path found in the list.
data Hit = HitFile RelPath | HitSymlink RelPath

-- | The first path that is a file or a symlink in the list.
firstListed :: FileList -> [RelPath] -> Maybe Hit
firstListed fl = listToMaybe . mapMaybe listed
  where
    listed p = case Map.lookup p fl.entries of
      Just EntryFile -> Just (HitFile p)
      Just EntrySymlink -> Just (HitSymlink p)
      _ -> Nothing

-- | What a hit is, with the component's tools.
found :: Tools -> Hit -> ModuleSource
found tools = \case
  HitSymlink p -> NoSource (SymlinkSource p)
  HitFile p -> fileSource tools p

-- | What a found file is, with the component's tools.
fileSource :: Tools -> RelPath -> ModuleSource
fileSource tools p
  | Just tool <- lookup ext preprocessorExts =
      GeneratedSource GeneratedFile {path = p, tool = tool}
  | Just tool <- tools.pgmF = preprocessedBy tool
  | literate,
    Just tool <- tools.pgmL =
      if tool == "markdown-unlit"
        then haskell MarkdownUnlit
        else preprocessedBy tool
  | literate = haskell BirdOrLatex
  | otherwise = haskell NotLiterate
  where
    ext = extension (relPathName p)
    literate = ext `elem` [".lhs", ".lhsig"]
    haskell l = HaskellSource HaskellFile {path = p, literate = l}
    preprocessedBy tool = PreprocessedSource PreprocessedFile {path = p, tool = tool}

-- | Group by name, first one first. Strict: no thunks left in the
-- lists.
groupByName :: [(ModuleName, a)] -> Map ModuleName [a]
groupByName = Map.map reverse . foldl' add Map.empty
  where
    add m (n, x) = Map.alter (Just . maybe [x] (x :)) n m

-- | The file a source points to, if any.
sourcePath :: ModuleSource -> Maybe RelPath
sourcePath = \case
  HaskellSource f -> Just f.path
  GeneratedSource f -> Just f.path
  PreprocessedSource f -> Just f.path
  NoSource _ -> Nothing

-- | An unclaimed file, if it goes in "Other files".
otherFile :: RelPath -> Maybe OtherFile
otherFile p
  | ext `elem` haskellExts =
      Just (OtherModule LooseFile {path = p, name = moduleNameFromPath p})
  | ext `elem` [".hs-boot", ".lhs-boot"] = Just (BootFile p)
  | otherwise = Nothing
  where
    ext = extension (relPathName p)

-- | The last extension of a file name, with its dot: @.hs@,
-- @.hs-boot@. Empty if none.
extension :: Text -> Text
extension name
  | Text.any (== '.') name = "." <> Text.takeWhileEnd (/= '.') name
  | otherwise = ""

-- | Under dist-newstyle or .stack-work.
isBuildOutput :: RelPath -> Bool
isBuildOutput p = any (`elem` ["dist-newstyle", ".stack-work"]) (relPathSegments p)

-- | In Cabal's order (knownSuffixHandlers).
preprocessorExts :: [(Text, Preprocessor)]
preprocessorExts =
  [ (".gc", GreenCard),
    (".chs", C2hs),
    (".hsc", Hsc2hs),
    (".x", Alex),
    (".y", Happy),
    (".ly", Happy),
    (".cpphs", Cpphs)
  ]

-- | In Cabal's order.
haskellExts :: [Text]
haskellExts = [".hs", ".lhs", ".hsig", ".lhsig"]
