-- | Read one .cabal file into a 'Package', with Cabal-syntax (D-67).
module HaskellReviewer.Core.CabalFile
  ( -- * Reading
    readCabalFile,

    -- * Conditions
    evalCondition,
    evalConfVar,
  )
where

import Data.Foldable (toList)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Distribution.Compiler (CompilerFlavor (..), PerCompilerFlavor (..))
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.ModuleName qualified as Cabal (ModuleName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.Error (showPError)
import Distribution.Pretty (prettyShow)
import Distribution.System (ClassificationStrictness (..), classifyArch, classifyOS)
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.Flag (PackageFlag (..), unFlagName)
import Distribution.Types.Flag qualified as Cabal (FlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.TestSuiteInterface (TestSuiteInterface (..))
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion, withinRange)
import HaskellReviewer.Core.CabalCompat
  ( benchMainIs,
    exeMainIs,
    includeDirPaths,
    testMainIs,
  )
import HaskellReviewer.Core.Project
  ( BadCabalPath (..),
    CabalProblem (..),
    Component (..),
    ComponentName (..),
    Extension (..),
    FlagKind (..),
    FlagName (..),
    FlagScope (..),
    FlagSetting (..),
    FlagSource (..),
    FlagValue (..),
    FlagsField (..),
    GhcVersion (..),
    Language (..),
    ListedModule (..),
    Loc (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Platform (..),
    ProjectSettings (..),
    ProjectWarning (..),
  )
import HaskellReviewer.Core.RelPath
  ( RelDir,
    RelPath,
    parentDir,
    relPathText,
    resolveUnder,
  )
import Language.Haskell.Extension qualified as Cabal

{- Note [Flag values]

A flag's value (SPEC §3.1, §11.4.2):

1. its default in the .cabal file;
2. then cabal.project: top-level flags:, then package <name>. Inside
   each, file order (imports spliced in), and the last one wins.

flags: in package * are ignored, with no warning, like cabal-install
does: it doesn't use them for planning.

Manual and automatic flags alike. There is no solver, so an automatic
flag is never flipped to make a build plan work: it keeps its default
unless cabal.project sets it.

cabal-install passes top-level flags: and package <name> flags to the
solver as two separate constraints, so a clash is a solver error. We
pick the package <name> value instead of failing.

A flag used in a condition but not declared is a parse error in
Cabal-syntax, so the package is broken.

A package <name> stanza that sets a flag the package doesn't declare
gives a warning. Top-level flags: don't: they are for every package,
and most packages don't declare most flags.
-}

{- Note [Own CondTree walk]

We don't use Cabal's finalizePD or simplifyCondTree. They merge the
taken branches with the component's Semigroup, and those of Library
and Executable call `error` when two nodes set the same single field
(e.g. main-is at top level and in an if). A user's file must not crash
us.

So we walk the tree ourselves: the root, then each branch whose
condition holds (else its else part), depth first, in file order. We
keep the list of nodes and take each field from it: lists are joined,
single values (main-is, language) come from the last node that sets
them, and buildable is False if any node says so. We never <> whole
components or BuildInfos.
-}

-- | Read one .cabal file. The flags fields are cabal.project's, in
-- order (see Note [Flag values]).
readCabalFile ::
  ProjectSettings ->
  [FlagsField] ->
  RelPath ->
  Text ->
  Either CabalProblem (Package, [ProjectWarning])
readCabalFile settings projectFlags path txt =
  case runParseResult (parseGenericPackageDescription (Text.Encoding.encodeUtf8 txt)) of
    (_, Left (_, errs)) -> Left (CabalSyntax (map message (toList errs)))
    (_, Right gpd) -> Right (fromGpd settings projectFlags path gpd)
  where
    -- One line each: the messages have newlines in them.
    message e = Text.unwords (Text.words (Text.pack (showPError file e)))
    file = Text.unpack (relPathText path)

-- | Is the condition true?
evalCondition :: (c -> Bool) -> Condition c -> Bool
evalCondition ev = \case
  Var c -> ev c
  Lit b -> b
  CNot c -> not (evalCondition ev c)
  COr a b -> evalCondition ev a || evalCondition ev b
  CAnd a b -> evalCondition ev a && evalCondition ev b

-- | Is @os(..)@, @arch(..)@, @impl(..)@ or @flag(..)@ true? Flags come
-- from the function. OS and arch names go through Cabal's aliases, so
-- @darwin@ and @osx@ are the same.
evalConfVar :: ProjectSettings -> (FlagName -> Bool) -> ConfVar -> Bool
evalConfVar settings flagValue = \case
  OS o -> o == classifyOS Permissive (Text.unpack settings.platform.os)
  Arch a -> a == classifyArch Permissive (Text.unpack settings.platform.arch)
  Impl GHC range ->
    let GhcVersion ns = settings.ghcVersion in withinRange (mkVersion ns) range
  Impl _ _ -> False
  PackageFlag f -> flagValue (FlagName (Text.pack (unFlagName f)))

-- * Internal

fromGpd ::
  ProjectSettings ->
  [FlagsField] ->
  RelPath ->
  GenericPackageDescription ->
  (Package, [ProjectWarning])
fromGpd settings projectFlags path gpd =
  (package, flagWarnings <> pathWarnings <> dupWarnings)
  where
    pd = gpd.packageDescription
    pkgName = PackageName (Text.pack (unPackageName pd.package.pkgName))
    dir = parentDir path

    -- Flags. See Note [Flag values].
    overrides = projectOverrides pkgName projectFlags
    flagValues =
      [ FlagValue {name = n, value = v, kind = kind, source = src}
      | f <- gpd.genPackageFlags,
        let n = flagNameOf f.flagName
            kind = if f.flagManual then Manual else Automatic
            (v, src) = case Map.lookup n overrides of
              Just o -> (o.value, FlagFromProject o.file)
              Nothing -> (f.flagDefault, FlagDefault)
      ]
    flagMap = Map.fromList [(v.name, v.value) | v <- flagValues]
    flagValue n = Map.findWithDefault False n flagMap
    isTrue = evalCondition (evalConfVar settings flagValue)
    flagWarnings =
      [ UnknownProjectFlag field.at name s.name
      | field@FlagsField {scope = OnePackage name} <- projectFlags,
        name == pkgName,
        s <- field.flags,
        not (Map.member s.name flagMap)
      ]

    -- Components, in .cabal order.
    -- Foreign libraries are skipped: their files go to "Other files".
    built =
      [libComponent Lib (nodes t) | t <- toList gpd.condLibrary]
        <> [libComponent (SubLib (unqual n)) (nodes t) | (n, t) <- gpd.condSubLibraries]
        <> [exeComponent (Exe (unqual n)) (nodes t) | (n, t) <- gpd.condExecutables]
        <> [testComponent (Test (unqual n)) (nodes t) | (n, t) <- gpd.condTestSuites]
        <> [benchComponent (Bench (unqual n)) (nodes t) | (n, t) <- gpd.condBenchmarks]
    nodes :: CondTree ConfVar c a -> [a]
    nodes = activeNodes isTrue
    pathWarnings = concatMap snd built
    (components, dupWarnings) = dropDuplicateComponents path (map fst built)

    libComponent name libs =
      component
        name
        (map (.libBuildInfo) libs)
        ( [(m, Exposed) | l <- libs, m <- l.exposedModules]
            <> [(m, Signature) | l <- libs, m <- l.signatures]
        )
        Nothing
    exeComponent name exes =
      component
        name
        (map (.buildInfo) exes)
        []
        (lastJust (map exeMainIs exes))
    testComponent name tests =
      component
        name
        (map (.testBuildInfo) tests)
        [(m, TestModule) | t <- tests, TestSuiteLibV09 _ m <- [t.testInterface]]
        (lastJust (map testMainIs tests))
    benchComponent name benches =
      component
        name
        (map (.benchmarkBuildInfo) benches)
        []
        (lastJust (map benchMainIs benches))

    component name bis extraModules mainIs = (comp, srcWarns <> incWarns)
      where
        rawSrcDirs = [getSymbolicPath d | bi <- bis, d <- bi.hsSourceDirs]
        (srcDirs, srcWarns) = resolveDirs path dir "hs-source-dirs" rawSrcDirs
        (incDirs, incWarns) =
          resolveDirs path dir "include-dirs" (concatMap includeDirPaths bis)
        autogen = Set.fromList [m | bi <- bis, m <- bi.autogenModules]
        comp =
          Component
            { name = name,
              buildable = all (.buildable) bis,
              sourceDirs = if null rawSrcDirs then [dir] else nub srcDirs,
              language =
                maybe Haskell98 language (lastJust (map (.defaultLanguage) bis)),
              extensions =
                [ extension e
                | bi <- bis,
                  e <- bi.defaultExtensions <> bi.oldExtensions
                ],
              buildDepends =
                nub
                  [ PackageName (Text.pack (unPackageName (depPkgName d)))
                  | bi <- bis,
                    d <- bi.targetBuildDepends
                  ],
              mainIs = Text.pack <$> mainIs,
              modules =
                listedModules
                  autogen
                  (extraModules <> [(m, Other) | bi <- bis, m <- bi.otherModules]),
              ghcOptions =
                [Text.pack o | bi <- bis, let PerCompilerFlavor g _ = bi.options, o <- g],
              cppOptions = [Text.pack o | bi <- bis, o <- bi.cppOptions],
              includeDirs = nub incDirs
            }

    package =
      Package
        { name = pkgName,
          version = Text.pack (prettyShow pd.package.pkgVersion),
          cabalFile = path,
          flags = flagValues,
          components = components
        }

-- | A flag value set by cabal.project.
data ProjectFlag = ProjectFlag
  { value :: Bool,
    -- | The project file that set it last.
    file :: RelPath
  }

-- | cabal.project's value for each flag it sets for this package. See
-- Note [Flag values].
projectOverrides :: PackageName -> [FlagsField] -> Map FlagName ProjectFlag
projectOverrides pkg fields =
  -- Map.fromList keeps the last value of a key: the list is in
  -- precedence order, lowest first.
  Map.fromList
    [ (s.name, ProjectFlag {value = s.value, file = f.at.file})
    | scopeOk <- [(== GlobalFlags), (== OnePackage pkg)],
      f <- fields,
      scopeOk f.scope,
      s <- f.flags
    ]

-- | Keep the first component of each name, warn about the others.
dropDuplicateComponents :: RelPath -> [Component] -> ([Component], [ProjectWarning])
dropDuplicateComponents path = go Set.empty
  where
    go _ [] = ([], [])
    go seen (c : rest)
      | Set.member c.name seen =
          let (cs, ws) = go seen rest in (cs, DuplicateComponent path c.name : ws)
      | otherwise =
          let (cs, ws) = go (Set.insert c.name seen) rest in (c : cs, ws)

-- | The data of the nodes the conditions take, root first. See
-- Note [Own CondTree walk].
activeNodes :: (Condition ConfVar -> Bool) -> CondTree ConfVar c a -> [a]
activeNodes isTrue (CondNode a _ branches) = a : concatMap branch branches
  where
    branch (CondBranch cond yes no)
      | isTrue cond = activeNodes isTrue yes
      | otherwise = maybe [] (activeNodes isTrue) no

-- | Resolve dirs under the package dir. Bad ones are dropped, with a
-- warning.
resolveDirs ::
  RelPath -> RelDir -> Text -> [FilePath] -> ([RelDir], [ProjectWarning])
resolveDirs path dir field raw = (good, bad)
  where
    results = [(t, resolveUnder dir t) | t <- map Text.pack raw]
    good = [d | (_, Right d) <- results]
    bad =
      [ CabalBadPath
          BadCabalPath {cabalFile = path, field = field, value = t, problem = e}
      | (t, Left e) <- results
      ]

-- | No duplicates; the first kind wins.
listedModules ::
  Set.Set Cabal.ModuleName -> [(Cabal.ModuleName, ModuleKind)] -> [ListedModule]
listedModules autogen = go Set.empty
  where
    go _ [] = []
    go seen ((m, k) : rest)
      | Set.member m seen = go seen rest
      | otherwise = listed m k : go (Set.insert m seen) rest
    listed m k =
      ListedModule
        { name = ModuleName (Text.pack (prettyShow m)),
          kind = k,
          autogen = Set.member m autogen
        }

language :: Cabal.Language -> Language
language = \case
  Cabal.Haskell98 -> Haskell98
  Cabal.Haskell2010 -> Haskell2010
  Cabal.GHC2021 -> GHC2021
  Cabal.GHC2024 -> GHC2024
  Cabal.UnknownLanguage s -> UnknownLanguage (Text.pack s)

extension :: Cabal.Extension -> Extension
extension = \case
  Cabal.EnableExtension k -> Extension {name = Text.pack (prettyShow k), enabled = True}
  Cabal.DisableExtension k -> Extension {name = Text.pack (prettyShow k), enabled = False}
  Cabal.UnknownExtension s -> Extension {name = Text.pack s, enabled = True}

flagNameOf :: Cabal.FlagName -> FlagName
flagNameOf = FlagName . Text.pack . unFlagName

unqual :: UnqualComponentName -> Text
unqual = Text.pack . unUnqualComponentName

lastJust :: [Maybe a] -> Maybe a
lastJust = listToMaybe . reverse . catMaybes
