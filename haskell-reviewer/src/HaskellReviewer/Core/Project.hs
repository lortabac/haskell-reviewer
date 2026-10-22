-- | The project model: packages and components, read from
-- @cabal.project@ and the @.cabal@ files. Types only; the loading is in
-- "HaskellReviewer.Core.ProjectLoad".
module HaskellReviewer.Core.Project
  ( -- * Settings
    ProjectSettings (..),
    Platform (..),
    hostPlatform,
    GhcVersion (..),
    defaultGhcVersion,
    renderGhcVersion,

    -- * Phase 1: cabal.project
    ProjectConfig (..),
    ProjectSource (..),
    ProjectItem (..),
    PackageEntry (..),
    Required (..),
    FlagsField (..),
    FlagScope (..),
    FlagSetting (..),
    WithCompiler (..),
    Loc (..),

    -- * Phase 2: packages
    Project (..),
    Package (..),
    PackageName (..),
    BrokenPackage (..),
    CabalProblem (..),
    FlagValue (..),
    FlagKind (..),
    FlagName (..),
    FlagSource (..),
    Component (..),
    ComponentName (..),
    renderComponentName,
    ListedModule (..),
    ModuleName (..),
    ModuleKind (..),
    Language (..),
    Extension (..),

    -- * Problems
    FileProblem (..),
    ProjectWarning (..),
    EntryIgnoredReason (..),
    BadCabalPath (..),
    renderProjectWarning,
    renderCabalProblem,
    renderFileProblem,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Types.Condition (Condition)
import Distribution.Types.ConfVar (ConfVar)
import HaskellReviewer.Core.RelPath
  ( PathProblem (..),
    RelDir,
    RelPath,
    relDirText,
    relPathText,
  )
import System.Info qualified

{- Note [Project model]

What we know about the project, without building it (SPEC §2.3,
§11.4.2; D-41, D-66, D-67):

- Packages come from cabal.project (packages: and optional-packages:,
  globs). Without one, from the *.cabal files in ROOT, like cabal.
- Each .cabal file is read with Cabal-syntax. Conditionals are decided
  with the host platform, the flag values (see Note [Flag values] in
  "HaskellReviewer.Core.CabalFile") and the GHC version.
- Nothing here throws. A bad file gives a warning (or a broken
  package), and the rest still loads.

Loading has two phases, so the GHC version can be picked in between
(it may come from with-compiler):

1. 'ProjectConfig': cabal.project and its imports. Needs no GHC version,
   and gives with-compiler.
2. 'Project': conditionals in cabal.project, then the .cabal files.
   Needs 'ProjectSettings'.

We only read files of the snapshot, through its file list: never the
real disk, never above ROOT, never a symlink (Note [File containment]
in "HaskellReviewer.Core.RelPath").
-}

-- * Settings

-- | What conditionals are decided with.
data ProjectSettings = ProjectSettings
  { platform :: Platform,
    ghcVersion :: GhcVersion
  }
  deriving (Eq, Show)

-- | OS and arch, with "System.Info" names (@linux@, @darwin@,
-- @x86_64@, @aarch64@, ...).
data Platform = Platform
  { os :: Text,
    arch :: Text
  }
  deriving (Eq, Show)

-- | The platform we run on.
hostPlatform :: Platform
hostPlatform =
  Platform
    { os = Text.pack System.Info.os,
      arch = Text.pack System.Info.arch
    }

-- | A GHC version, e.g. @[9, 14, 1]@.
newtype GhcVersion = GhcVersion [Int]
  deriving (Eq, Ord, Show)

-- | The parser's GHC version (ghc-lib-parser 9.14.1, D-83).
defaultGhcVersion :: GhcVersion
defaultGhcVersion = GhcVersion [9, 14, 1]

-- | E.g. @9.14.1@.
renderGhcVersion :: GhcVersion -> Text
renderGhcVersion (GhcVersion ns) = Text.intercalate "." (map (Text.pack . show) ns)

-- * Phase 1

-- | cabal.project and its imports, before conditionals are decided.
data ProjectConfig = ProjectConfig
  { source :: ProjectSource,
    -- | In file order, imports spliced in.
    items :: [ProjectItem],
    withCompiler :: Maybe WithCompiler,
    warnings :: [ProjectWarning]
  }
  deriving (Eq, Show)

-- | Where the package list comes from.
data ProjectSource
  = -- | cabal.project, and the imports we read, in read order.
    ProjectFile [RelPath]
  | -- | No usable cabal.project: the *.cabal files in ROOT.
    ImplicitRootCabalFiles
  deriving (Eq, Show)

-- | One thing cabal.project says that we use.
data ProjectItem
  = ItemPackage PackageEntry
  | ItemFlags FlagsField
  | -- | @if@, then and else branches. @elif@ is a nested @if@ in else.
    ItemIf (Condition ConfVar) [ProjectItem] [ProjectItem]
  deriving (Eq, Show)

-- | A place in a project file. Lines are 1-based.
data Loc = Loc
  { file :: RelPath,
    line :: Int
  }
  deriving (Eq, Show)

-- | One entry of @packages:@ or @optional-packages:@, as written.
data PackageEntry = PackageEntry
  { at :: Loc,
    glob :: Text,
    required :: Required
  }
  deriving (Eq, Show)

-- | @packages:@ or @optional-packages:@. Optional ones can match
-- nothing, silently.
data Required = Required | Optional
  deriving (Eq, Show)

-- | A @flags:@ field.
data FlagsField = FlagsField
  { at :: Loc,
    scope :: FlagScope,
    flags :: [FlagSetting]
  }
  deriving (Eq, Show)

-- | Which packages a @flags:@ field is for. See Note [Flag values] in
-- "HaskellReviewer.Core.CabalFile" for which one wins.
data FlagScope
  = -- | Top-level @flags:@.
    GlobalFlags
  | -- | In @package name@.
    OnePackage PackageName
  deriving (Eq, Show)

-- | @+name@ or @-name@.
data FlagSetting = FlagSetting
  { name :: FlagName,
    value :: Bool
  }
  deriving (Eq, Show)

-- | The top-level @with-compiler:@ field.
data WithCompiler = WithCompiler
  { -- | As written, e.g. @ghc-9.12.2@ or @\/opt\/bin\/ghc@.
    written :: Text,
    -- | From a name like @ghc-9.12.2@. 'Nothing' for plain @ghc@.
    version :: Maybe GhcVersion,
    file :: RelPath
  }
  deriving (Eq, Show)

-- * Phase 2

-- | The packages of one snapshot. See Note [Project model].
data Project = Project
  { source :: ProjectSource,
    withCompiler :: Maybe WithCompiler,
    -- | Sorted by .cabal path.
    packages :: [Package],
    -- | .cabal files we can't use.
    brokenPackages :: [BrokenPackage],
    warnings :: [ProjectWarning]
  }
  deriving (Eq, Show)

-- | A package name, as in the .cabal file.
newtype PackageName = PackageName Text
  deriving (Eq, Ord, Show)

-- | A local package.
data Package = Package
  { name :: PackageName,
    version :: Text,
    -- | Its dir is the package dir.
    cabalFile :: RelPath,
    -- | Every declared flag, in .cabal order.
    flags :: [FlagValue],
    components :: [Component]
  }
  deriving (Eq, Show)

-- | A .cabal file we can't use.
data BrokenPackage = BrokenPackage
  { cabalFile :: RelPath,
    problem :: CabalProblem
  }
  deriving (Eq, Show)

-- | Why a .cabal file can't be used.
data CabalProblem
  = CabalUnreadable FileProblem
  | -- | Cabal-syntax's errors, rendered.
    CabalSyntax [Text]
  deriving (Eq, Show)

-- | A flag name, lower case (flags are case-insensitive).
newtype FlagName = FlagName Text
  deriving (Eq, Ord, Show)

-- | The value a flag has here. See Note [Flag values] in
-- "HaskellReviewer.Core.CabalFile".
data FlagValue = FlagValue
  { name :: FlagName,
    value :: Bool,
    kind :: FlagKind,
    source :: FlagSource
  }
  deriving (Eq, Show)

-- | @manual: True@ or not.
data FlagKind = Manual | Automatic
  deriving (Eq, Show)

-- | Where a flag value comes from.
data FlagSource
  = FlagDefault
  | -- | The project file that set it last.
    FlagFromProject RelPath
  deriving (Eq, Show)

-- | A component, with its conditionals decided.
data Component = Component
  { name :: ComponentName,
    -- | @buildable: False@ components are kept, marked.
    buildable :: Bool,
    -- | Resolved. An empty @hs-source-dirs@ is the package dir.
    sourceDirs :: [RelDir],
    language :: Language,
    -- | @default-extensions@, then the old @extensions@, per block.
    extensions :: [Extension],
    -- | Package names, no duplicates, first one first.
    buildDepends :: [PackageName],
    -- | As written: relative to one of the source dirs.
    mainIs :: Maybe Text,
    modules :: [ListedModule],
    -- | GHC's @ghc-options@, as written, split in words.
    ghcOptions :: [Text],
    cppOptions :: [Text],
    -- | Resolved, like 'sourceDirs'.
    includeDirs :: [RelDir]
  }
  deriving (Eq, Show)

-- | Component names, as in D-44.
data ComponentName
  = Lib
  | SubLib Text
  | Exe Text
  | Test Text
  | Bench Text
  deriving (Eq, Ord, Show)

-- | @lib@, @lib:x@, @exe:x@, @test:x@, @bench:x@.
renderComponentName :: ComponentName -> Text
renderComponentName = \case
  Lib -> "lib"
  SubLib n -> "lib:" <> n
  Exe n -> "exe:" <> n
  Test n -> "test:" <> n
  Bench n -> "bench:" <> n

-- | A module name, like @Data.Map@.
newtype ModuleName = ModuleName Text
  deriving (Eq, Ord, Show)

-- | A module listed in a .cabal file.
data ListedModule = ListedModule
  { name :: ModuleName,
    kind :: ModuleKind,
    -- | In @autogen-modules@.
    autogen :: Bool
  }
  deriving (Eq, Show)

-- | Which field lists the module.
data ModuleKind
  = Exposed
  | Other
  | Signature
  | -- | @test-module@ of a detailed-0.9 test suite.
    TestModule
  deriving (Eq, Show)

-- | The language edition. No @default-language@ is Haskell98, like
-- Cabal (D-84).
data Language
  = Haskell98
  | Haskell2010
  | GHC2021
  | GHC2024
  | UnknownLanguage Text
  deriving (Eq, Show)

-- | An extension in @default-extensions@: @Foo@ is on, @NoFoo@ off.
-- Names Cabal doesn't know are kept as written, on.
data Extension = Extension
  { name :: Text,
    enabled :: Bool
  }
  deriving (Eq, Show)

-- * Problems

-- | Why a file can't be read.
data FileProblem
  = -- | Binary, or not UTF-8.
    NotText
  | -- | Not a regular file (e.g. a submodule).
    NotAFile
  | -- | Gone from disk since the file list was made.
    Gone
  | TooBig
  | -- | A symlink. We never follow them.
    IsSymlink
  | -- | The OS error.
    CantRead Text
  deriving (Eq, Show)

-- | Something odd in the project files. Never fatal.
data ProjectWarning
  = ProjectUnreadable RelPath FileProblem
  | -- | Doesn't lex. The text is the lexer's error.
    ProjectSyntax RelPath Text
  | -- | An @if@ we can't parse: the whole block is skipped.
    BadProjectCondition Loc
  | ImportRemote Loc Text
  | ImportBadPath Loc Text PathProblem
  | -- | Not a file of the snapshot.
    ImportMissing Loc Text
  | ImportCycle Loc Text
  | -- | @flag(..)@ in a cabal.project conditional: taken as false.
    FlagInProjectCondition Loc
  | WithCompilerInConditional Loc
  | EntryIgnored Loc Text EntryIgnoredReason
  | EntryNoMatch Loc Text
  | NoCabalFile RelDir
  | -- | A @package.yaml@, but no .cabal file (D-41).
    HpackOnly RelDir
  | -- | All of them are used.
    SeveralCabalFiles RelDir [RelPath]
  | -- | The first one by path is used.
    DuplicatePackage PackageName [RelPath]
  | -- | Set in a @package name@ stanza, not declared by that package.
    UnknownProjectFlag Loc PackageName FlagName
  | -- | A path field of a .cabal file, skipped.
    CabalBadPath BadCabalPath
  | -- | Two components with the same name. The first one is kept.
    DuplicateComponent RelPath ComponentName
  | -- | Over 'HaskellReviewer.Core.FileList.fileListCap' files: some
    -- packages may be missing.
    FileListTruncated
  | -- | Reading the project failed (a bug). The text is the error. No
    -- packages are shown.
    ProjectCrashed Text
  deriving (Eq, Show)

-- | Why a @packages:@ entry is skipped.
data EntryIgnoredReason
  = -- | A URL. We never fetch.
    RemoteEntry
  | TarballEntry
  | HomeDirEntry
  | EntryBadPath PathProblem
  deriving (Eq, Show)

-- | A path field of a .cabal file that we skip.
data BadCabalPath = BadCabalPath
  { cabalFile :: RelPath,
    -- | The field, e.g. @hs-source-dirs@.
    field :: Text,
    value :: Text,
    problem :: PathProblem
  }
  deriving (Eq, Show)

-- | Message for the user.
renderProjectWarning :: ProjectWarning -> Text
renderProjectWarning = \case
  ProjectUnreadable p e -> relPathText p <> ": can't read: " <> renderFileProblem e
  ProjectSyntax p e -> relPathText p <> ": can't parse: " <> e
  BadProjectCondition l -> loc l <> ": can't parse the condition, block skipped"
  ImportRemote l t -> loc l <> ": remote import skipped: " <> t
  ImportBadPath l t e ->
    loc l <> ": import skipped: " <> t <> " (" <> pathProblem e <> ")"
  ImportMissing l t -> loc l <> ": import not found: " <> t
  ImportCycle l t -> loc l <> ": import cycle: " <> t
  FlagInProjectCondition l -> loc l <> ": flag() in a condition is taken as false"
  WithCompilerInConditional l ->
    loc l <> ": with-compiler inside a conditional is ignored"
  EntryIgnored l t r -> loc l <> ": package skipped: " <> t <> " (" <> ignored r <> ")"
  EntryNoMatch l t -> loc l <> ": no package matches " <> t
  NoCabalFile d -> relDirText d <> ": no .cabal file"
  HpackOnly d ->
    relDirText d
      <> ": package.yaml but no .cabal file (Hpack is not supported: run hpack and commit"
      <> " the .cabal file)"
  SeveralCabalFiles d ps ->
    relDirText d <> ": several .cabal files: " <> commas (map relPathText ps)
  DuplicatePackage (PackageName n) ps ->
    "package "
      <> n
      <> " is defined more than once ("
      <> commas (map relPathText ps)
      <> "): the first one is used"
  UnknownProjectFlag l (PackageName n) (FlagName f) ->
    loc l <> ": package " <> n <> " has no flag " <> f
  CabalBadPath b ->
    relPathText b.cabalFile
      <> ": "
      <> b.field
      <> ": "
      <> b.value
      <> " skipped ("
      <> pathProblem b.problem
      <> ")"
  DuplicateComponent p c ->
    relPathText p
      <> ": "
      <> renderComponentName c
      <> " is defined twice, the first is used"
  FileListTruncated -> "too many files: some packages may be missing"
  ProjectCrashed e -> "can't read the project: " <> e
  where
    loc l = relPathText l.file <> ":" <> Text.pack (show l.line)
    commas = Text.intercalate ", "
    ignored = \case
      RemoteEntry -> "remote"
      TarballEntry -> "tarball"
      HomeDirEntry -> "home dir"
      EntryBadPath e -> pathProblem e
    pathProblem = \case
      AbsolutePath -> "absolute path"
      AboveRoot -> "outside ROOT"
      BadName -> "bad name"

-- | Message for the user.
renderCabalProblem :: CabalProblem -> Text
renderCabalProblem = \case
  CabalUnreadable e -> "can't read: " <> renderFileProblem e
  CabalSyntax es -> "can't parse: " <> Text.intercalate "; " es

-- | Message for the user.
renderFileProblem :: FileProblem -> Text
renderFileProblem = \case
  NotText -> "not UTF-8 text"
  NotAFile -> "not a file"
  Gone -> "gone"
  TooBig -> "too big"
  IsSymlink -> "a symlink"
  CantRead t -> t
