-- | Load a small project from disk, parse every module, and dump what
-- resolution gives: each module's export set, then one line per free
-- token: @line:col-line:col "text" namespace role -> result@. For golden
-- tests, and for dev/ResolveCheck.hs (the GHC oracle).
module ResolveDump
  ( -- * Fixtures
    Fixture (..),
    cases,
    fixtureDir,
    loadFixture,
    resolveInput,

    -- * Dumps
    dumpFixture,
    dumpInput,
    renderResolved,
    renderReason,
    renderGuessKind,
    renderWhy,
    renderOpenPart,
    renderSet,

    -- * GHC oracle files
    oracleCases,
    sourceHash,
    OracleFile (..),
    readOracleFile,
    renderOracleFile,
  )
where

import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word64)
import HaskellReviewer.Core.DocLinks (DocRef (..))
import HaskellReviewer.Core.Entities
  ( Avail (..),
    Entity (..),
    EntityKind (..),
    ExportSet (..),
    Names (..),
    OpenPart (..),
    Origin (..),
    openParts,
  )
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.ModuleGraph
  ( MappingGuess (..),
    ModuleGraph,
    buildModuleGraph,
  )
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    FoundModule (..),
    HaskellFile (..),
    LooseFile (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleSource (..),
    OtherFile (..),
    PackageModules (..),
    buildModuleIndex,
    looseModule,
    renderModuleId,
  )
import HaskellReviewer.Core.Names (ExternalModule (..), renderDeclId)
import HaskellReviewer.Core.Occurrences
  ( Namespace (..),
    Occurrence (..),
    Role (..),
    TokenIx (..),
    occurrenceAt,
  )
import HaskellReviewer.Core.Parser (moduleParseSettings, parseSource)
import HaskellReviewer.Core.Project (ModuleName (..), PackageName (..), Platform (..))
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.Resolve
  ( ExportSets (..),
    ExternalGuess (..),
    GuessKind (..),
    ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    Reason (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    UnresolvedWhy (..),
    exportSets,
    isPartial,
    resolveModule,
  )
import HaskellReviewer.Core.RichText qualified as Rich (RefNamespace (..))
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (renderSpan)
import HaskellReviewer.Core.Tokens (tokenSpanAt)
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Session.Project (readProject)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, listDirectory)

-- | The fixture cases: each is a dir of fixtures/resolve and its golden
-- file, and maybe a GHC oracle file (see Note [Oracle files]).
cases :: [FilePath]
cases =
  [ -- Lists, hiding, qualified, a shared alias.
    "scope",
    -- Chains, module exports, external re-exports, a SOURCE cycle.
    "reexports",
    -- Bundling onto an imported type, a qualified module export of
    -- associated types and type data, T(..) across qualifiers, a SOURCE
    -- cycle through the module resolved.
    "reexports2",
    -- A home Prelude, NoImplicitPrelude, hiding, qualified.
    "prelude",
    -- Sub-libraries, a test suite, package imports, unlisted, autogen.
    "components",
    -- Instance methods, record fields, wildcards, dots, DataKinds, a
    -- quasi-quoter.
    "contexts",
    -- Top-level splices and CPP imports.
    "th-cpp",
    -- Bundled pattern synonyms through lists, hiding and export items.
    "bundles",
    -- Modules that don't parse; the order of guesses; names nothing has.
    "partial",
    -- No project: loose files.
    "loose"
  ]

-- | Tests run from the package dir, so fixtures/resolve is one level up.
fixtureDir :: FilePath
fixtureDir = "../fixtures/resolve/"

-- | A project read from disk, every module parsed.
data Fixture = Fixture
  { index :: ModuleIndex,
    graph :: ModuleGraph,
    -- | Every module with a source, in index order.
    modules :: [ModuleId],
    states :: Map ModuleId ModuleState
  }

-- | Read a project dir, parse its modules.
loadFixture :: FilePath -> IO Fixture
loadFixture dir = do
  canon <- canonicalRoot (Root dir)
  w <- walkFiles canon (fileListCap + 1)
  let fl = mkFileList FromDisk w.skippedNames w.entries
  project <- readProject canon fl Platform {os = "linux", arch = "x86_64"} Nothing
  let idx = buildModuleIndex project fl
      sources =
        [ (ComponentModule p.name c.name m.name, m.source, c.settings)
        | p <- idx.packages,
          c <- p.components,
          m <- c.modules
        ]
          <> [ (LooseModule f.path, found.source, found.settings)
             | OtherModule f <- idx.other,
               let found = looseModule f.path
             ]
  states <- traverse (load idx) sources
  pure
    Fixture
      { index = idx,
        graph = buildModuleGraph idx,
        modules = [i | (i, _, _) <- sources],
        states = Map.fromList states
      }
  where
    load idx (i, source, settings) = case source of
      HaskellSource f -> do
        bytes <- BS.readFile (dir <> "/" <> Text.unpack (relPathText f.path))
        let text = Text.Encoding.decodeUtf8Lenient bytes
        pure $ (i,) $ case parseSource (moduleParseSettings idx.ghc settings f) text of
          Parsed m -> ModuleParsed ParsedInput {text = text, parsed = m}
          TextOnly _ -> ModuleNotParsed
      _ -> pure (i, ModuleNotParsed)

-- | The resolution input: every module done, nothing in the memo.
resolveInput :: Fixture -> ResolveInput
resolveInput fx =
  ResolveInput
    { graph = fx.graph,
      state = \i -> Map.findWithDefault ModuleNotParsed i fx.states,
      memo = Map.empty
    }

-- | Every parsed module: its export set, then its free tokens.
dumpFixture :: Fixture -> Text
dumpFixture fx = dumpInput (resolveInput fx) fx.modules

-- | These modules: export sets, then free tokens.
dumpInput :: ResolveInput -> [ModuleId] -> Text
dumpInput input ms = Text.unlines (concatMap one ms)
  where
    sets = (exportSets input ms).sets
    one i = case input.state i of
      ModuleParsed p ->
        ("== " <> renderModuleId i)
          : "exports:"
          : maybe ["  ?"] (map ("  " <>) . renderSet) (Map.lookup i sets)
            <> maybe [] (tokens p) (resolveModule input i)
      ModuleNotParsed -> ["== " <> renderModuleId i <> " (not parsed)"]
      ModuleNotReady -> ["== " <> renderModuleId i <> " (not ready)"]
    tokens p r =
      let src = sourceLines p.text
          m = p.parsed
          line k res pun =
            let s = tokenSpanAt m.tokens k
                o = occurrenceAt m.occurrences (TokenIx k)
             in "  "
                  <> renderSpan s
                  <> " "
                  <> quote (cut (sliceSpan src s))
                  <> maybe
                    ""
                    (\x -> " " <> nsText x.namespace <> " " <> roleText x.role)
                    o
                  <> (if pun then " [pun field]" else "")
                  <> maybe "" (\x -> if x.inSplice then " [splice]" else "") o
                  <> " -> "
                  <> renderResolved res
       in [line k res False | (k, res) <- IntMap.toList r.names]
            <> [line k res True | (k, res) <- IntMap.toList r.punFields]
            <> [ "  doc " <> docText d <> " -> " <> renderResolved res
               | (d, res) <- Map.toList r.docNames
               ]
            <> [ "partial: " <> Text.intercalate ", " (map renderModuleId r.notReady)
               | isPartial r
               ]
    docText (d :: DocRef) =
      nsPrefix d.namespace
        <> "'"
        <> maybe "" (\(ModuleName q) -> q <> ".") d.qualifier
        <> d.occ
        <> "'"
    nsPrefix = \case
      Rich.ValueNs -> "v"
      Rich.TypeNs -> "t"
      Rich.AnyNs -> ""
    cut t = if Text.length t > 40 then Text.take 40 t <> "..." else t
    quote t = "\"" <> t <> "\""

-- | One result.
renderResolved :: Resolved -> Text
renderResolved = \case
  Exact t -> "exact " <> renderTarget t
  Heuristic rs t ->
    "heuristic["
      <> Text.intercalate ", " (map renderReason (NonEmpty.toList rs))
      <> "] "
      <> renderTarget t
  External g -> "external " <> external g.module_ <> " (" <> renderGuessKind g.how <> ")"
  Unresolved w -> "unresolved (" <> renderWhy w <> ")"

renderTarget :: Target -> Text
renderTarget = \case
  DeclTarget m d -> renderModuleId m <> " " <> renderDeclId d
  ModuleTarget m -> renderModuleId m

renderGuessKind :: GuessKind -> Text
renderGuessKind = \case
  Listed -> "listed"
  OnlyOpenImport -> "only open import"
  OneOf k -> "one of " <> tshow k

renderWhy :: UnresolvedWhy -> Text
renderWhy = \case
  NoCandidate -> "no candidate"
  NotAGlobal -> "not a global"
  ExcludedByCpp -> "import excluded by CPP"

renderReason :: Reason -> Text
renderReason = \case
  Ambiguous k -> "ambiguous " <> tshow k
  NotReady -> "not ready"
  NotParsed -> "not parsed"
  MadeBySplice -> "made by splice"
  MadeBySpliceHere -> "made by splice here"
  ViaSplice -> "via splice"
  Wildcard -> "wildcard"
  RecordDot -> "record dot"
  DataKinds -> "datakinds"
  CppImport -> "cpp import"
  ImportCycle -> "import cycle"
  ModuleGuess AmbiguousModule -> "ambiguous module"
  ModuleGuess UnlistedModule -> "unlisted module"
  ModuleGuess LooseImport -> "loose import"
  ListedNotFound -> "listed, not found"

renderOpenPart :: OpenPart -> Text
renderOpenPart = \case
  ReexportsExternal x -> "external " <> external x
  UnparsedModule m -> "not parsed " <> renderModuleId m
  PendingModule m -> "not ready " <> renderModuleId m
  SpliceModule m -> "splices " <> renderModuleId m
  CycleCut m -> "cycle " <> renderModuleId m

-- | An export set, one entity per line: types with their children,
-- values, then open parts.
renderSet :: ExportSet -> [Text]
renderSet s =
  [ "type:" <> a.entity.occ <> origin a.entity <> kids (Set.toList a.children)
  | as <- Map.elems s.names.types,
    a <- as
  ]
    <> [kind e <> ":" <> e.occ <> origin e | es <- Map.elems s.names.values, e <- es]
    <> [ "lone:"
           <> p
           <> " "
           <> Text.unwords (map (.occ) (Set.toList cs))
       | (p, cs) <- Map.toList s.names.lone
       ]
    <> ["open: " <> renderOpenPart o | o <- openParts s]
  where
    kids [] = ""
    kids cs = " (" <> Text.intercalate ", " (map (.occ) cs) <> ")"
    origin e = case e.origin of
      HomeDecl m _ -> " @" <> renderModuleId m
      ExternalName x -> " @external " <> external x
      SpliceMade m -> " @splice " <> renderModuleId m
    kind e = case e.kind of
      ValueKind -> "val"
      MethodKind -> "method"
      FieldKind -> "field"
      BareFieldKind -> "bare-field"
      ConKind -> "con"
      PatSynKind -> "pat"
      TypeKind -> "type"
      ClassKind -> "class"
      TypeConKind -> "typecon"

external :: ExternalModule -> Text
external x =
  maybe "" (\(PackageName p) -> "\"" <> p <> "\" ") x.package
    <> let ModuleName n = x.name in n

nsText :: Namespace -> Text
nsText = \case
  VarNs -> "var"
  ConNs -> "con"
  TypeNs -> "type"
  TyVarNs -> "tyvar"
  FieldNs -> "field"
  IPNs -> "ip"
  ModuleNs -> "module"
  OtherNs -> "other"

roleText :: Role -> Text
roleText = \case
  Use -> "use"
  Def -> "def"
  Sig -> "sig"
  Import -> "import"
  Export -> "export"
  InstanceDef -> "instance-def"

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- * GHC oracle files

{- Note [Oracle files]

@make resolve-check@ (dev/ResolveCheck.hs) compiles each case below
with GHC (-fno-code -ddump-minimal-imports, boot packages only) and
writes @fixtures/resolve/<case>.imports@: a hash of the case's files,
then each module's minimal imports. Tests read these files, so they
never run GHC; a stale hash fails the test.
-}

-- | The fixture cases GHC compiles.
oracleCases :: [FilePath]
oracleCases =
  ["scope", "reexports", "reexports2", "prelude", "contexts", "th-cpp", "bundles"]

-- | A case's oracle file.
data OracleFile = OracleFile
  { hash :: Text,
    -- | Module id (long form) and its imports, as GHC prints them.
    modules :: [(Text, Text)]
  }
  deriving (Eq, Show)

-- | FNV-1a, 64 bits, over every file of the dir (path and bytes),
-- sorted by path.
sourceHash :: FilePath -> IO Text
sourceHash dir = do
  files <- sort <$> walk ""
  chunks <-
    mapM
      ( \f ->
          (\b -> [encode (Text.pack f), b])
            <$> BS.readFile (dir <> "/" <> f)
      )
      files
  pure (Text.pack (showHex (BS.foldl' step offset (BS.concat (concat chunks))) ""))
  where
    encode = Text.Encoding.encodeUtf8
    offset = 14695981039346656037 :: Word64
    step h b = (h `xor` fromIntegral b) * 1099511628211
    walk rel = do
      let here = if null rel then dir else dir <> "/" <> rel
      names <- listDirectory here
      concat
        <$> mapM
          ( \n -> do
              let r = if null rel then n else rel <> "/" <> n
              isDir <- doesDirectoryExist (dir <> "/" <> r)
              if isDir then walk r else pure [r]
          )
          names

-- | @-- source-hash: h@, then @-- module: id@ and its lines, per module.
renderOracleFile :: OracleFile -> Text
renderOracleFile f =
  Text.unlines $
    ("-- source-hash: " <> f.hash)
      : concat [("-- module: " <> m) : Text.lines is | (m, is) <- f.modules]

readOracleFile :: Text -> Maybe OracleFile
readOracleFile t = case Text.lines t of
  h : rest
    | Just hash <- Text.stripPrefix "-- source-hash: " h ->
        Just OracleFile {hash = hash, modules = go rest}
  _ -> Nothing
  where
    go = \case
      l : rest
        | Just m <- Text.stripPrefix "-- module: " l ->
            let (body, more) = break ("-- module: " `Text.isPrefixOf`) rest
             in (m, Text.unlines body) : go more
      _ : rest -> go rest
      [] -> []
