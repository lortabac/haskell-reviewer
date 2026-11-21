-- | Bodies of the API, written by hand (D-50): JSON, and CBOR for the
-- file view (D-51, Note [CBOR bodies] in "HaskellReviewer.Server.Cbor").
-- The module page is in "HaskellReviewer.Server.Wire.Page".
--
-- Field order is fixed (aeson's 'pairs', 'Cbor.obj'), so fixtures are
-- stable. See Note [API fixtures].
module HaskellReviewer.Server.Wire
  ( -- * Types
    DiffFiles (..),
    DiffEnd (..),
    RefsMissing (..),
    SessionInfo (..),

    -- * Bodies
    encodeFileList,
    encodeFileView,
    encodeModuleTree,
    encodeDiffFiles,
    encodeSession,
    diffEnds,
    encodeModuleNotFound,
    encodeFileNotFound,
    encodeIndexing,
    encodePrefs,
    encodeSearch,
    encodeRefs,
    encodeRefsNotFound,
    encodeTextSearch,
    helloPayload,

    -- * Shared
    noSourceReason,
    filteredValue,
  )
where

import Data.Aeson (Series, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import HaskellReviewer.Core.CommitFiles (OutsideChanges (..))
import HaskellReviewer.Core.Config (sidebarName, themeName)
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
  )
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
  )
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.Markers
  ( Marker (..),
    ModuleChange (..),
    SettingsChange (..),
    noMarker,
  )
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    GeneratedFile (..),
    HaskellFile (..),
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
    moduleRef,
    renderModuleId,
    renderPreprocessor,
  )
import HaskellReviewer.Core.NameTable (NameEntry (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..), renderDeclId)
import HaskellReviewer.Core.Prefs (Effective (..))
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    ModuleName (..),
    PackageName (..),
    renderCabalProblem,
    renderComponentName,
    renderProjectWarning,
  )
import HaskellReviewer.Core.References
  ( RefKind (..),
    RefLine (..),
    RefMatch (..),
    Reference (..),
    RefsAnswer (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, relDirText, relPathText)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ChangeNote (..),
    ExecChange (..),
    Limit (..),
    ModifiedFile (..),
    Pairing (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Revspec
  ( BaseFrom (..),
    CommitsInfo (..),
    DiffInfo (..),
    Range (..),
    Revspec (..),
    WorkTreeInfo (..),
    renderRevspec,
  )
import HaskellReviewer.Core.Search
  ( Hit (..),
    Indexing (..),
    ModuleCandidate (..),
    SearchResult (..),
  )
import HaskellReviewer.Core.Sides (Pair (..), Side, sideName)
import HaskellReviewer.Core.Similarity (scorePercent)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.TextSearch
  ( LineHit (..),
    Match (..),
    TextAnswer (..),
    TextHit (..),
  )
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.Wire.Targets (reasonName)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot (Generation (..))
import HaskellReviewer.Session.TextStore (Skipped (..))

{- Note [API fixtures]

D-50: the wire types are written by hand, here and in web/src/api/.
To keep them in sync, the Haskell tests (ApiGoldenTest) call the real
app with fixed data and write each body to fixtures/api/: a JSON body
to <name>.json, a CBOR body to <name>.cbor plus its JSON dump
(cborToJson) to <name>.cbor.json, so diffs stay readable. The
TypeScript tests feed every fixture to the decoder that production
uses (a CBOR one through cbor-x, checked against the dump), and check
that each decoder case has a fixture.

A change here changes a fixture: the golden test fails until the
fixture is updated (--accept), and then the TS test checks the
decoders still take it. CI sets TASTY_NO_CREATE, so a missing fixture
fails too.
-}

-- | GET api\/files\/\<side>:
--
-- > {"generation":1,"source":"git","entries":[{"path":"a.hs","kind":"file"}],
-- >  "skippedNames":0,"truncated":false}
--
-- A submodule entry also has @"commit"@. A nested repo that is not a
-- submodule is a submodule with commit null.
encodeFileList :: Generation -> FileList -> LBS.ByteString
encodeFileList (Generation g) fl =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("source" .= source)
      <> Encoding.pair "entries" (Encoding.list entry (Map.toList fl.entries))
      <> ("skippedNames" .= fl.skippedNames)
      <> ("truncated" .= fl.truncated)
  where
    source :: Text
    source = case fl.source of
      FromGit -> "git"
      FromDisk -> "disk"
    entry (p, kind) = Aeson.pairs (("path" .= relPathText p) <> kindFields kind)
    kindFields = \case
      EntryFile -> kindIs "file"
      EntrySymlink -> kindIs "symlink"
      EntrySubmodule c -> kindIs "submodule" <> ("commit" .= commitText c)
      EntryNestedRepo -> kindIs "submodule" <> ("commit" .= Aeson.Null)

-- | GET api\/file\/\<side>\/\<path>, as CBOR. In JSON terms, one of
--
-- > {"generation":1,"kind":"text","text":"...","filtered":false}
-- > {"generation":1,"kind":"binary","size":123,"filtered":false}
-- > {"generation":1,"kind":"too-large","size":123,"limit":1048576,"filtered":false}
-- > {"generation":1,"kind":"symlink","target":"../x","resolved":"x","filtered":false}
-- > {"generation":1,"kind":"submodule","commit":"0123...","filtered":false}
--
-- @resolved@ may be null, and @commit@ too (a nested repo). @filtered@
-- is true for a file read from disk that git would filter (see
-- 'FileNotes').
encodeFileView :: Generation -> FileNotes -> FileContent -> LBS.ByteString
encodeFileView (Generation g) notes content =
  Cbor.cborBody . Cbor.obj $
    ("generation", Cbor.word64 g) : fields <> [("filtered", filteredValue notes)]
  where
    fields = case content of
      TextContent t -> [kind "text", ("text", Cbor.text t)]
      Binary size -> [kind "binary", ("size", bytes size)]
      TooLarge info ->
        [kind "too-large", ("size", bytes info.size), ("limit", bytes info.limit)]
      Link info ->
        [ kind "symlink",
          ("target", Cbor.text info.target),
          ("resolved", Cbor.nullable (Cbor.text . relPathText) info.resolved)
        ]
      Submodule c -> [kind "submodule", ("commit", Cbor.text (commitText c))]
      NestedRepo -> [kind "submodule", ("commit", Cbor.null_)]
    kind k = ("kind", Cbor.text k)
    bytes (FileSize n) = Cbor.int n

-- | The @filtered@ field of the file and source views.
filteredValue :: FileNotes -> Cbor.Encoding
filteredValue notes = Cbor.bool $ case notes.filtered of
  Filtered -> True
  Unfiltered -> False

-- | GET api\/modules\/\<side>: the module tree.
--
-- > {"generation":1,
-- >  "notices":[{"kind":"stack"},{"kind":"implicit"},{"kind":"hpack","dirs":["."]}],
-- >  "packages":[{"name":"pkg","version":"0.1","cabalFile":"pkg/pkg.cabal",
-- >    "components":[{"name":"lib","buildable":true,"modules":[
-- >      {"name":"Foo","id":"pkg:lib:Foo","ref":"Foo","role":"exposed",
-- >       "source":{"kind":"module","path":"pkg/src/Foo.hs"}}]}]}],
-- >  "broken":[{"cabalFile":"bad/bad.cabal","problem":"can't parse: ..."}],
-- >  "other":[{"kind":"module","path":"x/Tool.hs","name":"Tool",
-- >            "id":"x/Tool.hs","ref":"Tool"},
-- >           {"kind":"boot","path":"pkg/src/A.hs-boot"}],
-- >  "warnings":["cabal.project:3: no package matches foo/"]}
--
-- Roles: exposed, other, signature, test-module, main. Sources: module
-- (path), generated and preprocessed (path, tool), no-source (reason:
-- autogen, missing, outside, symlink). An other module's name may be
-- null. See Note [Module discovery] in "HaskellReviewer.Core.Modules".
--
-- Every module (not a boot file) ends with its diff marker (D-325):
-- @"change":null|"added"|"removed"|"modified"|"renamed"@ and
-- @"settingsChanged":false@. A module not in the map gets null and
-- false: all of them in browse mode.
encodeModuleTree :: Generation -> Map ModuleId Marker -> ModuleIndex -> LBS.ByteString
encodeModuleTree (Generation g) markers idx =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> Encoding.pair "notices" (Encoding.list notice idx.notices)
      <> Encoding.pair "packages" (Encoding.list package idx.packages)
      <> Encoding.pair "broken" (Encoding.list broken idx.broken)
      <> Encoding.pair "other" (Encoding.list other idx.other)
      <> ("warnings" .= map renderProjectWarning idx.warnings)
  where
    notice = \case
      StackNotice -> Aeson.pairs (kindIs "stack")
      ImplicitNotice -> Aeson.pairs (kindIs "implicit")
      HpackNotice ds -> Aeson.pairs (kindIs "hpack" <> ("dirs" .= fmap relDirText ds))
    package p =
      let PackageName n = p.name
       in Aeson.pairs $
            ("name" .= n)
              <> ("version" .= p.version)
              <> ("cabalFile" .= relPathText p.cabalFile)
              <> Encoding.pair
                "components"
                (Encoding.list (component p.name) p.components)
    component pkg c =
      Aeson.pairs $
        ("name" .= renderComponentName c.name)
          <> ("buildable" .= c.buildable)
          <> Encoding.pair "modules" (Encoding.list (entry pkg c.name) c.modules)
    entry pkg comp m =
      let ModuleName n = m.name
          i = ComponentModule pkg comp m.name
       in Aeson.pairs $
            ("name" .= n)
              <> ("id" .= renderModuleId i)
              <> ("ref" .= moduleRef idx i)
              <> ("role" .= role m.role)
              <> Encoding.pair "source" (source m.source)
              <> marker i
    role :: ModuleRole -> Text
    role = \case
      RoleExposed -> "exposed"
      RoleOther -> "other"
      RoleSignature -> "signature"
      RoleTestModule -> "test-module"
      RoleMain -> "main"
    source = \case
      HaskellSource f -> Aeson.pairs (kindIs "module" <> ("path" .= relPathText f.path))
      GeneratedSource f ->
        Aeson.pairs $
          kindIs "generated"
            <> ("path" .= relPathText f.path)
            <> ("tool" .= renderPreprocessor f.tool)
      PreprocessedSource f ->
        Aeson.pairs $
          kindIs "preprocessed" <> ("path" .= relPathText f.path) <> ("tool" .= f.tool)
      NoSource r -> Aeson.pairs (kindIs "no-source" <> ("reason" .= noSourceReason r))
    broken b =
      Aeson.pairs $
        ("cabalFile" .= relPathText b.cabalFile)
          <> ("problem" .= renderCabalProblem b.problem)
    other = \case
      OtherModule f ->
        let i = LooseModule f.path
         in Aeson.pairs $
              kindIs "module"
                <> ("path" .= relPathText f.path)
                <> ("name" .= fmap (\(ModuleName n) -> n) f.name)
                <> ("id" .= renderModuleId i)
                <> ("ref" .= moduleRef idx i)
                <> marker i
      BootFile p -> Aeson.pairs (kindIs "boot" <> ("path" .= relPathText p))
    marker i =
      let mk = Map.findWithDefault noMarker i markers
       in ("change" .= fmap moduleChange mk.change)
            <> ("settingsChanged" .= (mk.settingsChange == SettingsChanged))
    moduleChange :: ModuleChange -> Text
    moduleChange = \case
      ModuleAdded -> "added"
      ModuleRemoved -> "removed"
      ModuleModified -> "modified"
      ModuleRenamed -> "renamed"

-- | What GET api\/diff-files says, besides the generation.
data DiffFiles = DiffFiles
  { base :: DiffEnd,
    head :: DiffEnd,
    pairing :: Pairing,
    -- | Modified paths whose head file is filtered (D-319): their note,
    -- when they have no other.
    filtered :: Set RelPath,
    -- | 'Nothing' when not known: a side read from disk (only tests do),
    -- or the working tree's outside step failed.
    outside :: Maybe OutsideChanges,
    -- | A side's list was cut at the file cap, so the pairing was cut
    -- there too: files past it are not listed.
    truncated :: Bool
  }

-- | One side of the diff: the revision as typed (or "working tree"),
-- and its commit, if it has one.
data DiffEnd = DiffEnd
  { label :: Text,
    commit :: Maybe ObjectId
  }

-- | The session's mode, for GET api\/session.
data SessionInfo
  = BrowseSession
  | DiffSession DiffInfo

-- | GET api\/session (D-332). No generation: it never changes.
--
-- > {"mode":"browse"}
-- > {"mode":"diff","spec":"main...feature",
-- >  "base":{"label":"main","commit":"<hex>"},
-- >  "head":{"label":"feature","commit":"<hex>"|null},
-- >  "mergeBases":null|2}
--
-- Labels as in api\/diff-files ('diffEnds'). spec is 'renderRevspec':
-- as typed, an empty end shown as HEAD. mergeBases only for @...@: how
-- many there are (base is the first).
encodeSession :: SessionInfo -> LBS.ByteString
encodeSession = \case
  BrowseSession ->
    Encoding.encodingToLazyByteString (Aeson.pairs ("mode" .= ("browse" :: Text)))
  DiffSession info ->
    let ends = diffEnds info
        (spec, mergeBases) = case info of
          CommitsDiff ci ->
            ( renderRevspec (RangeSpec ci.range),
              case ci.baseFrom of
                BaseTyped -> Nothing
                MergeBase n -> Just n
            )
          WorkTreeDiff wi -> (renderRevspec (AgainstWorkTree wi.rev), Nothing)
     in Encoding.encodingToLazyByteString . Aeson.pairs $
          ("mode" .= ("diff" :: Text))
            <> ("spec" .= spec)
            <> Encoding.pair "base" (diffEnd ends.base)
            <> Encoding.pair "head" (diffEnd ends.head)
            <> ("mergeBases" .= (mergeBases :: Maybe Int))

-- | Each side's label and commit (D-325): the revisions as typed. For
-- @...@ base's commit is the merge base; for @REV@ head is the
-- "working tree", with no commit.
diffEnds :: DiffInfo -> Pair DiffEnd
diffEnds = \case
  CommitsDiff ci ->
    Pair
      { base = DiffEnd {label = ci.range.base, commit = Just ci.sides.base},
        head = DiffEnd {label = ci.range.head, commit = Just ci.sides.head}
      }
  WorkTreeDiff wi ->
    Pair
      { base = DiffEnd {label = wi.rev, commit = Just wi.base},
        head = DiffEnd {label = "working tree", commit = Nothing}
      }

-- | One end: @{"label":"main","commit":"<hex>"|null}@.
diffEnd :: DiffEnd -> Aeson.Encoding
diffEnd e =
  Aeson.pairs $
    ("label" .= e.label) <> ("commit" .= fmap (\(ObjectId o) -> o) e.commit)

-- | GET api\/diff-files (D-325):
--
-- > {"generation":1,
-- >  "base":{"label":"main","commit":"<hex>"},
-- >  "head":{"label":"feature","commit":"<hex>"|null},
-- >  "files":[{"path":"b.hs","oldPath":"a.hs"|null,
-- >            "status":"added"|"removed"|"modified"|"renamed",
-- >            "similarity":60|null,
-- >            "note":null|"type-changed"|"mode-changed"|"filtered"}],
-- >  "outside":{"total":3,"paths":["docs/x.md"],"truncated":false}|null,
-- >  "renameLimitHit":false,"truncated":false}
--
-- Files by path: the head path, the base path for removed. Unchanged
-- files are left out. oldPath and similarity only for renamed.
encodeDiffFiles :: Generation -> DiffFiles -> LBS.ByteString
encodeDiffFiles (Generation g) d =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> Encoding.pair "base" (diffEnd d.base)
      <> Encoding.pair "head" (diffEnd d.head)
      <> Encoding.pair "files" (Encoding.list file d.pairing.changes)
      <> Encoding.pair "outside" (maybe Encoding.null_ outside d.outside)
      <> ("renameLimitHit" .= (d.pairing.limit /= WithinLimits))
      <> ("truncated" .= d.truncated)
  where
    file = \case
      Added p -> notRenamed p "added" Nothing
      Removed p -> notRenamed p "removed" Nothing
      Modified m -> notRenamed m.path "modified" (modifiedNote m)
      Renamed r ->
        row
          ( ("path" .= relPathText r.to)
              <> ("oldPath" .= relPathText r.from)
              <> ("status" .= ("renamed" :: Text))
              <> ("similarity" .= scorePercent r.score)
          )
          (renamedNote r.exec)
    notRenamed p status =
      row
        ( ("path" .= relPathText p)
            <> ("oldPath" .= Aeson.Null)
            <> ("status" .= (status :: Text))
            <> ("similarity" .= Aeson.Null)
        )
    row :: Series -> Maybe Text -> Aeson.Encoding
    row fields note = Aeson.pairs (fields <> ("note" .= note))
    modifiedNote m = case m.note of
      Just n -> Just (changeNote n)
      Nothing
        | Set.member m.path d.filtered -> Just "filtered"
        | otherwise -> Nothing
    renamedNote = \case
      ExecChanged -> Just "mode-changed"
      SameExec -> Nothing
    changeNote :: ChangeNote -> Text
    changeNote = \case
      TypeChanged -> "type-changed"
      ModeChanged -> "mode-changed"
    outside o =
      Aeson.pairs $
        ("total" .= o.total) <> ("paths" .= o.paths) <> ("truncated" .= o.truncated)

-- | The 404 body of a module that isn't there (JSON):
--
-- > {"generation":1,"kind":"not-found","what":"module"}
encodeModuleNotFound :: Generation -> LBS.ByteString
encodeModuleNotFound (Generation g) =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g) <> kindIs "not-found" <> ("what" .= ("module" :: Text))

-- | The 404 of the source route: @{"generation":1,"kind":"not-found","what":"file"}@.
-- A path not in the list, or a file gone.
encodeFileNotFound :: Generation -> LBS.ByteString
encodeFileNotFound (Generation g) =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g) <> kindIs "not-found" <> ("what" .= ("file" :: Text))

-- | The data of the @hello@ event: @{}@.
helloPayload :: ByteString
helloPayload = LBS.toStrict (Encoding.encodingToLazyByteString (Aeson.pairs mempty))

-- | GET api\/indexing\/\<side>, and the data of the @indexing@ event:
--
-- > {"generation":1,"side":"head","total":358,"ready":120,"failed":2}
--
-- Done when @ready + failed == total@. See Note [Indexing] in
-- "HaskellReviewer.Session.Index".
encodeIndexing :: Generation -> Side -> Progress -> ByteString
encodeIndexing (Generation g) side p =
  LBS.toStrict . Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("side" .= sideName side)
      <> ("total" .= p.total)
      <> ("ready" .= p.ready)
      <> ("failed" .= p.failed)

-- | GET and POST api\/prefs: every preference, defaults filled in.
--
-- > {"theme":"system","exportedOnly":false,"sidebar":"auto",
-- >  "singleKeyShortcuts":true}
--
-- No generation: preferences don't belong to a snapshot (D-197).
encodePrefs :: Effective -> LBS.ByteString
encodePrefs e =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("theme" .= themeName e.theme)
      <> ("exportedOnly" .= e.exportedOnly)
      <> ("sidebar" .= sidebarName e.sidebar)
      <> ("singleKeyShortcuts" .= e.singleKeyShortcuts)

-- | GET api\/search\/\<side>?q=…:
--
-- > {"generation":1,"query":"Map.ins","partial":false,"matches":2,
-- >  "results":[{"kind":"decl","ns":"val","name":"insert","parent":null,
-- >    "anchor":"val:insert","module":"Data.Map","ref":"Data.Map","exported":true},
-- >   {"kind":"module","name":"Data.Map","ref":"Data.Map"}]}
--
-- @query@ is q as sent. @matches@ counts every hit, before the cap
-- ('searchLimit' in "HaskellReviewer.Core.Search").
-- @ns@: type, class, con, field, val, pat. @parent@ is the type of a
-- field, else null. @anchor@ is the declaration id ('renderDeclId').
-- @partial@: indexing isn't done, names may be missing. See Note
-- [Search] in "HaskellReviewer.Core.Search".
encodeSearch :: Generation -> Text -> Indexing -> SearchResult -> LBS.ByteString
encodeSearch (Generation g) query indexing r =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("query" .= query)
      <> ("partial" .= (indexing == Partial))
      <> ("matches" .= r.matches)
      <> Encoding.pair "results" (Encoding.list hit r.hits)
  where
    hit = \case
      ModuleHit m ->
        Aeson.pairs (kindIs "module" <> ("name" .= m.name) <> ("ref" .= m.ref))
      DeclHit m e ->
        Aeson.pairs $
          kindIs "decl"
            <> ("ns" .= ns e.decl)
            <> ("name" .= e.name)
            <> ("parent" .= parent e.decl)
            <> ("anchor" .= renderDeclId e.decl)
            <> ("module" .= m.name)
            <> ("ref" .= m.ref)
            <> ("exported" .= e.exported)
    ns :: DeclId -> Text
    ns = \case
      Named DType _ -> "type"
      Named DClass _ -> "class"
      Named DCon _ -> "con"
      Named DVal _ -> "val"
      Named DPat _ -> "pat"
      Field _ _ -> "field"
      -- Search never gives one: instances have no name.
      Inst _ -> "inst"
    parent = \case
      Field p _ -> Just p
      _ -> Nothing

-- | GET api\/refs\/\<side>?module=…&decl=…, with each module's name and
-- ref, and its file:
--
-- > {"generation":1,"module":"A","decl":"val:area","partial":false,"total":1,
-- >  "results":[{"module":"B","ref":"B","path":"src/B.hs","span":[8,23,8,27],
-- >    "kind":"use","heuristic":false,"reasons":[],
-- >    "line":"sorted shapes = sortOn area shapes","lineCol":1}]}
--
-- - @module@: the home module's ref. @decl@: the id as asked.
-- - @partial@: indexing isn't done, or a module read isn't ready.
-- - @total@: every reference, before the cap
--   ('HaskellReviewer.Core.References.refsLimit').
-- - Each result: its module's name (as search has it), @ref@, @path@;
--   @span@ of the token (end exclusive, columns in code points);
--   @kind@: use, import, export, binding; @heuristic@, and @reasons@
--   (the source view's codes, empty when exact); @line@, cut to
--   'HaskellReviewer.Core.References.lineCap' code points around the
--   token; @lineCol@, the column of its first code point.
--
-- See Note [Find references] in "HaskellReviewer.Core.References".
encodeRefs ::
  Generation ->
  Text ->
  Text ->
  (ModuleId -> ModuleCandidate) ->
  (ModuleId -> Maybe Text) ->
  RefsAnswer ->
  LBS.ByteString
encodeRefs (Generation g) home decl candidate pathOf a =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("module" .= home)
      <> ("decl" .= decl)
      <> ("partial" .= a.partial)
      <> ("total" .= a.total)
      <> Encoding.pair "results" (Encoding.list result a.results)
  where
    result r =
      let m = candidate r.match.module_
          Span (Line l1) (Col c1) (Line l2) (Col c2) = r.match.span
          Col lc = r.line.col
       in Aeson.pairs $
            ("module" .= m.name)
              <> ("ref" .= m.ref)
              <> ("path" .= pathOf r.match.module_)
              <> ("span" .= [l1, c1, l2, c2])
              <> ("kind" .= refKind r.match.kind)
              <> ("heuristic" .= not (null r.match.reasons))
              <> ("reasons" .= map reasonName r.match.reasons)
              <> ("line" .= r.line.text)
              <> ("lineCol" .= lc)
    refKind :: RefKind -> Text
    refKind = \case
      RefUse -> "use"
      RefImport -> "import"
      RefExport -> "export"
      RefBinding -> "binding"

-- | What a find references 404 didn't find.
data RefsMissing = MissingModule | MissingDecl
  deriving (Eq, Show)

-- | The 404 of the refs route:
-- @{"generation":1,"kind":"not-found","what":"module"}@, or @"decl"@.
encodeRefsNotFound :: Generation -> RefsMissing -> LBS.ByteString
encodeRefsNotFound (Generation g) what =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g) <> kindIs "not-found" <> ("what" .= whatText)
  where
    whatText :: Text
    whatText = case what of
      MissingModule -> "module"
      MissingDecl -> "decl"

-- | GET api\/text-search\/\<side>?q=…&limit=…:
--
-- > {"generation":1,"query":"main","partial":false,"stopped":false,"total":2,
-- >  "files":1,"skipped":{"binary":1,"tooLarge":0,"unreadable":0,"overBudget":0},
-- >  "results":[{"path":"app/Main.hs","line":4,"lineCol":1,
-- >    "text":"main :: IO ()","matches":[[1,5]]}]}
--
-- - @query@: q as sent. @partial@: always false for now (the store is
--   loaded before we answer).
-- - @stopped@: the deadline came, some files weren't searched.
-- - @total@: every matching line; @files@: files with a hit.
-- - @skipped@: files not searched, by why (Note [Text store] in
--   "HaskellReviewer.Session.TextStore"). All 0 when nothing was
--   searched (an empty q).
-- - Each result is one line: @text@ cut to 240 code points around the
--   first hit, @lineCol@ the column of its first code point;
--   @matches@: the hits as @[start, end]@ columns of the whole line
--   (code points, from 1, end exclusive), at most 20.
--
-- See Note [Text search] in "HaskellReviewer.Core.TextSearch".
encodeTextSearch :: Generation -> Text -> Skipped -> TextAnswer -> LBS.ByteString
encodeTextSearch (Generation g) query sk a =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("query" .= query)
      <> ("partial" .= False)
      <> ("stopped" .= a.stopped)
      <> ("total" .= a.total)
      <> ("files" .= a.files)
      <> Encoding.pair "skipped" skipped
      <> Encoding.pair "results" (Encoding.list result a.results)
  where
    skipped =
      Aeson.pairs $
        ("binary" .= sk.binary)
          <> ("tooLarge" .= sk.tooLarge)
          <> ("unreadable" .= sk.unreadable)
          <> ("overBudget" .= sk.overBudget)
    result r =
      Aeson.pairs $
        ("path" .= relPathText r.path)
          <> ("line" .= r.hit.line)
          <> ("lineCol" .= r.hit.lineCol)
          <> ("text" .= r.hit.text)
          <> ("matches" .= [[m.start, m.end] | m <- r.hit.matches])

-- * Internal

kindIs :: Text -> Series
kindIs k = "kind" .= k

commitText :: CommitId -> Text
commitText (CommitId c) = c

-- | Why a listed module has no file, on the wire.
noSourceReason :: NoSourceReason -> Text
noSourceReason = \case
  Autogen -> "autogen"
  Missing -> "missing"
  Outside _ -> "outside"
  SymlinkSource _ -> "symlink"
