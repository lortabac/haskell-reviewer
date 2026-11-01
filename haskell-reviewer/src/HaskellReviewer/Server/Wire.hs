-- | Bodies of the API, written by hand (D-50): JSON, and CBOR for the
-- file view (D-51, Note [CBOR bodies] in "HaskellReviewer.Server.Cbor").
-- The module page is in "HaskellReviewer.Server.Wire.Page".
--
-- Field order is fixed (aeson's 'pairs', 'Cbor.obj'), so fixtures are
-- stable. See Note [API fixtures].
module HaskellReviewer.Server.Wire
  ( -- * Bodies
    encodeFileList,
    encodeFileView,
    encodeModuleTree,
    encodeModuleNotFound,
    encodeIndexing,
    encodePrefs,
    encodeSearch,
    helloPayload,

    -- * Shared
    noSourceReason,
  )
where

import Data.Aeson (Series, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.Config (sidebarName, themeName)
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
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
import HaskellReviewer.Core.RelPath (relDirText, relPathText)
import HaskellReviewer.Core.Search
  ( Hit (..),
    Indexing (..),
    ModuleCandidate (..),
    SearchResult (..),
  )
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot (Generation (..))

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
-- > {"generation":1,"kind":"text","text":"..."}
-- > {"generation":1,"kind":"binary","size":123}
-- > {"generation":1,"kind":"too-large","size":123,"limit":1048576}
-- > {"generation":1,"kind":"symlink","target":"../x","resolved":"x"}
-- > {"generation":1,"kind":"submodule","commit":"0123..."}
--
-- @resolved@ may be null, and @commit@ too (a nested repo).
encodeFileView :: Generation -> FileContent -> LBS.ByteString
encodeFileView (Generation g) content =
  Cbor.cborBody . Cbor.obj $ ("generation", Cbor.word64 g) : fields
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
encodeModuleTree :: Generation -> ModuleIndex -> LBS.ByteString
encodeModuleTree (Generation g) idx =
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
      BootFile p -> Aeson.pairs (kindIs "boot" <> ("path" .= relPathText p))

-- | The 404 body of a module that isn't there (JSON):
--
-- > {"generation":1,"kind":"not-found","what":"module"}
encodeModuleNotFound :: Generation -> LBS.ByteString
encodeModuleNotFound (Generation g) =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g) <> kindIs "not-found" <> ("what" .= ("module" :: Text))

-- | The data of the @hello@ event: @{}@.
helloPayload :: ByteString
helloPayload = LBS.toStrict (Encoding.encodingToLazyByteString (Aeson.pairs mempty))

-- | GET api\/indexing\/\<side>, and the data of the @indexing@ event:
--
-- > {"generation":1,"total":358,"ready":120,"failed":2}
--
-- Done when @ready + failed == total@. See Note [Indexing] in
-- "HaskellReviewer.Session.Index".
encodeIndexing :: Generation -> Progress -> ByteString
encodeIndexing (Generation g) p =
  LBS.toStrict . Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("total" .= p.total)
      <> ("ready" .= p.ready)
      <> ("failed" .= p.failed)

-- | GET and POST api\/prefs: every preference, defaults filled in.
--
-- > {"theme":"system","exportedOnly":false,"sidebar":"auto"}
--
-- No generation: preferences don't belong to a snapshot (D-197).
encodePrefs :: Effective -> LBS.ByteString
encodePrefs e =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("theme" .= themeName e.theme)
      <> ("exportedOnly" .= e.exportedOnly)
      <> ("sidebar" .= sidebarName e.sidebar)

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
