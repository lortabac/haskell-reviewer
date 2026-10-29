-- | Bodies of the API, written by hand (D-50): JSON, and CBOR for the
-- file text (D-51, Note [CBOR bodies] in "HaskellReviewer.Server.Cbor").
--
-- Field order is fixed (aeson's 'pairs', 'Cbor.obj'), so fixtures are
-- stable. See Note [API fixtures].
module HaskellReviewer.Server.Wire
  ( -- * Types
    ModuleView (..),
    ModuleAnswer (..),
    NotParsed (..),

    -- * Bodies
    encodeFileList,
    encodeFileView,
    encodeModuleTree,
    encodeModuleView,
    encodeModuleNotFound,
    encodeIndexing,
    helloPayload,
  )
where

import Data.Aeson (Series, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
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
import HaskellReviewer.Core.ModuleModel
  ( DocComment (..),
    Export (..),
    Extensions (..),
    Import (..),
    ImportLevel (..),
    ImportList (..),
    ImportSource (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    ModuleExport (..),
    ModuleHeader (..),
    ModuleParse (..),
    NameKeyword (..),
    ParseError (..),
    ParsedModule (..),
    Qualified (..),
    Section (..),
    SubList (..),
    Subs (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    WarningKind (..),
    WarningText (..),
    renderParseWarning,
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
    Preprocessor,
    moduleRef,
    renderModuleId,
    renderPreprocessor,
  )
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    ModuleName (..),
    PackageName (..),
    renderCabalProblem,
    renderComponentName,
    renderProjectWarning,
  )
import HaskellReviewer.Core.RelPath (RelPath, relDirText, relPathText)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
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

-- | One module, for GET api\/module\/\<side>\/\<ref>.
data ModuleView = ModuleView
  { moduleId :: ModuleId,
    -- | The file, if there is one.
    path :: Maybe RelPath,
    answer :: ModuleAnswer
  }

-- | What we have for a module.
data ModuleAnswer
  = -- | We ran the parser on the file.
    FromParser ModuleParse
  | -- | Text only, without running the parser.
    NotParsed NotParsed

-- | Why we didn't run the parser.
data NotParsed
  = -- | A preprocessor input. Known from the index.
    GeneratedModule Preprocessor
  | -- | -F -pgmF or -pgmL in ghc-options. Known from the index.
    PreprocessedModule Text
  | -- | A listed module with no file.
    NoSourceModule NoSourceReason
  | -- | Binary, or not UTF-8.
    NotTextModule FileSize
  | -- | Over the text limit.
    TooLargeModule TooLargeInfo

-- | GET api\/module\/\<side>\/\<ref>:
--
-- > {"generation":1,"id":"pkg:lib:Foo","ref":"Foo","path":"pkg/src/Foo.hs",
-- >  "tier":{"kind":"syntactic","reason":"no-hie"},
-- >  "parse":{"kind":"parsed","header":{...},"exports":[...],"imports":[...],
-- >           "extensions":{...},"warnings":["unknown extension: Foo"]}}
--
-- or @"parse":{"kind":"text-only","reason":{"kind":...},"warnings":[...]}@.
-- Reasons: parse-error and pragma-error (message, code, span),
-- preprocessed (tool), parser-crashed (message), too-slow, generated
-- (tool), no-source (reason), not-text (size), too-large (size,
-- limit). @path@ is null for no-source.
--
-- A span is @[startLine, startCol, endLine, endCol]@, in code points,
-- end exclusive (Note [Span convention] in "HaskellReviewer.Core.Span").
-- Fields that may be missing are null, never left out.
encodeModuleView :: Generation -> ModuleIndex -> ModuleView -> LBS.ByteString
encodeModuleView (Generation g) idx v =
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g)
      <> ("id" .= renderModuleId v.moduleId)
      <> ("ref" .= moduleRef idx v.moduleId)
      <> ("path" .= fmap relPathText v.path)
      -- Always syntactic: no HIE files.
      <> Encoding.pair "tier" (Aeson.pairs (kindIs "syntactic" <> ("reason" .= noHie)))
      <> Encoding.pair "parse" (Aeson.pairs (answer v.answer))
  where
    noHie :: Text
    noHie = "no-hie"
    answer = \case
      FromParser (Parsed m) -> parsedFields m
      FromParser (TextOnly t) -> textOnly (parserReason t.reason) t.warnings
      NotParsed r -> textOnly (notParsedReason r) []
    textOnly reason ws =
      kindIs "text-only"
        <> Encoding.pair "reason" (Aeson.pairs reason)
        <> ("warnings" .= map renderParseWarning ws)
    parserReason = \case
      ParseFailed e -> kindIs "parse-error" <> parseError e
      PragmaFailed e -> kindIs "pragma-error" <> parseError e
      Preprocessed tool -> kindIs "preprocessed" <> ("tool" .= tool)
      ParserCrashed t -> kindIs "parser-crashed" <> ("message" .= t)
      TooSlow -> kindIs "too-slow"
    parseError e =
      ("message" .= e.message)
        <> ("code" .= e.code)
        <> Encoding.pair "span" (maybe Encoding.null_ spanJson e.span)
    notParsedReason = \case
      GeneratedModule tool -> kindIs "generated" <> ("tool" .= renderPreprocessor tool)
      PreprocessedModule tool -> kindIs "preprocessed" <> ("tool" .= tool)
      NoSourceModule r -> kindIs "no-source" <> ("reason" .= noSourceReason r)
      NotTextModule (FileSize n) -> kindIs "not-text" <> ("size" .= n)
      TooLargeModule info ->
        kindIs "too-large" <> ("size" .= bytes info.size) <> ("limit" .= bytes info.limit)
    bytes (FileSize n) = n

-- | The fields of a parsed module. See 'encodeModuleView'.
parsedFields :: ParsedModule -> Series
parsedFields m =
  kindIs "parsed"
    <> Encoding.pair "header" (header m.header)
    <> Encoding.pair "exports" (maybe Encoding.null_ (Encoding.list export) m.exports)
    <> Encoding.pair "imports" (Encoding.list importDecl m.imports)
    <> Encoding.pair "extensions" (extensions m.extensions)
    <> ("warnings" .= map renderParseWarning m.warnings)
  where
    header h =
      Aeson.pairs $
        Encoding.pair "name" (nullable (located moduleName) h.name)
          <> Encoding.pair "haddock" (nullable docComment h.haddock)
          <> Encoding.pair "warning" (nullable warningText h.warning)
    -- {"span":[...],"kind":"deprecated"|"warning","category":null,"messages":[...]}
    warningText w =
      Aeson.pairs $
        Encoding.pair "span" (spanJson w.span) <> case w.kind of
          DeprecatedPragma ->
            kindIs "deprecated" <> ("category" .= Aeson.Null) <> messages
          WarningPragma c -> kindIs "warning" <> ("category" .= c) <> messages
      where
        messages = "messages" .= w.messages
    export = \case
      ExportItem i -> item (kindIs "item") i
      ExportModule e ->
        Aeson.pairs $
          kindIs "module"
            <> Encoding.pair "name" (located moduleName e.name)
            <> Encoding.pair "warning" (nullable warningText e.warning)
      ExportSection s ->
        Aeson.pairs $
          kindIs "section"
            <> ("level" .= s.level)
            <> Encoding.pair "heading" (docComment s.heading)
      ExportDoc d -> Aeson.pairs (kindIs "doc" <> docFields d)
      ExportDocNamed n ->
        Aeson.pairs $
          kindIs "named-doc"
            <> Encoding.pair "span" (spanJson n.span)
            <> ("name" .= n.value)
    -- Export items have a kind; import items don't need one.
    item first i =
      Aeson.pairs $
        first
          <> Encoding.pair "span" (spanJson i.span)
          <> Encoding.pair "name" (itemName i.name)
          <> Encoding.pair "subs" (subs i.subs)
          <> Encoding.pair "doc" (nullable docComment i.doc)
          <> Encoding.pair "warning" (nullable warningText i.warning)
    itemName n =
      Aeson.pairs $
        Encoding.pair "span" (spanJson n.span)
          <> ("qualifier" .= fmap moduleNameText n.qualifier)
          <> ("occ" .= n.occ)
          <> ("keyword" .= keyword n.keyword)
    keyword :: NameKeyword -> Maybe Text
    keyword = \case
      NoKeyword -> Nothing
      TypeKeyword -> Just "type"
      PatternKeyword -> Just "pattern"
      DataKeyword -> Just "data"
      DefaultKeyword -> Just "default"
    subs = \case
      NoSubs -> Aeson.pairs (kindIs "none")
      AllSubs -> Aeson.pairs (kindIs "all")
      SomeSubs l ->
        Aeson.pairs $
          kindIs "some"
            <> Encoding.pair "names" (Encoding.list itemName l.names)
            <> ("wildcardAt" .= l.wildcardAt)
    importDecl i =
      Aeson.pairs $
        Encoding.pair "span" (spanJson i.span)
          <> Encoding.pair "name" (located moduleName i.name)
          <> ("package" .= i.package)
          <> ("source" .= (i.source == SourceImport))
          <> ("safe" .= i.safe)
          <> ("qualified" .= qualified i.qualified)
          <> ("level" .= level i.level)
          <> Encoding.pair "as" (nullable (located moduleName) i.as)
          <> Encoding.pair "list" (nullable importList i.list)
    qualified :: Qualified -> Text
    qualified = \case
      NotQualified -> "no"
      QualifiedPre -> "pre"
      QualifiedPost -> "post"
    level :: ImportLevel -> Text
    level = \case
      NormalLevel -> "normal"
      SpliceLevel -> "splice"
      QuoteLevel -> "quote"
    importList l =
      Aeson.pairs $
        ( "kind" .= case l.kind of
            Only -> "only" :: Text
            Hiding -> "hiding"
        )
          <> Encoding.pair "items" (Encoding.list (item mempty) l.items)
    extensions e =
      Aeson.pairs $
        ("language" .= e.language)
          <> ("enabled" .= e.enabled)
          <> ("fromPragmas" .= e.fromPragmas)
    located f l =
      Aeson.pairs (Encoding.pair "span" (spanJson l.span) <> ("value" .= f l.value))
    moduleName = moduleNameText
    -- Only the text for now: the AST goes on the wire later.
    docComment :: DocComment a -> Aeson.Encoding
    docComment d = Aeson.pairs (docFields d)
    docFields :: DocComment a -> Series
    docFields d = Encoding.pair "span" (spanJson d.span) <> ("text" .= d.text)
    nullable = maybe Encoding.null_

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

-- * Internal

kindIs :: Text -> Series
kindIs k = "kind" .= k

commitText :: CommitId -> Text
commitText (CommitId c) = c

-- | @[startLine, startCol, endLine, endCol]@.
spanJson :: Span -> Aeson.Encoding
spanJson s =
  Aeson.toEncoding [line s.startLine, col s.startCol, line s.endLine, col s.endCol]
  where
    line (Line n) = n
    col (Col n) = n

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName n) = n

noSourceReason :: NoSourceReason -> Text
noSourceReason = \case
  Autogen -> "autogen"
  Missing -> "missing"
  Outside _ -> "outside"
  SymlinkSource _ -> "symlink"
