-- | JSON bodies of the API, written by hand (D-50).
--
-- Field order is fixed (aeson's 'pairs'), so fixtures are stable. See
-- Note [API fixtures].
module HaskellReviewer.Server.Wire
  ( -- * Bodies
    encodeFileList,
    encodeFileView,
    encodeModuleTree,
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
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    ModuleName (..),
    PackageName (..),
    renderCabalProblem,
    renderComponentName,
    renderProjectWarning,
  )
import HaskellReviewer.Core.RelPath (relDirText, relPathText)
import HaskellReviewer.Session.Snapshot (Generation (..))

{- Note [API fixtures]

D-50: the JSON types are written by hand, here and in web/src/api/.
To keep them in sync, the Haskell tests (ApiGoldenTest) call the real
app with fixed data and write each body to fixtures/api/<name>.json.
The TypeScript tests feed every fixture to the decoder that production
uses, and check that each decoder case has a fixture.

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

-- | GET api\/file\/\<side>\/\<path>: one of
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
  Encoding.encodingToLazyByteString . Aeson.pairs $
    ("generation" .= g) <> fields
  where
    fields = case content of
      TextContent t -> kindIs "text" <> ("text" .= t)
      Binary (FileSize n) -> kindIs "binary" <> ("size" .= n)
      TooLarge info ->
        kindIs "too-large" <> ("size" .= bytes info.size) <> ("limit" .= bytes info.limit)
      Link info ->
        kindIs "symlink"
          <> ("target" .= info.target)
          <> ("resolved" .= fmap relPathText info.resolved)
      Submodule c -> kindIs "submodule" <> ("commit" .= commitText c)
      NestedRepo -> kindIs "submodule" <> ("commit" .= Aeson.Null)
    bytes (FileSize n) = n

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
      NoSource r -> Aeson.pairs (kindIs "no-source" <> ("reason" .= reason r))
    reason :: NoSourceReason -> Text
    reason = \case
      Autogen -> "autogen"
      Missing -> "missing"
      Outside _ -> "outside"
      SymlinkSource _ -> "symlink"
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

-- | The data of the @hello@ event: @{}@.
helloPayload :: ByteString
helloPayload = LBS.toStrict (Encoding.encodingToLazyByteString (Aeson.pairs mempty))

-- * Internal

kindIs :: Text -> Series
kindIs k = "kind" .= k

commitText :: CommitId -> Text
commitText (CommitId c) = c
