-- | JSON bodies of the API, written by hand (D-50).
--
-- Field order is fixed (aeson's 'pairs'), so fixtures are stable. See
-- Note [API fixtures].
module HaskellReviewer.Server.Wire
  ( -- * Bodies
    encodeFileList,
    encodeFileView,
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
import HaskellReviewer.Core.RelPath (relPathText)
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

-- | The data of the @hello@ event: @{}@.
helloPayload :: ByteString
helloPayload = LBS.toStrict (Encoding.encodingToLazyByteString (Aeson.pairs mempty))

-- * Internal

kindIs :: Text -> Series
kindIs k = "kind" .= k

commitText :: CommitId -> Text
commitText (CommitId c) = c
