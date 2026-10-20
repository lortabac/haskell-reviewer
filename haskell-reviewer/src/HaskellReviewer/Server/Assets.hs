{-# LANGUAGE TemplateHaskell #-}

-- | The frontend bundle (@web\/dist@), built into the binary.
module HaskellReviewer.Server.Assets
  ( -- * Types
    Asset (..),

    -- * Operations
    lookupAsset,
    shellFor,

    -- * Internal (for tests)
    embeddedFiles,
    contentTypeFor,
    shellTemplate,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Address (Token, tokenText)
import System.FilePath (takeExtension)

{- Note [Embedded bundle]

web/dist is built by `make web` and committed, so building the binary needs
no Node (SPEC §11.8). It holds index.html and hashed files in assets/.

Vite builds with base './', so index.html refers to "./assets/<name>". The
page lives at /<token>/... and a deep link like /<token>/src/tree/a/b.hs
would resolve that against the wrong dir. A <base> tag can't fix it: the
CSP has base-uri 'none'. So 'shellFor' rewrites the refs to
"/<token>/assets/<name>", once per session.

The files are listed in extra-source-files in the .cabal file, so cabal
rebuilds this module when they change. Only js and css for now: a new kind
needs a glob there, a type in 'contentTypeFor' and an extension in
web/scripts/check-bundle.mjs.
-}

-- | A file to serve.
data Asset = Asset
  { contentType :: ByteString,
    body :: ByteString
  }

-- | A file under @assets\/@, by name. Exact match only: no dirs, no
-- @..@. 'Nothing' for unknown names.
lookupAsset :: Text -> Maybe Asset
lookupAsset name = do
  bytes <- lookup ("assets/" <> Text.unpack name) embeddedFiles
  ct <- contentTypeFor (Text.unpack name)
  pure Asset {contentType = ct, body = bytes}

-- | The app shell for a session: index.html with the asset refs pointing
-- under the token. See Note [Embedded bundle].
shellFor :: Token -> ByteString
shellFor token =
  Text.Encoding.encodeUtf8 $
    Text.replace "\"./assets/" ("\"/" <> tokenText token <> "/assets/") shellTemplate

-- | index.html as built by Vite.
shellTemplate :: Text
shellTemplate = case lookup "index.html" embeddedFiles of
  Just bytes -> Text.Encoding.decodeUtf8 bytes
  -- Missing, or not UTF-8 (decodeUtf8 throws): can't happen with the
  -- committed bundle. AssetsTest checks it.
  Nothing -> error "web/dist/index.html is not in the binary"

-- | Every file of web/dist, with paths like @assets\/index-abc.js@.
embeddedFiles :: [(FilePath, ByteString)]
embeddedFiles = $(makeRelativeToProject "../web/dist" >>= embedDir)

-- | The Content-Type for a file name, by extension. Only the kinds Vite
-- gives us now (web/scripts/check-bundle.mjs makes sure). A new kind also
-- needs its glob in extra-source-files, in the .cabal file.
contentTypeFor :: FilePath -> Maybe ByteString
contentTypeFor path = case takeExtension path of
  ".html" -> Just "text/html; charset=utf-8"
  ".js" -> Just "text/javascript; charset=utf-8"
  ".css" -> Just "text/css; charset=utf-8"
  _ -> Nothing
