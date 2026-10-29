-- | The HTTP application: checks, headers and routes (SPEC §11.6.1).
module HaskellReviewer.Server.Router
  ( -- * Application
    application,
    Routes (..),
    warmUpGzip,

    -- * Internal, for tests
    compressible,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import HaskellReviewer.Core.Address (Port, Token)
import HaskellReviewer.Server.Api
  ( FilesApi,
    fileApp,
    filesApp,
    indexingApp,
    moduleApp,
    modulesApp,
  )
import HaskellReviewer.Server.Assets (Asset (..), lookupAsset, shellFor)
import HaskellReviewer.Server.Events (EventStream, eventsApp)
import HaskellReviewer.Server.Security
  ( checkAccess,
    methodNotAllowed,
    notFound,
    withSecurityHeaders,
  )
import Network.HTTP.Types
  ( Header,
    hCacheControl,
    hContentLength,
    hContentType,
    methodGet,
    methodHead,
    status200,
  )
import Network.Wai
  ( Application,
    Request (..),
    Response,
    defaultRequest,
    responseLBS,
    responseToStream,
  )
import Network.Wai.Internal (ResponseReceived (..))
import Network.Wai.Middleware.Gzip
  ( GzipFiles (..),
    GzipSettings,
    defaultGzipSettings,
    gzip,
    gzipCheckMime,
    gzipFiles,
  )

{- Note [Routes]

Paths below are what the router sees, after the token.

- The app shell (index.html): [], [""], and any path whose first segment
  is docs, src, diff or changes. The client router handles the rest. So
  /<token> (no slash) gets the shell too, with no redirect.
- assets/<name>: a file of the bundle, exact name. Hashed names, so they
  are cached as immutable (Note [Caching] in
  "HaskellReviewer.Server.Security").
- api/events: the event stream (Note [Event stream] in
  "HaskellReviewer.Server.Events"). GET only (405 otherwise, HEAD too).
- api/files/<side>: the file list, as JSON. api/file/<side>/<path...>:
  one file, as CBOR (Note [File containment] in
  "HaskellReviewer.Core.RelPath"). api/modules/<side>: the module tree,
  as JSON. api/module/<side>/<ref...>: one module's page, as CBOR
  (Note [Module page] in "HaskellReviewer.Server.Wire.Page"). The ref's
  segments are joined with /, so a loose module's path works; an
  unknown ref, or a module whose file is gone, is a 404 with a JSON
  body. api/indexing/<side>: how far indexing is, as JSON. The only
  side is tree; another one is a plain 404. GET only (405 otherwise,
  HEAD too).
- Anything else: 404. index.html itself is not served under its name.
  So are api, api/x and api/events/x: the api/ namespace has only the
  routes listed here.

Shell and assets accept GET and HEAD only (405 otherwise).

Big responses may be gzipped: Note [Compression].
-}

{- Note [Compression]

wai-extra's gzip middleware sits between the security headers and the
access checks. It gzips a response when:

- the request has Accept-Encoding: gzip;
- the type is JSON, CBOR, HTML, CSS or JS ('compressible'). Not the
  event stream (text/event-stream): gzip would hold events back until
  its buffer fills. Not plain-text errors: they are tiny;
- the body is at least 860 bytes, or has no Content-Length.

A gzipped response has Content-Encoding: gzip and no Content-Length, so
warp sends it chunked. Every response gets Vary: Accept-Encoding. We
serve no files from disk (GzipIgnore would skip them anyway).

The browser loads the JS and the CSS at once, so two request threads
would build gzipSettings, the type list and the gzip code's own shared
values at the same time (Q-RTS-BLACKHOLE). 'warmUpGzip' builds them
first: it runs the middleware on a fake request before warp listens.

-}

-- | What the API routes need.
data Routes = Routes
  { events :: EventStream,
    files :: FilesApi
  }

-- | The whole app. Paths are relative to the token: the router never
-- sees it. See Note [Request checks] in "HaskellReviewer.Server.Security"
-- and Note [Routes].
application :: Port -> Token -> Routes -> Application
application port token routes =
  withSecurityHeaders (gzip gzipSettings (checkAccess port token (router shell routes)))
  where
    -- Built once per session, not per request.
    shell = shellResponse token

-- | Gzip one fake answer of each type we gzip, so the shared values of
-- the gzip path are built before request threads race on them. Call it
-- before serving. Takes well under a millisecond. See Note
-- [Compression].
warmUpGzip :: IO ()
warmUpGzip = do
  void (evaluate gzipSettings)
  mapM_ once types
  where
    types =
      [ "application/json",
        "application/cbor",
        "text/html; charset=utf-8",
        "text/css; charset=utf-8",
        "text/javascript; charset=utf-8"
      ]
    req = defaultRequest {requestHeaders = [("Accept-Encoding", "gzip")]}
    body = LBS.replicate 1000 120
    once contentType = do
      let app _ respond =
            respond (responseLBS status200 [(hContentType, contentType)] body)
      ResponseReceived <- gzip gzipSettings app req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        out <- newIORef mempty
        withBody $ \streaming ->
          streaming (\b -> modifyIORef' out (<> b)) (pure ())
        void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
        pure ResponseReceived
      pure ()

-- * Internal

-- | See Note [Compression].
gzipSettings :: GzipSettings
gzipSettings =
  defaultGzipSettings {gzipFiles = GzipIgnore, gzipCheckMime = compressible}

-- | The types we gzip, ignoring parameters like @; charset=utf-8@.
compressible :: ByteString -> Bool
compressible contentType =
  Char8.takeWhile (/= ';') contentType
    `elem` [ "application/json",
             "application/cbor",
             "text/html",
             "text/css",
             "text/javascript"
           ]

-- | Matches on the path after the token. See Note [Routes].
router :: Response -> Routes -> Application
router shell routes req respond = case req.pathInfo of
  path
    | isShellPath path -> getOnly shell
  ["assets", name]
    | Just asset <- lookupAsset name -> getOnly (assetResponse asset)
  ["api", "events"] -> eventsApp routes.events req respond
  ["api", "files", side] -> filesApp routes.files side req respond
  "api" : "file" : side : path -> fileApp routes.files side path req respond
  ["api", "modules", side] -> modulesApp routes.files side req respond
  "api" : "module" : side : ref -> moduleApp routes.files side ref req respond
  ["api", "indexing", side] -> indexingApp routes.files side req respond
  _ -> respond notFound
  where
    getOnly r
      | req.requestMethod == methodGet || req.requestMethod == methodHead = respond r
      | otherwise = respond (methodNotAllowed "GET, HEAD")

isShellPath :: [Text] -> Bool
isShellPath = \case
  [] -> True
  [""] -> True
  first : _ -> first `elem` ["docs", "src", "diff", "changes"]

-- | No Cache-Control here: it gets no-store. See Note [Caching] in
-- "HaskellReviewer.Server.Security".
shellResponse :: Token -> Response
shellResponse token =
  okWithLength [(hContentType, "text/html; charset=utf-8")] (shellFor token)

assetResponse :: Asset -> Response
assetResponse asset =
  okWithLength
    [ (hContentType, asset.contentType),
      (hCacheControl, "private, max-age=31536000, immutable")
    ]
    asset.body

-- | 200 with a Content-Length, so warp doesn't send it chunked. Warp
-- keeps the header on HEAD.
okWithLength :: [Header] -> ByteString -> Response
okWithLength headers body =
  responseLBS
    status200
    ((hContentLength, Char8.pack (show (BS.length body))) : headers)
    (LBS.fromStrict body)
