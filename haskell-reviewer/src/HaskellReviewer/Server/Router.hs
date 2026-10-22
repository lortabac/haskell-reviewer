-- | The HTTP application: checks, headers and routes (SPEC §11.6.1).
module HaskellReviewer.Server.Router
  ( -- * Application
    application,
    Routes (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import HaskellReviewer.Core.Address (Port, Token)
import HaskellReviewer.Server.Api (FilesApi, fileApp, filesApp, modulesApp)
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
import Network.Wai (Application, Request (..), Response, responseLBS)

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
  one file (Note [File containment] in "HaskellReviewer.Core.RelPath").
  api/modules/<side>: the module tree, as JSON. The only side is tree;
  another one is a 404. GET only (405 otherwise, HEAD too).
- Anything else: 404. index.html itself is not served under its name.
  So are api, api/x and api/events/x: the api/ namespace has only the
  routes listed here.

Shell and assets accept GET and HEAD only (405 otherwise).
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
  withSecurityHeaders (checkAccess port token (router shell routes))
  where
    -- Built once per session, not per request.
    shell = shellResponse token

-- * Internal

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
