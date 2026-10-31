-- | Checks and headers in front of every route (SPEC §9.2, §11.6.1).
module HaskellReviewer.Server.Security
  ( -- * Headers
    contentSecurityPolicy,
    securityHeaders,
    addSecurityHeaders,
    withSecurityHeaders,

    -- * Access checks
    checkAccess,

    -- * Error responses
    notFound,
    forbidden,
    forbiddenOrigin,
    methodNotAllowed,
  )
where

import Data.ByteString (ByteString)
import HaskellReviewer.Core.Address
  ( Port,
    Token,
    hostAllowed,
    originAllowed,
    tokenMatches,
  )
import Network.HTTP.Types
  ( Header,
    hAllow,
    hCacheControl,
    hContentType,
    hOrigin,
    methodGet,
    methodHead,
    status403,
    status404,
    status405,
  )
import Network.Wai
  ( Middleware,
    Request (..),
    Response,
    mapResponseHeaders,
    responseLBS,
  )

{- Note [Request checks]

Every request goes through 'checkAccess', in this order:

1. Host header (hostAllowed). Bad or missing: 403. This is what stops DNS
   rebinding. It comes first: such a request is turned away before we
   look at anything else.
2. Token, the first path segment. Missing or wrong: 404, the same response
   as an unknown page under a good token. So a wrong token looks like
   "nothing here" and says nothing about what exists. Compared in constant
   time (tokenMatches).
3. The token segment is removed from pathInfo. rawPathInfo is left as
   is: the router doesn't use it.
4. A method other than GET and HEAD (POST, ...) needs an Origin header
   with our own origin (originAllowed). Bad, missing or "null": 403,
   whatever the path. Then the router sees the rest.

Step 4 is against CSRF (D-198): a page on another site can send a POST
to us, without reading the answer. It can't know the token, but that's
one secret; the Origin check is a second line. Browsers send Origin
on POST, but it is "null" in some cases: with Referrer-Policy
no-referrer (ours) and a fetch in no-cors mode, for one. So our own
page must POST with a plain fetch (cors mode, the default), which
sends our origin. More than one Origin header: 403 too. On top of it,
POST routes take only JSON bodies (application/json), which a
cross-site page can send only after a CORS
preflight, and we never answer preflights (OPTIONS has a foreign
Origin: 403). Bodies are capped (4 KiB for api/prefs).

pathInfo is percent-decoded, so /%41bc.../ matches a token starting with
"Abc". Harmless: you still need the token.

The security headers are added by 'withSecurityHeaders', outside this
check, so rejections get them too. Warp's own error responses (bad
request, ...) get them in "HaskellReviewer.Server.Listen".
-}

{- Note [Caching]

'addSecurityHeaders' adds Cache-Control: no-store to every response that
has no Cache-Control of its own. Only the hashed assets set one
(immutable, see Note [Routes] in "HaskellReviewer.Server.Router").
-}

-- | The CSP of SPEC §9.2, exactly.
contentSecurityPolicy :: ByteString
contentSecurityPolicy =
  "default-src 'self'; img-src 'self' data:; connect-src 'self'; \
  \script-src 'self'; object-src 'none'; base-uri 'none'; \
  \form-action 'none'; frame-ancestors 'none'"

-- | Headers forced on every response. @Referrer-Policy: no-referrer@
-- keeps the token in the URL from leaking.
securityHeaders :: [Header]
securityHeaders =
  [ ("Content-Security-Policy", contentSecurityPolicy),
    ("Referrer-Policy", "no-referrer"),
    ("X-Content-Type-Options", "nosniff")
  ]

-- | Add 'securityHeaders' to a response. A header with the same name
-- already there is replaced, not doubled. Also adds
-- @Cache-Control: no-store@ if the response has no Cache-Control.
-- See Note [Caching].
addSecurityHeaders :: Response -> Response
addSecurityHeaders = mapResponseHeaders $ \hs ->
  let rest = filter (\(name, _) -> name `notElem` names) hs
      cache
        | any ((== hCacheControl) . fst) rest = []
        | otherwise = [(hCacheControl, "no-store")]
   in securityHeaders <> cache <> rest
  where
    names = map fst securityHeaders

-- | 'addSecurityHeaders' on every response of the app.
withSecurityHeaders :: Middleware
withSecurityHeaders app req respond = app req (respond . addSecurityHeaders)

-- | Check the Host header, the token and, for methods that change
-- something, the Origin header. See Note [Request checks].
checkAccess :: Port -> Token -> Middleware
checkAccess port token app req respond
  | not (hostAllowed port req.requestHeaderHost) = respond forbidden
  | segment : rest <- req.pathInfo,
    tokenMatches token segment =
      if safe || originOk
        then app req {pathInfo = rest} respond
        else respond forbiddenOrigin
  | otherwise = respond notFound
  where
    safe = req.requestMethod == methodGet || req.requestMethod == methodHead
    originOk = case [v | (n, v) <- req.requestHeaders, n == hOrigin] of
      [origin] -> originAllowed port (Just origin)
      _ -> False

-- | 404, for unknown pages and bad tokens alike.
notFound :: Response
notFound = responseLBS status404 [plainText] "not found"

-- | 403, for a bad Host header.
forbidden :: Response
forbidden = responseLBS status403 [plainText] "forbidden: bad Host header"

-- | 403, for a bad Origin header.
forbiddenOrigin :: Response
forbiddenOrigin = responseLBS status403 [plainText] "forbidden: bad Origin header"

-- | 405, with the methods the route takes (e.g. @GET, HEAD@) in @Allow@.
methodNotAllowed :: ByteString -> Response
methodNotAllowed allow =
  responseLBS status405 [plainText, (hAllow, allow)] "method not allowed"

-- * Internal

plainText :: Header
plainText = (hContentType, "text/plain; charset=utf-8")
