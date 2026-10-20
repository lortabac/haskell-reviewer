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
  )
where

import Data.ByteString (ByteString)
import HaskellReviewer.Core.Address (Port, Token, hostAllowed, tokenMatches)
import Network.HTTP.Types (Header, hCacheControl, hContentType, status403, status404)
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
3. The token segment is removed from pathInfo, and the router sees the
   rest. rawPathInfo is left as is: the router doesn't use it.

pathInfo is percent-decoded, so /%41bc.../ matches a token starting with
"Abc". Harmless: you still need the token.

The security headers are added by 'withSecurityHeaders', outside this
check, so rejections get them too. Warp's own error responses (bad
request, ...) get them in "HaskellReviewer.Server.Listen".
-}

-- | The CSP of SPEC §9.2, exactly.
contentSecurityPolicy :: ByteString
contentSecurityPolicy =
  "default-src 'self'; img-src 'self' data:; connect-src 'self'; \
  \script-src 'self'; object-src 'none'; base-uri 'none'; \
  \form-action 'none'; frame-ancestors 'none'"

-- | Headers on every response. @Referrer-Policy: no-referrer@ keeps the
-- token in the URL from leaking.
securityHeaders :: [Header]
securityHeaders =
  [ ("Content-Security-Policy", contentSecurityPolicy),
    ("Referrer-Policy", "no-referrer"),
    ("X-Content-Type-Options", "nosniff"),
    (hCacheControl, "no-store")
  ]

-- | Add 'securityHeaders' to a response. A header with the same name
-- already there is replaced, not doubled.
addSecurityHeaders :: Response -> Response
addSecurityHeaders = mapResponseHeaders $ \hs ->
  securityHeaders <> filter (\(name, _) -> name `notElem` names) hs
  where
    names = map fst securityHeaders

-- | 'addSecurityHeaders' on every response of the app.
withSecurityHeaders :: Middleware
withSecurityHeaders app req respond = app req (respond . addSecurityHeaders)

-- | Check the Host header and the token. See Note [Request checks].
checkAccess :: Port -> Token -> Middleware
checkAccess port token app req respond
  | not (hostAllowed port req.requestHeaderHost) = respond forbidden
  | segment : rest <- req.pathInfo,
    tokenMatches token segment =
      app req {pathInfo = rest} respond
  | otherwise = respond notFound

-- | 404, for unknown pages and bad tokens alike.
notFound :: Response
notFound = responseLBS status404 [plainText] "not found"

-- | 403, for a bad Host header.
forbidden :: Response
forbidden = responseLBS status403 [plainText] "forbidden: bad Host header"

-- * Internal

plainText :: Header
plainText = (hContentType, "text/plain; charset=utf-8")
