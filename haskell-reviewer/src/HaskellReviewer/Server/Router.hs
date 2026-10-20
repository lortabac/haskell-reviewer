-- | The HTTP application: checks, headers and routes (SPEC §11.6.1).
module HaskellReviewer.Server.Router
  ( -- * Application
    application,
  )
where

import Data.ByteString.Lazy qualified as LBS
import HaskellReviewer.Core.Address (Port, Token)
import HaskellReviewer.Server.Security (checkAccess, notFound, withSecurityHeaders)
import Network.HTTP.Types
  ( hAllow,
    hContentType,
    methodGet,
    methodHead,
    status200,
    status405,
  )
import Network.Wai (Application, Request (..), Response, responseLBS)

-- | The whole app. Paths are relative to the token: the router never
-- sees it. See Note [Request checks] in "HaskellReviewer.Server.Security".
application :: Port -> Token -> Application
application port token = withSecurityHeaders (checkAccess port token router)

-- * Internal

-- | A plain match on method and path segments. @\/\<token\>\/@ gives @[""]@.
router :: Application
router req respond = case req.pathInfo of
  [""]
    | req.requestMethod == methodGet || req.requestMethod == methodHead ->
        respond shell
    | otherwise -> respond methodNotAllowed
  _ -> respond notFound

-- | Placeholder page until the frontend exists. No inline script or
-- style: the CSP would block them.
shell :: Response
shell =
  responseLBS
    status200
    [(hContentType, "text/html; charset=utf-8")]
    shellHtml

shellHtml :: LBS.ByteString
shellHtml =
  "<!doctype html>\n\
  \<html lang=\"en\">\n\
  \<head>\n\
  \<meta charset=\"utf-8\">\n\
  \<title>haskell-reviewer</title>\n\
  \</head>\n\
  \<body>\n\
  \<p>haskell-reviewer is running. There is no UI yet.</p>\n\
  \</body>\n\
  \</html>\n"

methodNotAllowed :: Response
methodNotAllowed =
  responseLBS
    status405
    [(hContentType, "text/plain; charset=utf-8"), (hAllow, "GET, HEAD")]
    "method not allowed"
