-- | The app, called in process (no socket).
module HaskellReviewer.Server.RouterTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Server.Router (application)
import HaskellReviewer.Server.Security (addSecurityHeaders, contentSecurityPolicy)
import Network.HTTP.Types (Header, Status (..), hContentType, status500)
import Network.Wai
  ( Request (..),
    Response,
    defaultRequest,
    responseLBS,
    responseToStream,
  )
import Network.Wai.Internal (ResponseReceived (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Router"
    [ testCase "right token: 200, HTML" $ do
        r <- get [tokenText token, ""]
        r.status @?= 200
        lookup hContentType r.headers @?= Just "text/html; charset=utf-8",
      testCase "HEAD: 200" $ do
        r <- call (request "HEAD" goodHost [tokenText token, ""])
        r.status @?= 200,
      testCase "no token: 404" $ do
        r1 <- get [""]
        r1.status @?= 404
        r2 <- get []
        r2.status @?= 404,
      testCase "wrong token: 404" $ do
        r <- get [Text.replicate 32 "A", ""]
        r.status @?= 404,
      testCase "wrong length token: 404" $ do
        r1 <- get [Text.take 31 (tokenText token), ""]
        r1.status @?= 404
        r2 <- get [tokenText token <> "A", ""]
        r2.status @?= 404,
      testCase "wrong token looks like an unknown page" $ do
        bad <- get [Text.replicate 32 "A", ""]
        unknown <- get [tokenText token, "nope"]
        unknown.status @?= 404
        (bad.status, bad.headers, bad.body)
          @?= (unknown.status, unknown.headers, unknown.body)
        bad.body @?= "not found",
      testCase "POST: 405" $ do
        r <- call (request "POST" goodHost [tokenText token, ""])
        r.status @?= 405
        lookup "Allow" r.headers @?= Just "GET, HEAD",
      testCase "bad Host: 403 even with the right token" $
        mapM_
          ( \host -> do
              r <- call (request "GET" host [tokenText token, ""])
              r.status @?= 403
              r.body @?= "forbidden: bad Host header"
          )
          [Just "evil.com:8080", Nothing, Just "127.0.0.1:9999"],
      testCase "bad Host comes before the token" $ do
        r <- call (request "GET" (Just "evil.com") ["x"])
        r.status @?= 403,
      testCase "security headers on 200, 404, 403, 405" $ do
        ok <- get [tokenText token, ""]
        missing <- get ["x"]
        forbidden <- call (request "GET" Nothing [tokenText token, ""])
        notAllowed <- call (request "PUT" goodHost [tokenText token, ""])
        mapM_ (assertSecurityHeaders . (.headers)) [ok, missing, forbidden, notAllowed],
      testCase "security headers on warp's error response" $ do
        -- Same as Listen's setOnExceptionResponse, on a response that
        -- already has a Cache-Control header.
        let resp =
              addSecurityHeaders
                (responseLBS status500 [("Cache-Control", "max-age=1")] "x")
        r <- collect resp
        assertSecurityHeaders r.headers
        length (filter ((== "Cache-Control") . fst) r.headers) @?= 1
    ]

token :: Token
token = tokenFromBytes (BS.pack [1 .. 24])

port :: Port
port = Port 8080

goodHost :: Maybe ByteString
goodHost = Just "127.0.0.1:8080"

data Result = Result
  { status :: Int,
    headers :: [Header],
    body :: LBS.ByteString
  }

get :: [Text] -> IO Result
get = call . request "GET" goodHost

request :: ByteString -> Maybe ByteString -> [Text] -> Request
request method host path =
  defaultRequest
    { requestMethod = method,
      requestHeaderHost = host,
      requestHeaders = maybe [] (\h -> [("Host", h)]) host,
      pathInfo = path
    }

call :: Request -> IO Result
call req = do
  saved <- newIORef Nothing
  ResponseReceived <-
    application port token req $ \resp -> do
      writeIORef saved (Just resp)
      pure ResponseReceived
  readIORef saved >>= \case
    Nothing -> assertFailure "no response"
    Just resp -> collect resp

collect :: Response -> IO Result
collect resp = do
  let (st, hs, withBody) = responseToStream resp
  chunks <- newIORef mempty
  withBody $ \streaming ->
    streaming (\b -> modifyIORef' chunks (<> b)) (pure ())
  b <- readIORef chunks
  pure
    Result
      { status = statusCode st,
        headers = hs,
        body = Builder.toLazyByteString b
      }

assertSecurityHeaders :: [Header] -> Assertion
assertSecurityHeaders hs = do
  lookup "Content-Security-Policy" hs @?= Just contentSecurityPolicy
  lookup "Content-Security-Policy" hs
    @?= Just
      "default-src 'self'; img-src 'self' data:; connect-src 'self'; \
      \script-src 'self'; object-src 'none'; base-uri 'none'; \
      \form-action 'none'; frame-ancestors 'none'"
  lookup "Referrer-Policy" hs @?= Just "no-referrer"
  lookup "X-Content-Type-Options" hs @?= Just "nosniff"
  lookup "Cache-Control" hs @?= Just "no-store"
