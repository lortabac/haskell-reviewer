-- | The app, called in process (no socket).
module HaskellReviewer.Server.RouterTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (FilesApi (..))
import HaskellReviewer.Server.Events (EventStream (..))
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Server.Security (addSecurityHeaders, contentSecurityPolicy)
import HaskellReviewer.Session.Clients (newClients)
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
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

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
      testCase "wrong token on an asset path looks like an unknown page" $ do
        name <- firstAsset
        bad <- get [Text.replicate 32 "A", "assets", name]
        unknown <- get [tokenText token, "nope"]
        (bad.status, bad.headers, bad.body)
          @?= (unknown.status, unknown.headers, unknown.body),
      testCase "POST: 405" $ do
        r <- call (request "POST" goodHost [tokenText token, ""])
        r.status @?= 405
        lookup "Allow" r.headers @?= Just "GET, HEAD",
      testCase "POST on a deep link and on an asset: 405" $ do
        name <- firstAsset
        mapM_
          ( \path -> do
              r <- call (request "POST" goodHost path)
              (path, r.status) @?= (path, 405)
              lookup "Allow" r.headers @?= Just "GET, HEAD"
              assertNoStore r.headers
          )
          [[tokenText token, "src", "x"], [tokenText token, "assets", name]],
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
      testCase "security headers and no-store on 200, 404, 403, 405" $ do
        ok <- get [tokenText token, ""]
        missing <- get ["x"]
        forbidden <- call (request "GET" Nothing [tokenText token, ""])
        notAllowed <- call (request "PUT" goodHost [tokenText token, ""])
        mapM_
          (\r -> assertSecurityHeaders r.headers >> assertNoStore r.headers)
          [ok, missing, forbidden, notAllowed],
      testCase "warp's error response: no-store added" $ do
        -- Same as Listen's setOnExceptionResponse.
        r <- collect (addSecurityHeaders (responseLBS status500 [] "x"))
        assertSecurityHeaders r.headers
        assertNoStore r.headers,
      testCase "a response with its own Cache-Control keeps it, once" $ do
        let resp =
              addSecurityHeaders
                ( responseLBS
                    status500
                    [("Cache-Control", "max-age=1"), ("Referrer-Policy", "x")]
                    "x"
                )
        r <- collect resp
        assertSecurityHeaders r.headers
        cacheControl r.headers @?= ["max-age=1"]
        length (filter ((== "Referrer-Policy") . fst) r.headers) @?= 1,
      testCase "the shell on every app path, same body" $ do
        root <- get [tokenText token, ""]
        mapM_
          ( \path -> do
              r <- get (tokenText token : path)
              (path, r.status) @?= (path, 200)
              lookup hContentType r.headers @?= Just "text/html; charset=utf-8"
              assertSecurityHeaders r.headers
              assertNoStore r.headers
              assertContentLength r
              r.body @?= root.body
          )
          [ [],
            [""],
            ["src", "tree", "a", "b.hs"],
            ["docs", "tree", "pkg:lib:Data.Foo"],
            ["diff", "src", "A.hs"],
            ["changes"]
          ],
      testCase "the shell refers to assets under the token" $ do
        r <- get [tokenText token, ""]
        let body = LBS.toStrict r.body
            new = "/" <> Text.Encoding.encodeUtf8 (tokenText token) <> "/assets/"
        assertBool "no /<token>/assets/ ref" (new `BS.isInfixOf` body)
        assertBool "a ./assets/ ref is left" (not ("\"./assets/" `BS.isInfixOf` body)),
      testCase "every src and href of the shell: 200, right type, immutable" $ do
        shell <- get [tokenText token, ""]
        let refs = filter (/= "data:,") (shellRefs (LBS.toStrict shell.body))
        assertBool "no refs" (not (null refs))
        mapM_
          ( \ref -> do
              let path = Text.splitOn "/" (Text.Encoding.decodeUtf8 ref)
              case path of
                "" : t : "assets" : [name] | t == tokenText token -> do
                  r <- get [t, "assets", name]
                  (ref, r.status) @?= (ref, 200)
                  lookup hContentType r.headers
                    @?= Just
                      ( if ".js" `Text.isSuffixOf` name
                          then "text/javascript; charset=utf-8"
                          else "text/css; charset=utf-8"
                      )
                  assertSecurityHeaders r.headers
                  cacheControl r.headers @?= ["private, max-age=31536000, immutable"]
                  assertContentLength r
                  h <- call (request "HEAD" goodHost [t, "assets", name])
                  h.status @?= 200
                  lookup "Content-Length" h.headers @?= lookup "Content-Length" r.headers
                _ -> assertFailure ("unexpected ref: " <> show ref)
          )
          refs,
      testCase "unknown paths: 404 with no-store" $
        mapM_
          ( \path -> do
              r <- get (tokenText token : path)
              (path, r.status) @?= (path, 404)
              r.body @?= "not found"
              assertSecurityHeaders r.headers
              assertNoStore r.headers
          )
          [ ["assets", "nope.js"],
            ["assets"],
            ["assets", ""],
            ["assets", "a", "b"],
            ["assets", ".."],
            ["assets", "..", "index.html"],
            ["nope"],
            ["api"],
            ["api", ""],
            ["api", "x"],
            ["api", "events", "x"],
            ["api", "events", ""],
            ["api", "files"],
            ["api", "file"],
            ["api", "files", "tree", "x"],
            ["api", "modules"],
            ["api", "modules", "tree", "x"],
            ["api", "module"],
            ["index.html"]
          ]
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
  clients <- newClients
  let events =
        EventStream
          { clients = clients,
            keepAlive = Microseconds 1000000,
            indexing = pure Nothing,
            minGap = Microseconds 0,
            logger = newLogger LevelInfo (\_ -> pure ())
          }
  saved <- newIORef Nothing
  ResponseReceived <-
    application port token Routes {events = events, files = noFiles} req $ \resp -> do
      writeIORef saved (Just resp)
      pure ResponseReceived
  readIORef saved >>= \case
    Nothing -> assertFailure "no response"
    Just resp -> collect resp

-- | File routes that never list. ApiTest covers them.
noFiles :: FilesApi
noFiles =
  FilesApi
    { snapshot = pure (Left (ListCrashed "no files here")),
      readEntry = \_ _ _ -> pure (Left ReadGone),
      logger = newLogger LevelInfo (\_ -> pure ())
    }

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

assertContentLength :: Result -> Assertion
assertContentLength r =
  lookup "Content-Length" r.headers
    @?= Just (Char8.pack (show (LBS.length r.body)))

assertNoStore :: [Header] -> Assertion
assertNoStore hs = cacheControl hs @?= ["no-store"]

cacheControl :: [Header] -> [ByteString]
cacheControl hs = [v | (name, v) <- hs, name == "Cache-Control"]

-- | The values of every @src="..."@ and @href="..."@ in the HTML.
shellRefs :: ByteString -> [ByteString]
shellRefs html = concatMap (`valuesOf` html) [" src=\"", " href=\""]
  where
    valuesOf attr s = case BS.breakSubstring attr s of
      (_, rest)
        | BS.null rest -> []
        | otherwise ->
            let (value, after) = Char8.break (== '"') (BS.drop (BS.length attr) rest)
             in value : valuesOf attr after

-- | The name of one asset the shell uses.
firstAsset :: IO Text
firstAsset = do
  r <- get [tokenText token, ""]
  case [ name
       | ref <- shellRefs (LBS.toStrict r.body),
         _ : _ : "assets" : [name] <- [Text.splitOn "/" (Text.Encoding.decodeUtf8 ref)]
       ] of
    name : _ -> pure name
    [] -> assertFailure "no asset in the shell"
