-- | The preferences route, @api/prefs@ (D-197).
module HaskellReviewer.Server.Prefs
  ( -- * Route
    prefsApp,
    prefsBodyLimit,

    -- * Warm-up
    warmUpPrefs,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Config (builtinDefaults)
import HaskellReviewer.Core.Prefs
  ( DecodedPrefs (..),
    PrefsRead (..),
    applyPatchFile,
    decodePatch,
    decodePrefsFile,
    effective,
    encodePrefsFile,
    renderBadPrefsFile,
    renderBadValue,
    renderPatchError,
  )
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Security (methodNotAllowed)
import HaskellReviewer.Server.Wire (encodePrefs)
import HaskellReviewer.Session.Prefs (PrefsStore (..), newPrefsStore)
import Network.HTTP.Types
  ( Status,
    hContentLength,
    hContentType,
    methodGet,
    methodPost,
    status200,
    status400,
    status413,
    status415,
  )
import Network.Wai
  ( Application,
    Request (..),
    RequestBodyLength (..),
    Response,
    defaultRequest,
    getRequestBodyChunk,
    responseLBS,
    responseToStream,
    setRequestBodyChunks,
  )
import Network.Wai.Internal (ResponseReceived (..))

{- Note [Prefs route]

D-197. GET api/prefs: every preference, defaults filled in (encodePrefs).
POST api/prefs: a JSON object with the keys to change, e.g.
{"theme":"dark"}; the answer is the same as GET's, after the change.

POST answers, in this order:

- 413 if Content-Length is over 4 KiB (the body isn't read);
- 415 if the Content-Type isn't application/json (parameters like
  charset are fine);
- 413 if the body is over 4 KiB after all (chunked, no length);
- 400 if it isn't a good patch (decodePatch): the reason, as text;
- else 200, even if the change couldn't be saved: the store keeps it
  in memory (Note [Saving preferences] in "HaskellReviewer.Session.Prefs").

Other methods, HEAD too: 405 with Allow: GET, POST. The Origin check
comes before all this (Note [Request checks] in
"HaskellReviewer.Server.Security").
-}

-- | @GET@ and @POST api\/prefs@. See Note [Prefs route].
prefsApp :: PrefsStore -> Application
prefsApp store req respond
  | req.requestMethod == methodGet = respond . json =<< store.current
  | req.requestMethod == methodPost = post
  | otherwise = respond (methodNotAllowed "GET, POST")
  where
    post
      | KnownLength n <- req.requestBodyLength,
        n > fromIntegral prefsBodyLimit =
          respond tooLarge
      | not (isJson (lookup hContentType req.requestHeaders)) =
          respond (plain status415 "the body must be application/json")
      | otherwise =
          readBody req >>= \case
            Nothing -> respond tooLarge
            Just body -> case decodePatch body of
              Left why -> respond (plain status400 (textBytes (renderPatchError why)))
              Right patch -> respond . json =<< store.update patch
    tooLarge = plain status413 "the body is over 4 KiB"
    json e = withLength status200 "application/json" (encodePrefs e)

-- | The biggest POST body we take, in bytes: 4 KiB.
prefsBodyLimit :: Int
prefsBodyLimit = 4 * 1024

-- | Run this route once of each kind (GET, a good POST, each error)
-- on fake requests, against a store in memory, and read the ui.json
-- code's paths too. So their shared values are built before request
-- threads race on them (Q-RTS-BLACKHOLE). Call it before serving, like
-- warmUpGzip (Note [Compression] in "HaskellReviewer.Server.Router").
warmUpPrefs :: IO ()
warmUpPrefs = do
  let silent = newLogger LevelInfo (\_ -> pure ())
  store <- newPrefsStore silent builtinDefaults (Left HomeUnset)
  mapM_
    (once store)
    [ ("GET", [], ""),
      ("POST", json, "{\"theme\":\"light\",\"exportedOnly\":true,\"sidebar\":\"files\"}"),
      ("POST", json, "{"),
      ("POST", json, "[]"),
      ("POST", json, "{}"),
      ("POST", json, "{\"x\":1}"),
      ("POST", json, "{\"theme\":1}"),
      ("POST", json, "{\"exportedOnly\":null}"),
      ("POST", json, BS.replicate (prefsBodyLimit + 1) 32),
      ("POST", [], "{}"),
      ("PUT", json, "{}")
    ]
  mapM_
    ( \bytes -> case decodePrefsFile bytes of
        Left bad -> void (evaluate (Text.length (renderBadPrefsFile bad)))
        Right (PrefsNewer p) ->
          void (evaluate (LBS.length (encodePrefs (effective builtinDefaults p))))
        Right (PrefsCurrent d) -> do
          let new = either (const d.file) (`applyPatchFile` d.file) darkPatch
          void (evaluate (LBS.length (encodePrefsFile new)))
          void (evaluate (sum (map (Text.length . renderBadValue) d.oddValues)))
    )
    [ "{\"version\":1,\"theme\":\"dark\",\"exportedOnly\":true,\
      \\"sidebar\":\"high\",\"other\":[1,\"x\",null,{},2.5]}",
      "",
      "[]",
      "{\"version\":2}",
      "{\"version\":\"1\"}"
    ]
  where
    json = [(hContentType, "application/json")]
    darkPatch = decodePatch "{\"theme\":\"dark\"}"
    once store (method, headers, body) = do
      chunks <- newIORef [body]
      let next = atomicModifyIORef' chunks $ \case
            [] -> ([], BS.empty)
            b : rest -> (rest, b)
          req =
            setRequestBodyChunks next $
              defaultRequest
                { requestMethod = method,
                  requestHeaders = headers,
                  requestBodyLength = KnownLength (fromIntegral (BS.length body))
                }
      ResponseReceived <- prefsApp store req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        out <- newIORef mempty
        withBody $ \streaming ->
          streaming (\b -> modifyIORef' out (<> b)) (pure ())
        void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
        pure ResponseReceived
      pure ()

-- * Internal

-- | Up to the limit. 'Nothing' if there is more.
readBody :: Request -> IO (Maybe ByteString)
readBody req = go 0 []
  where
    go size acc = do
      chunk <- getRequestBodyChunk req
      next (size + BS.length chunk) chunk acc
    next size chunk acc
      | BS.null chunk = pure (Just (BS.concat (reverse acc)))
      | size > prefsBodyLimit = pure Nothing
      | otherwise = go size (chunk : acc)

-- | @application/json@, maybe with parameters.
isJson :: Maybe ByteString -> Bool
isJson = \case
  Nothing -> False
  Just ct ->
    Char8.map toLower (Char8.strip (Char8.takeWhile (/= ';') ct)) == "application/json"

-- | With a Content-Length, so warp doesn't send it chunked.
withLength :: Status -> ByteString -> LBS.ByteString -> Response
withLength st contentType body =
  responseLBS
    st
    [ (hContentType, contentType),
      (hContentLength, Char8.pack (show (LBS.length body)))
    ]
    body

plain :: Status -> LBS.ByteString -> Response
plain st = responseLBS st [(hContentType, "text/plain; charset=utf-8")]

textBytes :: Text -> LBS.ByteString
textBytes = LBS.fromStrict . Text.Encoding.encodeUtf8
