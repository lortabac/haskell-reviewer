-- | The reload route, @POST api\/reload@ (D-351).
module HaskellReviewer.Server.Reload
  ( -- * Route
    reloadApp,

    -- * Warm-up
    warmUpReload,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Server.Body
  ( bodyLimit,
    callOnce,
    isJson,
    plain,
    readBody,
    tooLarge,
  )
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (encodeGeneration, encodeReloaded)
import HaskellReviewer.Session.Reload (ReloadOutcome (..))
import HaskellReviewer.Session.Snapshot (Generation (..))
import Network.HTTP.Types
  ( hContentLength,
    hContentType,
    methodPost,
    status200,
    status400,
    status415,
    status500,
    status503,
  )
import Network.Wai
  ( Application,
    Request (..),
    RequestBodyLength (..),
    responseLBS,
  )
import Network.Wai.Handler.Warp qualified as Warp

{- Note [Reload route]

POST api/reload, body {} (D-351). Lists the files again, builds a new
generation, swaps it in (Note [Live] in "HaskellReviewer.Session.Live")
and answers {"generation":n} once the new workers are started. Browse
mode and --diff REV; not a commit range, where nothing can change
(D-352).

Answers, in this order (the Origin check comes before, Note [Request
checks] in "HaskellReviewer.Server.Security"):

- 405 with Allow: POST for any other method, HEAD too;
- 404 in a mode without reload (a commit range);
- 413 if Content-Length is over 4 KiB (the body isn't read);
- 415 if the Content-Type isn't application/json;
- 413 if the body is over 4 KiB after all (chunked);
- 400 if the body isn't an empty JSON object;
- then we wait for the reload: 200 {"generation":n}. If it didn't
  work, the old generation stays: 500 "cannot list files" when a side
  can't be listed, 500 "cannot reload" when the build crashed (a bug,
  logged). 503 "the server is stopping" on the way out, and 503
  "reload is not available: <why>" if the listing thread crashed while
  the server goes on (logged too).

POSTs that come together share one reload (Note [Reload queue] in
"HaskellReviewer.Session.Reload"). A reload can take seconds, so warp's
timeout is paused while we wait.
-}

-- | @POST api\/reload@. 'Nothing': the mode has no reload. See Note
-- [Reload route].
reloadApp :: Maybe (IO ReloadOutcome) -> Application
reloadApp reload req respond
  | req.requestMethod /= methodPost = respond (methodNotAllowed "POST")
  | otherwise = case reload of
      Nothing -> respond notFound
      Just go -> post go
  where
    post go
      | KnownLength n <- req.requestBodyLength,
        n > fromIntegral bodyLimit =
          respond tooLarge
      | not (isJson (lookup hContentType req.requestHeaders)) =
          respond (plain status415 "the body must be application/json")
      | otherwise =
          readBody req >>= \case
            Nothing -> respond tooLarge
            Just body
              | emptyObject body -> do
                  Warp.pauseTimeout req
                  respond . answer =<< go
              | otherwise -> respond (plain status400 "the body must be {}")
    answer = \case
      Reloaded g -> reloaded g
      ReloadFailed -> plain status500 "cannot list files"
      ReloadCrashed _ -> plain status500 "cannot reload"
      ReloadStopping -> plain status503 "the server is stopping"
      ReloadUnavailable why ->
        plain status503 ("reload is not available: " <> textBytes why)
    reloaded g =
      let body = encodeReloaded g
       in responseLBS
            status200
            [ (hContentType, "application/json"),
              (hContentLength, Char8.pack (show (LBS.length body)))
            ]
            body

-- | @{}@, maybe with spaces.
emptyObject :: ByteString -> Bool
emptyObject body = case Aeson.decodeStrict body of
  Just (Aeson.Object o) -> KeyMap.null o
  _ -> False

textBytes :: Text -> LBS.ByteString
textBytes = LBS.fromStrict . Text.Encoding.encodeUtf8

-- | Run this route once of each kind, on fake requests with a fake
-- reload, and the encoders. So their shared values are built before
-- request threads race on them (Q-RTS-BLACKHOLE). Call it before
-- serving, like warmUpPrefs in "HaskellReviewer.Server.Prefs".
warmUpReload :: IO ()
warmUpReload = do
  void (evaluate (BS.length (encodeGeneration (Generation 2))))
  mapM_
    once
    [ (Just (Reloaded (Generation 2)), "POST", json, "{}"),
      (Just ReloadFailed, "POST", json, " { } "),
      (Just (ReloadCrashed "x"), "POST", json, "{}"),
      (Just ReloadStopping, "POST", json, "{}"),
      (Just (ReloadUnavailable "x"), "POST", json, "{}"),
      (Nothing, "POST", json, "{}"),
      (Just ReloadFailed, "POST", json, "[]"),
      (Just ReloadFailed, "POST", json, "{\"a\":1}"),
      (Just ReloadFailed, "POST", json, "{"),
      (Just ReloadFailed, "POST", json, BS.replicate (bodyLimit + 1) 32),
      (Just ReloadFailed, "POST", [], "{}"),
      (Just ReloadFailed, "GET", [], "")
    ]
  where
    json = [(hContentType, "application/json")]
    once (outcome, method, headers, body) =
      callOnce (reloadApp (pure <$> outcome)) method headers body
