-- | Request bodies of the POST routes, and their plain answers (D-198).
module HaskellReviewer.Server.Body
  ( -- * Bodies
    bodyLimit,
    readBody,
    isJson,

    -- * Answers
    tooLarge,
    plain,

    -- * Warm-ups
    callOnce,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LChar8
import Data.Char (toLower)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Network.HTTP.Types (Header, Method, Status, hContentType, status413)
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

-- | The biggest POST body we take, in bytes: 4 KiB.
bodyLimit :: Int
bodyLimit = 4 * 1024

-- | The body, up to 'bodyLimit'. 'Nothing' if there is more.
readBody :: Request -> IO (Maybe ByteString)
readBody req = go 0 []
  where
    go size acc = do
      chunk <- getRequestBodyChunk req
      next (size + BS.length chunk) chunk acc
    next size chunk acc
      | BS.null chunk = pure (Just (BS.concat (reverse acc)))
      | size > bodyLimit = pure Nothing
      | otherwise = go size (chunk : acc)

-- | @application/json@, maybe with parameters.
isJson :: Maybe ByteString -> Bool
isJson = \case
  Nothing -> False
  Just ct ->
    Char8.map toLower (Char8.strip (Char8.takeWhile (/= ';') ct)) == "application/json"

-- | The 413 for a body over 'bodyLimit'.
tooLarge :: Response
tooLarge = plain status413 ("the body is over " <> kib <> " KiB")
  where
    kib = LChar8.pack (show (bodyLimit `div` 1024))

-- | A plain-text answer.
plain :: Status -> LBS.ByteString -> Response
plain st = responseLBS st [(hContentType, "text/plain; charset=utf-8")]

-- | Run the app once on a fake request with this method, headers and
-- body, and read the whole answer. For warm-ups (Q-RTS-BLACKHOLE).
callOnce :: Application -> Method -> [Header] -> ByteString -> IO ()
callOnce app method headers body = do
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
  ResponseReceived <- app req $ \resp -> do
    let (_, _, withBody) = responseToStream resp
    out <- newIORef mempty
    withBody $ \streaming ->
      streaming (\b -> modifyIORef' out (<> b)) (pure ())
    void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
    pure ResponseReceived
  pure ()
