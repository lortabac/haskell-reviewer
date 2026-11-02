-- | A minimal raw HTTP\/1.1 client, for the benchmark harness only.
--
-- Copied (and trimmed) from the request\/response code in "ExeTest"
-- (@haskell-reviewer\/test\/ExeTest.hs@): same wire format, same
-- dechunking, but errors come back as 'Either' since this isn't a tasty
-- test and has no 'Test.Tasty.HUnit.assertFailure' to call. Plus gzip,
-- like a browser: timed requests ask for it.
module Bench.Http
  ( HttpResponse (..),
    Gzip (..),
    requestBytes,
    rawHttp,
  )
where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, bracket, evaluate, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Numeric qualified
import System.Timeout (timeout)

-- | A parsed HTTP response.
data HttpResponse = HttpResponse
  { status :: Int,
    -- | Names in lower case.
    headers :: [(String, ByteString)],
    -- | Already gunzipped, if it came gzipped.
    body :: ByteString,
    -- | Bytes of the body on the wire (before gunzip).
    wireBytes :: Int
  }

-- | Ask for a gzipped answer?
data Gzip = Gzip | NoGzip
  deriving (Eq, Show)

-- | An HTTP\/1.1 GET-like request with @Connection: close@.
requestBytes :: ByteString -> Maybe String -> Gzip -> String -> ByteString
requestBytes method host gzip path =
  method
    <> " "
    <> Char8.pack path
    <> " HTTP/1.1\r\n"
    <> maybe "" (\h -> "Host: " <> Char8.pack h <> "\r\n") host
    <> (if gzip == Gzip then "Accept-Encoding: gzip\r\n" else "")
    <> "Connection: close\r\n\r\n"

-- | Send the bytes, read the answer up to EOF. Up to 120 s (a module
-- page can wait for indexing on the large reference project).
rawHttp :: Int -> ByteString -> IO (Either String HttpResponse)
rawHttp port req = do
  result <-
    timeout 120000000 $
      bracket open Socket.close $ \s -> do
        Socket.BS.sendAll s req
        readAll s []
  case result of
    Nothing -> pure (Left "no HTTP answer within 120 s")
    Just bytes -> case parseResponse bytes of
      Nothing -> pure (Left ("bad HTTP answer: " <> show (BS.take 200 bytes)))
      Just r -> gunzip r
  where
    open = do
      s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.connect s addr
      pure s
    addr =
      Socket.SockAddrInet
        (fromIntegral port)
        (Socket.tupleToHostAddress (127, 0, 0, 1))
    -- Chunks in reverse, joined once: big answers (a module page) come
    -- in many chunks.
    readAll s acc = do
      chunk <- Socket.BS.recv s 65536
      if BS.null chunk then pure (BS.concat (reverse acc)) else readAll s (chunk : acc)

-- | Undo @Content-Encoding: gzip@.
gunzip :: HttpResponse -> IO (Either String HttpResponse)
gunzip r = case lookup "content-encoding" r.headers of
  Just "gzip" -> do
    let plain = LBS.toStrict (GZip.decompress (LBS.fromStrict r.body))
    out <- try @SomeException (evaluate plain)
    pure $ case out of
      Left e -> Left ("bad gzip body: " <> show e)
      Right b -> Right r {body = b}
  _ -> pure (Right r)

-- | Status line, headers and body (chunked or not).
parseResponse :: ByteString -> Maybe HttpResponse
parseResponse bytes = do
  let (head', rest) = BS.breakSubstring "\r\n\r\n" bytes
  statusLine : headerLines <- Just (splitCrlf head')
  _ : code : _ <- Just (Char8.words statusLine)
  (status, _) <- Char8.readInt code
  let headers = map header headerLines
      raw = BS.drop 4 rest
  body <- case lookup "transfer-encoding" headers of
    Just "chunked" -> BS.concat <$> dechunk raw
    _ -> Just raw
  pure
    HttpResponse
      { status = status,
        headers = headers,
        body = body,
        wireBytes = BS.length body
      }
  where
    dechunk s = do
      let (sizeLine, afterSize) = BS.breakSubstring "\r\n" s
      size <- readHex (Char8.unpack (Char8.takeWhile (/= ';') sizeLine))
      let chunk = BS.take size (BS.drop 2 afterSize)
      if size == 0
        then Just []
        else (chunk :) <$> dechunk (BS.drop (size + 4) afterSize)
    readHex str = case Numeric.readHex str of
      [(n, "")] -> Just n
      _ -> Nothing
    header l =
      let (name, value) = Char8.break (== ':') l
       in (map toLower (Char8.unpack name), Char8.dropWhile (== ' ') (BS.drop 1 value))
    splitCrlf s
      | BS.null s = []
      | otherwise =
          let (l, r) = BS.breakSubstring "\r\n" s
           in l : splitCrlf (BS.drop 2 r)
