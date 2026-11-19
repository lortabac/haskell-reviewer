-- | The text search route, @api/text-search/<side>@ (D-307).
module HaskellReviewer.Server.TextSearch
  ( -- * Route
    textSearchApp,

    -- * Warm-up
    warmUpTextSearch,

    -- * Internal, for tests
    searchFiles,
    textSearchDeadline,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as Vector
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..), ListSource (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromBytes, relPathText)
import HaskellReviewer.Core.Sides (sideTag)
import HaskellReviewer.Core.TextSearch
  ( Needle,
    TextAnswer,
    addFile,
    finishSearch,
    forceTextAnswer,
    maxTextQueryLength,
    needle,
    startSearch,
    textResultsLimit,
  )
import HaskellReviewer.Effects.Clock
  ( Clock (..),
    MonotonicTime,
    Nanoseconds (..),
    elapsed,
    systemClock,
  )
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, logVerbose, newLogger)
import HaskellReviewer.Effects.Source (sharedBlob)
import HaskellReviewer.Server.Api (FilesApi (..), fromSideSnapshot)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (encodeTextSearch)
import HaskellReviewer.Session.Index (withRequest)
import HaskellReviewer.Session.Snapshot (Generation, Snapshot (..), firstGeneration)
import HaskellReviewer.Session.TextStore
  ( Skipped (..),
    StoreError (..),
    TextFile (..),
    TextFiles (..),
    awaitTextFiles,
    loadTextFiles,
    newBlobTexts,
    newTextStore,
    storeBlobTexts,
    storeBytes,
    textStoreBudget,
  )
import Network.HTTP.Types
  ( Status,
    hContentLength,
    hContentType,
    methodGet,
    parseQueryReplacePlus,
    status200,
    status400,
    status500,
  )
import Network.Wai
  ( Application,
    Request (..),
    Response,
    defaultRequest,
    responseLBS,
    responseToStream,
  )
import Network.Wai.Internal (ResponseReceived (..))

{- Note [Text search route]

D-307. GET api/text-search/<side>?q=<text>&limit=<n>. A side the
session doesn't have is a plain 404 (D-313). Other methods, HEAD too: 405 with
Allow: GET.

Params are read with parseQueryReplacePlus False: a + stays a +, only
%20 is a space. The first of each counts. q is the text as is:
spaces are kept, so q=%20 looks for a space. No q, or an empty one: 200
with no results, and nothing is read. limit: at most this many results,
1 or more; past 'textResultsLimit' (the default) it is cut to it.

Errors are plain text: 400 "bad query" if q isn't UTF-8 or limit isn't
a number of 1 or more, 400 "query too long" if q is over 128 code
points, 500 "cannot list files" if the listing failed, 500 "cannot
read files" if the store's load crashed.

The first text search loads the store (Note [Text store] in
"HaskellReviewer.Session.TextStore"); requests wait for it, outside
withRequest. The search and the encode run in withRequest. Between
files we check the clock: after 'textSearchDeadline' we stop and say
stopped. See Note [Text search] in "HaskellReviewer.Core.TextSearch".
-}

-- | @GET api\/text-search\/\<side>?q=…&limit=…@. See Note [Text search
-- route].
textSearchApp :: FilesApi -> Text -> Application
textSearchApp api side =
  textSearchWith (fromSideSnapshot api side (fromSnapshot api))

-- | How long a search may run: 1 s.
textSearchDeadline :: Nanoseconds
textSearchDeadline = Nanoseconds 1_000_000_000

-- | Search the files in order, checking the clock before each one. See
-- Note [Text search route].
searchFiles ::
  IO MonotonicTime -> Nanoseconds -> Needle -> Int -> TextFiles -> IO TextAnswer
searchFiles now deadline n limit fs = do
  t0 <- now
  let go !i s
        | i >= Vector.length fs.files = pure (finishSearch False s)
        | otherwise = do
            t <- now
            if elapsed t0 t > deadline
              then pure (finishSearch True s)
              else do
                let f = fs.files Vector.! i
                s' <- evaluate (addFile n s f.path f.bytes)
                go (i + 1) s'
  -- Every file: no path filter here.
  go (0 :: Int) (startSearch (const True) limit)

-- | Run the route on fake requests and a fake store, for every kind of
-- query and error, and load a fake store, so the shared values of the
-- loader, the matcher and the encoder are built before request threads
-- race on them (Q-RTS-BLACKHOLE). Call it before serving.
warmUpTextSearch :: IO ()
warmUpTextSearch = do
  table <- newBlobTexts
  fs <- loadTextFiles textStoreBudget table (const Nothing) fakeRead fakeList
  _ <- loadTextFiles 10 table (const Nothing) fakeRead fakeList
  store <- newTextStore table
  _ <- awaitTextFiles silent store (pure fs)
  failing <- newTextStore table
  _ <- awaitTextFiles silent failing (ioError (userError "x"))
  let fake = Just (pure (Right (fakeSource (pure (Right fs)))))
  mapM_
    (once fake "GET")
    [ "?q=main",
      "?q=main&limit=1",
      "?q=e",
      "?q=e&limit=2",
      "?q=a",
      "?q=%CE%BB",
      "?q=%C3%BC",
      "?q=%EF%BB%BFmain",
      "?q=%20",
      "?q=a%20%22b",
      "?q=x+y",
      "?q=%22%5C",
      "?q=a%0Ab",
      "?q=a%0D",
      "?q=zzz",
      "?q=",
      "",
      "?x=1&q=main&q=other&limit=3&limit=x",
      "?q=" <> Char8.replicate (maxTextQueryLength + 1) 'a',
      "?q=%FF",
      "?q=a&limit=0",
      "?q=a&limit=abc",
      "?q=a&limit",
      "?q=a&limit=99999999999999999999"
    ]
  once fake "POST" "?q=a"
  once Nothing "GET" "?q=a"
  once (Just (pure (Right (fakeSource (pure (Left (StoreError "x"))))))) "GET" "?q=a"
  once (Just (pure (Left (ListCrashed "x")))) "GET" "?q=a"
  where
    once found method query = do
      let req = defaultRequest {requestMethod = method, rawQueryString = query}
      ResponseReceived <- textSearchWith found req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        out <- newIORef mempty
        withBody $ \streaming ->
          streaming (\b -> modifyIORef' out (<> b)) (pure ())
        void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
        pure ResponseReceived
      pure ()

-- * Internal

-- | What the route reads from a snapshot. The warm-up and the tests
-- plug in fakes.
data Source = Source
  { generation :: Generation,
    -- | The store's files; waits for the load.
    files :: IO (Either StoreError TextFiles),
    -- | Run the search and the encode ('withRequest').
    compute :: IO LBS.ByteString -> IO LBS.ByteString,
    clock :: IO MonotonicTime
  }

fromSnapshot :: FilesApi -> Snapshot -> Source
fromSnapshot api snap =
  Source
    { generation = snap.generation,
      files = awaitTextFiles api.logger snap.textStore load,
      compute = withRequest snap.index,
      clock = systemClock.monotonic
    }
  where
    load = do
      t0 <- systemClock.monotonic
      let readOne p = api.readEntry snap p EntryFile
      fs <-
        loadTextFiles
          textStoreBudget
          (storeBlobTexts snap.textStore)
          (sharedBlob snap.source)
          readOne
          snap.files
      t1 <- systemClock.monotonic
      let Nanoseconds ns = elapsed t0 t1
          sk = fs.skipped
      logVerbose api.logger $
        sideTag snap.side
          <> "text store: "
          <> showText (Vector.length fs.files)
          <> " files, "
          <> showText (storeBytes fs `div` (1024 * 1024))
          <> " MiB, "
          <> showText (ns `div` 1_000_000)
          <> " ms (not searched: "
          <> showText sk.binary
          <> " binary, "
          <> showText sk.tooLarge
          <> " too large, "
          <> showText sk.unreadable
          <> " unreadable, "
          <> showText sk.overBudget
          <> " over budget)"
      pure fs

-- | Why the params are a 400. See Note [Text search route].
data QueryError = BadQuery | TooLong

renderQueryError :: QueryError -> LBS.ByteString
renderQueryError = \case
  BadQuery -> "bad query"
  TooLong -> "query too long"

-- | The route, over the side's source: 'Nothing' if the session has no
-- such side. See Note [Text search route].
textSearchWith :: Maybe (IO (Either ListError Source)) -> Application
textSearchWith found req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Nothing <- found = respond notFound
  | Just source <- found = case (queryText params, limitOf params) of
      (Left why, _) -> respond (plain status400 (renderQueryError why))
      (_, Left why) -> respond (plain status400 (renderQueryError why))
      (Right q, Right limit) ->
        source >>= \case
          Left _ -> respond (plain status500 "cannot list files")
          Right src -> case needle q of
            Nothing ->
              respond . json $
                encodeTextSearch src.generation q noneSkipped $
                  finishSearch False (startSearch (const True) limit)
            Just n ->
              src.files >>= \case
                Left _ -> respond (plain status500 "cannot read files")
                Right fs -> do
                  body <- src.compute $ do
                    a <- searchFiles src.clock textSearchDeadline n limit fs
                    _ <- evaluate (forceTextAnswer a)
                    b <- evaluate (encodeTextSearch src.generation q fs.skipped a)
                    _ <- evaluate (LBS.length b)
                    pure b
                  respond (json body)
  where
    params = parseQueryReplacePlus False req.rawQueryString
    noneSkipped = Skipped {binary = 0, tooLarge = 0, unreadable = 0, overBudget = 0}

-- | q, empty if there is none. See Note [Text search route].
queryText :: [(ByteString, Maybe ByteString)] -> Either QueryError Text
queryText params = case lookup "q" params of
  Just (Just bytes) -> case Text.Encoding.decodeUtf8' bytes of
    Left _ -> Left BadQuery
    Right t
      | Text.length t > maxTextQueryLength -> Left TooLong
      | otherwise -> Right t
  _ -> Right ""

-- | limit, 'textResultsLimit' if there is none. See Note [Text search
-- route].
limitOf :: [(ByteString, Maybe ByteString)] -> Either QueryError Int
limitOf params = case lookup "limit" params of
  Nothing -> Right textResultsLimit
  Just (Just bytes)
    | not (Char8.null bytes),
      Char8.all (\c -> c >= '0' && c <= '9') bytes,
      Just (n, _) <- Char8.readInteger bytes,
      n >= 1 ->
        Right (fromInteger (min n (toInteger textResultsLimit)))
  _ -> Left BadQuery

-- | With a Content-Length, so warp doesn't send it chunked (gzip may
-- still do).
json :: LBS.ByteString -> Response
json body =
  responseLBS
    status200
    [ (hContentType, "application/json"),
      (hContentLength, Char8.pack (show (LBS.length body)))
    ]
    body

plain :: Status -> LBS.ByteString -> Response
plain st = responseLBS st [(hContentType, "text/plain; charset=utf-8")]

showText :: (Show a) => a -> Text
showText = Text.pack . show

-- * Warm-up data

silent :: Logger
silent = newLogger LevelInfo (\_ -> pure ())

fakeSource :: IO (Either StoreError TextFiles) -> Source
fakeSource fs =
  Source
    { generation = firstGeneration,
      files = fs,
      compute = id,
      clock = systemClock.monotonic
    }

-- | A file of each kind, and texts with a BOM, CRLF, a long line, many
-- hits on a line, non-ASCII text and JSON escapes.
fakeList :: FileList
fakeList =
  FileList
    { source = FromGit,
      entries =
        Map.fromList
          ( mapMaybe
              (\(p, k) -> (,k) <$> relPathFromBytes p)
              ( [(p, EntryFile) | (p, _) <- fakeContents]
                  <> [("gone.txt", EntryFile), ("link.md", EntrySymlink)]
              )
          ),
      skippedNames = 0,
      truncated = False
    }

-- | The fake contents; gone.txt is gone.
fakeRead :: RelPath -> IO (Either ReadError FileContent)
fakeRead p =
  pure (maybe (Left ReadGone) Right (lookup key fakeContents))
  where
    key = Text.Encoding.encodeUtf8 (relPathText p)

fakeContents :: [(ByteString, FileContent)]
fakeContents =
  [ ("a.hs", TextContent "\xFEFFmodule A where\r\nmain = pure ()\r\n"),
    ("b.md", TextContent "main λ ü \"\\\" e e e e\nno hit\nmain\n"),
    ("c.txt", TextContent (Text.replicate 300 "e" <> "main" <> Text.replicate 300 "a")),
    ("d.bin", Binary (FileSize 10)),
    ("e.csv", TooLarge TooLargeInfo {size = FileSize 2_000_000, limit = textLimit})
  ]
