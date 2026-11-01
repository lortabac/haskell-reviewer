-- | The search route, @api/search/<side>@ (D-207).
module HaskellReviewer.Server.Search
  ( -- * Route
    searchApp,

    -- * Warm-up
    warmUpSearch,
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Modules (ModuleId (..))
import HaskellReviewer.Core.NameTable (NameEntry (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.RelPath (relPathFromBytes)
import HaskellReviewer.Core.Search
  ( Indexing (..),
    ModuleCandidate (..),
    SearchResult (..),
    forceCandidates,
    maxQueryLength,
    parseQuery,
    search,
    searchLimit,
  )
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Server.Api (FilesApi (..))
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (encodeSearch)
import HaskellReviewer.Session.Index (Progress (..), readNames, readProgress)
import HaskellReviewer.Session.Snapshot (Generation, Snapshot (..), firstGeneration)
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

{- Note [Search route]

D-207. GET api/search/<side>?q=<text>. The only side is tree; another
one is a plain 404. Other methods, HEAD too: 405 with Allow: GET.

q is read with parseQueryReplacePlus False: a + stays a + (an
operator), only %20 is a space. The first q counts. No q, or a blank
one: 200 with no results. Errors are plain text: 400 "bad query" if q
isn't UTF-8, 400 "query too long" if it's over 128 code points, 500
"cannot list files" if the listing failed. Else 200 with
'encodeSearch'.

It waits for the snapshot, never for indexing: it searches the names
the index has now, and says partial if indexing isn't done. The names
and the progress are read in one transaction, so they agree.
-}

-- | @GET api\/search\/\<side>?q=…@. See Note [Search route].
searchApp :: FilesApi -> Text -> Application
searchApp api = searchWith (fmap fromSnapshot <$> api.snapshot)

-- | Run this route once of each kind on fake requests and a small fake
-- index, so the shared values of the query parser, the scorer and the
-- encoder are built before request threads race on them
-- (Q-RTS-BLACKHOLE). Call it before serving, like warmUpGzip (Note
-- [Compression] in "HaskellReviewer.Server.Router").
warmUpSearch :: IO ()
warmUpSearch = do
  void (evaluate (forceCandidates fakeModules))
  mapM_
    (once (pure (Right fakeSource)))
    [ ("GET", "tree", q)
    | q <-
        [ "?q=insert",
          "?q=Data.Map.insert",
          "?q=M.ins",
          "?q=Map.%21",
          "?q=%3C%24%3E",
          "?q=a+b",
          "?q=na%C3%AFve",
          "?q=port",
          "?q=x",
          -- Just and Unit: a con and a pat hit.
          "?q=u",
          -- JSON escapes, in the query and in a name.
          "?q=%22%5C",
          "?q=%5C%5C",
          "?q=a",
          "?q=aaab",
          "?q=zzz",
          "?q=",
          "?q=%20",
          "",
          "?x=1&q=Map&q=other",
          "?q=" <> Char8.replicate (maxQueryLength + 1) 'a',
          "?q=%FF"
        ]
    ]
  once (pure (Right fakeSource)) ("POST", "tree", "?q=a")
  once (pure (Right fakeSource)) ("GET", "base", "?q=a")
  once (pure (Left (ListCrashed "x"))) ("GET", "tree", "?q=a")
  where
    once source (method, side, query) = do
      let req = defaultRequest {requestMethod = method, rawQueryString = query}
      ResponseReceived <- searchWith source side req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        out <- newIORef mempty
        withBody $ \streaming ->
          streaming (\b -> modifyIORef' out (<> b)) (pure ())
        void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
        pure ResponseReceived
      pure ()

-- * Internal

-- | What a search reads from a snapshot.
data Source = Source
  { generation :: Generation,
    modules :: Map ModuleId ModuleCandidate,
    current :: STM Current
  }

-- | The names so far, and how far indexing is: read together.
data Current = Current
  { names :: Map ModuleId [NameEntry],
    indexing :: Indexing
  }

-- | Why q is a 400. See Note [Search route].
data QueryError = NotUtf8 | TooLong

renderQueryError :: QueryError -> LBS.ByteString
renderQueryError = \case
  NotUtf8 -> "bad query"
  TooLong -> "query too long"

fromSnapshot :: Snapshot -> Source
fromSnapshot snap =
  Source
    { generation = snap.generation,
      modules = snap.searchModules,
      current = do
        ns <- readNames snap.index
        p <- readProgress snap.index
        pure
          Current
            { names = ns,
              indexing = if p.ready + p.failed < p.total then Partial else Complete
            }
    }

searchWith :: IO (Either ListError Source) -> Text -> Application
searchWith source side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise = case queryText req of
      Left why -> respond (plain status400 (renderQueryError why))
      Right raw ->
        source >>= \case
          Left _ -> respond (plain status500 "cannot list files")
          Right src -> do
            now <- atomically src.current
            let result = case parseQuery raw of
                  Just q -> search searchLimit q src.modules now.names
                  Nothing -> SearchResult {matches = 0, hits = []}
            respond (json (encodeSearch src.generation raw now.indexing result))

-- | q, empty if there is none. See Note [Search route].
queryText :: Request -> Either QueryError Text
queryText req = case lookup "q" (parseQueryReplacePlus False req.rawQueryString) of
  Just (Just bytes) -> case Text.Encoding.decodeUtf8' bytes of
    Left _ -> Left NotUtf8
    Right t
      | Text.length t > maxQueryLength -> Left TooLong
      | otherwise -> Right t
  _ -> Right ""

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

-- * Warm-up data

-- | A few loose modules with a name of each kind, a non-ASCII one, one
-- with a JSON escape and one long enough for the V1 scorer.
fakeSource :: Source
fakeSource =
  Source
    { generation = firstGeneration,
      modules = fakeModules,
      current = pure Current {names = fakeNames, indexing = Partial}
    }

fakeModules :: Map ModuleId ModuleCandidate
fakeModules =
  Map.fromList
    [ (c.id, c)
    | (path, name) <- [("Data/Map.hs", "Data.Map"), ("x/y z.hs", "x/y z.hs")],
      Just p <- [relPathFromBytes path],
      let c = ModuleCandidate {id = LooseModule p, name = name, ref = name}
    ]

fakeNames :: Map ModuleId [NameEntry]
fakeNames = Map.fromList [(i, entries) | i <- Map.keys fakeModules]
  where
    entries =
      [ named DType "Map",
        named DClass "Container",
        named DCon "Just",
        named DPat "Unit",
        named DVal "insert",
        named DVal "<$>",
        named DVal "!",
        named DVal "\\\\",
        named DVal "naïve",
        named DVal (Text.replicate 2000 "a" <> "b"),
        field "port"
      ]
        -- Over 'searchLimit' hits for x: the full-map path runs too.
        <> [named DVal ("x" <> Text.pack (show i)) | i <- [1 :: Int .. 60]]
    named ns n = NameEntry {name = n, decl = Named ns n, exported = True}
    field n = NameEntry {name = n, decl = Field "Config" n, exported = False}
