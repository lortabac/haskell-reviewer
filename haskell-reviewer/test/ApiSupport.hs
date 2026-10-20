-- | Shared by the API tests: a fixed fake project, and calling the app
-- in process.
module ApiSupport
  ( -- * The fixture project
    fixtureApi,

    -- * Calling the app
    ApiResult (..),
    getApi,
    requestApi,
    callApp,
    apiToken,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    textLimit,
  )
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
  )
import HaskellReviewer.Core.RelPath (relPathFromBytes, relPathText, resolveLinkTarget)
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot)
import HaskellReviewer.Server.Events (EventStream (..))
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Session.Clients (newClients)
import HaskellReviewer.Session.Snapshot (Snapshot)
import Network.HTTP.Types (Header, Status (..))
import Network.Wai (Application, Request (..), defaultRequest, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Tasty.HUnit (assertFailure)

-- * The fixture project

-- | A fixed snapshot and file contents. The API fixtures come from it.
fixtureApi :: FilesApi
fixtureApi =
  FilesApi
    { snapshot = pure (Right fixtureSnapshot),
      readEntry = \_ p _ ->
        pure (maybe (Left ReadGone) Right (lookup (relPathText p) contents)),
      logger = newLogger LevelInfo (\_ -> pure ())
    }

fixtureSnapshot :: Snapshot
fixtureSnapshot =
  newSnapshot
    CanonicalRoot {path = "/fixture", bytes = "/fixture"}
    FileList
      { source = FromGit,
        entries = Map.fromList (mapMaybe entry entries),
        skippedNames = 0,
        truncated = False
      }
  where
    entry (p, k) = (,k) <$> relPathFromBytes p
    entries =
      [ ("app/Main.hs", EntryFile),
        ("assets/logo.png", EntryFile),
        ("data/big.csv", EntryFile),
        ("docs/guide.md", EntrySymlink),
        ("docs/old.md", EntrySymlink),
        ("shared/guide.md", EntryFile),
        ("vendor/dep", EntrySubmodule commit),
        ("vendor/nested", EntryNestedRepo)
      ]

commit :: CommitId
commit = CommitId "0123456789abcdef0123456789abcdef01234567"

-- | What readEntry gives, by path.
contents :: [(Text, FileContent)]
contents =
  [ ( "app/Main.hs",
      TextContent
        "module Main (main) where\r\n\
        \\r\n\
        \-- <script>alert(1)</script>\r\n\
        \main :: IO ()\r\n\
        \main = putStrLn \"héllo, wörld → ✓\"\r\n"
    ),
    ("assets/logo.png", Binary (FileSize 2048)),
    ( "data/big.csv",
      TooLarge TooLargeInfo {size = FileSize 5_242_880, limit = textLimit}
    ),
    ("docs/guide.md", link "docs/guide.md" "../shared/guide.md"),
    -- Points under ROOT, but not to a listed file: resolved is null.
    ("docs/old.md", link "docs/old.md" "gone.md"),
    ("vendor/dep", Submodule commit),
    ("vendor/nested", NestedRepo)
  ]
  where
    link p t =
      Link
        LinkInfo
          { target = t,
            resolved =
              relPathFromBytes p >>= (`resolveLinkTarget` Text.Encoding.encodeUtf8 t)
          }

-- * Calling the app

-- | Status, headers and body of one request.
data ApiResult = ApiResult
  { status :: Int,
    headers :: [Header],
    body :: LBS.ByteString
  }

-- | GET through the real app in process, with the given file routes.
-- The path is after the token.
getApi :: FilesApi -> [Text] -> IO ApiResult
getApi = requestApi "GET"

-- | Like 'getApi', with the method given.
requestApi :: ByteString -> FilesApi -> [Text] -> IO ApiResult
requestApi method files path = do
  clients <- newClients
  let events =
        EventStream
          { clients = clients,
            keepAlive = Microseconds 1_000_000,
            logger = newLogger LevelInfo (\_ -> pure ())
          }
      app = application (Port 8080) apiToken Routes {events = events, files = files}
  callApp app method "127.0.0.1:8080" (tokenText apiToken : path)

-- | Call an app with a request that ends. The path has the token.
callApp :: Application -> ByteString -> ByteString -> [Text] -> IO ApiResult
callApp app method host path = do
  let req =
        defaultRequest
          { requestMethod = method,
            requestHeaderHost = Just host,
            pathInfo = path
          }
  saved <- newIORef Nothing
  ResponseReceived <- app req $ \resp -> do
    writeIORef saved (Just resp)
    pure ResponseReceived
  readIORef saved >>= \case
    Nothing -> assertFailure "no response"
    Just resp -> do
      let (st, hs, withBody) = responseToStream resp
      chunks <- newIORef mempty
      withBody $ \streaming -> streaming (\b -> modifyIORef' chunks (<> b)) (pure ())
      b <- readIORef chunks
      pure
        ApiResult
          { status = statusCode st,
            headers = hs,
            body = Builder.toLazyByteString b
          }

-- | The token of 'getApi'.
apiToken :: Token
apiToken = tokenFromBytes "0123456789abcdefghijklmn"
