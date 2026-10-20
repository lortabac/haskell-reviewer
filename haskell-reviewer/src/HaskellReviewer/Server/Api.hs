-- | The file list and file text routes.
module HaskellReviewer.Server.Api
  ( -- * Types
    FilesApi (..),

    -- * Snapshots
    newSnapshot,

    -- * Routes
    filesApp,
    fileApp,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.FileContent (FileContent (..), LinkInfo (..))
import HaskellReviewer.Core.FileList (EntryKind, FileList (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Core.Root (CanonicalRoot)
import HaskellReviewer.Effects.FileSystem (ListError, ReadError (..))
import HaskellReviewer.Effects.Log (Logger, logInfo)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (encodeFileList, encodeFileView)
import HaskellReviewer.Session.Snapshot (Snapshot (..), firstGeneration)
import Network.HTTP.Types
  ( hContentLength,
    hContentType,
    methodGet,
    status200,
    status500,
  )
import Network.Wai (Application, Request (..), Response, responseLBS)

-- | What the file routes need. Tests plug in fakes.
data FilesApi = FilesApi
  { -- | The snapshot. Waits until the list is made.
    snapshot :: IO (Either ListError Snapshot),
    -- | Read one entry from disk. See Note [File containment] in
    -- "HaskellReviewer.Core.RelPath".
    readEntry :: Snapshot -> RelPath -> EntryKind -> IO (Either ReadError FileContent),
    -- | For read errors.
    logger :: Logger
  }

-- | The snapshot of a file list, with its JSON body ready.
newSnapshot :: CanonicalRoot -> FileList -> Snapshot
newSnapshot root fl =
  Snapshot
    { generation = firstGeneration,
      root = root,
      files = fl,
      fileListJson = encodeFileList firstGeneration fl
    }

-- | @GET api\/files\/\<side>@: the file list. The only side is @tree@.
filesApp :: FilesApi -> Text -> Application
filesApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> respond (json snap.fileListJson)

-- | @GET api\/file\/\<side>\/\<path segments>@: one file of the list.
-- Anything not in the list is a 404. See Note [File containment] in
-- "HaskellReviewer.Core.RelPath".
fileApp :: FilesApi -> Text -> [Text] -> Application
fileApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | Just path <- relPathFromSegments segs =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case Map.lookup path snap.files.entries of
          Nothing -> respond notFound
          Just kind ->
            api.readEntry snap path kind >>= \case
              Left ReadGone -> respond notFound
              Left (ReadFailed why) -> do
                logInfo api.logger $
                  "haskell-reviewer: cannot read " <> relPathText path <> ": " <> why
                respond (serverError "cannot read file")
              Right content ->
                respond (json (encodeFileView snap.generation (inSnapshot snap content)))
  | otherwise = respond notFound

-- * Internal

-- | A symlink target is shown as resolved only if it's in the list.
inSnapshot :: Snapshot -> FileContent -> FileContent
inSnapshot snap = \case
  Link info ->
    Link info {resolved = keep =<< info.resolved}
  other -> other
  where
    keep p
      | Map.member p snap.files.entries = Just p
      | otherwise = Nothing

-- | No Cache-Control: Security adds no-store.
json :: LBS.ByteString -> Response
json body =
  responseLBS
    status200
    [ (hContentType, "application/json"),
      (hContentLength, Char8.pack (show (LBS.length body)))
    ]
    body

-- | The listing failed. The reason is on stderr.
cannotList :: Response
cannotList = serverError "cannot list files"

serverError :: LBS.ByteString -> Response
serverError =
  responseLBS status500 [(hContentType, "text/plain; charset=utf-8")]
