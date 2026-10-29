-- | The routes that read the snapshot: file list, file text, module
-- tree, one module, indexing progress.
module HaskellReviewer.Server.Api
  ( -- * Types
    FilesApi (..),

    -- * Snapshots
    newSnapshot,

    -- * Routes
    filesApp,
    fileApp,
    modulesApp,
    moduleApp,
    indexingApp,

    -- * Internal, for tests
    cborWarmUp,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Exception (evaluate)
import Control.Monad (guard, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    FoundModule (..),
    GeneratedFile (..),
    HaskellFile (..),
    ModuleIndex (..),
    ModuleSource (..),
    PackageModules (..),
    PreprocessedFile (..),
    buildModuleIndex,
    findModule,
    forceSettings,
    resolveModuleRef,
  )
import HaskellReviewer.Core.Project (GhcVersion (..), Project)
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Core.Root (CanonicalRoot)
import HaskellReviewer.Effects.FileSystem (ListError, ReadError (..))
import HaskellReviewer.Effects.Log (Logger, logInfo)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire
  ( ModuleAnswer (..),
    ModuleView (..),
    NotParsed (..),
    encodeFileList,
    encodeFileView,
    encodeIndexing,
    encodeModuleNotFound,
    encodeModuleTree,
    encodeModuleView,
  )
import HaskellReviewer.Session.Index
  ( IndexFailure (..),
    Indexed (..),
    awaitModule,
    newIndex,
    readProgress,
  )
import HaskellReviewer.Session.Snapshot (Snapshot (..), firstGeneration)
import Network.HTTP.Types
  ( Status,
    hContentLength,
    hContentType,
    methodGet,
    status200,
    status404,
    status500,
  )
import Network.Wai (Application, Request (..), Response, responseLBS)

-- | What the snapshot routes need. Tests plug in fakes.
data FilesApi = FilesApi
  { -- | The snapshot. Waits until the list is made.
    snapshot :: IO (Either ListError Snapshot),
    -- | Read one entry from disk. See Note [File containment] in
    -- "HaskellReviewer.Core.RelPath".
    readEntry :: Snapshot -> RelPath -> EntryKind -> IO (Either ReadError FileContent),
    -- | For read errors.
    logger :: Logger
  }

{- Note [Forced snapshot]

On GHC 9.12 and 9.14, threads that force the same thunk at once can
livelock the RTS (Q-RTS-BLACKHOLE). Request threads and parse workers
all read the same snapshot. So 'newSnapshot' builds it fully
('forceSnapshot', 'newIndex') before the listing thread fills the slot:
the other threads then find no thunk to force. The listing thread also
warms up the parser before the fill (warmUpParser), so the workers find
the parser's shared values built too. 'newSnapshot' also encodes a file
view of each kind ('cborWarmUp'), so the shared values of cborg and of
our CBOR code are built before request threads race on them. The gzip
path gets the same before warp listens (Note [Compression] in
"HaskellReviewer.Server.Router").

What the workers put in the slots is forced before it gets there (Note
[Indexing] in "HaskellReviewer.Session.Index").
-}

-- | The snapshot of a file list and its project, with the JSON bodies
-- and an index with every module Pending. Fully built before it
-- returns. See Note [Forced snapshot].
newSnapshot :: CanonicalRoot -> FileList -> Project -> IO Snapshot
newSnapshot root fl project = do
  let modules = buildModuleIndex project fl
      fileListJson = encodeFileList firstGeneration fl
      moduleTreeJson = encodeModuleTree firstGeneration modules
  forceSnapshot modules fileListJson moduleTreeJson
  mapM_ (evaluate . LBS.length . encodeFileView firstGeneration) cborWarmUp
  index <- newIndex modules
  pure
    Snapshot
      { generation = firstGeneration,
        root = root,
        files = fl,
        modules = modules,
        fileListJson = fileListJson,
        moduleTreeJson = moduleTreeJson,
        index = index
      }

-- | Force both JSON bodies, the module index (strict fields), every
-- list of its name maps, the id set and the parse settings. The JSON
-- walks the rest of the index. See Note [Forced snapshot].
forceSnapshot :: ModuleIndex -> LBS.ByteString -> LBS.ByteString -> IO ()
forceSnapshot idx fileListJson moduleTreeJson = do
  void (evaluate idx)
  mapM_ (mapM_ evaluate) (Map.elems idx.ownedByName)
  mapM_ (mapM_ evaluate) (Map.elems idx.looseByName)
  void (evaluate (Set.size idx.ids))
  let GhcVersion ghc = idx.ghc
  mapM_ evaluate ghc
  mapM_
    (\c -> evaluate (forceSettings c.settings))
    [c | p <- idx.packages, c <- p.components]
  void (evaluate (LBS.length fileListJson))
  void (evaluate (LBS.length moduleTreeJson))

-- | @GET api\/files\/\<side>@: the file list. The only side is @tree@.
filesApp :: FilesApi -> Text -> Application
filesApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> respond (json snap.fileListJson)

-- | @GET api\/modules\/\<side>@: the module tree. The only side is
-- @tree@.
modulesApp :: FilesApi -> Text -> Application
modulesApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> respond (json snap.moduleTreeJson)

-- | @GET api\/file\/\<side>\/\<path segments>@: one file of the list,
-- as CBOR ('encodeFileView'). Anything not in the list is a 404. See
-- Note [File containment] in "HaskellReviewer.Core.RelPath".
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
              Left (ReadFailed why) -> respond =<< readFailed api path why
              Right content ->
                respond (cbor (encodeFileView snap.generation (inSnapshot snap content)))
  | otherwise = respond notFound

-- | @GET api\/module\/\<side>\/\<ref segments>@: one module. The
-- segments are joined with @/@, so a loose module's path works too.
--
-- A module we parse comes from its slot: if it's not done, it jumps the
-- queue and we wait for it (Note [Indexing] in
-- "HaskellReviewer.Session.Index"). Generated, preprocessed and
-- no-source modules answer from the module index, without reading
-- anything.
--
-- An unknown ref, or a module whose file is gone, is a 404 with a JSON
-- body ('encodeModuleNotFound'). A bad side is a plain 404.
moduleApp :: FilesApi -> Text -> [Text] -> Application
moduleApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case found snap of
          Nothing -> respond (missing snap)
          Just (i, m) -> case m.source of
            HaskellSource f -> fromIndex snap i f.path
            GeneratedSource f -> answer snap i (Just f.path) (GeneratedModule f.tool)
            PreprocessedSource f ->
              answer snap i (Just f.path) (PreprocessedModule f.tool)
            NoSource r -> answer snap i Nothing (NoSourceModule r)
  where
    found snap = do
      -- An escaped / (%2F) in a segment: not a path, as on the file route.
      guard (not (any (Text.elem '/') segs))
      i <- resolveModuleRef snap.modules (Text.intercalate "/" segs)
      m <- findModule snap.modules i
      Just (i, m)
    missing snap = jsonWith status404 (encodeModuleNotFound snap.generation)
    view snap v = respond (json (encodeModuleView snap.generation snap.modules v))
    answer snap i path r =
      view snap ModuleView {moduleId = i, path = path, answer = NotParsed r}
    -- The worker logged read errors and crashes already.
    fromIndex snap i path =
      awaitModule snap.index i >>= \case
        Just (Right (IndexedParse r)) ->
          view snap ModuleView {moduleId = i, path = Just path, answer = FromParser r}
        Just (Right (IndexedNotText size)) ->
          answer snap i (Just path) (NotTextModule size)
        Just (Right (IndexedTooLarge info)) ->
          answer snap i (Just path) (TooLargeModule info)
        Just (Left SourceGone) -> respond (missing snap)
        Just (Left (SourceUnreadable _)) -> respond (serverError "cannot read file")
        Just (Left (IndexCrashed _)) -> respond (serverError "cannot index module")
        -- Every Haskell source has a slot.
        Nothing -> respond (missing snap)

-- | @GET api\/indexing\/\<side>@: how far indexing is. The only side is
-- @tree@. The same body as the @indexing@ event.
indexingApp :: FilesApi -> Text -> Application
indexingApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> do
          p <- atomically (readProgress snap.index)
          respond (json (LBS.fromStrict (encodeIndexing snap.generation p)))

-- * Internal

-- | A file view of each kind. See Note [Forced snapshot].
cborWarmUp :: [FileContent]
cborWarmUp =
  [ TextContent "x",
    Binary (FileSize 1),
    TooLarge TooLargeInfo {size = FileSize 2, limit = FileSize 1},
    Link LinkInfo {target = "x", resolved = Nothing},
    Submodule (CommitId "0"),
    NestedRepo
  ]

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

-- | A 200 JSON body. No Cache-Control: Security adds no-store.
json :: LBS.ByteString -> Response
json = jsonWith status200

-- | A JSON body with this status.
jsonWith :: Status -> LBS.ByteString -> Response
jsonWith = withLength "application/json"

-- | A 200 CBOR body.
cbor :: LBS.ByteString -> Response
cbor = withLength "application/cbor" status200

-- | With a Content-Length, so warp doesn't send it chunked (gzip may
-- still do). See Note [Compression] in "HaskellReviewer.Server.Router".
withLength :: ByteString -> Status -> LBS.ByteString -> Response
withLength contentType st body =
  responseLBS
    st
    [ (hContentType, contentType),
      (hContentLength, Char8.pack (show (LBS.length body)))
    ]
    body

-- | A read error: logged, then a 500.
readFailed :: FilesApi -> RelPath -> Text -> IO Response
readFailed api path why = do
  logInfo api.logger $
    "haskell-reviewer: cannot read " <> relPathText path <> ": " <> why
  pure (serverError "cannot read file")

-- | The listing failed. The reason is on stderr.
cannotList :: Response
cannotList = serverError "cannot list files"

serverError :: LBS.ByteString -> Response
serverError =
  responseLBS status500 [(hContentType, "text/plain; charset=utf-8")]
