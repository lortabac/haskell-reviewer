-- | The routes that read the snapshot: file list, file text, module
-- tree, one module, indexing progress.
module HaskellReviewer.Server.Api
  ( -- * Types
    FilesApi (..),

    -- * Snapshots
    snapshotFor,
    fromSideSnapshot,
    newSnapshot,
    warmUpSideNames,

    -- * Routes
    filesApp,
    fileApp,
    modulesApp,
    moduleApp,
    sourceApp,
    indexingApp,

    -- * Responses
    json,
    serverError,

    -- * Internal, for tests
    cborWarmUp,
    computed,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Exception (evaluate)
import Control.Monad (guard, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    unfiltered,
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..), FileList (..))
import HaskellReviewer.Core.Markers (Marker)
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    FoundModule (..),
    GeneratedFile (..),
    HaskellFile (..),
    ModuleId,
    ModuleIndex (..),
    ModuleSource (..),
    PackageModules (..),
    PreprocessedFile (..),
    findModule,
    forceSettings,
    moduleAtPath,
    resolveModuleRef,
  )
import HaskellReviewer.Core.Project (GhcVersion (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Core.Resolve (ModuleState (..), ParsedInput (..), isPartial)
import HaskellReviewer.Core.Search (forceCandidates, moduleCandidates)
import HaskellReviewer.Core.Sides (Side, parseSide, sideName, sideTag)
import HaskellReviewer.Core.SourceLinks (LinksInput, linksInput, pageLinks, sourceLinks)
import HaskellReviewer.Effects.FileSystem (ListError, ReadError (..))
import HaskellReviewer.Effects.Log (Logger, logInfo)
import HaskellReviewer.Effects.Source (EntrySource, sharedBlob)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire
  ( encodeFileList,
    encodeFileNotFound,
    encodeFileView,
    encodeIndexing,
    encodeModuleNotFound,
    encodeModuleTree,
  )
import HaskellReviewer.Server.Wire.Page
  ( ModuleAnswer (..),
    ModuleView (..),
    NotParsed (..),
    encodeModulePage,
  )
import HaskellReviewer.Server.Wire.Source
  ( SourceBody (..),
    SourceView (..),
    encodeSource,
    parsedSource,
    textOnlySource,
  )
import HaskellReviewer.Session.Index
  ( IndexFailure (..),
    Indexed (..),
    ParsedSource (..),
    awaitModule,
    newIndex,
    readProgress,
    withRequest,
  )
import HaskellReviewer.Session.Resolve
  ( awaitImports,
    importWait,
    newResolver,
    resolveModuleWith,
  )
import HaskellReviewer.Session.Snapshot
  ( SessionShare (..),
    Snapshot (..),
    firstGeneration,
  )
import HaskellReviewer.Session.TextStore (newTextStore)
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
  { -- | The snapshot of a side, if the session has that side. Waits
    -- until the list is made.
    snapshotOn :: Side -> Maybe (IO (Either ListError Snapshot)),
    -- | Read one entry from the snapshot's source. See Note [Entry
    -- sources] in "HaskellReviewer.Effects.Source".
    readEntry :: Snapshot -> RelPath -> EntryKind -> IO (Either ReadError FileContent),
    -- | What the snapshot's source says about a file (@filtered@).
    -- Production: 'HaskellReviewer.Effects.Source.fileNotes'.
    fileNotes :: Snapshot -> RelPath -> FileNotes,
    -- | For read errors.
    logger :: Logger
  }

-- | The snapshot for the side in a URL. 'Nothing' if it's not a side
-- of this session: a plain 404, on every route (D-313).
snapshotFor :: FilesApi -> Text -> Maybe (IO (Either ListError Snapshot))
snapshotFor api t = parseSide t >>= api.snapshotOn

-- | Build the side names' shared values (the texts 'parseSide' matches
-- and 'sideName' gives) before request threads race on them
-- (Q-RTS-BLACKHOLE). Run before warp listens.
warmUpSideNames :: IO ()
warmUpSideNames = do
  mapM_ (evaluate . parseSide) ["tree", "base", "head", "x"]
  mapM_ (\s -> evaluate (sideName s) >> evaluate (sideTag s)) [minBound .. maxBound]

-- | Like 'snapshotFor', with what a route reads from the snapshot.
fromSideSnapshot ::
  FilesApi -> Text -> (Snapshot -> a) -> Maybe (IO (Either ListError a))
fromSideSnapshot api side f = case snapshotFor api side of
  Nothing -> Nothing
  Just get -> Just (fmap f <$> get)

{- Note [Forced snapshot]

On GHC 9.12 and 9.14, threads that force the same thunk at once can
livelock the RTS (Q-RTS-BLACKHOLE). Request threads and parse workers
all read the same snapshot. So 'newSnapshot' builds it fully
('forceSnapshot', 'newIndex', the search's module list, the resolver's
module graph) before the listing thread fills the slot: the other
threads then find no thunk to force.

The listing thread also warms up before the fill: it runs the parser
(warmUpParser), so the workers find the parser's shared values built,
resolves a small project (warmUpResolve in
"HaskellReviewer.Session.Resolve"), and encodes some pages (Note [Page
warm-up] in "HaskellReviewer.Server.Wire.Page"). 'newSnapshot' encodes
a file view of each kind ('cborWarmUp'), so the shared values of cborg
and of our CBOR code are built before request threads race on them. The
gzip path gets the same before warp listens (Note [Compression] in
"HaskellReviewer.Server.Router"), and so does the search route
(warmUpSearch in "HaskellReviewer.Server.Search"). The listing thread
runs the refs route too (warmUpRefs in "HaskellReviewer.Server.Refs"),
and the text search route runs before warp listens (warmUpTextSearch in
"HaskellReviewer.Server.TextSearch").

What the workers put in the slots is forced before it gets there (Note
[Indexing] in "HaskellReviewer.Session.Index").
-}

-- | The snapshot of one side's file list and its modules (from
-- 'HaskellReviewer.Core.Modules.buildModuleIndex'), with the JSON
-- bodies (the module tree with these diff markers, D-325; none in
-- browse mode), an index in this pool with every module Pending, and
-- its resolver. Its index and text store share results by blob with
-- the session's other snapshots (D-320). Fully built before it
-- returns. See Note [Forced snapshot].
newSnapshot ::
  Side ->
  SessionShare ->
  EntrySource ->
  FileList ->
  ModuleIndex ->
  Map ModuleId Marker ->
  IO Snapshot
newSnapshot side share source fl modules markers = do
  let fileListJson = encodeFileList firstGeneration fl
      moduleTreeJson = encodeModuleTree firstGeneration markers modules
  forceSnapshot modules fileListJson moduleTreeJson
  searchModules <- evaluate (moduleCandidates modules)
  _ <- evaluate (forceCandidates searchModules)
  sequence_
    [ evaluate (LBS.length (encodeFileView firstGeneration n c))
    | n <- [unfiltered, FileNotes {filtered = Filtered}],
      c <- cborWarmUp
    ]
  -- Slots read only files of the list (indexAndReport in
  -- "HaskellReviewer.App.Run"): a symlink's blob is not its text.
  let blobOf p = case Map.lookup p fl.entries of
        Just EntryFile -> sharedBlob source p
        _ -> Nothing
  index <- newIndex share.pool blobOf modules
  resolver <- newResolver modules index
  textStore <- newTextStore share.blobTexts
  pure
    Snapshot
      { generation = firstGeneration,
        side = side,
        source = source,
        files = fl,
        modules = modules,
        fileListJson = fileListJson,
        moduleTreeJson = moduleTreeJson,
        searchModules = searchModules,
        index = index,
        resolver = resolver,
        textStore = textStore
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
  void (evaluate (Map.size idx.paths))
  let GhcVersion ghc = idx.ghc
  mapM_ evaluate ghc
  mapM_
    (\c -> evaluate (forceSettings c.settings))
    [c | p <- idx.packages, c <- p.components]
  void (evaluate (LBS.length fileListJson))
  void (evaluate (LBS.length moduleTreeJson))

-- | @GET api\/files\/\<side>@: the file list. A side the session
-- doesn't have is a plain 404 ('snapshotFor').
filesApp :: FilesApi -> Text -> Application
filesApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> respond (json snap.fileListJson)
  | otherwise = respond notFound

-- | @GET api\/modules\/\<side>@: the module tree.
modulesApp :: FilesApi -> Text -> Application
modulesApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> respond (json snap.moduleTreeJson)
  | otherwise = respond notFound

-- | @GET api\/file\/\<side>\/\<path segments>@: one file of the list,
-- as CBOR ('encodeFileView'). Anything not in the list is a 404. See
-- Note [File containment] in "HaskellReviewer.Core.RelPath".
fileApp :: FilesApi -> Text -> [Text] -> Application
fileApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side,
    Just path <- relPathFromSegments segs =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case Map.lookup path snap.files.entries of
          Nothing -> respond notFound
          Just kind ->
            api.readEntry snap path kind >>= \case
              Left ReadGone -> respond notFound
              Left (ReadFailed why) -> respond =<< readFailed api snap path why
              Right content ->
                respond . cbor $
                  encodeFileView
                    snap.generation
                    (api.fileNotes snap path)
                    (inSnapshot snap content)
  | otherwise = respond notFound

-- | @GET api\/module\/\<side>\/\<ref segments>@: one module's page, as
-- CBOR ('encodeModulePage'), built per request. The segments are joined
-- with @/@, so a loose module's path works too.
--
-- A module we parse comes from its slot: if it's not done, it jumps the
-- queue and we wait for it (Note [Indexing] in
-- "HaskellReviewer.Session.Index"), then for its direct imports of
-- ours, up to 'importWait' ('awaitImports'). Then we resolve it, for the
-- links out of the module (Note [Page links] in
-- "HaskellReviewer.Core.SourceLinks"). Generated, preprocessed and
-- no-source modules answer from the module index, without reading
-- anything.
--
-- An unknown ref, or a module whose file is gone, is a 404 with a JSON
-- body ('encodeModuleNotFound'). A bad side is a plain 404. Exceptions
-- from resolution are not caught: a 500.
moduleApp :: FilesApi -> Text -> [Text] -> Application
moduleApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case found snap of
          Nothing -> respond (missing snap)
          Just (i, m) -> case m.source of
            HaskellSource f -> do
              awaitImports snap.resolver importWait i
              fromIndex snap i f.path
            GeneratedSource f -> answer snap i (Just f.path) (GeneratedModule f.tool)
            PreprocessedSource f ->
              answer snap i (Just f.path) (PreprocessedModule f.tool)
            NoSource r -> answer snap i Nothing (NoSourceModule r)
  | otherwise = respond notFound
  where
    found snap = do
      -- An escaped / (%2F) in a segment: not a path, as on the file route.
      guard (not (any (Text.elem '/') segs))
      i <- resolveModuleRef snap.modules (Text.intercalate "/" segs)
      m <- findModule snap.modules i
      Just (i, m)
    missing snap = jsonWith status404 (encodeModuleNotFound snap.generation)
    view snap v = viewWith snap (pure v)
    viewWith snap mk =
      computed snap (encodeModulePage snap.generation snap.modules <$> mk) >>= respond
    unlinked i path a =
      ModuleView {moduleId = i, path = path, partial = False, answer = a}
    answer snap i path r = view snap (unlinked i path (NotParsed r))
    fromIndex snap i path =
      awaitModule snap.index i >>= \case
        Just (Right (IndexedParsed p)) -> viewWith snap $ do
          (input, partial) <- linksFor snap i p
          pure
            ModuleView
              { moduleId = i,
                path = Just path,
                partial = partial,
                answer = ParsedAnswer p (pageLinks input)
              }
        Just (Right (IndexedTextOnly t)) ->
          view snap (unlinked i (Just path) (TextOnlyAnswer t))
        Just (Right (IndexedNotText size)) ->
          answer snap i (Just path) (NotTextModule size)
        Just (Right (IndexedTooLarge info)) ->
          answer snap i (Just path) (TooLargeModule info)
        Just (Left e) -> respond (indexFailed (missing snap) e)
        -- Every Haskell source has a slot.
        Nothing -> respond (missing snap)

-- | @GET api\/source\/\<side>\/\<path segments>@: one file of the list
-- as the source view, as CBOR ('encodeSource'). See Note [Source view]
-- in "HaskellReviewer.Server.Wire.Source".
--
-- The module at that path ('moduleAtPath'): if we parse it, we wait
-- for its slot and its imports, as for the page, then resolve it and
-- link its tokens. Text only: highlighted, no links. Anything else is
-- plain, and nothing is read.
--
-- A path not in the list (a @%2F@ in a segment too), or a file gone, is
-- a 404 with a JSON body ('encodeFileNotFound'). A bad side is a plain
-- 404. Exceptions from resolution are not caught: a 500.
sourceApp :: FilesApi -> Text -> [Text] -> Application
sourceApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case relPathFromSegments segs of
          Just path
            | Map.member path snap.files.entries -> answer snap path
          _ -> respond (missing snap)
  | otherwise = respond notFound
  where
    missing snap = jsonWith status404 (encodeFileNotFound snap.generation)
    view snap v = viewWith snap (pure v)
    viewWith snap mk =
      computed snap (encodeSource snap.generation snap.modules <$> mk) >>= respond
    plain snap path i = viewOf snap path i False PlainBody
    viewOf snap path i partial b =
      SourceView
        { path = path,
          moduleId = i,
          partial = partial,
          notes = api.fileNotes snap path,
          body = b
        }
    answer snap path = case moduleAtPath snap.modules path of
      Nothing -> view snap (plain snap path Nothing)
      Just i -> case (.source) <$> findModule snap.modules i of
        Just (HaskellSource _) -> do
          awaitImports snap.resolver importWait i
          fromIndex snap path i
        _ -> view snap (plain snap path (Just i))
    fromIndex snap path i =
      awaitModule snap.index i >>= \case
        Just (Right (IndexedParsed s)) -> viewWith snap $ do
          (input, partial) <- linksFor snap i s
          let lexed = parsedSource s.parsed (sourceLinks input)
          pure (viewOf snap path (Just i) partial (LexedBody lexed))
        Just (Right (IndexedTextOnly t)) ->
          view snap (viewOf snap path (Just i) False (textOnlySource t))
        Just (Right (IndexedNotText _)) -> view snap (plain snap path (Just i))
        Just (Right (IndexedTooLarge _)) -> view snap (plain snap path (Just i))
        Just (Left e) -> respond (indexFailed (missing snap) e)
        -- Every Haskell source has a slot.
        Nothing -> respond (missing snap)

-- | Build and force a CBOR body inside 'withRequest'. No slot waits in
-- here (Note [Indexing] in "HaskellReviewer.Session.Index").
computed :: Snapshot -> IO LBS.ByteString -> IO Response
computed snap mk =
  withRequest snap.index $ do
    body <- mk
    _ <- evaluate (LBS.length body)
    pure (cbor body)

-- | What the links of a parsed module read, resolved, and whether it's
-- partial. For the source view and the page.
linksFor :: Snapshot -> ModuleId -> ParsedSource -> IO (LinksInput, Bool)
linksFor snap i s = do
  -- i is Ready: resolveModuleWith doesn't wait.
  resolved <- resolveModuleWith snap.resolver i
  let res = fst <$> resolved
      state = maybe (const ModuleNotParsed) snd resolved
      p = ParsedInput {text = s.text, parsed = s.parsed}
  pure (linksInput snap.modules state i p res, maybe False isPartial res)

-- | @GET api\/indexing\/\<side>@: how far indexing is on that side.
-- The same body as the @indexing@ event.
indexingApp :: FilesApi -> Text -> Application
indexingApp api side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just snapshot <- snapshotFor api side =
      snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> do
          p <- atomically (readProgress snap.index)
          respond (json (LBS.fromStrict (encodeIndexing snap.generation snap.side p)))
  | otherwise = respond notFound

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
readFailed :: FilesApi -> Snapshot -> RelPath -> Text -> IO Response
readFailed api snap path why = do
  logInfo api.logger $
    "haskell-reviewer: "
      <> sideTag snap.side
      <> "cannot read "
      <> relPathText path
      <> ": "
      <> why
  pure (serverError "cannot read file")

-- | A slot with no answer: the route's 404 if the file is gone, else a
-- 500. The worker logged read errors and crashes already.
indexFailed :: Response -> IndexFailure -> Response
indexFailed gone = \case
  SourceGone -> gone
  SourceUnreadable _ -> serverError "cannot read file"
  IndexCrashed _ -> serverError "cannot index module"

-- | The listing failed. The reason is on stderr.
cannotList :: Response
cannotList = serverError "cannot list files"

-- | A plain-text 500.
serverError :: LBS.ByteString -> Response
serverError =
  responseLBS status500 [(hContentType, "text/plain; charset=utf-8")]
