-- | The routes that read the snapshot: file list, file text, module
-- tree, one module.
module HaskellReviewer.Server.Api
  ( -- * Types
    FilesApi (..),

    -- * Snapshots
    newSnapshot,
    forceSnapshot,

    -- * Routes
    filesApp,
    fileApp,
    modulesApp,
    moduleApp,
  )
where

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent (FileContent (..), LinkInfo (..))
import HaskellReviewer.Core.FileList (EntryKind (..), FileList (..))
import HaskellReviewer.Core.ModuleModel (ModuleParse)
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
import HaskellReviewer.Core.Parser (ParseSettings, moduleParseSettings)
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
    encodeModuleTree,
    encodeModuleView,
  )
import HaskellReviewer.Session.Snapshot (Snapshot (..), firstGeneration)
import Network.HTTP.Types
  ( hContentLength,
    hContentType,
    methodGet,
    status200,
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
    -- | Parse a module, fully forced: 'parseGuarded' in
    -- "HaskellReviewer.Effects.Parser".
    parse :: ParseSettings -> Text -> IO ModuleParse,
    -- | For read errors.
    logger :: Logger
  }

{- Note [Forced snapshot]

On GHC 9.12 and 9.14, threads that force the same thunk at once can
livelock the RTS (Q-RTS-BLACKHOLE). Request threads all read the same
snapshot. So the listing thread builds it fully ('forceSnapshot')
before it fills the slot: request threads then find no thunk to force.
It also warms up the parser first (warmUpParser), since every request
that parses a module uses the parser's shared values.

A parse in a request thread is fine: its result is the thread's own,
and parseGuarded forces it fully before it's encoded.
-}

-- | The snapshot of a file list and its project, with the JSON bodies.
-- Lazy: call 'forceSnapshot' before requests see it.
newSnapshot :: CanonicalRoot -> FileList -> Project -> Snapshot
newSnapshot root fl project =
  Snapshot
    { generation = firstGeneration,
      root = root,
      files = fl,
      modules = modules,
      fileListJson = encodeFileList firstGeneration fl,
      moduleTreeJson = encodeModuleTree firstGeneration modules
    }
  where
    modules = buildModuleIndex project fl

-- | Force both JSON bodies, the module index (strict fields), every
-- list of its name maps, the id set and the parse settings. The JSON
-- walks the rest of the index. See Note [Forced snapshot].
forceSnapshot :: Snapshot -> IO ()
forceSnapshot snap = do
  let idx = snap.modules
  void (evaluate idx)
  mapM_ (mapM_ evaluate) (Map.elems idx.ownedByName)
  mapM_ (mapM_ evaluate) (Map.elems idx.looseByName)
  void (evaluate (Set.size idx.ids))
  let GhcVersion ghc = idx.ghc
  mapM_ evaluate ghc
  mapM_
    (\c -> evaluate (forceSettings c.settings))
    [c | p <- idx.packages, c <- p.components]
  void (evaluate (LBS.length snap.fileListJson))
  void (evaluate (LBS.length snap.moduleTreeJson))

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
              Left (ReadFailed why) -> respond =<< readFailed api path why
              Right content ->
                respond (json (encodeFileView snap.generation (inSnapshot snap content)))
  | otherwise = respond notFound

-- | @GET api\/module\/\<side>\/\<ref segments>@: one module, parsed
-- now, in this thread. The segments are joined with @/@, so a loose
-- module's path works too. Not cached.
--
-- The file is read like the file route reads it (Note [File
-- containment] in "HaskellReviewer.Core.RelPath"). Generated,
-- preprocessed and no-source modules answer from the index, without
-- reading anything.
moduleApp :: FilesApi -> Text -> [Text] -> Application
moduleApp api side segs req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  -- An escaped / (%2F) in a segment: not a path, as on the file route.
  | any (Text.elem '/') segs = respond notFound
  | otherwise =
      api.snapshot >>= \case
        Left _ -> respond cannotList
        Right snap -> case found snap of
          Nothing -> respond notFound
          Just (i, m) -> case m.source of
            HaskellSource f -> readAndParse snap i m f
            GeneratedSource f -> answer snap i (Just f.path) (GeneratedModule f.tool)
            PreprocessedSource f ->
              answer snap i (Just f.path) (PreprocessedModule f.tool)
            NoSource r -> answer snap i Nothing (NoSourceModule r)
  where
    found snap = do
      i <- resolveModuleRef snap.modules (Text.intercalate "/" segs)
      m <- findModule snap.modules i
      Just (i, m)
    view snap v = respond (json (encodeModuleView snap.generation snap.modules v))
    answer snap i path r =
      view snap ModuleView {moduleId = i, path = path, answer = NotParsed r}
    readAndParse snap i m f = case Map.lookup f.path snap.files.entries of
      -- The index only has files of the list.
      Just EntryFile ->
        api.readEntry snap f.path EntryFile >>= \case
          Left ReadGone -> respond notFound
          Left (ReadFailed why) -> respond =<< readFailed api f.path why
          Right (TextContent t) -> do
            r <- api.parse (moduleParseSettings snap.modules.ghc m.settings f) t
            view snap ModuleView {moduleId = i, path = Just f.path, answer = FromParser r}
          Right (Binary size) -> answer snap i (Just f.path) (NotTextModule size)
          Right (TooLarge info) -> answer snap i (Just f.path) (TooLargeModule info)
          -- No longer a file: like gone.
          Right _ -> respond notFound
      _ -> respond notFound

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
