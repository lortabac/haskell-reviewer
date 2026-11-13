-- | The find references route, @api/refs/<side>@ (D-294).
module HaskellReviewer.Server.Refs
  ( -- * Route
    refsApp,

    -- * Warm-up
    warmUpRefs,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Exception (evaluate)
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.ModuleModel (ParsedModule (..))
import HaskellReviewer.Core.Modules
  ( FoundModule (..),
    ModuleId,
    ModuleIndex,
    ModuleSource (..),
    findModule,
    modulePath,
    moduleRef,
    resolveModuleRef,
  )
import HaskellReviewer.Core.NameTable (NameEntry (..), moduleNames)
import HaskellReviewer.Core.Names (DeclId, renderDeclId)
import HaskellReviewer.Core.References
  ( RefsAnswer,
    forceRefsAnswer,
  )
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.Resolve
  ( ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
  )
import HaskellReviewer.Core.ResolveWarmUp
  ( resolveWarmUpIndex,
    resolveWarmUpInput,
    resolveWarmUpModules,
  )
import HaskellReviewer.Core.Search (ModuleCandidate (..), moduleCandidates)
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Server.Api (FilesApi (..))
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Server.Wire (RefsMissing (..), encodeRefs, encodeRefsNotFound)
import HaskellReviewer.Session.Index
  ( IndexFailure (..),
    Indexed (..),
    awaitModule,
    readNames,
    withRequest,
  )
import HaskellReviewer.Session.Resolve (findReferences, referencesIn, warmUpStates)
import HaskellReviewer.Session.Snapshot (Generation, Snapshot (..), firstGeneration)
import Network.HTTP.Types
  ( Status,
    hContentLength,
    hContentType,
    methodGet,
    parseQueryReplacePlus,
    renderSimpleQuery,
    status200,
    status400,
    status404,
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

{- Note [Refs route]

D-294. GET api/refs/<side>?module=<ref>&decl=<id>. The only side is
tree; another one is a plain 404. Other methods, HEAD too: 405 with
Allow: GET.

Both params are read with parseQueryReplacePlus False: a + stays a +
(an operator). The first of each counts. One missing, or not UTF-8: a
plain 400 "bad query". module is a module ref, as on api/module; decl
a declaration id ('renderDeclId').

404 with {"generation":1,"kind":"not-found","what":"module"}:

- an unknown ref;
- no source to parse (generated, no source);
- the parse gave no module (text only, not text, too large);
- the module's file is gone.

The same with "decl": a name the module doesn't declare (instances
have none).

A read error or a crash on the module: 500, as on api/module. The
listing failed: 500 "cannot list files".

It waits for the module (it jumps the queue), never for indexing: it
answers from what is parsed now, and says partial. The client asks
again later. The compute and the encode run in withRequest. Else 200
with 'encodeRefs'. See Note [Find references] in
"HaskellReviewer.Core.References".
-}

-- | @GET api\/refs\/\<side>?module=…&decl=…@. See Note [Refs route].
refsApp :: FilesApi -> Text -> Application
refsApp api = refsWith (fmap fromSnapshot <$> api.snapshot)

-- | Builds the shared values (keys, finder, lines, encoder) before
-- request threads race on them (Q-RTS-BLACKHOLE): runs the route on
-- fake requests over the resolution warm-up project, for every
-- declaration of every module and each error. Run in the listing
-- thread, after the parser's and resolution's warm-ups.
warmUpRefs :: IO ()
warmUpRefs = do
  forM_ resolveWarmUpModules $ \i -> do
    let ref = moduleRef resolveWarmUpIndex i
    forM_ (namesOf i) $ \e ->
      once (pure (Right warmUpSource)) "GET" "tree" $
        renderSimpleQuery
          True
          [("module", utf8 ref), ("decl", utf8 (renderDeclId e.decl))]
  let anyRef = case resolveWarmUpModules of
        i : _ -> utf8 (moduleRef resolveWarmUpIndex i)
        [] -> "x"
      fake = pure (Right warmUpSource)
  mapM_
    (once fake "GET" "tree")
    [ renderSimpleQuery True [("module", anyRef), ("decl", "val:nope")],
      renderSimpleQuery True [("module", "Nope"), ("decl", "val:x")],
      renderSimpleQuery True [("module", utf8 gone), ("decl", "val:x")],
      renderSimpleQuery True [("module", utf8 unreadable), ("decl", "val:x")],
      renderSimpleQuery True [("module", utf8 crashed), ("decl", "val:x")],
      "?module=" <> anyRef,
      "?decl=val:x",
      "?module=%FF&decl=val:x",
      "?module=A&module=B&decl=val%3A%2B&x=1",
      ""
    ]
  once fake "POST" "tree" "?module=A&decl=val:x"
  once fake "GET" "base" "?module=A&decl=val:x"
  once (pure (Left (ListCrashed "x"))) "GET" "tree" "?module=A&decl=val:x"
  where
    utf8 = Text.Encoding.encodeUtf8
    once source method side query = do
      let req = defaultRequest {requestMethod = method, rawQueryString = query}
      ResponseReceived <- refsWith source side req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        out <- newIORef mempty
        withBody $ \streaming ->
          streaming (\b -> modifyIORef' out (<> b)) (pure ())
        void (evaluate . LBS.length . Builder.toLazyByteString =<< readIORef out)
        pure ResponseReceived
      pure ()

-- * Internal

-- | What the route reads from a snapshot. Tests and the warm-up plug in
-- fakes.
data Source = Source
  { generation :: Generation,
    -- | The module of a ref, once it's done (it waits).
    home :: Text -> IO Home,
    -- | Find and encode, in 'withRequest'.
    find :: ModuleId -> DeclId -> (RefsAnswer -> LBS.ByteString) -> IO LBS.ByteString,
    -- | The ref, name and file of a module.
    candidate :: ModuleId -> ModuleCandidate,
    pathOf :: ModuleId -> Maybe Text
  }

-- | The home module.
data Home
  = -- | Unknown, not parsed, or gone.
    NoHome
  | -- | A read error or a crash.
    HomeFailed IndexFailure
  | -- | Parsed, with its names.
    HomeParsed ModuleId [NameEntry]

fromSnapshot :: Snapshot -> Source
fromSnapshot snap =
  Source
    { generation = snap.generation,
      home = \ref -> case resolveModuleRef snap.modules ref of
        Just i
          | Just (HaskellSource _) <- (.source) <$> findModule snap.modules i ->
              awaitModule snap.index i >>= \case
                Just (Right (IndexedParsed _)) -> do
                  names <- atomically (readNames snap.index)
                  pure (HomeParsed i (Map.findWithDefault [] i names))
                Just (Left e) -> pure (HomeFailed e)
                _ -> pure NoHome
        _ -> pure NoHome,
      find = \i d enc -> withRequest snap.index $ do
        a <- findReferences snap.resolver (orderOf candidate path) i d
        forcedBody enc a,
      candidate = candidate,
      pathOf = path
    }
  where
    candidate = candidateIn snap.modules snap.searchModules
    path i = relPathText <$> modulePath snap.modules i

-- | The body of an answer, built and forced.
forcedBody :: (RefsAnswer -> LBS.ByteString) -> RefsAnswer -> IO LBS.ByteString
forcedBody enc a = do
  body <- evaluate (enc a)
  _ <- evaluate (LBS.length body)
  pure body

-- | Modules by name, then path.
orderOf ::
  (ModuleId -> ModuleCandidate) ->
  (ModuleId -> Maybe Text) ->
  ModuleId ->
  (Text, Maybe Text)
orderOf candidate path i = ((candidate i).name, path i)

-- | A module's ref and name. Every module is in the map.
candidateIn ::
  ModuleIndex -> Map ModuleId ModuleCandidate -> ModuleId -> ModuleCandidate
candidateIn idx cs i =
  Map.findWithDefault
    ModuleCandidate {id = i, name = moduleRef idx i, ref = moduleRef idx i}
    i
    cs

-- | The route, over any source. See Note [Refs route].
refsWith :: IO (Either ListError Source) -> Text -> Application
refsWith source side req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | side /= "tree" = respond notFound
  | Just ref <- param "module",
    Just declText <- param "decl" =
      source >>= \case
        Left _ -> respond (plain status500 "cannot list files")
        Right src ->
          src.home ref >>= \case
            NoHome -> respond (missing src MissingModule)
            HomeFailed SourceGone -> respond (missing src MissingModule)
            HomeFailed (SourceUnreadable _) ->
              respond (plain status500 "cannot read file")
            HomeFailed (IndexCrashed _) ->
              respond (plain status500 "cannot index module")
            HomeParsed i names ->
              case [e.decl | e <- names, renderDeclId e.decl == declText] of
                d : _ -> do
                  let enc =
                        encodeRefs
                          src.generation
                          (src.candidate i).ref
                          declText
                          src.candidate
                          src.pathOf
                  body <- src.find i d enc
                  respond (json status200 body)
                [] -> respond (missing src MissingDecl)
  | otherwise = respond (plain status400 "bad query")
  where
    params = parseQueryReplacePlus False req.rawQueryString
    param :: ByteString -> Maybe Text
    param k = case lookup k params of
      Just (Just bytes) -> either (const Nothing) Just (Text.Encoding.decodeUtf8' bytes)
      _ -> Nothing
    missing src what = json status404 (encodeRefsNotFound src.generation what)

-- | With a Content-Length, so warp doesn't send it chunked (gzip may
-- still do).
json :: Status -> LBS.ByteString -> Response
json st body =
  responseLBS
    st
    [ (hContentType, "application/json"),
      (hContentLength, Char8.pack (show (LBS.length body)))
    ]
    body

plain :: Status -> LBS.ByteString -> Response
plain st = responseLBS st [(hContentType, "text/plain; charset=utf-8")]

-- * Warm-up data

-- | The resolution warm-up project's slot states ('warmUpStates'),
-- through the same finder and order as a snapshot. Some slots are not
-- done, so answers are partial, and three refs fail.
warmUpSource :: Source
warmUpSource =
  Source
    { generation = firstGeneration,
      home = \ref -> pure (warmUpHome ref),
      find = \i d enc -> do
        let a = referencesIn resolveWarmUpInput.graph order warmUpStates Map.empty i d
        _ <- evaluate (forceRefsAnswer a)
        forcedBody enc a,
      candidate = candidate,
      pathOf = path
    }
  where
    candidate = candidateIn resolveWarmUpIndex (moduleCandidates resolveWarmUpIndex)
    path i = relPathText <$> modulePath resolveWarmUpIndex i
    order = orderOf candidate path

-- | The home module of a warm-up ref.
warmUpHome :: Text -> Home
warmUpHome ref
  | ref == gone = HomeFailed SourceGone
  | ref == unreadable = HomeFailed (SourceUnreadable "x")
  | ref == crashed = HomeFailed (IndexCrashed "x")
  | Just i <- resolveModuleRef resolveWarmUpIndex ref,
    ModuleParsed _ <- resolveWarmUpInput.state i =
      HomeParsed i (namesOf i)
  | otherwise = NoHome

-- | A warm-up module's names.
namesOf :: ModuleId -> [NameEntry]
namesOf i = case resolveWarmUpInput.state i of
  ModuleParsed p -> moduleNames p.parsed.declarations
  _ -> []

-- | Refs of the fake failures.
gone, unreadable, crashed :: Text
gone = "warm-up:gone"
unreadable = "warm-up:unreadable"
crashed = "warm-up:crashed"
