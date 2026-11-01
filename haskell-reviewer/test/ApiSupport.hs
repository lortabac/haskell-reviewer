-- | Shared by the API tests: fixed fake projects, and calling the app
-- in process.
module ApiSupport
  ( -- * The fixture projects
    TestApi (..),
    fixtureApi,
    projectApi,
    implicitApi,
    pageApi,
    searchApi,
    forceFixtures,
    indexed,
    indexAll,
    fixedPrefs,

    -- * Calling the app
    ApiResult (..),
    getApi,
    getApiWith,
    getApiQuery,
    requestApi,
    requestApiWith,
    RequestBody (..),
    sendApi,
    goodOrigin,
    callApp,
    apiToken,
  )
where

import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate)
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Core.Config (builtinDefaults)
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
import HaskellReviewer.Core.ModuleModel (ModuleParse)
import HaskellReviewer.Core.Parser (ParseSettings)
import HaskellReviewer.Core.Prefs (effective, noPrefs)
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    CabalProblem (..),
    Component (..),
    ComponentName (..),
    DetectedGhc (..),
    Extension (..),
    GhcSource (..),
    Language (..),
    ListedModule (..),
    Loc (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    ProjectWarning (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.RelPath
  ( RelDir,
    RelPath,
    relDirFromSegments,
    relPathFromBytes,
    relPathText,
    resolveLinkTarget,
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError, ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.Parser (parseGuarded)
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot)
import HaskellReviewer.Server.Events (EventStream (..))
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Session.Clients (newClients)
import HaskellReviewer.Session.Index (IndexDeps (..), runWorkers)
import HaskellReviewer.Session.Prefs (PrefsStore (..), newPrefsStore)
import HaskellReviewer.Session.Snapshot (Snapshot (files, index))
import Network.HTTP.Types (Header, Status (..))
import Network.Wai
  ( Application,
    Request (..),
    RequestBodyLength (..),
    defaultRequest,
    responseToStream,
    setRequestBodyChunks,
  )
import Network.Wai.Internal (ResponseReceived (..))
import System.Timeout (timeout)
import Test.Tasty.HUnit (assertFailure)

-- * The fixture projects

-- | The snapshot routes and the parse workers, with fakes. Each request
-- gets a fresh snapshot, so a fresh index: tests share no slot.
data TestApi = TestApi
  { snapshot :: IO (Either ListError Snapshot),
    readEntry :: Snapshot -> RelPath -> EntryKind -> IO (Either ReadError FileContent),
    parse :: ParseSettings -> Text -> IO ModuleParse,
    -- | For read errors.
    logger :: Logger,
    -- | Parse workers that run while a request does. With 0 nothing is
    -- read in the background (and a request for a module we parse
    -- waits for ever).
    workers :: Int,
    -- | The preferences store, made for each request. By default in
    -- memory, with the built-in defaults.
    prefs :: IO PrefsStore
  }

-- | A fixed snapshot and file contents. The file API fixtures come
-- from it.
fixtureApi :: TestApi
fixtureApi = apiFor fixtureSnapshot

-- | A project with every kind of module source, role and no-source
-- reason, a broken package and warnings. For the module tree fixture.
projectApi :: TestApi
projectApi = apiFor projectSnapshot

-- | No project: stack.yaml and package.yaml in ROOT, loose modules.
implicitApi :: TestApi
implicitApi = apiFor implicitSnapshot

-- | Loose modules for the module page fixtures: every kind of
-- declaration, doc markup and section.
pageApi :: TestApi
pageApi = apiFor pageSnapshot

-- | Loose modules for search: names of every kind, an operator with a
-- +, a private name, and insert in two modules.
searchApi :: TestApi
searchApi = apiFor searchSnapshot

-- | Build the shared inputs, before parallel tests use them. See
-- Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
forceFixtures :: IO ()
forceFixtures = do
  forM_
    [fixtureSnapshot, projectSnapshot, implicitSnapshot, pageSnapshot, searchSnapshot]
    void
  forM_ contents $ \(p, c) -> evaluate p >> evaluate (forceContent c)
  where
    forceContent = \case
      TextContent t -> t `seq` ()
      Link info -> info.target `seq` maybe () (`seq` ()) info.resolved
      c -> c `seq` ()

-- | Every module indexed before the request, with no workers during
-- it: the indexing progress is final.
indexed :: TestApi -> TestApi
indexed api =
  api
    { snapshot =
        api.snapshot >>= \case
          Right snap -> do
            indexAll api snap
            pure (Right snap)
          listed -> pure listed,
      workers = 0
    }

-- | Index every module of the snapshot, with one worker.
indexAll :: TestApi -> Snapshot -> IO ()
indexAll api snap = runWorkers (indexDeps api snap) 1 snap.index

apiFor :: IO Snapshot -> TestApi
apiFor snap =
  TestApi
    { snapshot = Right <$> snap,
      readEntry = \_ p _ ->
        pure (maybe (Left ReadGone) Right (lookup (relPathText p) contents)),
      parse = parseGuarded,
      logger = newLogger LevelInfo (\_ -> pure ()),
      workers = 1,
      prefs =
        newPrefsStore silent builtinDefaults (Left HomeUnset)
    }

silent :: Logger
silent = newLogger LevelInfo (\_ -> pure ())

-- | Built-in defaults, and a save changes nothing. For tests that need
-- routes but don't look at api/prefs.
fixedPrefs :: PrefsStore
fixedPrefs =
  PrefsStore
    { current = pure (effective builtinDefaults noPrefs),
      update = \_ -> pure (effective builtinDefaults noPrefs)
    }

-- | What the workers need, as the server wires it.
indexDeps :: TestApi -> Snapshot -> IndexDeps
indexDeps api snap =
  IndexDeps
    { readModule = \p -> case Map.lookup p snap.files.entries of
        Just EntryFile -> api.readEntry snap p EntryFile
        _ -> pure (Left ReadGone),
      parse = api.parse,
      logger = api.logger
    }

fixtureSnapshot :: IO Snapshot
fixtureSnapshot =
  newSnapshot
    fixtureRoot
    ( fileList
        [ ("app/Main.hs", EntryFile),
          ("assets/logo.png", EntryFile),
          ("data/big.csv", EntryFile),
          ("docs/guide.md", EntrySymlink),
          ("docs/old.md", EntrySymlink),
          ("shared/guide.md", EntryFile),
          ("vendor/dep", EntrySubmodule commit),
          ("vendor/nested", EntryNestedRepo)
        ]
    )
    noProject

projectSnapshot :: IO Snapshot
projectSnapshot =
  newSnapshot
    fixtureRoot
    ( fileList
        [ ("bad/bad.cabal", EntryFile),
          ("cabal.project", EntryFile),
          ("hp/package.yaml", EntryFile),
          ("pkg/app/Main.hs", EntryFile),
          ("pkg/bench/Bench.hs", EntryFile),
          ("pkg/pkg.cabal", EntryFile),
          ("pkg/src/Boot.hs-boot", EntryFile),
          ("pkg/src/Foo.hs", EntryFile),
          ("pkg/src/Lexer.hs", EntryFile),
          ("pkg/src/Lexer.x", EntryFile),
          ("pkg/src/Linked.hs", EntrySymlink),
          ("pkg/src/Lit.lhs", EntryFile),
          ("pkg/src/Sig.hsig", EntryFile),
          ("pkg/test/Spec.hs", EntryFile),
          ("pkg/test/Tests.hs", EntryFile),
          ("scripts/Tool.hs", EntryFile),
          ("scripts/build-docs.hs", EntryFile)
        ]
    )
    Project
      { source = ProjectFile [],
        withCompiler = Nothing,
        ghc = fixtureGhc,
        packages = [fixturePackage],
        brokenPackages =
          [ BrokenPackage
              { cabalFile = relPath "bad/bad.cabal",
                problem = CabalSyntax ["bad/bad.cabal:3:1: unexpected end of input"]
              }
          ],
        warnings =
          [ EntryNoMatch Loc {file = relPath "cabal.project", line = 3} "foo/",
            HpackOnly (relDir "hp")
          ]
      }

-- | One package with one component of each kind.
fixturePackage :: Package
fixturePackage =
  Package
    { name = PackageName "pkg",
      version = "0.1.0",
      cabalFile = relPath "pkg/pkg.cabal",
      flags = [],
      components =
        [ (component Lib "pkg/src")
            { -- Only for parsing: the tree doesn't show them.
              extensions =
                [ Extension "ExplicitLevelImports" True,
                  Extension "PackageImports" True,
                  Extension "NoSuchThing" True
                ],
              modules =
                [ listed "Foo" Exposed,
                  listed "Lexer" Exposed,
                  listed "Sig" Signature,
                  listed "Lit" Other,
                  listed "Paths_pkg" Other,
                  (listed "Gen" Other) {autogen = True},
                  listed "Gone" Other,
                  listed "Linked" Other
                ]
            },
          (component (Exe "app") "pkg/app") {mainIs = Just "Main.hs"},
          (component (Exe "outside") "pkg/app") {mainIs = Just "../../../Main.hs"},
          (component (Test "spec") "pkg/test")
            { mainIs = Just "Spec.hs",
              ghcOptions = ["-F", "-pgmF", "hspec-discover"]
            },
          (component (Test "detailed") "pkg/test")
            { modules = [listed "Tests" TestModule]
            },
          (component (Bench "slow") "pkg/bench")
            { buildable = False,
              mainIs = Just "Bench.hs"
            }
        ]
    }
  where
    listed n k = ListedModule {name = ModuleName n, kind = k, autogen = False}

-- | A component with nothing in it but its source dir.
component :: ComponentName -> Text -> Component
component name src =
  Component
    { name = name,
      buildable = True,
      sourceDirs = [relDir src],
      language = Haskell2010,
      extensions = [],
      buildDepends = [],
      mainIs = Nothing,
      modules = [],
      ghcOptions = [],
      cppOptions = [],
      includeDirs = []
    }

implicitSnapshot :: IO Snapshot
implicitSnapshot =
  newSnapshot
    fixtureRoot
    ( fileList
        [ ("Main.hs", EntryFile),
          ("dist-newstyle/build/Gen.hs", EntryFile),
          ("notes.txt", EntryFile),
          ("package.yaml", EntryFile),
          ("src/Data/Deep/Mod.hs", EntryFile),
          ("src/Data/Deep/Mod.hs-boot", EntryFile),
          ("stack.yaml", EntryFile)
        ]
    )
    noProject

pageSnapshot :: IO Snapshot
pageSnapshot =
  newSnapshot
    fixtureRoot
    (fileList [("Page.hs", EntryFile), ("Sections.hs", EntryFile)])
    noProject

searchSnapshot :: IO Snapshot
searchSnapshot =
  newSnapshot
    fixtureRoot
    ( fileList
        [ ("Data/IntMap.hs", EntryFile),
          ("Data/Map.hs", EntryFile),
          ("Util.hs", EntryFile)
        ]
    )
    noProject

-- | No cabal.project, no packages.
noProject :: Project
noProject =
  Project
    { source = ImplicitRootCabalFiles,
      withCompiler = Nothing,
      ghc = fixtureGhc,
      packages = [],
      brokenPackages = [],
      warnings = []
    }

-- | No --ghc, no with-compiler: both fixtures just take the parser's
-- own version.
fixtureGhc :: DetectedGhc
fixtureGhc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser}

fixtureRoot :: CanonicalRoot
fixtureRoot = CanonicalRoot {path = "/fixture", bytes = "/fixture"}

fileList :: [(ByteString, EntryKind)] -> FileList
fileList entries =
  FileList
    { source = FromGit,
      entries = Map.fromList (mapMaybe entry entries),
      skippedNames = 0,
      truncated = False
    }
  where
    entry (p, k) = (,k) <$> relPathFromBytes p

relPath :: ByteString -> RelPath
relPath p = maybe (error ("bad path " <> show p)) id (relPathFromBytes p)

relDir :: Text -> RelDir
relDir d =
  maybe (error ("bad dir " <> show d)) id (relDirFromSegments (Text.splitOn "/" d))

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
    ("vendor/nested", NestedRepo),
    -- Modules of the project fixture.
    ("pkg/src/Foo.hs", TextContent fooModule),
    ("pkg/src/Lit.lhs", TextContent "> module Lit (x) where\n> x = 1\n"),
    ("pkg/src/Sig.hsig", TextContent "signature Sig (T) where\ndata T\n"),
    -- A parse error after a tab.
    ("pkg/app/Main.hs", TextContent "module Main where\nmain =\t= 1\n"),
    ( "pkg/test/Tests.hs",
      TextContent "{-# LANGUAGE NoSuchThing #-}\nmodule Tests where\n"
    ),
    -- CPP: a macro in the export list, and a warning.
    ( "pkg/bench/Bench.hs",
      TextContent
        "{-# LANGUAGE CPP #-}\n#define M main\nmodule Main (M) where\n\
        \#if 1\nmain = pure ()\n#endif\n#warning slow\n"
    ),
    ("scripts/Tool.hs", TextContent "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n"),
    ("scripts/build-docs.hs", TextContent "main = pure ()\n"),
    -- Modules of the page fixture.
    ("Page.hs", TextContent pageModule),
    ("Sections.hs", TextContent sectionsModule),
    -- Modules of the search fixture.
    ( "Data/Map.hs",
      TextContent
        "module Data.Map (Map, insert, map, (!)) where\n\
        \data Map k v = Tip\n\
        \insert k v m = m\n\
        \map f m = m\n\
        \m ! k = undefined\n"
    ),
    ( "Data/IntMap.hs",
      TextContent
        "module Data.IntMap (IntMap, insert) where\n\
        \data IntMap v = Nil\n\
        \insert k v m = m\n"
    ),
    ( "Util.hs",
      TextContent
        "{-# LANGUAGE PatternSynonyms #-}\n\
        \module Util (Config (..), Mappable (..), pattern MapNone, (<+>)) where\n\
        \data Config = Mapping {mapSize :: Int}\n\
        \class Mappable f where\n\
        \  fmapIt :: (a -> b) -> f a -> f b\n\
        \pattern MapNone = Nothing\n\
        \a <+> b = a\n\
        \mapHelper = 1\n"
    ),
    -- Modules of the implicit fixture.
    ("Main.hs", Binary (FileSize 10)),
    ( "src/Data/Deep/Mod.hs",
      TooLarge TooLargeInfo {size = FileSize 2_000_000, limit = textLimit}
    )
  ]
  where
    link p t =
      Link
        LinkInfo
          { target = t,
            resolved =
              relPathFromBytes p >>= (`resolveLinkTarget` Text.Encoding.encodeUtf8 t)
          }

-- | A header with every kind of export, import and doc.
fooModule :: Text
fooModule =
  Text.unlines
    [ "{-# LANGUAGE PatternSynonyms, ExplicitNamespaces, NamedDefaults #-}",
      "{-# LANGUAGE Trustworthy #-}",
      "{-# LANGUAGE ImportQualifiedPost #-}",
      "{-# OPTIONS_GHC -Wall #-}",
      "",
      "-- | Foo things.",
      "module Foo",
      "  {-# DEPRECATED \"Use Bar\" #-}",
      "  ( -- * Values",
      "    f, -- ^ The f.",
      "    {-# WARNING in \"x-old\" \"Going\" #-} old,",
      "    (+++),",
      "    Q.qualified,",
      "",
      "    -- ** Types",
      "    T,",
      "    U (..),",
      "    V (A, b),",
      "    W (.., C),",
      "    type (:+:),",
      "    pattern P,",
      "    data D,",
      "    default Num,",
      "",
      "    -- $chunk",
      "",
      "    -- | Re-exports.",
      "    module Data.Map,",
      "  )",
      "where",
      "",
      "import Data.Map",
      "import qualified Data.Map as Q",
      "import Data.Set qualified as S (Set, empty)",
      "import Data.Char hiding (ord)",
      "import {-# SOURCE #-} Loop (Loop)",
      "import safe Data.Bits",
      "import \"base\" Data.Word ()",
      "import splice Language.Haskell.TH (Q)",
      "import Control.Monad quote (when)"
    ]

-- | Every kind of declaration, instance placement and doc markup.
-- No @\@since pkg-1.0@: GHC 9.10 reads it differently (D-150).
pageModule :: Text
pageModule =
  Text.unlines
    [ "{-# LANGUAGE TypeFamilies, TypeData, GADTs, PatternSynonyms #-}",
      "{-# LANGUAGE DerivingStrategies, StandaloneDeriving, TemplateHaskell #-}",
      "",
      "-- | Every kind of declaration. See 'area', 'Missing', \"Sections\",",
      "-- \"Sections#v:x\", [the sections](\"Sections\"), \"Nowhere\" and",
      "-- <https://example.com/a?b=1 the site>. Not a link: <javascript:alert(1)>.",
      "-- Math: \\(a + b\\).",
      "--",
      "-- 9007199254740993. A big item number.",
      "--",
      "-- @since 9007199254740993.2",
      "module Page",
      "  ( Shape (..),",
      "    Box (..),",
      "    area,",
      "    perimeter,",
      "    (<+>),",
      "    pattern Unit,",
      "    Container (..),",
      "    Nat,",
      "    G (..),",
      "    Family,",
      "    Closed,",
      "  )",
      "where",
      "",
      "import Data.Kind (Type)",
      "",
      "-- | A shape, with 'Circle' and t'Shape'.",
      "data Shape = Circle Double | Rect {width :: Double, height :: !Double}",
      "  deriving (Eq, Show)",
      "",
      "-- | A box.",
      "newtype Box a = Box a",
      "  deriving newtype (Eq)",
      "",
      "-- | Both share one signature.",
      "area, perimeter :: Shape -> Double",
      "area _ = 0",
      "perimeter _ = 0",
      "",
      "infixl 6 <+>",
      "",
      "(<+>) :: Shape -> Shape -> Shape",
      "a <+> _ = a",
      "",
      "pattern Unit :: Shape",
      "pattern Unit = Circle 1",
      "",
      "-- | A container.",
      "--",
      "-- = Usage",
      "--",
      "-- * /one/ __two__ @three@",
      "--",
      "-- [term]: definition",
      "--",
      "-- > code",
      "--",
      "-- >>> 1 + 1",
      "-- 2",
      "--",
      "-- prop> x == x",
      "--",
      "-- +---+",
      "-- | a |",
      "-- +---+",
      "class Container f where",
      "  type Elem f :: Type",
      "  empty :: f",
      "  insert :: Elem f -> f -> f",
      "  {-# MINIMAL empty, insert #-}",
      "",
      "instance Container [a] where",
      "  type Elem [a] = a",
      "  empty = []",
      "  insert = (:)",
      "",
      "instance Ord Shape where",
      "  compare _ _ = EQ",
      "",
      "instance Num Bool where",
      "  fromInteger = (/= 0)",
      "",
      "type data Nat = Zero | Succ Nat",
      "",
      "data G a where",
      "  GInt :: {gval :: Int} -> G Int",
      "  GBool, GTrue :: Bool -> G Bool",
      "",
      "data P2 = P2 {px, py :: Double}",
      "",
      "-- A field in two constructors; fh's doc is fb's (B's ff).",
      "data Fld",
      "  = FA {ff :: Int}",
      "  | FB",
      "      { ff, fh :: Int",
      "        -- ^ Shared doc.",
      "      }",
      "",
      "newtype V = V Int deriving (Eq, Ord) via Int",
      "",
      "data Pair = Int :* Int",
      "",
      "type Area = Double",
      "",
      "data family DF a",
      "",
      "type family Family a",
      "",
      "type instance Family Shape = Double",
      "",
      "type family Closed a where",
      "  Closed Int = Bool",
      "  Closed a = a",
      "",
      "deriving stock instance Ord (Box Int)",
      "",
      "helper, helper2 :: Int",
      "helper = 1",
      "helper2 = 2",
      "",
      "{-# DEPRECATED helper, helper2 \"Use area\" #-}",
      "{-# RULES \"area/id\" forall s. area s = area s #-}",
      "{-# ANN helper \"note\" #-}",
      "{-# COMPLETE Unit #-}",
      "default (Int)",
      "",
      "$(pure [])"
    ]

-- | Headings, chunks and export sections.
sectionsModule :: Text
sectionsModule =
  Text.unlines
    [ "-- | Sections.",
      "module Sections",
      "  ( -- * Values",
      "",
      "    -- | Some values.",
      "    x,",
      "",
      "    -- ** More",
      "",
      "    -- $chunk",
      "    y,",
      "  )",
      "where",
      "",
      "-- * Body heading",
      "",
      "-- $chunk",
      "-- A named chunk, about 'x'.",
      "",
      "x :: Int",
      "x = 1",
      "",
      "-- | Like 'x'.",
      "y :: Int",
      "y = 2"
    ]

-- * Calling the app

-- | Status, headers and body of one request.
data ApiResult = ApiResult
  { status :: Int,
    headers :: [Header],
    body :: LBS.ByteString
  }

-- | GET through the real app in process, with the given fakes, and the
-- parse workers running meanwhile. The path is after the token.
getApi :: TestApi -> [Text] -> IO ApiResult
getApi = requestApi "GET"

-- | Like 'getApi', with this raw query string (@?q=...@).
getApiQuery :: TestApi -> [Text] -> ByteString -> IO ApiResult
getApiQuery api path query = sendApiQuery "GET" [goodOrigin] NoBody query api path

-- | Like 'getApi', with these request headers.
getApiWith :: [Header] -> TestApi -> [Text] -> IO ApiResult
getApiWith = requestApiWith "GET"

-- | Like 'getApi', with the method given, and our own Origin (the
-- browser sends it on POST).
requestApi :: ByteString -> TestApi -> [Text] -> IO ApiResult
requestApi method = requestApiWith method [goodOrigin]

-- | Like 'requestApi', with these request headers only.
requestApiWith :: ByteString -> [Header] -> TestApi -> [Text] -> IO ApiResult
requestApiWith method headers = sendApi method headers NoBody

-- | The Origin of the page 'getApi' talks to.
goodOrigin :: Header
goodOrigin = ("Origin", "http://127.0.0.1:8080")

-- | A request body.
data RequestBody
  = NoBody
  | -- | With a known length.
    Sized ByteString
  | -- | Chunked: no length.
    Chunked [ByteString]

-- | Like 'requestApiWith', with a body.
sendApi :: ByteString -> [Header] -> RequestBody -> TestApi -> [Text] -> IO ApiResult
sendApi method headers reqBody = sendApiQuery method headers reqBody ""

-- | Like 'sendApi', with a raw query string.
sendApiQuery ::
  ByteString -> [Header] -> RequestBody -> ByteString -> TestApi -> [Text] -> IO ApiResult
sendApiQuery method headers reqBody query api path = do
  listed <- api.snapshot
  prefsStore <- api.prefs
  clients <- newClients
  let events =
        EventStream
          { clients = clients,
            keepAlive = Microseconds 1_000_000,
            indexing = pure Nothing,
            minGap = Microseconds 0,
            logger = newLogger LevelInfo (\_ -> pure ())
          }
      files =
        FilesApi
          { snapshot = pure listed,
            readEntry = api.readEntry,
            logger = api.logger
          }
      routes = Routes {events = events, files = files, prefs = prefsStore}
      app = application (Port 8080) apiToken routes
      call = do
        r <-
          timeout 10_000_000 $
            callAppBody app method headers reqBody query host (tokenText apiToken : path)
        maybe (assertFailure ("no answer within 10 s: " <> show path)) pure r
      host = "127.0.0.1:8080"
  case listed of
    Right snap
      | api.workers > 0 ->
          withAsync (runWorkers (indexDeps api snap) api.workers snap.index) (const call)
    _ -> call

-- | Call an app with a request that ends. The path has the token.
callApp :: Application -> ByteString -> ByteString -> [Text] -> IO ApiResult
callApp app method = callAppWith app method []

-- | Like 'callApp', with these request headers.
callAppWith ::
  Application -> ByteString -> [Header] -> ByteString -> [Text] -> IO ApiResult
callAppWith app method headers = callAppBody app method headers NoBody ""

callAppBody ::
  Application ->
  ByteString ->
  [Header] ->
  RequestBody ->
  -- | The raw query string.
  ByteString ->
  ByteString ->
  [Text] ->
  IO ApiResult
callAppBody app method headers reqBody query host path = do
  chunks <- newIORef $ case reqBody of
    NoBody -> []
    Sized b -> [b]
    Chunked bs -> bs
  let next = atomicModifyIORef' chunks $ \case
        [] -> ([], BS.empty)
        b : rest -> (rest, b)
      bodyLength = case reqBody of
        NoBody -> KnownLength 0
        Sized b -> KnownLength (fromIntegral (BS.length b))
        Chunked _ -> ChunkedBody
      req =
        setRequestBodyChunks next $
          defaultRequest
            { requestMethod = method,
              requestHeaders = headers,
              requestHeaderHost = Just host,
              requestBodyLength = bodyLength,
              pathInfo = path,
              rawQueryString = query
            }
  saved <- newIORef Nothing
  ResponseReceived <- app req $ \resp -> do
    writeIORef saved (Just resp)
    pure ResponseReceived
  readIORef saved >>= \case
    Nothing -> assertFailure "no response"
    Just resp -> do
      let (st, hs, withBody) = responseToStream resp
      out <- newIORef mempty
      withBody $ \streaming -> streaming (\b -> modifyIORef' out (<> b)) (pure ())
      b <- readIORef out
      pure
        ApiResult
          { status = statusCode st,
            headers = hs,
            body = Builder.toLazyByteString b
          }

-- | The token of 'getApi'.
apiToken :: Token
apiToken = tokenFromBytes "0123456789abcdefghijklmn"
