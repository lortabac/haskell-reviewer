-- | Shared by the API tests: fixed fake projects, and calling the app
-- in process.
module ApiSupport
  ( -- * The fixture projects
    TestApi (..),
    fixtureApi,
    projectApi,
    implicitApi,
    forceFixtures,
    indexed,

    -- * Calling the app
    ApiResult (..),
    getApi,
    requestApi,
    callApp,
    apiToken,
  )
where

import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate)
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
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
import HaskellReviewer.Core.ModuleModel (ModuleParse)
import HaskellReviewer.Core.Parser (ParseSettings)
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
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError, ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import HaskellReviewer.Effects.Parser (parseGuarded)
import HaskellReviewer.Server.Api (FilesApi (..), newSnapshot)
import HaskellReviewer.Server.Events (EventStream (..))
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Session.Clients (newClients)
import HaskellReviewer.Session.Index (IndexDeps (..), runWorkers)
import HaskellReviewer.Session.Snapshot (Snapshot (files, index))
import Network.HTTP.Types (Header, Status (..))
import Network.Wai (Application, Request (..), defaultRequest, responseToStream)
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
    workers :: Int
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

-- | Build the shared inputs, before parallel tests use them. See
-- Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
forceFixtures :: IO ()
forceFixtures = do
  forM_ [fixtureSnapshot, projectSnapshot, implicitSnapshot] void
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
            runWorkers (indexDeps api snap) 1 snap.index
            pure (Right snap)
          listed -> pure listed,
      workers = 0
    }

apiFor :: IO Snapshot -> TestApi
apiFor snap =
  TestApi
    { snapshot = Right <$> snap,
      readEntry = \_ p _ ->
        pure (maybe (Left ReadGone) Right (lookup (relPathText p) contents)),
      parse = parseGuarded,
      logger = newLogger LevelInfo (\_ -> pure ()),
      workers = 1
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

-- | Like 'getApi', with the method given.
requestApi :: ByteString -> TestApi -> [Text] -> IO ApiResult
requestApi method api path = do
  listed <- api.snapshot
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
      app = application (Port 8080) apiToken Routes {events = events, files = files}
      call = do
        r <- timeout 10_000_000 (callApp app method host (tokenText apiToken : path))
        maybe (assertFailure ("no answer within 10 s: " <> show path)) pure r
      host = "127.0.0.1:8080"
  case listed of
    Right snap
      | api.workers > 0 ->
          withAsync (runWorkers (indexDeps api snap) api.workers snap.index) (const call)
    _ -> call

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
