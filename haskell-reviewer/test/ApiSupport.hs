-- | Shared by the API tests: fixed fake projects, and calling the app
-- in process.
module ApiSupport
  ( -- * The fixture projects
    fixtureApi,
    projectApi,
    implicitApi,
    forceFixtures,

    -- * Calling the app
    ApiResult (..),
    getApi,
    requestApi,
    callApp,
    apiToken,
  )
where

import Control.Monad (forM_)
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
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    CabalProblem (..),
    Component (..),
    ComponentName (..),
    DetectedGhc (..),
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
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (FilesApi (..), forceSnapshot, newSnapshot)
import HaskellReviewer.Server.Events (EventStream (..))
import HaskellReviewer.Server.Router (Routes (..), application)
import HaskellReviewer.Session.Clients (newClients)
import HaskellReviewer.Session.Snapshot (Snapshot)
import Network.HTTP.Types (Header, Status (..))
import Network.Wai (Application, Request (..), defaultRequest, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Tasty.HUnit (assertFailure)

-- * The fixture projects

-- | A fixed snapshot and file contents. The file API fixtures come
-- from it.
fixtureApi :: FilesApi
fixtureApi = apiFor fixtureSnapshot

-- | A project with every kind of module source, role and no-source
-- reason, a broken package and warnings. For the module tree fixture.
projectApi :: FilesApi
projectApi = apiFor projectSnapshot

-- | No project: stack.yaml and package.yaml in ROOT, loose modules.
implicitApi :: FilesApi
implicitApi = apiFor implicitSnapshot

-- | Build the snapshots, before parallel tests share them. See
-- Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
forceFixtures :: IO ()
forceFixtures = forM_ [fixtureSnapshot, projectSnapshot, implicitSnapshot] forceSnapshot

apiFor :: Snapshot -> FilesApi
apiFor snap =
  FilesApi
    { snapshot = pure (Right snap),
      readEntry = \_ p _ ->
        pure (maybe (Left ReadGone) Right (lookup (relPathText p) contents)),
      logger = newLogger LevelInfo (\_ -> pure ())
    }

fixtureSnapshot :: Snapshot
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

projectSnapshot :: Snapshot
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
            { modules =
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

implicitSnapshot :: Snapshot
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
