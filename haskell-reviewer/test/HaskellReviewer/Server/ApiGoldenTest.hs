-- | Writes the API fixtures (D-50). See Note [API fixtures] in
-- "HaskellReviewer.Server.Wire".
--
-- Tests run from the package dir, so fixtures/api is one level up.
module HaskellReviewer.Server.ApiGoldenTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    fixtureApi,
    gatedOn,
    getApi,
    getApiQuery,
    implicitApi,
    indexed,
    pageApi,
    projectApi,
    refsApi,
    refsGatedOn,
    searchApi,
    sourceApi,
  )
import Control.Concurrent.MVar (newEmptyMVar)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config (SidebarView (..), Theme (..), UiDefaults (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpIndex)
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)
import HaskellReviewer.Server.Wire (encodeIndexing, helloPayload)
import HaskellReviewer.Server.Wire.Source (coveringView, encodeSource)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Prefs (newPrefsStore)
import HaskellReviewer.Session.Snapshot (Generation (..))
import System.Directory qualified as Dir
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "API fixtures" $
    testCase "every fixture file has a test, and the other way round" noStale
      : concatMap golden cases

-- | One fixture: its name and how it's written.
data Case = Case
  { name :: String,
    fixture :: Fixture
  }

-- | How a fixture is written.
data Fixture
  = -- | Compact JSON and a newline, to <name>.json.
    Json (IO LBS.ByteString)
  | -- | CBOR bytes to <name>.cbor, their JSON dump and a newline to
    -- <name>.cbor.json.
    Cbor (IO LBS.ByteString)

-- | The golden tests of one case: one per file it writes.
golden :: Case -> [TestTree]
golden c = case c.fixture of
  Json body -> [goldenVsString c.name (path ".json") body]
  Cbor body ->
    [ goldenVsString c.name (path ".cbor") body,
      goldenVsString (c.name <> " (dump)") (path ".cbor.json") $ do
        bytes <- body
        either
          (assertFailure . Text.unpack . renderCborJsonError)
          (pure . (<> "\n"))
          (cborToJson bytes)
    ]
  where
    path ext = fixtureDir <> c.name <> ext

cases :: [Case]
cases =
  [ json "events-hello" (pure (LBS.fromStrict helloPayload <> "\n")),
    json "events-indexing" (pure (event (encodeIndexing (Generation 1) halfway))),
    -- Done, with a failed module (a loose file readEntry doesn't have).
    json "indexing" (okJson (indexed projectApi) ["api", "indexing", "tree"]),
    json "files" (okJson fixtureApi ["api", "files", "tree"]),
    cbor "file-text" (okCbor fixtureApi (file ["app", "Main.hs"])),
    cbor "file-binary" (okCbor fixtureApi (file ["assets", "logo.png"])),
    cbor "file-too-large" (okCbor fixtureApi (file ["data", "big.csv"])),
    cbor "file-symlink" (okCbor fixtureApi (file ["docs", "guide.md"])),
    cbor "file-submodule" (okCbor fixtureApi (file ["vendor", "dep"])),
    json "modules" (okJson projectApi ["api", "modules", "tree"]),
    json "modules-implicit" (okJson implicitApi ["api", "modules", "tree"]),
    -- One per reason a module is text only, a parsed one and one after
    -- CPP.
    cbor "module-parsed" (okCbor projectApi (modulePath "Foo")),
    cbor "module-parse-error" (okCbor projectApi (modulePath "pkg:exe:app:Main")),
    cbor "module-pragma-error" (okCbor projectApi (modulePath "Tests")),
    cbor "module-preprocessed" (okCbor projectApi (modulePath "pkg:test:spec:Main")),
    cbor "module-cpp" (okCbor projectApi (modulePath "pkg:bench:slow:Main")),
    cbor "module-crashed" (okCbor crashingApi (modulePath "Foo")),
    cbor "module-too-slow" (okCbor slowApi (modulePath "Foo")),
    cbor "module-generated" (okCbor projectApi (modulePath "Lexer")),
    cbor "module-no-source" (okCbor projectApi (modulePath "Gone")),
    cbor "module-not-text" (okCbor implicitApi (modulePath "Main")),
    cbor "module-too-large" (okCbor implicitApi (modulePath "Data.Deep.Mod")),
    -- The page: every kind of declaration and doc markup; sections.
    cbor "module-page" (okCbor pageApi (modulePath "Page")),
    cbor "module-page-sections" (okCbor pageApi (modulePath "Sections")),
    -- Links out of the module, in code and docs; partial while an
    -- import isn't parsed.
    cbor "module-page-links" (okCbor (indexed sourceApi) (modulePath "C")),
    cbor "module-page-partial" $ do
      gate <- newEmptyMVar
      okCbor (gatedOn "s/src/A.hs" gate) {workers = 2} (modulePath "C"),
    json "module-not-found" (notFoundJson projectApi (modulePath "Nope")),
    json "prefs-defaults" (okJson fixtureApi ["api", "prefs"]),
    -- The source view: linked, CPP, partial (an import that never gets
    -- parsed), text only, plain, not found, and the warm-up's covering
    -- view (every value).
    cbor "source-parsed" (okCbor (indexed sourceApi) (source ["s", "src", "B.hs"])),
    cbor "source-cpp" (okCbor (indexed sourceApi) (source ["s", "src", "Cpp.hs"])),
    cbor "source-partial" $ do
      gate <- newEmptyMVar
      okCbor (gatedOn "s/src/A.hs" gate) {workers = 2} (source ["s", "src", "B.hs"]),
    cbor "source-text-only" (okCbor (indexed sourceApi) (source ["s", "src", "Bad.hs"])),
    cbor "source-plain" (okCbor sourceApi {workers = 0} (source ["s", "notes.txt"])),
    json "source-not-found" $
      notFoundJson sourceApi {workers = 0} (source ["s", "nope.hs"]),
    cbor "source-warm-up" $
      pure (encodeSource (Generation 1) resolveWarmUpIndex coveringView),
    -- Every kind of name, a private one and a field.
    json "search" (okSearch (indexed searchApi) "?q=map"),
    json "search-qualified" (okSearch (indexed searchApi) "?q=Data.Map.insert"),
    -- Nothing indexed yet: modules only.
    json "search-partial" (okSearch searchApi {workers = 0} "?q=map"),
    -- Find references: every kind, a guess; partial while a module
    -- isn't parsed; not found.
    json "refs" (okRefs (indexed refsApi) "?module=R.Shape&decl=val:area"),
    json "refs-partial" $ do
      gate <- newEmptyMVar
      okRefs
        (refsGatedOn "r/src/R/Use.hs" gate) {workers = 2}
        "?module=R.Shape&decl=con:Square",
    json "refs-not-found" $ do
      r <-
        getApiQuery
          (indexed refsApi)
          ["api", "refs", "tree"]
          "?module=R.Shape&decl=val:nope"
      r.status @?= 404
      lookup "Content-Type" r.headers @?= Just "application/json"
      pure (r.body <> "\n"),
    -- Every value other than the built-in one.
    json "prefs" (okJson fixtureApi {prefs = configured} ["api", "prefs"])
  ]
  where
    json n body = Case {name = n, fixture = Json body}
    cbor n body = Case {name = n, fixture = Cbor body}
    file p = ["api", "file", "tree"] <> p
    modulePath ref = ["api", "module", "tree", ref]
    source p = ["api", "source", "tree"] <> p
    crashingApi = answering (ParserCrashed "boom")
    slowApi = answering TooSlow
    halfway = Progress {total = 358, ready = 120, failed = 2}
    event e = LBS.fromStrict e <> "\n"
    configured =
      newPrefsStore
        (newLogger LevelInfo (\_ -> pure ()))
        UiDefaults {theme = ThemeDark, exportedOnly = True, sidebar = SidebarModules}
        (Left HomeUnset)
    answering r =
      projectApi
        { parse = \_ _ -> pure (TextOnly (textOnlyModule r []))
        }

fixtureDir :: FilePath
fixtureDir = "../fixtures/api/"

-- | A fixture no test writes would never be checked again.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort files @?= sort (concatMap fileNames cases)
  where
    fileNames c = case c.fixture of
      Json _ -> [c.name <> ".json"]
      Cbor _ -> [c.name <> ".cbor", c.name <> ".cbor.json"]

-- | GET through the real app; must be a 200 with JSON.
okJson :: TestApi -> [Text] -> IO LBS.ByteString
okJson api path = (<> "\n") <$> answer 200 "application/json" api path

-- | GET api/search/tree with this query string; must be a 200 with
-- JSON.
okSearch :: TestApi -> ByteString -> IO LBS.ByteString
okSearch api query = do
  r <- getApiQuery api ["api", "search", "tree"] query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET api/refs/tree with this query string; must be a 200 with JSON.
okRefs :: TestApi -> ByteString -> IO LBS.ByteString
okRefs api query = do
  r <- getApiQuery api ["api", "refs", "tree"] query
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")

-- | GET through the real app; must be a 200 with CBOR. The bytes as
-- they come.
okCbor :: TestApi -> [Text] -> IO LBS.ByteString
okCbor = answer 200 "application/cbor"

-- | GET through the real app; must be a 404 with JSON.
notFoundJson :: TestApi -> [Text] -> IO LBS.ByteString
notFoundJson api path = (<> "\n") <$> answer 404 "application/json" api path

-- | GET through the real app: check the status and the Content-Type,
-- give back the body.
answer :: Int -> ByteString -> TestApi -> [Text] -> IO LBS.ByteString
answer status contentType api path = do
  r <- getApi api path
  r.status @?= status
  lookup "Content-Type" r.headers @?= Just contentType
  pure r.body
