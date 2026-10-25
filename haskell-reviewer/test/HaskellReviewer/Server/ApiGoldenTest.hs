-- | Writes the API fixtures (D-50). See Note [API fixtures] in
-- "HaskellReviewer.Server.Wire".
--
-- Tests run from the package dir, so fixtures/api is one level up.
module HaskellReviewer.Server.ApiGoldenTest (tests) where

import ApiSupport (ApiResult (..), fixtureApi, getApi, implicitApi, projectApi)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Server.Api (FilesApi (..))
import HaskellReviewer.Server.Wire (helloPayload)
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "API fixtures" $
    testCase "every fixture file has a test, and the other way round" noStale
      : [goldenVsString name (fixtureDir <> name <> ".json") body | (name, body) <- cases]

-- | Each fixture: compact JSON and a newline.
cases :: [(String, IO LBS.ByteString)]
cases =
  [ ("events-hello", pure (LBS.fromStrict helloPayload <> "\n")),
    ("files", okBody fixtureApi ["api", "files", "tree"]),
    ("file-text", okBody fixtureApi (file ["app", "Main.hs"])),
    ("file-binary", okBody fixtureApi (file ["assets", "logo.png"])),
    ("file-too-large", okBody fixtureApi (file ["data", "big.csv"])),
    ("file-symlink", okBody fixtureApi (file ["docs", "guide.md"])),
    ("file-submodule", okBody fixtureApi (file ["vendor", "dep"])),
    ("modules", okBody projectApi ["api", "modules", "tree"]),
    ("modules-implicit", okBody implicitApi ["api", "modules", "tree"]),
    -- One per reason a module is text only, a parsed one and one after
    -- CPP.
    ("module-parsed", okBody projectApi (modulePath "Foo")),
    ("module-parse-error", okBody projectApi (modulePath "pkg:exe:app:Main")),
    ("module-pragma-error", okBody projectApi (modulePath "Tests")),
    ("module-preprocessed", okBody projectApi (modulePath "pkg:test:spec:Main")),
    ("module-cpp", okBody projectApi (modulePath "pkg:bench:slow:Main")),
    ("module-crashed", okBody crashingApi (modulePath "Foo")),
    ("module-too-slow", okBody slowApi (modulePath "Foo")),
    ("module-generated", okBody projectApi (modulePath "Lexer")),
    ("module-no-source", okBody projectApi (modulePath "Gone")),
    ("module-not-text", okBody implicitApi (modulePath "Main")),
    ("module-too-large", okBody implicitApi (modulePath "Data.Deep.Mod"))
  ]
  where
    file p = ["api", "file", "tree"] <> p
    modulePath ref = ["api", "module", "tree", ref]
    crashingApi = answering (ParserCrashed "boom")
    slowApi = answering TooSlow
    answering r =
      projectApi
        { parse = \_ _ -> pure (TextOnly TextOnlyModule {reason = r, warnings = []})
        }

fixtureDir :: FilePath
fixtureDir = "../fixtures/api/"

-- | A fixture no test writes would never be checked again.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".json"]
    @?= sort (map fst cases)
  [f | f <- files, takeExtension f /= ".json"] @?= []

-- | GET through the real app; must be a 200 with JSON.
okBody :: FilesApi -> [Text] -> IO LBS.ByteString
okBody api path = do
  r <- getApi api path
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")
