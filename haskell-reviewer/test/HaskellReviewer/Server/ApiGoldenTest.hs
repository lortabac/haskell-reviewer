-- | Writes the API fixtures (D-50). See Note [API fixtures] in
-- "HaskellReviewer.Server.Wire".
--
-- Tests run from the package dir, so fixtures/api is one level up.
module HaskellReviewer.Server.ApiGoldenTest (tests) where

import ApiSupport (ApiResult (..), fixtureApi, getApi)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
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
    ("files", okBody ["api", "files", "tree"]),
    ("file-text", okBody (file ["app", "Main.hs"])),
    ("file-binary", okBody (file ["assets", "logo.png"])),
    ("file-too-large", okBody (file ["data", "big.csv"])),
    ("file-symlink", okBody (file ["docs", "guide.md"])),
    ("file-submodule", okBody (file ["vendor", "dep"]))
  ]
  where
    file p = ["api", "file", "tree"] <> p

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
okBody :: [Text] -> IO LBS.ByteString
okBody path = do
  r <- getApi fixtureApi path
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/json"
  pure (r.body <> "\n")
