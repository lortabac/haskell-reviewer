-- | Writes the API fixtures (D-50). See Note [API fixtures] in
-- "HaskellReviewer.Server.Wire".
--
-- Tests run from the package dir, so fixtures/api is one level up.
module HaskellReviewer.Server.ApiGoldenTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    fixtureApi,
    getApi,
    implicitApi,
    indexed,
    projectApi,
  )
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Server.Wire (encodeIndexing, helloPayload)
import HaskellReviewer.Session.Index (Progress (..))
import HaskellReviewer.Session.Snapshot (Generation (..))
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
