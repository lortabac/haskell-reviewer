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
    textSearchApi,
  )
import Control.Concurrent.MVar (newEmptyMVar)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config (SidebarView (..), Theme (..), UiDefaults (..))
import HaskellReviewer.Core.Eol (Filtering (..))
import HaskellReviewer.Core.FileContent (FileNotes (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpIndex)
import HaskellReviewer.Core.Sides (Side (..))
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
