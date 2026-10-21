module Main (main) where

import Bench.ProjectsTest qualified
import Bench.RtsStatsTest qualified
import Bench.StatsTest qualified
import ExeTest qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import HaskellReviewer.App.CliTest qualified
import HaskellReviewer.App.RunTest qualified
import HaskellReviewer.Core.AddressTest qualified
import HaskellReviewer.Core.FileListTest qualified
import HaskellReviewer.Core.GitTest qualified
import HaskellReviewer.Core.RelPathTest qualified
import HaskellReviewer.Core.RootTest qualified
import HaskellReviewer.Effects.FileSystemTest qualified
import HaskellReviewer.Effects.GitTest qualified
import HaskellReviewer.Effects.HelperTest qualified
import HaskellReviewer.Effects.LogTest qualified
import HaskellReviewer.Effects.TraceTest qualified
import HaskellReviewer.Server.ApiGoldenTest qualified
import HaskellReviewer.Server.ApiTest qualified
import HaskellReviewer.Server.AssetsTest qualified
import HaskellReviewer.Server.EventsTest qualified
import HaskellReviewer.Server.RouterTest qualified
import HaskellReviewer.Session.ClientsTest qualified
import HaskellReviewer.Session.SnapshotTest qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  -- The binary writes UTF-8 on stderr. Read it as UTF-8 even when the
  -- test runs under LC_ALL=C.
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "haskell-reviewer"
      [ Bench.ProjectsTest.tests,
        Bench.RtsStatsTest.tests,
        Bench.StatsTest.tests,
        HaskellReviewer.App.CliTest.tests,
        HaskellReviewer.App.RunTest.tests,
        HaskellReviewer.Core.AddressTest.tests,
        HaskellReviewer.Core.FileListTest.tests,
        HaskellReviewer.Core.GitTest.tests,
        HaskellReviewer.Core.RelPathTest.tests,
        HaskellReviewer.Core.RootTest.tests,
        HaskellReviewer.Effects.FileSystemTest.tests,
        HaskellReviewer.Effects.GitTest.tests,
        HaskellReviewer.Effects.HelperTest.tests,
        HaskellReviewer.Effects.LogTest.tests,
        HaskellReviewer.Effects.TraceTest.tests,
        HaskellReviewer.Server.ApiGoldenTest.tests,
        HaskellReviewer.Server.ApiTest.tests,
        HaskellReviewer.Server.AssetsTest.tests,
        HaskellReviewer.Server.EventsTest.tests,
        HaskellReviewer.Server.RouterTest.tests,
        HaskellReviewer.Session.ClientsTest.tests,
        HaskellReviewer.Session.SnapshotTest.tests,
        ExeTest.tests
      ]
