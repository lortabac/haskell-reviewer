module Main (main) where

import ApiSupport (forceFixtures)
import Bench.ProjectsTest qualified
import Bench.RtsStatsTest qualified
import Bench.StatsTest qualified
import ExeTest qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import HaskellReviewer.App.CliTest qualified
import HaskellReviewer.App.RunTest qualified
import HaskellReviewer.Core.AddressTest qualified
import HaskellReviewer.Core.CabalFileTest qualified
import HaskellReviewer.Core.FileListTest qualified
import HaskellReviewer.Core.GitTest qualified
import HaskellReviewer.Core.ModulesGoldenTest qualified
import HaskellReviewer.Core.ModulesTest qualified
import HaskellReviewer.Core.PackageGlobTest qualified
import HaskellReviewer.Core.ProjectFileTest qualified
import HaskellReviewer.Core.ProjectGoldenTest qualified
import HaskellReviewer.Core.ProjectLoadTest qualified
import HaskellReviewer.Core.RelPathTest qualified
import HaskellReviewer.Core.RootTest qualified
import HaskellReviewer.Effects.FileSystemTest qualified
import HaskellReviewer.Effects.GitTest qualified
import HaskellReviewer.Effects.HelperTest qualified
import HaskellReviewer.Effects.LogTest qualified
import HaskellReviewer.Effects.Spawn (prepareSpawning)
import HaskellReviewer.Effects.SpawnTest qualified
import HaskellReviewer.Effects.TraceTest qualified
import HaskellReviewer.Server.ApiGoldenTest qualified
import HaskellReviewer.Server.ApiTest qualified
import HaskellReviewer.Server.AssetsTest qualified
import HaskellReviewer.Server.EventsTest qualified
import HaskellReviewer.Server.ListenTest qualified
import HaskellReviewer.Server.RouterTest qualified
import HaskellReviewer.Session.ClientsTest qualified
import HaskellReviewer.Session.SnapshotTest qualified
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main = do
  -- Like the real main, first: the tests spawn a lot, and some check
  -- what children inherit. See Note [No inherited fds] in
  -- "HaskellReviewer.Effects.Spawn".
  _ <- prepareSpawning
  -- The binary writes UTF-8 on stderr. Read it as UTF-8 even when the
  -- test runs under LC_ALL=C.
  setLocaleEncoding utf8
  -- Tests run in parallel: share expensive values through withResource.
  -- See Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
  defaultMain $
    testGroup
      "haskell-reviewer"
      [ Bench.ProjectsTest.tests,
        Bench.RtsStatsTest.tests,
        Bench.StatsTest.tests,
        HaskellReviewer.App.CliTest.tests,
        HaskellReviewer.App.RunTest.tests,
        HaskellReviewer.Core.AddressTest.tests,
        HaskellReviewer.Core.CabalFileTest.tests,
        HaskellReviewer.Core.FileListTest.tests,
        HaskellReviewer.Core.GitTest.tests,
        HaskellReviewer.Core.ModulesGoldenTest.tests,
        HaskellReviewer.Core.ModulesTest.tests,
        HaskellReviewer.Core.PackageGlobTest.tests,
        HaskellReviewer.Core.ProjectFileTest.tests,
        HaskellReviewer.Core.ProjectGoldenTest.tests,
        HaskellReviewer.Core.ProjectLoadTest.tests,
        HaskellReviewer.Core.RelPathTest.tests,
        HaskellReviewer.Core.RootTest.tests,
        HaskellReviewer.Effects.FileSystemTest.tests,
        HaskellReviewer.Effects.GitTest.tests,
        HaskellReviewer.Effects.HelperTest.tests,
        HaskellReviewer.Effects.LogTest.tests,
        HaskellReviewer.Effects.SpawnTest.tests,
        HaskellReviewer.Effects.TraceTest.tests,
        -- Both share the API fixtures: built once, here.
        withResource forceFixtures (const (pure ())) $ \_ ->
          testGroup
            "API"
            [ HaskellReviewer.Server.ApiGoldenTest.tests,
              HaskellReviewer.Server.ApiTest.tests
            ],
        HaskellReviewer.Server.AssetsTest.tests,
        HaskellReviewer.Server.EventsTest.tests,
        HaskellReviewer.Server.ListenTest.tests,
        HaskellReviewer.Server.RouterTest.tests,
        HaskellReviewer.Session.ClientsTest.tests,
        HaskellReviewer.Session.SnapshotTest.tests,
        ExeTest.tests
      ]
