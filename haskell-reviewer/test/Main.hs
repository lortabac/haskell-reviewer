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
import HaskellReviewer.Core.CodeTest qualified
import HaskellReviewer.Core.ConfigGoldenTest qualified
import HaskellReviewer.Core.CppGoldenTest qualified
import HaskellReviewer.Core.CppTest qualified
import HaskellReviewer.Core.DeclsGoldenTest qualified
import HaskellReviewer.Core.DeclsTest qualified
import HaskellReviewer.Core.DocLinksTest qualified
import HaskellReviewer.Core.FileListTest qualified
import HaskellReviewer.Core.GitTest qualified
import HaskellReviewer.Core.HaddockTest qualified
import HaskellReviewer.Core.IndexQueueTest qualified
import HaskellReviewer.Core.InstanceKeyTest qualified
import HaskellReviewer.Core.ModulesGoldenTest qualified
import HaskellReviewer.Core.ModulesTest qualified
import HaskellReviewer.Core.NameTableTest qualified
import HaskellReviewer.Core.PackageGlobTest qualified
import HaskellReviewer.Core.ParseGoldenTest qualified
import HaskellReviewer.Core.ParserTest qualified
import HaskellReviewer.Core.PrefsTest qualified
import HaskellReviewer.Core.ProjectFileTest qualified
import HaskellReviewer.Core.ProjectGoldenTest qualified
import HaskellReviewer.Core.ProjectLoadTest qualified
import HaskellReviewer.Core.ProjectTest qualified
import HaskellReviewer.Core.RelPathTest qualified
import HaskellReviewer.Core.RichTextGoldenTest qualified
import HaskellReviewer.Core.RootTest qualified
import HaskellReviewer.Core.SourceTextTest qualified
import HaskellReviewer.Core.TokensTest qualified
import HaskellReviewer.Core.XdgTest qualified
import HaskellReviewer.Effects.AtomicFileTest qualified
import HaskellReviewer.Effects.Config (warmUpConfig)
import HaskellReviewer.Effects.ConfigTest qualified
import HaskellReviewer.Effects.FileSystemTest qualified
import HaskellReviewer.Effects.GitTest qualified
import HaskellReviewer.Effects.HelperTest qualified
import HaskellReviewer.Effects.LogTest qualified
import HaskellReviewer.Effects.Parser (warmUpParser)
import HaskellReviewer.Effects.Spawn (prepareSpawning)
import HaskellReviewer.Effects.SpawnTest qualified
import HaskellReviewer.Effects.TraceTest qualified
import HaskellReviewer.Server.ApiGoldenTest qualified
import HaskellReviewer.Server.ApiTest qualified
import HaskellReviewer.Server.AssetsTest qualified
import HaskellReviewer.Server.CborTest qualified
import HaskellReviewer.Server.EventsTest qualified
import HaskellReviewer.Server.ListenTest qualified
import HaskellReviewer.Server.PageTest qualified
import HaskellReviewer.Server.Prefs (warmUpPrefs)
import HaskellReviewer.Server.Router (warmUpGzip)
import HaskellReviewer.Server.RouterTest qualified
import HaskellReviewer.Server.Wire.Page (warmUpPages)
import HaskellReviewer.Session.ClientsTest qualified
import HaskellReviewer.Session.IndexTest qualified
import HaskellReviewer.Session.PrefsTest qualified
import HaskellReviewer.Session.SnapshotTest qualified
import Test.Tasty (defaultMain, testGroup, withResource)
import TestSupport (warmUpTempDirs)

main :: IO ()
main = do
  -- Like the real main, first: the tests spawn a lot, and some check
  -- what children inherit. See Note [No inherited fds] in
  -- "HaskellReviewer.Effects.Spawn".
  _ <- prepareSpawning
  -- The binary writes UTF-8 on stderr. Read it as UTF-8 even when the
  -- test runs under LC_ALL=C.
  setLocaleEncoding utf8
  -- Many tests make temp dirs, and the first ones run at the same time:
  -- build the shared values behind temp dir names once
  -- (Q-RTS-BLACKHOLE). Without this the 9.10 binary hung about 1 run in
  -- 100 in "Config load".
  warmUpTempDirs
  -- Tests run in parallel: share expensive values through withResource.
  -- See Note [Shared values in tests] in HaskellReviewer.Core.PackageGlobTest.
  defaultMain $
    testGroup
      "haskell-reviewer"
      [ Bench.ProjectsTest.tests,
        Bench.RtsStatsTest.tests,
        Bench.StatsTest.tests,
        HaskellReviewer.App.CliTest.tests,
        -- Each test runs serve, which runs warmUpGzip and warmUpPrefs:
        -- tests in parallel would build their shared values at once
        -- (Q-RTS-BLACKHOLE). Built once here first.
        withResource (warmUpGzip >> warmUpPrefs) (const (pure ())) $ \_ ->
          HaskellReviewer.App.RunTest.tests,
        HaskellReviewer.Core.AddressTest.tests,
        HaskellReviewer.Core.CabalFileTest.tests,
        -- toml-parser's and aeson's shared values, built before the
        -- tests use them in parallel (Q-RTS-BLACKHOLE).
        withResource (warmUpConfig >> warmUpPrefs) (const (pure ())) $ \_ ->
          testGroup
            "Settings"
            [ HaskellReviewer.Core.ConfigGoldenTest.tests,
              HaskellReviewer.Core.PrefsTest.tests,
              HaskellReviewer.Effects.ConfigTest.tests,
              HaskellReviewer.Session.PrefsTest.tests
            ],
        HaskellReviewer.Core.FileListTest.tests,
        HaskellReviewer.Core.GitTest.tests,
        HaskellReviewer.Core.IndexQueueTest.tests,
        HaskellReviewer.Core.ModulesGoldenTest.tests,
        HaskellReviewer.Core.ModulesTest.tests,
        HaskellReviewer.Core.PackageGlobTest.tests,
        HaskellReviewer.Core.ProjectFileTest.tests,
        HaskellReviewer.Core.ProjectGoldenTest.tests,
        HaskellReviewer.Core.ProjectLoadTest.tests,
        HaskellReviewer.Core.ProjectTest.tests,
        HaskellReviewer.Core.RelPathTest.tests,
        HaskellReviewer.Core.RootTest.tests,
        HaskellReviewer.Core.TokensTest.tests,
        HaskellReviewer.Core.XdgTest.tests,
        HaskellReviewer.Effects.AtomicFileTest.tests,
        HaskellReviewer.Effects.FileSystemTest.tests,
        HaskellReviewer.Effects.GitTest.tests,
        HaskellReviewer.Effects.HelperTest.tests,
        HaskellReviewer.Effects.LogTest.tests,
        HaskellReviewer.Effects.SpawnTest.tests,
        HaskellReviewer.Effects.TraceTest.tests,
        -- Build the parser's and CPP's shared values before the tests
        -- use them in parallel (Q-RTS-BLACKHOLE). A smaller warm-up
        -- still hung once in 300 runs on 9.12. The API tests parse too,
        -- and encode pages (Note [Page warm-up] in
        -- "HaskellReviewer.Server.Wire.Page").
        withResource (warmUpParser >> warmUpPages) (const (pure ())) $ \_ ->
          testGroup
            "Parsing"
            [ HaskellReviewer.Core.CodeTest.tests,
              HaskellReviewer.Core.CppGoldenTest.tests,
              HaskellReviewer.Core.CppTest.tests,
              HaskellReviewer.Core.DeclsGoldenTest.tests,
              HaskellReviewer.Core.DeclsTest.tests,
              HaskellReviewer.Core.DeclsTest.linearTests,
              HaskellReviewer.Core.DocLinksTest.tests,
              HaskellReviewer.Core.HaddockTest.tests,
              HaskellReviewer.Core.HaddockTest.linearTests,
              HaskellReviewer.Core.InstanceKeyTest.tests,
              HaskellReviewer.Core.NameTableTest.tests,
              HaskellReviewer.Core.ParseGoldenTest.tests,
              HaskellReviewer.Core.ParserTest.tests,
              HaskellReviewer.Core.RichTextGoldenTest.tests,
              HaskellReviewer.Core.SourceTextTest.tests,
              HaskellReviewer.Session.IndexTest.parsingTests,
              -- Both share the API fixtures: built once, here. And the
              -- prefs route runs in parallel: warmed up like the server.
              withResource (forceFixtures >> warmUpPrefs) (const (pure ())) $ \_ ->
                testGroup
                  "API"
                  [ HaskellReviewer.Server.ApiGoldenTest.tests,
                    HaskellReviewer.Server.ApiTest.tests
                  ],
              HaskellReviewer.Server.PageTest.tests
            ],
        HaskellReviewer.Server.AssetsTest.tests,
        HaskellReviewer.Server.CborTest.tests,
        HaskellReviewer.Server.EventsTest.tests,
        HaskellReviewer.Server.ListenTest.tests,
        HaskellReviewer.Server.RouterTest.tests,
        HaskellReviewer.Session.ClientsTest.tests,
        HaskellReviewer.Session.IndexTest.tests,
        HaskellReviewer.Session.SnapshotTest.tests,
        ExeTest.tests
      ]
