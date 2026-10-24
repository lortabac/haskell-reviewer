module HaskellReviewer.App.CliTest (tests) where

import Data.Text qualified as Text
import HaskellReviewer.App.Cli
import HaskellReviewer.Core.Address (Port (..))
import HaskellReviewer.Core.Project (GhcVersion (..))
import HaskellReviewer.Effects.Log (LogLevel (..))
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Cli"
    [ testCase "defaults" $
        parseArgs [] @?= CliCommand (Run defaults),
      testCase "ROOT" $
        parseOk ["some/dir"] (\o -> o.root @?= "some/dir"),
      testCase "--port" $ do
        parseOk ["--port", "1"] (\o -> o.port @?= Just (Port 1))
        parseOk ["--port", "65535"] (\o -> o.port @?= Just (Port 65535)),
      testCase "bad --port" $
        mapM_ (\p -> usageError ["--port", p]) ["0", "65536", "-1", "abc", ""],
      testCase "--no-open" $
        parseOk ["--no-open"] (\o -> o.browser @?= NoBrowser),
      testCase "--idle-exit" $ do
        parseOk ["--idle-exit", "0"] (\o -> o.idleExit @?= IdleExitOff)
        parseOk
          ["--idle-exit", "30"]
          (\o -> o.idleExit @?= IdleExitAfter (Minutes 30))
        parseOk
          ["--idle-exit", show maxMinutes]
          (\o -> o.idleExit @?= IdleExitAfter maxIdleExit),
      testCase "bad --idle-exit" $
        mapM_
          (\m -> usageError ["--idle-exit", m])
          ["-3", "x", show (maxMinutes + 1), "99999999999999999999"],
      testCase "--verbose" $
        parseOk ["--verbose"] (\o -> o.logLevel @?= LevelVerbose),
      testCase "--debug" $
        parseOk ["--debug"] (\o -> o.logLevel @?= LevelDebug),
      testCase "--verbose --debug" $ do
        parseOk ["--verbose", "--debug"] (\o -> o.logLevel @?= LevelDebug)
        parseOk ["--debug", "--verbose"] (\o -> o.logLevel @?= LevelDebug),
      testCase "--trace-file" $
        parseOk ["--trace-file", "t.jsonl"] (\o -> o.traceFile @?= Just "t.jsonl"),
      testCase "--ghc" $ do
        parseOk ["--ghc", "9.12"] (\o -> o.ghc @?= Just (GhcVersion [9, 12]))
        parseOk ["--ghc", "9.12.2"] (\o -> o.ghc @?= Just (GhcVersion [9, 12, 2])),
      testCase "bad --ghc" $
        mapM_
          (\v -> usageError ["--ghc", v])
          ["9", "9.x", "9.12.", "9.99999999999.1", ""],
      testCase "--version" $
        parseArgs ["--version"] @?= CliCommand ShowVersion,
      testCase "--help lists every flag" $
        case parseArgs ["--help"] of
          CliExit ExitSuccess txt ->
            mapM_
              (\w -> assertBool (show w <> " in help") (w `Text.isInfixOf` txt))
              [ "ROOT",
                "--port",
                "--no-open",
                "--idle-exit",
                "--help",
                "--version",
                "--verbose",
                "--debug",
                "--trace-file",
                "--ghc"
              ]
          other -> assertFailure ("expected help, got " <> show other),
      testCase "unknown flag" $ usageError ["--bogus"],
      testCase "two ROOTs" $ usageError ["a", "b"]
    ]

defaults :: Options
defaults =
  Options
    { root = ".",
      port = Nothing,
      browser = OpenBrowser,
      idleExit = IdleExitAfter (Minutes 5),
      logLevel = LevelInfo,
      traceFile = Nothing,
      ghc = Nothing
    }

maxMinutes :: Int
maxMinutes = let Minutes n = maxIdleExit in n

parseOk :: [String] -> (Options -> Assertion) -> Assertion
parseOk args check = case parseArgs args of
  CliCommand (Run opts) -> check opts
  other -> assertFailure (show args <> ": expected Run, got " <> show other)

usageError :: [String] -> Assertion
usageError args = case parseArgs args of
  CliExit (ExitFailure 2) _ -> pure ()
  other -> assertFailure (show args <> ": expected exit 2, got " <> show other)
