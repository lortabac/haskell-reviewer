-- | Tests that run the real binary.
module ExeTest (tests) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.List (isInfixOf)
import Data.Text qualified as Text
import HaskellReviewer.App.Version (toolVersion)
import System.Directory qualified as Dir
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Executable"
    [ testCase "temp dir as ROOT: exit 0" $
        withTemp $ \dir -> do
          (code, _) <- runExe [dir]
          code @?= ExitSuccess,
      testCase "--help: exit 0, flags on stderr" $ do
        (code, err) <- runExe ["--help"]
        code @?= ExitSuccess
        assertContains "--port" err,
      testCase "--version: exit 0, same version as the library" $ do
        (code, err) <- runExe ["--version"]
        code @?= ExitSuccess
        -- Catches a stale binary on PATH.
        assertContains ("haskell-reviewer " <> Text.unpack toolVersion) err,
      testCase "--bogus: exit 2" $ do
        (code, _) <- runExe ["--bogus"]
        code @?= ExitFailure 2,
      testCase "--port abc: exit 2" $ do
        (code, _) <- runExe ["--port", "abc"]
        code @?= ExitFailure 2,
      testCase "missing ROOT: exit 2" $ do
        (code, err) <- runExe ["/no/such/dir"]
        code @?= ExitFailure 2
        assertContains "ROOT does not exist" err,
      testCase "file as ROOT: exit 2" $
        withTemp $ \dir -> do
          let file = dir </> "file"
          writeFile file ""
          (code, err) <- runExe [file]
          code @?= ExitFailure 2
          assertContains "ROOT is not a directory" err,
      testCase "unreadable ROOT: exit 2" $
        withTemp $ \dir -> do
          let sub = dir </> "locked"
              -- Give the rights back, so the temp dir can be removed.
              unlock =
                Dir.setPermissions sub $
                  Dir.setOwnerReadable True $
                    Dir.setOwnerWritable True $
                      Dir.setOwnerSearchable True Dir.emptyPermissions
          Dir.createDirectory sub
          flip finally unlock $ do
            Dir.setPermissions sub Dir.emptyPermissions
            perms <- Dir.getPermissions sub
            -- Root can read anything: nothing to test then.
            unless (Dir.readable perms) $ do
              (code, err) <- runExe [sub]
              code @?= ExitFailure 2
              assertContains "cannot read ROOT" err,
      testCase "--trace-file: one startup span" $
        withTemp $ \dir -> do
          let file = dir </> "t.jsonl"
          (code, _) <- runExe ["--trace-file", file, dir]
          code @?= ExitSuccess
          contents <- readFile file
          case lines contents of
            [line] -> assertContains "\"name\":\"startup\"" line
            ls -> assertFailure ("expected one line, got " <> show ls),
      testCase "--trace-file /dev/full: exit 0 with a warning" $ do
        -- Linux only: elsewhere, nothing to do.
        hasDevFull <- Dir.doesPathExist "/dev/full"
        when hasDevFull $
          withTemp $ \dir -> do
            (code, err) <- runExe ["--trace-file", "/dev/full", dir]
            code @?= ExitSuccess
            assertContains "haskell-reviewer: warning: trace file:" err,
      testCase "--trace-file in a missing dir: exit 2" $
        withTemp $ \dir -> do
          (code, err) <- runExe ["--trace-file", dir </> "missing" </> "t.jsonl", dir]
          code @?= ExitFailure 2
          assertContains "cannot open trace file" err,
      testCase "non-ASCII ROOT with LC_ALL=C: exit 2, not 1" $ do
        -- "dïr" as raw UTF-8 bytes (C3 AF), written as GHC's escapes for
        -- undecodable bytes, so the test works whatever our own locale is.
        -- The binary prints them as U+FFFD.
        let root = "/no/such/d\xDCC3\xDCAFr"
        env <- getEnvironment
        let cEnv = ("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) env
        (code, err) <- runExeWith (\p -> p {env = Just cEnv}) [root]
        code @?= ExitFailure 2
        assertContains "ROOT does not exist: /no/such/d" err
    ]

withTemp :: (FilePath -> IO a) -> IO a
withTemp = withSystemTempDirectory "haskell-reviewer-test"

runExe :: [String] -> IO (ExitCode, String)
runExe = runExeWith id

-- | Run the binary, check that stdout is empty, give back the exit code
-- and stderr.
runExeWith :: (CreateProcess -> CreateProcess) -> [String] -> IO (ExitCode, String)
runExeWith tweak args = do
  exe <- findExe
  (code, out, err) <- readCreateProcessWithExitCode (tweak (proc exe args)) ""
  out @?= ""
  pure (code, err)

-- | @HASKELL_REVIEWER_EXE@ if set, else the binary on PATH (cabal puts it
-- there through build-tool-depends).
findExe :: IO FilePath
findExe = do
  override <- lookupEnv "HASKELL_REVIEWER_EXE"
  case override of
    Just exe -> pure exe
    Nothing -> do
      found <- Dir.findExecutable "haskell-reviewer"
      case found of
        Just exe -> pure exe
        Nothing ->
          assertFailure
            "haskell-reviewer not found: run through cabal test, \
            \or set HASKELL_REVIEWER_EXE"

assertContains :: String -> String -> Assertion
assertContains needle haystack =
  assertBool
    (show needle <> " not found in " <> show haystack)
    (needle `isInfixOf` haystack)
