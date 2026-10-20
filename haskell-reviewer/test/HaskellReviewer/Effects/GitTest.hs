-- | Running git: the hardened env and flags, and the start-up probe
-- with fake gits.
module HaskellReviewer.Effects.GitTest (tests) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Text qualified as Text
import HaskellReviewer.Core.Git (GitVersion (..))
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.Git
  ( Call (..),
    CannotRun (..),
    Failure (..),
    GitError (..),
    GitSetup (..),
    Listing (..),
    TooOld (..),
    Unreadable (..),
    gitProbeTimeLimit,
    hardenedArgs,
    hardenedEnv,
    probeGitWith,
    renderGitError,
  )
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, newLogger)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import System.Posix.Signals (nullSignal, signalProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport (gitSetup, withCanonicalTemp, writeExecutable, writeScript)

tests :: TestTree
tests =
  testGroup
    "Git"
    [ testCase "hardenedEnv: built from scratch" $ do
        let inherited =
              [ ("HOME", "/home/u"),
                ("XDG_CONFIG_HOME", "/home/u/.config"),
                ("GIT_DIR", "/nope"),
                ("GIT_CONFIG_PARAMETERS", "'core.fsmonitor=evil'"),
                ("GIT_CONFIG_COUNT", "1"),
                ("GIT_CONFIG_KEY_0", "core.pager"),
                ("GIT_CONFIG_VALUE_0", "evil"),
                ("GIT_EXEC_PATH", "/evil"),
                ("LANG", "fr_FR.UTF-8"),
                ("LC_ALL", "fr_FR.UTF-8"),
                ("PAGER", "evil"),
                ("PATH", "/root/bin:/usr/bin")
              ]
        hardenedEnv inherited ["/usr/bin", "/bin"]
          @?= [ ("HOME", "/home/u"),
                ("XDG_CONFIG_HOME", "/home/u/.config"),
                ("PATH", "/usr/bin:/bin"),
                ("LC_ALL", "C"),
                ("GIT_OPTIONAL_LOCKS", "0"),
                ("GIT_NO_LAZY_FETCH", "1"),
                ("GIT_NO_REPLACE_OBJECTS", "1"),
                ("GIT_TERMINAL_PROMPT", "0")
              ],
      testCase "hardenedEnv: no HOME, no XDG_CONFIG_HOME" $
        map fst (hardenedEnv [] [])
          @?= [ "PATH",
                "LC_ALL",
                "GIT_OPTIONAL_LOCKS",
                "GIT_NO_LAZY_FETCH",
                "GIT_NO_REPLACE_OBJECTS",
                "GIT_TERMINAL_PROMPT"
              ],
      testCase "hardenedArgs: flags first" $
        hardenedArgs ["ls-files"]
          @?= [ "--no-pager",
                "-c",
                "core.fsmonitor=false",
                "-c",
                "core.untrackedCache=false",
                "-c",
                "core.hooksPath=/dev/null",
                "-c",
                "safe.bareRepository=explicit",
                "ls-files"
              ],
      testCase "no git: from disk" $
        withDirs $ \root bin -> do
          r <- probeIn [bin] root
          assertDisk r,
      testCase "git too old: error" $
        withDirs $ \root bin -> do
          exe <- fakeGit bin ["echo 'git version 2.43.0'"]
          r <- probeIn [bin] root
          r `errorIs` GitTooOld TooOld {exe = exe, version = GitVersion 2 43 0}
          fmap renderGitError (leftOf r)
            @?= Just
              ("git at " <> Text.pack exe <> " is version 2.43.0; need 2.44 or newer"),
      testCase "Apple git 2.39: error" $
        withDirs $ \root bin -> do
          exe <- fakeGit bin ["echo 'git version 2.39.3 (Apple Git-146)'"]
          r <- probeIn [bin] root
          r `errorIs` GitTooOld TooOld {exe = exe, version = GitVersion 2 39 3},
      testCase "garbage version: error with the first line" $
        withDirs $ \root bin -> do
          exe <- fakeGit bin ["echo 'hello there'", "echo 'more'"]
          r <- probeIn [bin] root
          r
            `errorIs` GitVersionUnreadable
              Unreadable {exe = exe, firstLine = "hello there"},
      testCase "macOS shim without the tools: error" $
        withDirs $ \root bin -> do
          exe <-
            fakeGit
              bin
              [ "echo 'xcode-select: note: No developer tools were found' >&2",
                "exit 1"
              ]
          r <- probeIn [bin] root
          r
            `errorIs` GitVersionUnreadable
              Unreadable
                { exe = exe,
                  firstLine = "xcode-select: note: No developer tools were found"
                },
      testCase "git refusing ROOT: error with git's stderr" $
        withDirs $ \root bin -> do
          let msg = "fatal: detected dubious ownership in repository at '/x'"
          exe <-
            fakeGit
              bin
              [ "case \"$*\" in",
                "  *--version*) echo 'git version 2.50.0' ;;",
                "  *) echo \"" <> msg <> "\" >&2; exit 128 ;;",
                "esac"
              ]
          r <- probeIn [bin] root
          case r of
            Left (GitFailed f) -> do
              f.exe @?= exe
              f.code @?= 128
              f.stderr @?= Text.pack msg
              fmap renderGitError (leftOf r)
                @?= Just ("git failed in ROOT: " <> Text.pack msg)
            _ -> assertFailure "expected GitFailed",
      testCase "not a git repository: from disk" $
        withDirs $ \root bin -> do
          _ <-
            fakeGit
              bin
              [ "case \"$*\" in",
                "  *--version*) echo 'git version 2.50.0' ;;",
                "  *) echo 'fatal: not a git repository (or any of the parent \
                \directories): .git' >&2; exit 128 ;;",
                "esac"
              ]
          r <- probeIn [bin] root
          assertDisk r,
      testCase "git that can't run: error" $
        withDirs $ \root bin -> do
          -- Executable, but no such interpreter.
          let exe = bin </> "git"
          writeExecutable exe "#!/no/such/interpreter\n"
          r <- probeIn [bin] root
          case r of
            Left (GitCannotRun e) -> e.exe @?= exe
            Left (GitVersionUnreadable e) -> e.exe @?= exe
            _ -> assertFailure "expected an error",
      testCase "git that hangs: killed at the time limit" $
        withDirs $ \root bin -> do
          let pidFile = bin </> "pid"
          exe <- fakeGit bin ["echo $$ > '" <> pidFile <> "'", "exec /bin/sleep 60"]
          r <- probeWithin (Microseconds 2_000_000) [bin] root
          r `errorIs` GitTimedOut Call {exe = exe, args = ["--version"]}
          -- No pid file: killed before it even started. Fine too.
          started <- Dir.doesFileExist pidFile
          when started $ do
            pid <- read <$> readFile pidFile
            alive <- try @IOException (signalProcess nullSignal pid)
            assertBool "git still running" (isLeft alive),
      testCase "real git: .git file pointing nowhere is an error" $
        withDirs $ \root _ -> do
          writeFile (root.path </> ".git") "gitdir: /no/such/dir\n"
          r <- probeReal root
          case r of
            Left (GitFailed f) ->
              assertBool
                (show f.stderr)
                ("not a git repository" `Text.isInfixOf` f.stderr)
            _ -> assertFailure "expected GitFailed",
      testCase "real git: a garbage .git file is an error" $
        withDirs $ \root _ -> do
          writeFile (root.path </> ".git") "hello\n"
          r <- probeReal root
          case r of
            Left (GitFailed _) -> pure ()
            _ -> assertFailure "expected GitFailed",
      testCase "real git: not a repo, from disk" $
        withDirs $
          \root _ -> probeReal root >>= assertDisk,
      testCase "git inside ROOT is not used" $
        withDirs $ \root _ -> do
          let planted = root.path </> "bin"
          Dir.createDirectory planted
          _ <- fakeGit planted ["echo 'git version 2.50.0'"]
          r <- probeIn [planted] root
          assertDisk r
    ]
  where
    assertDisk = \case
      Right ListFromDisk -> pure ()
      Right (ListFromGit _) -> assertFailure "expected from disk, got git"
      Left e -> assertFailure ("expected from disk, got " <> show e)
    errorIs r e = leftOf r @?= Just e
    leftOf = either Just (const Nothing)

-- | Probe with only these PATH entries, and a clean HOME.
probeIn :: [FilePath] -> Root -> IO (Either GitError Listing)
probeIn = probeWithin gitProbeTimeLimit

probeWithin :: Microseconds -> [FilePath] -> Root -> IO (Either GitError Listing)
probeWithin limit dirs root = do
  setup <- gitSetup (takeDirectory root.path </> "home") limit
  probeGitWith setup {searchPath = dirs} quiet root

-- | Probe with the git on our PATH.
probeReal :: Root -> IO (Either GitError Listing)
probeReal root = do
  setup <- gitSetup (takeDirectory root.path </> "home") gitProbeTimeLimit
  probeGitWith setup quiet root

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | A ROOT and a bin dir outside it.
withDirs :: (Root -> FilePath -> IO a) -> IO a
withDirs action =
  withCanonicalTemp $ \tmp -> do
    let root = tmp </> "root"
        bin = tmp </> "bin"
    Dir.createDirectory root
    Dir.createDirectory bin
    Dir.createDirectory (tmp </> "home")
    action (Root root) bin

fakeGit :: FilePath -> [String] -> IO FilePath
fakeGit bin = writeScript bin "git"
