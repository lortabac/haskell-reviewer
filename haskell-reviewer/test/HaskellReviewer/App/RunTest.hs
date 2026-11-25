module HaskellReviewer.App.RunTest (tests) where

import ApiSupport (ApiResult (..), RequestBody (..), callApp, callAppBody)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async
  ( async,
    asyncThreadId,
    concurrently,
    forConcurrently,
    poll,
    wait,
    withAsync,
  )
import Control.Concurrent.MVar
  ( MVar,
    isEmptyMVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
  )
import Control.Concurrent.STM qualified as STM
import Control.Exception
  ( AsyncException (..),
    Exception,
    evaluate,
    finally,
    throwIO,
    try,
  )
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Version (versionBranch)
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import HaskellReviewer.App.Run (Deps (..), run)
import HaskellReviewer.Core.Address (Port (..), Token, tokenFromBytes, tokenText)
import HaskellReviewer.Core.Config (builtinDefaults)
import HaskellReviewer.Core.FileContent (FileContent (..))
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList (..),
    ListSource (..),
    mkFileList,
  )
import HaskellReviewer.Core.Git (GitVersion (..), History (..), ObjectId (..))
import HaskellReviewer.Core.Lang (noLangs)
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    CabalProblem (..),
    FileProblem (..),
    GhcVersion (..),
    Project (..),
  )
import HaskellReviewer.Core.RelPath (relPathFromBytes, relPathText)
import HaskellReviewer.Core.Renames (RenameLimit (..), Settings (..), defaultSettings)
import HaskellReviewer.Core.Revspec
  ( BaseFrom (..),
    CommitsInfo (..),
    DiffInfo (..),
    Range (..),
    RangeForm (..),
    Revspec (..),
    WorkTreeInfo (..),
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..), Root (..))
import HaskellReviewer.Core.Sides (Pair (..), Sides (..))
import HaskellReviewer.Core.Xdg (XdgProblem (..))
import HaskellReviewer.Effects.Clock
  ( Clock (..),
    Microseconds (..),
    MonotonicTime (..),
    Nanoseconds (..),
  )
import HaskellReviewer.Effects.FileSystem
  ( ListError (..),
    RootError (..),
  )
import HaskellReviewer.Effects.Git
  ( DiskReason (..),
    GitError (..),
    Listing (..),
    TooOld (..),
    gitProbeTimeLimit,
    repoInfo,
    resolveCommit,
  )
import HaskellReviewer.Effects.GitObjects (defaultReaderSettings, withObjectReader)
import HaskellReviewer.Effects.Log (LogLevel (..), Logger, logInfo, newLogger)
import HaskellReviewer.Effects.Pairing (pairListed)
import HaskellReviewer.Effects.Revisions (DiffError (..))
import HaskellReviewer.Effects.Shutdown (Shutdown (..))
import HaskellReviewer.Effects.Source
  ( CommitSource (..),
    EntrySource (..),
    Listed (..),
    Lister (..),
    Probe (..),
    ProbeError (..),
    commitLister,
    diskLister,
    probeFilesWith,
    readSourceEntry,
  )
import HaskellReviewer.Effects.Spawn (SoftLimit (..), SpawnSetup (..))
import HaskellReviewer.Effects.Trace (Span (..), TraceFileError (..), Tracer (..))
import HaskellReviewer.Server.CborJson (cborToJson)
import HaskellReviewer.Server.Listen (ListenError (..), Listener (..))
import HaskellReviewer.Session.Project qualified as Session.Project
import Network.HTTP.Types (Header)
import Network.Wai (Application, Request (..), defaultRequest, responseToStream)
import Network.Wai.Internal (ResponseReceived (..))
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (compilerVersion)
import System.Posix.Resource (ResourceLimit (..))
import System.Timeout (timeout)
import Test.Tasty (TestName, TestTree, testGroup, withResource)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import TestSupport (fixtureRepo, gitOut, gitSetup, realGitAt, withCanonicalTemp)

tests :: TestTree
tests =
  testGroup
    "Run"
    [ testCase "unexpected exception: exit 1" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> ioError (userError "boom")}
        code <- run deps []
        code @?= ExitFailure 1
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: internal error" out,
      testCase "async exception goes through" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> throwIO UserInterrupt}
        result <- try (run deps [])
        result @?= Left UserInterrupt,
      testCase "ExitCode exception goes through" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> throwIO (ExitFailure 3)}
        result <- try (run deps [])
        result @?= Left (ExitFailure 3),
      testCase "missing ROOT: exit 2" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \p -> pure (Left (RootMissing p))}
        code <- run deps ["nope"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out @?= ["haskell-reviewer: ROOT does not exist: nope"],
      testCase "bad trace file: exit 2" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { openTracer = \_ p -> pure (Left (TraceFileError p "does not exist"))
                }
        code <- run deps ["--trace-file", "x/t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: cannot open trace file x/t.jsonl" out,
      testCase "success: URL line, startup span, browser, all closed, exit 0" $ do
        fake <- newFake
        code <- run fake.deps ["--trace-file", "t.jsonl"]
        code @?= ExitSuccess
        out <- readIORef fake.console
        out @?= ["haskell-reviewer: serving at " <> fakeUrl]
        spans <- readIORef fake.spans
        spans
          @?= [ Span
                  { name = "startup",
                    start = fakeWallTime,
                    duration = Nanoseconds 4000
                  }
              ]
        closed <- readIORef fake.closed
        closed @?= True
        listenerClosed <- readIORef fake.listenerClosed
        listenerClosed @?= True
        opened <- readIORef fake.opened
        opened @?= [fakeUrl],
      testCase "git too old: exit 2, no listen, tracer closed" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { probeFiles = \_ _ _ ->
                    pure . Left . ProbeGit . GitTooOld $
                      TooOld {exe = "/usr/bin/git", version = GitVersion 2 43 0},
                  listen = \_ _ -> assertFailure "listen called"
                }
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out
          @?= [ "haskell-reviewer: git at /usr/bin/git is version 2.43.0; \
                \need 2.44.1 or newer"
              ]
        readIORef fake.closed >>= (@?= True),
      testCase "bad --diff: exit 2, no listen, tracer closed" $ do
        fake <- newFake
        specs <- newIORef []
        let deps =
              (fake.deps)
                { probeFiles = \_ _ spec -> do
                    modifyIORef' specs (<> [spec])
                    pure (Left (ProbeDiff (DiffUnknownRev "nope" FullHistory))),
                  listen = \_ _ -> assertFailure "listen called"
                }
        code <- run deps ["--diff", "nope..HEAD", "--trace-file", "t.jsonl"]
        code @?= ExitFailure 2
        readIORef fake.console
          >>= (@?= ["haskell-reviewer: --diff: unknown revision or not a commit: nope"])
        readIORef specs
          >>= (@?= [Just (RangeSpec Range {form = TwoDot, base = "nope", head = "HEAD"})])
        readIORef fake.closed >>= (@?= True),
      testCase "config warnings: on the console before the URL, exit 0" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { loadConfig = \logger -> do
                    logInfo logger "haskell-reviewer: /c/config.toml:2:9: bad"
                    pure builtinDefaults
                }
        code <- run deps []
        code @?= ExitSuccess
        out <- readIORef fake.console
        out
          @?= [ "haskell-reviewer: /c/config.toml:2:9: bad",
                "haskell-reviewer: serving at " <> fakeUrl
              ],
      testCase "config is read after ROOT, before git" $ do
        fake <- newFake
        steps <- newIORef []
        let step s = modifyIORef' steps (<> [s :: Text])
            deps =
              (fake.deps)
                { checkRoot = \p -> step "root" >> fake.deps.checkRoot p,
                  loadConfig = \l -> step "config" >> fake.deps.loadConfig l,
                  probeFiles = \l r m -> step "git" >> fake.deps.probeFiles l r m
                }
        _ <- run deps []
        readIORef steps >>= (@?= ["root", "config", "git"]),
      listingTests,
      indexingTests,
      reloadTests,
      diffTests,
      cabalChangeTests,
      renamesTests,
      testCase "--port is passed to listen" $ do
        fake <- newFake
        ports <- newIORef []
        let deps =
              (fake.deps)
                { listen = \l p -> modifyIORef' ports (<> [p]) >> fake.deps.listen l p
                }
        code <- run deps ["--port", "1234"]
        code @?= ExitSuccess
        readIORef ports >>= (@?= [Just (Port 1234)]),
      testCase "--no-open: browser not opened" $ do
        fake <- newFake
        code <- run fake.deps ["--no-open"]
        code @?= ExitSuccess
        opened <- readIORef fake.opened
        opened @?= [],
      testCase "listen error: exit 2, tracer closed" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { listen = \_ p -> pure (Left (ListenError p "Address already in use"))
                }
        code <- run deps ["--port", "8080", "--trace-file", "t.jsonl"]
        code @?= ExitFailure 2
        out <- readIORef fake.console
        out
          @?= [ "haskell-reviewer: cannot listen on 127.0.0.1:8080: \
                \Address already in use"
              ]
        closed <- readIORef fake.closed
        closed @?= True,
      testCase "server throws: exit 1, all closed" $ do
        fake <- newFake
        let failing = fake.listener {serve = \_ _ -> ioError (userError "boom")}
            deps = (fake.deps) {listen = \_ _ -> pure (Right failing)}
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitFailure 1
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer: internal error" out
        closed <- readIORef fake.closed
        closed @?= True
        listenerClosed <- readIORef fake.listenerClosed
        listenerClosed @?= True,
      testCase "signal during start-up: exit 0, no listen, no URL" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { installShutdown = \_ -> pure alreadyStopped,
                  listen = \_ _ -> assertFailure "listen called"
                }
        code <- run deps ["--trace-file", "t.jsonl"]
        code @?= ExitSuccess
        readIORef fake.console >>= (@?= [])
        readIORef fake.spans >>= (@?= [])
        readIORef fake.opened >>= (@?= [])
        readIORef fake.closed >>= (@?= True),
      testCase "signal while listening: no URL, no span, no browser" $ do
        fake <- newFake
        stoppedRef <- newIORef False
        let deps =
              (fake.deps)
                { installShutdown = \_ ->
                    pure
                      Shutdown
                        { wait = pure (),
                          stopped = readIORef stoppedRef
                        },
                  listen = \l p -> do
                    modifyIORef' stoppedRef (const True)
                    fake.deps.listen l p
                }
        code <- run deps []
        code @?= ExitSuccess
        readIORef fake.console >>= (@?= [])
        readIORef fake.spans >>= (@?= [])
        readIORef fake.opened >>= (@?= [])
        readIORef fake.listenerClosed >>= (@?= True),
      testCase "start-up error wins over an early signal" $ do
        fake <- newFake
        let deps =
              (fake.deps)
                { installShutdown = \_ -> pure alreadyStopped,
                  checkRoot = \p -> pure (Left (RootMissing p))
                }
        code <- run deps ["nope"]
        code @?= ExitFailure 2,
      testCase "signals caught before ROOT is checked" $ do
        fake <- newFake
        installed <- newIORef False
        let deps =
              (fake.deps)
                { installShutdown = \level -> do
                    modifyIORef' installed (const True)
                    fake.deps.installShutdown level,
                  checkRoot = \p -> do
                    ok <- readIORef installed
                    assertBool "handlers not installed yet" ok
                    pure (Left (RootMissing p))
                }
        code <- run deps ["nope"]
        code @?= ExitFailure 2,
      testCase "server returns: exit 1" $ do
        fake <- newFake
        let stopping = fake.listener {serve = \_ _ -> pure ()}
            deps = (fake.deps) {listen = \_ _ -> pure (Right stopping)}
        code <- run deps []
        code @?= ExitFailure 1,
      testCase "--version: exit 0, ROOT not checked" $ do
        fake <- newFake
        let deps = (fake.deps) {checkRoot = \_ -> assertFailure "checkRoot called"}
        code <- run deps ["--version"]
        code @?= ExitSuccess
        out <- readIORef fake.console
        assertOneLineWith "haskell-reviewer " out,
      testCase "usage error: exit 2" $ do
        fake <- newFake
        code <- run fake.deps ["--bogus"]
        code @?= ExitFailure 2,
      idleTests
    ]

-- * Listing

listingTests :: TestTree
listingTests =
  testGroup
    "listing"
    [ testCase "files are listed after the URL line"
        $ withRunning
          ( \fake -> fakeLister $ do
              fake.deps.console "listing"
              pure (Right (mkFileList FromDisk 0 []))
          )
        $ \fake _ -> do
          ok <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
          assertBool "not listed" ok
          out <- readIORef fake.console
          take 2 out @?= ["haskell-reviewer: serving at " <> fakeUrl, "listing"],
      testCase "browse mode: api/diff-files is a plain 404" $
        withRunning (const oneLister) $ \_ app -> do
          r <- getSidePath app ["diff-files"]
          (r.status, r.body) @?= (404, "not found"),
      testCase "listing fails: a line, 500, no exit" $
        withRunning (\_ -> fakeLister (pure (Left (ListCrashed "boom")))) $
          \fake app -> do
            r <- getFiles app
            r.status @?= 500
            r.body @?= "cannot list files"
            -- The line comes after the slot is filled: wait for it.
            _ <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
            out <- readIORef fake.console
            out
              @?= [ "haskell-reviewer: serving at " <> fakeUrl,
                    "haskell-reviewer: cannot list files: boom"
                  ],
      testCase "lister throws: a line, 500, no hang" $
        withRunning (\_ -> fakeLister (ioError (userError "boom"))) $ \fake app -> do
          r <- timeout 5_000_000 (getFiles app)
          fmap (.status) r @?= Just 500
          _ <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
          out <- readIORef fake.console
          drop 1 out @?= ["haskell-reviewer: cannot list files: user error (boom)"],
      testCase "a request before the list is done waits, then 200" $ do
        release <- newEmptyMVar
        let lister _ = fakeLister $ do
              readMVar release
              pure (Right (mkFileList FromDisk 0 []))
        withRunning lister $ \_ app ->
          withAsync (getFiles app) $ \request -> do
            threadDelay 200_000
            early <- poll request
            assertBool "answered before the list was done" (isNothing early)
            putMVar release ()
            r <- timeout 5_000_000 (wait request)
            fmap (.status) r @?= Just 200,
      testCase "the parser warms up before requests get the snapshot" $ do
        release <- newEmptyMVar
        let slowWarmUp d = d {warmUp = [readMVar release]}
            empty _ = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
        withRunningWith slowWarmUp empty $ \_ app ->
          withAsync (getFiles app) $ \request -> do
            threadDelay 200_000
            early <- poll request
            assertBool "answered before the warm-up" (isNothing early)
            putMVar release ()
            r <- timeout 5_000_000 (wait request)
            fmap (.status) r @?= Just 200,
      testCase "stop during a slow warm-up: exit 0 within 5 s" $ do
        fake <- newFake
        stop <- newEmptyMVar
        warming <- newEmptyMVar
        let serve ready _ = ready >> blockForever
            -- Pure work that never ends, like a big warm-up.
            busy = mapM_ (\n -> evaluate (length (show (n * n :: Integer)))) [1 ..]
            empty = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ -> pure (Right (fakeProbe (BrowseSides empty))),
                  warmUp = [putMVar warming () >> busy]
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          started <- timeout 5_000_000 (takeMVar warming)
          started @?= Just ()
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess,
      testCase "warm-up: runs when the project crashes, not when listing fails" $ do
        let counting ref d = d {warmUp = [modifyIORef' ref (+ 1)]}
            crash ref d =
              (counting ref d) {readProject = \_ _ _ _ -> ioError (userError "boom")}
            empty _ = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
            failing _ = fakeLister (pure (Left (ListCrashed "boom")))
        crashed <- newIORef (0 :: Int)
        withRunningWith (crash crashed) empty $ \_ app -> do
          r <- getFiles app
          r.status @?= 200
        readIORef crashed >>= (@?= 1)
        failed <- newIORef (0 :: Int)
        withRunningWith (counting failed) failing $ \_ app -> do
          r <- getFiles app
          r.status @?= 500
        readIORef failed >>= (@?= 0),
      testCase "warm-ups throw: a line each, the others run, the snapshot is filled" $ do
        -- Two throw: a line each, and the one after them still runs.
        ran <- newIORef False
        let throwing d =
              d
                { warmUp =
                    [ ioError (userError "boom"),
                      ioError (userError "bang"),
                      writeIORef ran True
                    ]
                }
            empty _ = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
        withRunningWith throwing empty $ \fake app -> do
          r <- getFiles app
          r.status @?= 200
          _ <- waitFor 5 ((>= 3) . length <$> readIORef fake.console)
          out <- readIORef fake.console
          drop 1 out
            @?= [ "haskell-reviewer: parser warm-up failed: user error (boom)",
                  "haskell-reviewer: parser warm-up failed: user error (bang)"
                ]
          readIORef ran >>= assertBool "the last warm-up ran",
      testCase "project crashes: a line, files still work, warning in the tree" $ do
        let crash d = d {readProject = \_ _ _ _ -> ioError (userError "boom")}
            empty _ = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
        withRunningWith crash empty $ \fake app -> do
          r <- getFiles app
          r.status @?= 200
          m <-
            callApp app "GET" "127.0.0.1:4321" $
              [tokenText fakeToken, "api", "modules", "tree"]
          m.status @?= 200
          let warning = "can't read the project: user error (boom)"
          assertBool (show m.body) (warning `BS.isInfixOf` LBS.toStrict m.body)
          out <- readIORef fake.console
          drop 1 out @?= ["haskell-reviewer: cannot read the project: user error (boom)"],
      testCase "--ghc reaches readProject" $ do
        captured <- newIORef Nothing
        let capture source fl platform cliGhc = do
              writeIORef captured (Just cliGhc)
              Session.Project.readProject source fl platform cliGhc
            change d = d {readProject = capture}
            empty _ = fakeLister (pure (Right (mkFileList FromDisk 0 [])))
        withRunningArgs ["--idle-exit", "0", "--ghc", "9.10"] change empty $ \_ app -> do
          r <- getFiles app
          r.status @?= 200
        got <- readIORef captured
        got @?= Just (Just (GhcVersion [9, 10])),
      testCase "warnings for skipped names and the cap" $ do
        let big = (mkFileList FromDisk 3 []) {truncated = True}
        withRunning (\_ -> fakeLister (pure (Right big))) $ \fake app -> do
          _ <- getFiles app
          ok <- waitFor 5 ((== 3) . length <$> readIORef fake.console)
          assertBool "no lines" ok
          out <- readIORef fake.console
          drop 1 out
            @?= [ "haskell-reviewer: skipped 3 file name(s) that are not UTF-8",
                  "haskell-reviewer: more than 100000 files under ROOT; \
                  \listing the first 100000"
                ]
    ]

-- * Indexing

indexingTests :: TestTree
indexingTests =
  testGroup
    "indexing"
    [ testCase "workers start after the URL line; verbose lines and span" $ do
        let marking d = d {parse = \_ _ -> d.console "parsing" >> pure textOnly}
            args = ["--idle-exit", "0", "--verbose", "--trace-file", "t.jsonl"]
        withRunningArgs args marking (\_ -> fakeLister (pure (Right oneModule))) $
          \fake _ -> do
            -- The parsed line comes right after the done line: wait for it.
            let counts = "indexing: 1 parsed, 0 shared"
            ok <- waitFor 5 (elem counts <$> readIORef fake.console)
            out <- readIORef fake.console
            assertBool (show out) ok
            let url = "haskell-reviewer: serving at " <> fakeUrl
            assertBool (show out) (elemIndex url out < elemIndex "parsing" out)
            let indexing = filter ("indexing: " `Text.isPrefixOf`) out
            indexing
              @?= [ "indexing: 1 modules, 1 workers",
                    "indexing: 1/1",
                    "indexing: done, 1 modules (1 ready, 0 failed, 1 text only) in 0 ms",
                    "indexing: 1 parsed, 0 shared"
                  ]
            spans <- readIORef fake.spans
            assertBool (show spans) ("indexing" `elem` map (.name) spans),
      testCase "stop during indexing: exit 0 within 5 s" $ do
        fake <- newFake
        stop <- newEmptyMVar
        parsing <- newEmptyMVar
        let serve ready _ = ready >> blockForever
            -- Pure work that never ends, like a slow parse.
            busy = mapM_ (\n -> evaluate (length (show (n * n :: Integer)))) [1 ..]
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ -> pure (Right (fakeProbe (BrowseSides oneLister))),
                  parse = \_ _ -> putMVar parsing () >> busy >> pure textOnly
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          started <- timeout 5_000_000 (takeMVar parsing)
          started @?= Just ()
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess,
      testCase "a crash after the fill: still indexed" $ do
        let failing d =
              d
                { openTracer = \_ _ ->
                    pure . Right $
                      Tracer
                        { record = \sp ->
                            when (sp.name == "files") (ioError (userError "boom")),
                          close = pure ()
                        }
                }
            args = ["--idle-exit", "0", "--verbose", "--trace-file", "t.jsonl"]
        withRunningArgs args failing (\_ -> fakeLister (pure (Right oneModule))) $
          \fake _ -> do
            let isDone = ("indexing: done" `Text.isPrefixOf`)
            ok <- waitFor 5 (any isDone <$> readIORef fake.console)
            out <- readIORef fake.console
            assertBool (show out) ok,
      testCase "indexing throws before the workers: requests get a 500, no hang" $ do
        let failing d = d {indexWorkers = ioError (userError "boom")}
            lister _ = fakeLister (pure (Right oneModule))
        withRunningWith failing lister $ \fake app -> do
          let path = [tokenText fakeToken, "api", "module", "tree", "A"]
          r <- timeout 5_000_000 (callApp app "GET" "127.0.0.1:4321" path)
          fmap (\x -> (x.status, x.body)) r @?= Just (500, "cannot index module")
          let crashed = "haskell-reviewer: indexing crashed: user error (boom)"
          ok <- waitFor 5 (elem crashed <$> readIORef fake.console)
          out <- readIORef fake.console
          assertBool (show out) ok,
      testCase "listing fails: no workers" $ do
        asked <- newIORef False
        let counting d = d {indexWorkers = writeIORef asked True >> pure 1}
        withRunningWith counting (\_ -> fakeLister (pure (Left (ListCrashed "boom")))) $
          \fake app -> do
            r <- getFiles app
            r.status @?= 500
            _ <- waitFor 5 ((>= 2) . length <$> readIORef fake.console)
            pure ()
        readIORef asked >>= (@?= False)
    ]

-- * Reload

-- | POST api/reload in browse mode (D-351). Gates are MVars and TVars,
-- never sleeps.
reloadTests :: TestTree
reloadTests =
  testGroup
    "Reload"
    [ testCase "browse: edit, POST, generation 2 with the new text, event" $
        withCanonicalTemp $ \tmp -> do
          dir <- makeProject (tmp </> "p") ["A"]
          let real d = d {readEntry = readSourceEntry}
          let args = reloadArgs <> ["--trace-file", "t.jsonl"]
          withRunningArgs args real (const (diskAt dir)) $ \fake app -> do
            ok <- waitFor 5 (any isIndexed <$> readIORef fake.console)
            assertBool "not indexed" ok
            opened <- newEmptyMVar
            seen <- newIORef BS.empty
            withAsync (recordStream app opened seen) $ \_ -> do
              takeMVar opened
              writeFile (dir </> "src" </> "A.hs") "module A where\nb = 2\n"
              writeFile (dir </> "New.txt") "new\n"
              r <- reload app
              (r.status, r.body) @?= (200, "{\"generation\":2}")
              files <- getFiles app
              generationOf files @?= Just 2
              assertBool (show files.body) ("New.txt" `inBody` files)
              f <- getSidePath app ["file", "tree", "src", "A.hs"]
              generationOf f @?= Just 2
              assertBool (show f.body) ("b = 2" `inJson` f)
              let event = "event: generation\ndata: {\"generation\":2}\n"
                  indexing = "\"generation\":2,\"side\":\"tree\""
                  -- Each write is kept on its own: wait for the indexing
                  -- event after the generation one too.
                  both bytes =
                    let (_, after) = BS.breakSubstring event bytes
                     in event `BS.isPrefixOf` after && indexing `BS.isInfixOf` after
              streamed <- waitFor 5 (both <$> readIORef seen)
              bytes <- readIORef seen
              assertBool (show bytes) streamed
              -- The generation, then its indexing: nothing of gen 2 before.
              let (before, _) = BS.breakSubstring event bytes
              assertBool (show before) (not ("\"generation\":2" `BS.isInfixOf` before))
            -- Logged after the answer: wait for it. The span comes first.
            let reloaded = any ("reload: generation 2 in " `Text.isPrefixOf`)
            logged <- waitFor 5 (reloaded <$> readIORef fake.console)
            out <- readIORef fake.console
            assertBool (show out) logged
            names <- map (.name) <$> readIORef fake.spans
            assertBool (show names) ("reload" `elem` names),
      testCase "browse: a reload parses only the edited module (D-353)" $ do
        parses <- newIORef (0 :: Int)
        let count = atomicModifyIORef' parses (\n -> (n + 1, ()))
            counting d =
              d {readEntry = readSourceEntry, parse = \st t -> count >> d.parse st t}
        withCanonicalTemp $ \tmp -> do
          dir <- makeProject (tmp </> "p") ["A", "B", "C"]
          withRunningArgs reloadArgs counting (const (diskAt dir)) $ \fake app -> do
            -- Logged after the done line: wait for the line itself.
            let waitLine l = do
                  ok <- waitFor 10 (elem l <$> readIORef fake.console)
                  out <- readIORef fake.console
                  assertBool (show (l, out)) ok
            waitLine "indexing: 3 parsed, 0 shared"
            writeFile (dir </> "src" </> "A.hs") "module A where\na = 2\n"
            r <- reload app
            (r.status, r.body) @?= (200, "{\"generation\":2}")
            -- A's old result stays until the next reload.
            waitLine "reload: dropped 0 parse results and 0 texts"
            waitLine "indexing: 4 parsed, 2 shared"
            readIORef parses >>= (@?= 4)
            f <- getSidePath app ["file", "tree", "src", "A.hs"]
            assertBool (show f.body) ("a = 2" `inJson` f)
            -- No edit: no parse, and A's old result goes.
            r2 <- reload app
            (r2.status, r2.body) @?= (200, "{\"generation\":3}")
            waitLine "reload: dropped 1 parse results and 0 texts"
            waitLine "indexing: 4 parsed, 5 shared"
            readIORef parses >>= (@?= 4),
      testCase "route: methods, Origin, type, size, body" $
        withRunning (const oneLister) $ \_ app -> do
          let post headers body = postReload app headers body
              json = [ourOrigin, jsonType]
              check what r (st, body) =
                (what, r.status, r.body) @?= (what :: Text, st, body)
              typeError = (415, "the body must be application/json")
              bodyError = (400, "the body must be {}")
          forM_ ["GET", "HEAD", "PUT"] $ \m -> do
            r <-
              callAppBody app m [ourOrigin] NoBody "" "127.0.0.1:4321" reloadPath
            (m, r.status, lookup "Allow" r.headers) @?= (m, 405, Just "POST")
          post [jsonType] (Sized "{}") >>= \r -> r.status @?= 403
          post [("Origin", "http://evil.com"), jsonType] (Sized "{}")
            >>= \r -> r.status @?= 403
          post [ourOrigin] (Sized "{}") >>= \r -> check "no type" r typeError
          post [ourOrigin, ("Content-Type", "text/plain")] (Sized "{}")
            >>= \r -> check "text" r typeError
          post json (Sized (BS.replicate 4097 32))
            >>= \r -> check "length" r (413, "the body is over 4 KiB")
          post json (Chunked (replicate 5 (BS.replicate 1000 32)))
            >>= \r -> check "chunked" r (413, "the body is over 4 KiB")
          forM_ ["[]", "{\"a\":1}", "{", "", "null"] $ \b ->
            post json (Sized b) >>= \r -> check (Text.Encoding.decodeUtf8 b) r bodyError
          post json (Sized "{}") >>= \r -> check "ok" r (200, "{\"generation\":2}")
          let withCharset = ("Content-Type", "application/json; charset=utf-8")
          post [ourOrigin, withCharset] (Sized " { } ")
            >>= \r -> check "spaces" r (200, "{\"generation\":3}"),
      testCase "commit range: 404 after the method check" $ do
        let one = fakeLister (pure (Right oneModule))
            listers _ = DiffSides Pair {head = one, base = one}
        withRunningSides ["--idle-exit", "0"] id listers $ \_ app -> do
          r <- reload app
          (r.status, r.body) @?= (404, "not found")
          g <- callAppBody app "GET" [ourOrigin] NoBody "" "127.0.0.1:4321" reloadPath
          g.status @?= 405,
      testCase "coalesced: two POSTs during a build share the next one" $ do
        calls <- newIORef (0 :: Int)
        building <- newEmptyMVar
        release <- newEmptyMVar
        let lister _ = fakeLister $ do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              when (n == 2) (putMVar building () >> readMVar release)
              pure (Right oneModule)
        withRunningArgs reloadArgs id lister $ \fake app -> do
          firstPost <- async (reload app)
          takeMVar building
          others <- mapM (const (async (reload app))) [1 :: Int, 2]
          -- All three are in the queue: the first one taken, two waiting.
          let queuedLines = length . filter (== "reload: queued")
          queued <- waitFor 5 ((== 3) . queuedLines <$> readIORef fake.console)
          assertBool "not queued" queued
          putMVar release ()
          r1 <- timeout 5_000_000 (wait firstPost)
          fmap (.body) r1 @?= Just "{\"generation\":2}"
          rs <- timeout 5_000_000 (mapM wait others)
          fmap (map (.body)) rs @?= Just ["{\"generation\":3}", "{\"generation\":3}"]
          readIORef calls >>= (@?= 3),
      testCase "listing fails: 500, generation 1 kept, next reload is 2" $ do
        calls <- newIORef (0 :: Int)
        let lister _ = fakeLister $ do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              pure (if n == 2 then Left (ListCrashed "boom") else Right oneModule)
        withRunning lister $ \fake app -> do
          before <- getFiles app
          generationOf before @?= Just 1
          r <- reload app
          (r.status, r.body) @?= (500, "cannot list files")
          after <- getFiles app
          (after.status, generationOf after) @?= (200, Just 1)
          m <- getSidePath app ["module", "tree", "A"]
          (m.status, generationOf m) @?= (200, Just 1)
          out <- readIORef fake.console
          assertBool (show out) ("haskell-reviewer: cannot list files: boom" `elem` out)
          again <- reload app
          (again.status, again.body) @?= (200, "{\"generation\":2}"),
      testCase "lister throws on reload: 500, no hang" $ do
        calls <- newIORef (0 :: Int)
        let lister _ = fakeLister $ do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              when (n == 2) (ioError (userError "boom"))
              pure (Right oneModule)
        withRunning lister $ \_ app -> do
          r <- timeout 5_000_000 (reload app)
          fmap (\x -> (x.status, x.body)) r @?= Just (500, "cannot list files")
          files <- getFiles app
          generationOf files @?= Just 1,
      testCase "stop during a reload: the POST gets 503, exit 0" $ do
        fake <- newFake
        stop <- newEmptyMVar
        appVar <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        building <- newEmptyMVar
        let serve ready app = ready >> putMVar appVar app >> blockForever
            lister = fakeLister $ do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              when (n == 2) (putMVar building () >> blockForever)
              pure (Right oneModule)
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ -> pure (Right (fakeProbe (BrowseSides lister)))
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          ready <- timeout 5_000_000 (takeMVar appVar)
          app <- maybe (assertFailure "not ready") pure ready
          -- After the first listing: a reload is taken only then.
          _ <- getFiles app
          posted <- async (reload app)
          started <- timeout 5_000_000 (takeMVar building)
          started @?= Just ()
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess
          r <- timeout 5_000_000 (wait posted)
          fmap (\x -> (x.status, x.body)) r @?= Just (503, "the server is stopping")
          late <- timeout 5_000_000 (reload app)
          fmap (.status) late @?= Just 503,
      testCase "while indexing: old run stopped first, old modules failed" $ do
        blocking <- newTVarIO True
        inFlight <- newIORef (0 :: Int, 0 :: Int)
        let three =
              mkFileList
                FromDisk
                0
                [ (p, EntryFile)
                | n <- ["A.hs", "B.hs", "C.hs"],
                  Just p <- [relPathFromBytes n]
                ]
            counted = do
              atomicModifyIORef' inFlight (\(n, m) -> ((n + 1, max m (n + 1)), ()))
              atomically (readTVar blocking >>= STM.check . not)
            parse _ _ = do
              (counted >> pure textOnly)
                `finally` atomicModifyIORef' inFlight (\(n, m) -> ((n - 1, m), ()))
            change d = d {parse = parse, indexWorkers = pure 2}
            lister _ = fakeLister (pure (Right three))
        withRunningArgs reloadArgs change lister $ \fake app -> do
          busy <- waitFor 5 ((== 2) . fst <$> readIORef inFlight)
          assertBool "workers not busy" busy
          -- Waits for C on generation 1: both workers hold a module.
          oldPage <- async (getSidePath app ["module", "tree", "C"])
          let waiting =
                (== ThreadBlocked BlockedOnSTM) <$> threadStatus (asyncThreadId oldPage)
          onSlot <- waitFor 5 waiting
          assertBool "the request is not waiting" onSlot
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          old <- timeout 5_000_000 (wait oldPage)
          fmap (\x -> (x.status, x.body)) old @?= Just (500, "cannot index module")
          atomically (writeTVar blocking False)
          done <- waitFor 5 (any isIndexed <$> readIORef fake.console)
          assertBool "gen 2 not indexed" done
          page <- getSidePath app ["module", "tree", "C"]
          (page.status, generationOf page) @?= (200, Just 2)
          (_, most) <- readIORef inFlight
          assertBool ("in flight: " <> show most) (most <= 2)
          out <- readIORef fake.console
          -- Gen 1 never finished: one done line, gen 2's.
          length (filter isIndexed out) @?= 1,
      testCase "listing thread crashes: a line, 503, the server goes on" $ do
        armed <- newIORef False
        -- The clock throws once armed: the next reload's first call.
        let crashing d =
              d
                { clock =
                    Clock
                      { now = do
                          on <- readIORef armed
                          when on (ioError (userError "boom"))
                          d.clock.now,
                        monotonic = d.clock.monotonic
                      }
                }
        withRunningArgs reloadArgs crashing (const oneLister) $ \fake app -> do
          -- Indexed: nothing else reads the clock now.
          done <- waitFor 5 (any isIndexed <$> readIORef fake.console)
          assertBool "not indexed" done
          writeIORef armed True
          r <- timeout 5_000_000 (reload app)
          fmap (\x -> (x.status, x.body)) r
            @?= Just (503, "reload is not available: user error (boom)")
          out <- readIORef fake.console
          let crashed = "haskell-reviewer: the listing thread crashed: user error (boom)"
          assertBool (show out) (crashed `elem` out)
          again <- reload app
          (again.status, again.body)
            @?= (503, "reload is not available: user error (boom)")
          files <- getFiles app
          (files.status, generationOf files) @?= (200, Just 1)
          page <- getSidePath app ["module", "tree", "A"]
          (page.status, generationOf page) @?= (200, Just 1),
      testCase "stopped before the old workers ran: old modules still fail" $ do
        calls <- newIORef (0 :: Int)
        held <- newEmptyMVar
        -- The first indexing waits before runWorkers, until the reload
        -- stops it.
        let workers = do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              when (n == 1) (putMVar held () >> blockForever)
              pure 1
            change d = d {indexWorkers = workers}
        withRunningWith change (const oneLister) $ \_ app -> do
          takeMVar held
          oldPage <- async (getSidePath app ["module", "tree", "A"])
          let waiting =
                (== ThreadBlocked BlockedOnSTM) <$> threadStatus (asyncThreadId oldPage)
          onSlot <- waitFor 5 waiting
          assertBool "the request is not waiting" onSlot
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          old <- timeout 5_000_000 (wait oldPage)
          fmap (\x -> (x.status, x.body)) old @?= Just (500, "cannot index module")
          page <- getSidePath app ["module", "tree", "A"]
          (page.status, generationOf page) @?= (200, Just 2),
      testCase "a request started before the swap answers from its generation" $ do
        armed <- newIORef True
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let files =
              mkFileList
                FromDisk
                0
                [(p, EntryFile) | n <- ["A.hs", "B.txt"], Just p <- [relPathFromBytes n]]
            readEntry _ p _
              | relPathText p == "B.txt" = do
                  hold <- atomicModifyIORef' armed (False,)
                  when hold (putMVar entered () >> readMVar release)
                  pure (Right (TextContent (if hold then "old" else "new")))
              | otherwise = pure (Right (TextContent "module A where\n"))
            change d = d {readEntry = readEntry}
        withRunningWith change (\_ -> fakeLister (pure (Right files))) $ \_ app -> do
          oldRead <- async (getSidePath app ["file", "tree", "B.txt"])
          takeMVar entered
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          putMVar release ()
          old <- timeout 5_000_000 (wait oldRead)
          fmap (\x -> (x.status, generationOf x)) old @?= Just (200, Just 1)
          assertBool "old text" (maybe False ("old" `inJson`) old)
          new <- getSidePath app ["file", "tree", "B.txt"]
          (new.status, generationOf new) @?= (200, Just 2),
      testCase "--diff HEAD: base kept, only dirty modules parsed, caches pruned" $ do
        parses <- newIORef (0 :: Int)
        let count = atomicModifyIORef' parses (\n -> (n + 1, ()))
            counting d = d {parse = \st t -> count >> d.parse st t}
        withDiffRepo "edits" [] "" counting $ \repo fake app -> do
          -- A, S (index), B, New on head; A, S on base (B shared).
          readIORef parses >>= (@?= 6)
          -- Loads head's text store: its clean files go in the shared table.
          t <- textSearch app "head" "module"
          t.status @?= 200
          writeFile (repo </> "src" </> "S.hs") "module S (s) where\n\ns :: Int\ns = 3\n"
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          -- Logged after the done line: wait for it.
          let parsed = "indexing: 7 parsed, 7 shared"
          done <- waitFor 10 (elem parsed <$> readIORef fake.console)
          assertBool "gen 2 not indexed" done
          -- Only S is parsed. A and New are read from disk too, but
          -- their text didn't change: hits by text hash (D-353).
          readIORef parses >>= (@?= 7)
          forM_ [["files", "head"], ["files", "base"], ["diff-files"]] $ \route -> do
            x <- getSidePath app route
            (route, x.status, generationOf x) @?= (route, 200, Just 2)
          d <- getSidePath app ["diff", "src", "S.hs"]
          (d.status, generationOf d) @?= (200, Just 2)
          assertBool (show d.body) ("\"status\":\"modified\"" `inJson` d)
          f <- getSidePath app ["file", "head", "src", "S.hs"]
          (f.status, generationOf f) @?= (200, Just 2)
          assertBool (show f.body) ("s = 3\\n" `inJson` f)
          out <- readIORef fake.console
          let has l = assertBool (show (l, out)) (l `elem` out)
          -- S's index blob: no live file has it now.
          has "reload: dropped 1 parse results and 1 texts"
          has "base: files: 6 kept"
          -- Base's project is read once, head's twice.
          let logged p = length (filter (p `Text.isPrefixOf`) out)
          (logged "base: ghc: ", logged "head: ghc: ") @?= (1, 2)
          length (filter (== "changed: 3 on head, 2 on base, indexed first") out) @?= 2,
      testCase "--diff HEAD: a file edited back to its committed text: no parse" $ do
        parses <- newIORef (0 :: Int)
        let count = atomicModifyIORef' parses (\n -> (n + 1, ()))
            counting d = d {parse = \st t -> count >> d.parse st t}
        withDiffRepo "edits" [] "" counting $ \repo fake app -> do
          readIORef parses >>= (@?= 6)
          -- S's index has s = 2; on disk it goes back to HEAD's s = 1.
          -- So it is read from disk, and its text is base's blob.
          writeFile (repo </> "src" </> "S.hs") "module S (s) where\n\ns :: Int\ns = 1\n"
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          let parsed = "indexing: 6 parsed, 8 shared"
          done <- waitFor 10 (elem parsed <$> readIORef fake.console)
          out <- readIORef fake.console
          assertBool (show out) done
          readIORef parses >>= (@?= 6)
          f <- getSidePath app ["file", "head", "src", "S.hs"]
          (f.status, generationOf f) @?= (200, Just 2)
          assertBool (show f.body) ("s = 1\\n" `inJson` f),
      testCase "--diff HEAD: an edit outside ROOT counts after a reload" $
        withDiffRepo "renames" ["sha1", "worktree"] "pkg" id $ \repo _ app -> do
          let total n = "\"outside\":{\"total\":" <> n <> ","
          before <- getSidePath app ["diff-files"]
          assertBool (show before.body) (total "4" `inBody` before)
          appendFile (repo </> "other" </> "keep") "more\n"
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          after <- getSidePath app ["diff-files"]
          (after.status, generationOf after) @?= (200, Just 2)
          assertBool (show after.body) (total "5" `inBody` after),
      testCase "--diff HEAD: a diff started before the swap stays on its generation" $ do
        armed <- newIORef False
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let holding d =
              d
                { readEntry = \src p k -> do
                    hold <- case src of
                      CommitEntries _
                        | relPathText p == "src/A.hs" -> atomicModifyIORef' armed (False,)
                      _ -> pure False
                    when hold (putMVar entered () >> readMVar release)
                    d.readEntry src p k
                }
        withDiffRepo "edits" [] "" holding $ \repo _ app -> do
          writeIORef armed True
          old <- async (getSidePath app ["diff", "src", "A.hs"])
          takeMVar entered
          writeFile (repo </> "src" </> "B.hs") "module B (b) where\n\nb :: Int\nb = 2\n"
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          putMVar release ()
          o <- timeout 5_000_000 (wait old)
          fmap (\x -> (x.status, generationOf x)) o @?= Just (200, Just 1)
          -- The new generation: every route agrees, B is modified now.
          new <- getSidePath app ["diff", "src", "A.hs"]
          (new.status, generationOf new) @?= (200, Just 2)
          forM_ [["files", "head"], ["modules", "base"], ["diff-files"]] $ \route -> do
            x <- getSidePath app route
            (route, x.status, generationOf x) @?= (route, 200, Just 2)
          files <- getSidePath app ["diff-files"]
          assertBool (show files.body) ("\"path\":\"src/B.hs\"" `inBody` files),
      testCase "--diff HEAD: base is listed again only if it failed" $ do
        headCalls <- newIORef (0 :: Int)
        baseCalls <- newIORef (0 :: Int)
        let counted ref ok = fakeLister $ do
              n <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
              pure (if ok n then Right oneModule else Left (ListCrashed "boom"))
            listers =
              Pair
                { head = counted headCalls (const True),
                  base = counted baseCalls (> 1)
                }
            change _ d = d {probeFiles = \_ _ _ -> pure (Right (fakeRevProbe listers))}
        withRunningProbe ["--idle-exit", "0"] change $ \_ app -> do
          b1 <- getSide app "files" "base"
          b1.status @?= 500
          r <- reload app
          (r.status, r.body) @?= (200, "{\"generation\":2}")
          b2 <- getSide app "files" "base"
          (b2.status, generationOf b2) @?= (200, Just 2)
          r3 <- reload app
          (r3.status, r3.body) @?= (200, "{\"generation\":3}")
          b3 <- getSide app "files" "base"
          (b3.status, generationOf b3) @?= (200, Just 3)
          m <- getSidePath app ["module", "base", "A"]
          (m.status, generationOf m) @?= (200, Just 3)
          -- Base: failed, then listed again once, then kept.
          (,) <$> readIORef headCalls <*> readIORef baseCalls >>= (@?= (3, 2)),
      testCase "--diff HEAD: base is read again if its project read crashed" $ do
        projectReads <- newIORef (0 :: Int)
        baseCalls <- newIORef (0 :: Int)
        let one = fakeLister (pure (Right oneModule))
            baseLister = fakeLister $ do
              atomicModifyIORef' baseCalls (\c -> (c + 1, ()))
              pure (Right oneModule)
            -- Head is read first: the second read is base's.
            crashOnce d =
              d
                { readProject = \src fl p g -> do
                    n <- atomicModifyIORef' projectReads (\c -> (c + 1, c + 1))
                    when (n == 2) (ioError (userError "boom"))
                    d.readProject src fl p g
                }
            probe = fakeRevProbe Pair {head = one, base = baseLister}
            change _ d = crashOnce d {probeFiles = \_ _ _ -> pure (Right probe)}
        withRunningProbe ["--idle-exit", "0"] change $ \fake app -> do
          _ <- getSide app "modules" "base"
          let line = "haskell-reviewer: base: cannot read the project: user error (boom)"
          logged <- waitFor 5 (elem line <$> readIORef fake.console)
          assertBool "no crash line" logged
          forM_ [2, 3 :: Int] $ \g -> do
            r <- reload app
            (r.status, generationOf r) @?= (200, Just g)
          -- Gen 2 reads base again; gen 3 keeps it.
          (,) <$> readIORef projectReads <*> readIORef baseCalls >>= (@?= (5, 2))
          m <- getSidePath app ["module", "base", "A"]
          (m.status, generationOf m) @?= (200, Just 3),
      testCase "--diff HEAD: base is read again if a project file could not be read" $
        withCanonicalTemp $ \tmp -> do
          headDir <- makeProject (tmp </> "head") ["A"]
          baseDir <- makeProject (tmp </> "base") ["A"]
          projectReads <- newIORef (0 :: Int)
          let cabal = fromMaybe (error "path") (relPathFromBytes "p.cabal")
              -- Head is read first: base's first read can't read p.cabal,
              -- as a one-off git read error would.
              failOnce d =
                d
                  { readEntry = readSourceEntry,
                    readProject = \src fl p g -> do
                      n <- atomicModifyIORef' projectReads (\c -> (c + 1, c + 1))
                      proj <- d.readProject src fl p g
                      pure $
                        if n /= 2
                          then proj
                          else
                            proj
                              { packages = [],
                                brokenPackages =
                                  [ BrokenPackage
                                      { cabalFile = cabal,
                                        problem = CabalUnreadable (CantRead "boom")
                                      }
                                  ]
                              }
                  }
              probe = fakeRevProbe Pair {head = diskAt headDir, base = diskAt baseDir}
              change _ d = failOnce d {probeFiles = \_ _ _ -> pure (Right probe)}
          withRunningProbe ["--idle-exit", "0"] change $ \_ app -> do
            let package = "\"name\":\"p\""
            m1 <- getSide app "modules" "base"
            assertBool (show m1.body) (not (package `inBody` m1))
            forM_ [2, 3 :: Int] $ \g -> do
              r <- reload app
              (r.status, generationOf r) @?= (200, Just g)
            -- Gen 2 reads base again; gen 3 keeps it.
            readIORef projectReads >>= (@?= 5)
            m3 <- getSide app "modules" "base"
            (m3.status, generationOf m3) @?= (200, Just 3)
            assertBool (show m3.body) (package `inBody` m3),
      skipOnGhc912 "many threads while 20 reloads run: well formed, never going down" $
        withCanonicalTemp $ \tmp -> do
          dir <- makeProject (tmp </> "p") ["A", "B", "C"]
          let real d = d {readEntry = readSourceEntry, indexWorkers = pure 2}
          withRunningArgs ["--idle-exit", "0"] real (const (diskAt dir)) $ \_ app -> do
            stopFlag <- newTVarIO False
            let routes =
                  [ (["files", "tree"], ""),
                    (["modules", "tree"], ""),
                    (["module", "tree", "A"], ""),
                    (["source", "tree", "src", "B.hs"], ""),
                    (["file", "tree", "src", "C.hs"], ""),
                    (["search", "tree"], "?q=A"),
                    (["indexing", "tree"], "")
                  ]
                hammer k = go (0 :: Int) (0 :: Int)
                  where
                    go seen n = do
                      let (path, q) = routes !! ((k + n) `mod` length routes)
                      r <-
                        callAppBody app "GET" [] NoBody q "127.0.0.1:4321" $
                          tokenText fakeToken : "api" : path
                      g <- case (r.status, generationOf r) of
                        (200, Just g) -> pure g
                        (500, _) | r.body == "cannot index module" -> pure seen
                        other -> assertFailure (show (path, other, r.body))
                      unless (g >= seen) $
                        assertFailure ("went down: " <> show (path, seen, g))
                      finished <- readTVarIO stopFlag
                      if finished then pure (n + 1) else go g (n + 1)
                reloads = do
                  forM_ [2 .. 21 :: Int] $ \g -> do
                    r <- reload app
                    (r.status, generationOf r) @?= (200, Just g)
                  atomically (writeTVar stopFlag True)
            result <-
              timeout 60_000_000 $
                concurrently reloads (forConcurrently [0 .. 15] hammer)
            case result of
              Nothing -> assertFailure "did not end"
              Just (_, counts) -> assertBool (show counts) (all (> 0) counts)
    ]

-- | The indexing done line.
isIndexed :: Text -> Bool
isIndexed = ("indexing: done" `Text.isPrefixOf`)

-- | Verbose and debug: the done lines and "reload: queued".
reloadArgs :: [String]
reloadArgs = ["--idle-exit", "0", "--verbose", "--debug"]

reloadPath :: [Text]
reloadPath = [tokenText fakeToken, "api", "reload"]

-- | The page's own Origin, as a browser sends it on POST.
ourOrigin :: Header
ourOrigin = ("Origin", "http://127.0.0.1:4321")

jsonType :: Header
jsonType = ("Content-Type", "application/json")

-- | POST api/reload with these headers and body.
postReload :: Application -> [Header] -> RequestBody -> IO ApiResult
postReload app headers body =
  callAppBody app "POST" headers body "" "127.0.0.1:4321" reloadPath

-- | POST api/reload as the client does.
reload :: Application -> IO ApiResult
reload app = postReload app [ourOrigin, jsonType] (Sized "{}")

-- | GET api/text-search/<side>?q=<q>.
textSearch :: Application -> Text -> BS.ByteString -> IO ApiResult
textSearch app side q =
  callAppBody app "GET" [] NoBody ("?q=" <> q) "127.0.0.1:4321" $
    [tokenText fakeToken, "api", "text-search", side]

-- | A fixture repo with these args, served with @--diff HEAD@ at this
-- folder of it (\"\" for the top), through the real probe, verbose.
-- Runs the action once indexing is done, with the repo's path.
withDiffRepo ::
  String ->
  [String] ->
  FilePath ->
  (Deps -> Deps) ->
  (FilePath -> Fake -> Application -> IO a) ->
  IO a
withDiffRepo name extra sub tweak action = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "repo"
  fixtureRepo name (repo : extra)
  Dir.createDirectory (tmp </> "home")
  setup <- gitSetup (tmp </> "home") gitProbeTimeLimit
  let change _ d =
        tweak
          d
            { checkRoot = \p -> pure (Right (Root p)),
              probeFiles = probeFilesWith setup,
              readEntry = readSourceEntry
            }
      args = [repo </> sub, "--idle-exit", "0", "--verbose", "--diff", "HEAD"]
  withRunningProbe args change $ \fake app -> do
    ok <- waitFor 10 (any isIndexed <$> readIORef fake.console)
    assertBool "not indexed" ok
    action repo fake app

-- | The generation of a JSON or CBOR body.
generationOf :: ApiResult -> Maybe Int
generationOf r = case Aeson.decode r.body <|> fromCbor of
  Just (Aeson.Object o) -> case KeyMap.lookup "generation" o of
    Just (Aeson.Number n) -> Just (round n)
    _ -> Nothing
  _ -> Nothing
  where
    fromCbor = either (const Nothing) Aeson.decode (cborToJson r.body)

-- | The text is in the body, as JSON (a CBOR body is turned to JSON).
inJson :: BS.ByteString -> ApiResult -> Bool
inJson needle r = case cborToJson r.body of
  Right j -> needle `BS.isInfixOf` LBS.toStrict j
  Left _ -> needle `BS.isInfixOf` LBS.toStrict r.body

-- | Open the event stream and keep what it writes. The MVar is filled
-- at the first write (hello).
recordStream :: Application -> MVar () -> IORef BS.ByteString -> IO ()
recordStream app opened seen = do
  let req =
        defaultRequest
          { requestMethod = "GET",
            requestHeaderHost = Just "127.0.0.1:4321",
            pathInfo = [tokenText fakeToken, "api", "events"]
          }
      write b = do
        modifyIORef' seen (<> LBS.toStrict (Builder.toLazyByteString b))
        void (tryPutMVar opened ())
  ResponseReceived <- app req $ \resp -> do
    let (_, _, withBody) = responseToStream resp
    withBody (\body -> body write (pure ()))
    pure ResponseReceived
  pure ()

-- | A lister of 'oneModule'.
oneLister :: Lister
oneLister = fakeLister (pure (Right oneModule))

-- | A list with one module, A.hs.
oneModule :: FileList
oneModule =
  mkFileList FromDisk 0 [(p, EntryFile) | Just p <- [relPathFromBytes "A.hs"]]

textOnly :: ModuleParse
-- Not TooSlow: a reload drops those (D-353).
textOnly = TextOnly (textOnlyModule (Preprocessed "fake") [])

-- | Run with this lister until the action is done, then stop with a
-- signal. The action gets the app warp would serve.
withRunning :: (Fake -> Lister) -> (Fake -> Application -> IO a) -> IO a
withRunning = withRunningWith id

-- | Like 'withRunning', with changed deps.
withRunningWith ::
  (Deps -> Deps) -> (Fake -> Lister) -> (Fake -> Application -> IO a) -> IO a
withRunningWith = withRunningArgs ["--idle-exit", "0"]

-- | Like 'withRunningWith', with the given CLI args instead of just
-- @--idle-exit 0@.
withRunningArgs ::
  [String] ->
  (Deps -> Deps) ->
  (Fake -> Lister) ->
  (Fake -> Application -> IO a) ->
  IO a
withRunningArgs args change mkLister =
  withRunningSides args change (BrowseSides . mkLister)

-- | Like 'withRunningArgs', with a lister per side.
withRunningSides ::
  [String] ->
  (Deps -> Deps) ->
  (Fake -> Sides Lister) ->
  (Fake -> Application -> IO a) ->
  IO a
withRunningSides args change mkListers =
  withRunningProbe args $ \fake d ->
    change d {probeFiles = \_ _ _ -> pure (Right (fakeProbe (mkListers fake)))}

-- | Run until the test is done, with deps made from the fake's (the
-- probe is the test's).
withRunningProbe ::
  [String] -> (Fake -> Deps -> Deps) -> (Fake -> Application -> IO a) -> IO a
withRunningProbe args change action = do
  fake <- newFake
  stop <- newEmptyMVar
  appVar <- newEmptyMVar
  let serve ready app = ready >> putMVar appVar app >> blockForever
      deps = change fake (idleDeps fake stop serve)
  withAsync (run deps args) $ \running -> do
    app <- timeout 5_000_000 (takeMVar appVar)
    result <- maybe (assertFailure "not ready") (action fake) app
    putMVar stop ()
    code <- timeout 5_000_000 (wait running)
    code @?= Just ExitSuccess
    pure result

-- * Diff mode

-- | A diff session through 'run', with a fake probe: each side answers
-- from its own snapshot (D-313).
diffTests :: TestTree
diffTests =
  testGroup
    "diff session"
    [ testCase "two disk dirs: each side from its own snapshot, one done line" $
        withCanonicalTemp $ \tmp -> do
          headDir <- makeProject (tmp </> "head") ["A", "C"]
          baseDir <- makeProject (tmp </> "base") ["A", "B"]
          let listers _ =
                DiffSides Pair {head = diskAt headDir, base = diskAt baseDir}
              real d = d {readEntry = readSourceEntry}
              args = ["--idle-exit", "0", "--verbose", "--trace-file", "t.jsonl"]
          withRunningSides args real listers $ \fake app -> do
            ok <- waitFor 5 (any isDone <$> readIORef fake.console)
            assertBool "not indexed" ok
            checkSides app "C" "B"
            d <- getSidePath app ["diff-files"]
            (d.status, d.body) @?= (500, "cannot pair files")
            out <- readIORef fake.console
            filter isDone out
              @?= [allDone]
            assertBool (show out) (any ("head: files: " `Text.isPrefixOf`) out)
            assertBool (show out) (any ("base: project: " `Text.isPrefixOf`) out)
            names <- map (.name) <$> readIORef fake.spans
            forM_ ["files:head", "files:base", "project:head", "project:base"] $ \n ->
              assertBool (show names) (n `elem` names)
            assertBool (show names) ("parser-warm-up" `elem` names)
            length (filter (== "indexing") names) @?= 1
            stream <- streamUntil app ["\"side\":\"head\"", "\"side\":\"base\""]
            assertBool (show stream) ("event: indexing" `BS.isInfixOf` stream),
      testCase "commit sides: HEAD and HEAD~1 of the sides repo" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "repo"
          fixtureRepo "sides" [repo]
          Dir.createDirectory (tmp </> "home")
          g <- realGitAt (tmp </> "home") repo
          info <- repoInfo g >>= either (assertFailure . show) pure
          let commit rev =
                resolveCommit g rev >>= \case
                  Right (Just c) -> pure c
                  other -> assertFailure (show other)
          headCommit <- commit "HEAD"
          baseCommit <- commit "HEAD~1"
          withObjectReader quiet defaultReaderSettings g $ \reader -> do
            let listers _ =
                  DiffSides
                    Pair
                      { head = commitLister quiet g info reader headCommit,
                        base = commitLister quiet g info reader baseCommit
                      }
                real d = d {readEntry = readSourceEntry}
                args = ["--idle-exit", "0", "--verbose"]
            withRunningSides args real listers $ \fake app -> do
              ok <- waitFor 5 (any isDone <$> readIORef fake.console)
              assertBool "not indexed" ok
              checkSides app "C" "B"
              headFiles <- getSide app "files" "head"
              assertBool (show headFiles.body) ("readme-link" `inBody` headFiles)
              baseFiles <- getSide app "files" "base"
              assertBool (show baseFiles.body) (not ("readme-link" `inBody` baseFiles))
              out <- readIORef fake.console
              filter isDone out
                @?= [allDone],
      testCase "--diff HEAD: head the working tree, through the real probe" $
        withCanonicalTemp $ \tmp -> do
          let repo = tmp </> "repo"
          fixtureRepo "edits" [repo]
          Dir.createDirectory (tmp </> "home")
          setup <- gitSetup (tmp </> "home") gitProbeTimeLimit
          let change _ d =
                d
                  { checkRoot = \p -> pure (Right (Root p)),
                    probeFiles = probeFilesWith setup,
                    readEntry = readSourceEntry
                  }
              args = [repo, "--idle-exit", "0", "--verbose", "--diff", "HEAD"]
          headId <- gitOut repo ["rev-parse", "HEAD"]
          let short = Text.take 7 (Text.strip (Text.pack headId))
          withRunningProbe args change $ \fake app -> do
            ok <- waitFor 10 (any isDone <$> readIORef fake.console)
            assertBool "not indexed" ok
            let view route side p = do
                  r <- getSidePath app ([route, side] <> p)
                  (route, side, p, r.status) @?= (route, side, p, 200)
                  either (assertFailure . show) (pure . LBS.toStrict) (cborToJson r.body)
                has needle body = assertBool (show body) (needle `BS.isInfixOf` body)
            -- Edited, staged only, untracked; LFS: filtered on head only.
            view "file" "head" ["src", "A.hs"] >>= has "a = 2\\n"
            view "file" "base" ["src", "A.hs"] >>= has "a = 1\\n"
            view "file" "head" ["src", "S.hs"] >>= has "s = 2\\n"
            view "file" "base" ["src", "S.hs"] >>= has "s = 1\\n"
            view "file" "head" ["src", "New.hs"] >>= has "module New"
            view "file" "head" ["data.bin"] >>= has "big data\\n\",\"filtered\":true"
            view "source" "head" ["data.bin"] >>= has "\"filtered\":true"
            view "file" "base" ["data.bin"] >>= has "\"filtered\":false"
            view "source" "base" ["data.bin"] >>= has "\"filtered\":false"
            view "file" "head" ["src", "B.hs"] >>= has "\"filtered\":false"
            -- api/diff: modified, untracked, filtered, unchanged.
            let diffAt p = do
                  r <- getSidePath app ("diff" : p)
                  (p, r.status) @?= (p, 200)
                  either (assertFailure . show) (pure . LBS.toStrict) (cborToJson r.body)
            diffAt ["src", "A.hs"]
              >>= has "\"status\":\"modified\",\"similarity\":null,\"note\":null"
            diffAt ["src", "New.hs"] >>= has "\"status\":\"added\""
            diffAt ["data.bin"] >>= has "\"note\":\"filtered\""
            diffAt ["src", "B.hs"] >>= has "\"status\":\"unchanged\""
            newOnBase <- getSidePath app ["file", "base", "src", "New.hs"]
            newOnBase.status @?= 404
            t <- getSide app "files" "tree"
            t.status @?= 404
            out <- readIORef fake.console
            forM_
              [ "diff: HEAD (against the working tree)",
                "head: working tree",
                "base: HEAD = " <> short,
                "changed: 3 on head, 2 on base, indexed first"
              ]
              $ \l -> assertBool (show out) (l `elem` out)
            filter isDone out
              @?= ["indexing: done, 7 modules (7 ready, 0 failed, 7 text only) in 0 ms"],
      testCase "stop during indexing: exit 0, both sides failed" $ do
        fake <- newFake
        stop <- newEmptyMVar
        appVar <- newEmptyMVar
        parsing <- newEmptyMVar
        let serve ready app = ready >> putMVar appVar app >> blockForever
            busy = mapM_ (\n -> evaluate (length (show (n * n :: Integer)))) [1 ..]
            one = fakeLister (pure (Right oneModule))
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ ->
                    pure (Right (fakeProbe (DiffSides Pair {head = one, base = one}))),
                  parse = \_ _ -> void (tryPutMVar parsing ()) >> busy >> pure textOnly
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          ready <- timeout 5_000_000 (takeMVar appVar)
          app <- maybe (assertFailure "not ready") pure ready
          started <- timeout 5_000_000 (takeMVar parsing)
          started @?= Just ()
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess
          forM_ ["head", "base"] $ \side -> do
            r <- timeout 5_000_000 (getSidePath app ["module", side, "A"])
            fmap (\x -> (x.status, x.body)) r @?= Just (500, "cannot index module"),
      testCase "head's listing crashes: 500 there, base works" $ do
        let listers _ =
              DiffSides
                Pair
                  { head = fakeLister (ioError (userError "boom")),
                    base = fakeLister (pure (Right oneModule))
                  }
        withRunningSides ["--idle-exit", "0"] id listers $ \fake app -> do
          h <- timeout 5_000_000 (getSide app "files" "head")
          fmap (\x -> (x.status, x.body)) h @?= Just (500, "cannot list files")
          b <- getSide app "files" "base"
          b.status @?= 200
          m <- timeout 5_000_000 (getSidePath app ["module", "base", "A"])
          fmap (.status) m @?= Just 200
          t <- getSide app "files" "tree"
          t.status @?= 404
          let line = "haskell-reviewer: head: cannot list files: user error (boom)"
          ok <- waitFor 5 (elem line <$> readIORef fake.console)
          out <- readIORef fake.console
          assertBool (show out) ok,
      testCase "base's listing fails: 500 there, head is indexed" $ do
        let listers _ =
              DiffSides
                Pair
                  { head = fakeLister (pure (Right oneModule)),
                    base = fakeLister (pure (Left (ListCrashed "boom")))
                  }
        withRunningSides ["--idle-exit", "0", "--verbose"] id listers $ \fake app -> do
          b <- getSide app "files" "base"
          (b.status, b.body) @?= (500, "cannot list files")
          ok <- waitFor 5 (any isDone <$> readIORef fake.console)
          out <- readIORef fake.console
          assertBool (show out) ok
          filter isDone out
            @?= ["indexing: done, 1 modules (1 ready, 0 failed, 1 text only) in 0 ms"]
          assertBool
            (show out)
            ("haskell-reviewer: base: cannot list files: boom" `elem` out),
      testCase "warm-ups run once, before any slot is filled" $ do
        count <- newIORef (0 :: Int)
        release <- newEmptyMVar
        let warming d =
              d {warmUp = [modifyIORef' count (+ 1) >> readMVar release]}
            one = fakeLister (pure (Right oneModule))
            listers _ = DiffSides Pair {head = one, base = one}
        withRunningSides ["--idle-exit", "0"] warming listers $ \_ app ->
          withAsync (getSide app "files" "head") $ \h ->
            withAsync (getSide app "files" "base") $ \b -> do
              threadDelay 200_000
              early <- (,) <$> poll h <*> poll b
              assertBool "answered before the warm-up" (bothNothing early)
              putMVar release ()
              rh <- timeout 5_000_000 (wait h)
              rb <- timeout 5_000_000 (wait b)
              (fmap (.status) rh, fmap (.status) rb) @?= (Just 200, Just 200)
        readIORef count >>= (@?= 1),
      testCase "stop before the listing starts: both sides 500, exit 0" $ do
        fake <- newFake
        stop <- newEmptyMVar
        appVar <- newEmptyMVar
        -- Never ready: the listing never starts.
        let serve _ app = putMVar appVar app >> blockForever
            one = fakeLister (pure (Right oneModule))
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ ->
                    pure (Right (fakeProbe (DiffSides Pair {head = one, base = one})))
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          ready <- timeout 5_000_000 (takeMVar appVar)
          app <- maybe (assertFailure "not served") pure ready
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess
          forM_ ["head", "base"] $ \side -> do
            r <- timeout 5_000_000 (getSide app "files" side)
            fmap (\x -> (x.status, x.body)) r @?= Just (500, "cannot list files")
          d <- timeout 5_000_000 (getSidePath app ["diff-files"])
          fmap (\x -> (x.status, x.body)) d @?= Just (500, "cannot list files"),
      testCase "stop during listing: both sides 500, exit 0" $ do
        fake <- newFake
        stop <- newEmptyMVar
        appVar <- newEmptyMVar
        listed <- newEmptyMVar
        let serve ready app = ready >> putMVar appVar app >> blockForever
            slow = fakeLister neverDone
            quick = fakeLister (putMVar listed () >> pure (Right oneModule))
            deps =
              (idleDeps fake stop serve)
                { probeFiles = \_ _ _ ->
                    pure (Right (fakeProbe (DiffSides Pair {head = slow, base = quick})))
                }
        withAsync (run deps ["--idle-exit", "0"]) $ \running -> do
          ready <- timeout 5_000_000 (takeMVar appVar)
          app <- maybe (assertFailure "not ready") pure ready
          started <- timeout 5_000_000 (takeMVar listed)
          started @?= Just ()
          putMVar stop ()
          code <- timeout 5_000_000 (wait running)
          code @?= Just ExitSuccess
          forM_ ["head", "base"] $ \side -> do
            r <- timeout 5_000_000 (getSide app "files" side)
            fmap (\x -> (x.status, x.body)) r @?= Just (500, "cannot list files")
          d <- timeout 5_000_000 (getSidePath app ["diff-files"])
          fmap (\x -> (x.status, x.body)) d @?= Just (500, "cannot list files"),
      testCase "both sides fail to list: no warm-up, no workers, both 500" $ do
        count <- newIORef (0 :: Int)
        asked <- newIORef False
        let counting d =
              d
                { warmUp = [modifyIORef' count (+ 1)],
                  indexWorkers = writeIORef asked True >> pure 1
                }
            failing = fakeLister (pure (Left (ListCrashed "boom")))
            listers _ = DiffSides Pair {head = failing, base = failing}
        withRunningSides ["--idle-exit", "0", "--verbose"] counting listers $
          \fake app -> do
            forM_ ["head", "base"] $ \side -> do
              r <- getSide app "files" side
              (side, r.status) @?= (side, 500)
            let lines' = filter ("cannot list files" `Text.isInfixOf`)
            ok <- waitFor 5 ((== 2) . length . lines' <$> readIORef fake.console)
            out <- readIORef fake.console
            assertBool (show out) ok
            filter ("indexing:" `Text.isPrefixOf`) out @?= []
        readIORef count >>= (@?= 0)
        readIORef asked >>= (@?= False),
      testCase "a crash after the fill: both sides still indexed" $ do
        let failing d =
              d
                { openTracer = \_ _ ->
                    pure . Right $
                      Tracer
                        { record = \sp ->
                            when (sp.name == "files:head") (ioError (userError "boom")),
                          close = pure ()
                        }
                }
            one = fakeLister (pure (Right oneModule))
            listers _ = DiffSides Pair {head = one, base = one}
            args = ["--idle-exit", "0", "--verbose", "--trace-file", "t.jsonl"]
        withRunningSides args failing listers $ \fake _ -> do
          ok <- waitFor 5 (any isDone <$> readIORef fake.console)
          out <- readIORef fake.console
          assertBool (show out) ok
          filter isDone out
            @?= ["indexing: done, 2 modules (2 ready, 0 failed, 2 text only) in 0 ms"]
    ]
  where
    bothNothing = \case
      (Nothing, Nothing) -> True
      _ -> False
    isDone = ("indexing: done" `Text.isPrefixOf`)
    allDone = "indexing: done, 4 modules (4 ready, 0 failed, 4 text only) in 0 ms"

-- | A disk lister of this dir, not in a repo.
diskAt :: FilePath -> Lister
diskAt dir =
  diskLister quiet (CanonicalRoot dir (Char8.pack dir)) (ListFromDisk NotInRepo)

-- | A real diff session on @cabal-change.sh@, through the real probe,
-- one worker (D-317). Built once for the group.
--
-- Not tested: a crash while finding the changed modules only logs a
-- line. Making it crash would need a new hook in 'Deps'.
cabalChangeTests :: TestTree
cabalChangeTests =
  withResource runCabalChange (\_ -> pure ()) $ \get ->
    testGroup
      "cabal-change"
      [ goldenVsString "head module tree" (golden "head") ((.headTree) <$> get),
        goldenVsString "base module tree" (golden "base") ((.baseTree) <$> get),
        testCase "changed first, head then base, then the rest; twins not read" $ do
          r <- get
          -- Base's A, B, M and N have head's blob and settings: they
          -- come from the cache, unread (D-320).
          r.claims
            @?= [ ("head", "added/src/C.hs"),
                  ("head", "ext/src/X.hs"),
                  ("head", "ext/src/Y.hs"),
                  ("base", "ext/src/X.hs"),
                  ("base", "ext/src/Y.hs"),
                  ("base", "added/src/C.hs"),
                  ("head", "added/src/A.hs"),
                  ("head", "added/src/B.hs"),
                  ("head", "moved/lib/M.hs"),
                  ("head", "moved/lib/N.hs")
                ],
        testCase "log lines, tree 404" $ do
          r <- get
          let has l = assertBool (show r.console) (l `elem` r.console)
          has "changed: 3 on head, 3 on base, indexed first"
          -- C, X and Y twice (other settings), the others once.
          has "indexing: 10 parsed, 4 shared"
          has "diff: HEAD~1..HEAD"
          let baseLine = ("base: HEAD~1 = " `Text.isPrefixOf`)
          assertBool (show r.console) (any baseLine r.console)
          length (filter ("indexing: done" `Text.isPrefixOf`) r.console) @?= 1
          r.treeStatus @?= 404
      ]
  where
    golden side = "../fixtures/repos/cabal-change." <> side <> ".json"

-- | api/diff-files through 'run' and the real probe on @renames.sh@
-- (D-325): ROOT is pkg/, so other/ is outside. Goldens in
-- fixtures/repos.
renamesTests :: TestTree
renamesTests =
  testGroup
    "renames"
    [ goldenVsString "commits" (golden "diff-files") $ do
        r <- runRenames [] "HEAD~1..HEAD" id
        let has l = assertBool (show r.console) (any (l `Text.isPrefixOf`) r.console)
        (r.status, r.contentType) @?= (200, Just "application/json")
        has "pairing: 21 changed, 11 renamed in "
        assertBool (show r.spans) ("pairing" `elem` r.spans)
        pure r.body,
      goldenVsString "rename limit 2" (golden "diff-files-limit") $
        (.body) <$> runRenames [] "HEAD~1..HEAD" limitTwo,
      goldenVsString "working tree" (golden "diff-files-worktree") $ do
        r <- runRenames ["sha1", "worktree"] "HEAD" id
        -- Edit, deletion, chmod and an untracked file outside ROOT; the
        -- clean other/keep and the edits inside ROOT don't count.
        let outside =
              "\"outside\":{\"total\":4,\"paths\":[\"other/del\",\"other/mode.sh\","
                <> "\"other/untracked\",\"other/x\"],\"truncated\":false}"
        assertBool (show r.body) (outside `BS.isInfixOf` LBS.toStrict r.body)
        pure r.body,
      testCase "pairing crashes: a line, 500, no markers" $ do
        r <- runRenames [] "HEAD~1..HEAD" crashing
        (r.status, r.body) @?= (500, "cannot pair files\n")
        let line = "haskell-reviewer: cannot pair files: user error (boom)"
        assertBool (show r.console) (line `elem` r.console)
        forM_ [r.headTree, r.baseTree] $ \tree -> do
          let t = LBS.toStrict tree
              none = "\"change\":null,\"settingsChanged\":false"
          assertBool (show t) (none `BS.isInfixOf` t)
          assertBool (show t) (not ("\"change\":\"" `BS.isInfixOf` t)),
      goldenVsString "api/diff: one file per kind of change" (golden "diff") $ do
        (_, diffs) <- runRenamesWith [] "HEAD~1..HEAD" id $ \app -> do
          let get p = getSidePath app ("diff" : Text.splitOn "/" p)
          renamedFrom <- get "exact/a.txt"
          renamedTo <- get "exact/b.txt"
          renamedFrom.body @?= renamedTo.body
          forM_ ["../other/x", "other/x", "nope.txt"] $ \p -> do
            r <- get p
            (p, r.status) @?= (p, 404)
          mapM
            (\p -> (,) p <$> get p)
            ["exact/b.txt", "sim60/new.txt", "ln/c", "tc/f", "sub/m2", "mode/x.sh"]
        dumps <- mapM dump diffs
        pure ("{" <> LBS.intercalate ",\n" dumps <> "}\n"),
      testCase "module trees: loose modules by path, added and removed" $ do
        r <- runRenames [] "HEAD~1..HEAD" id
        let has needle tree =
              assertBool (show tree) (needle `BS.isInfixOf` LBS.toStrict tree)
        has "\"id\":\"d2/Same.hs\",\"ref\":\"Same\",\"change\":\"added\"" r.headTree
        has "\"id\":\"d1/Same.hs\",\"ref\":\"Same\",\"change\":\"removed\"" r.baseTree
    ]
  where
    golden n = "../fixtures/repos/renames." <> n <> ".json"
    dump (p, r) = do
      (p, r.status, lookup "Content-Type" r.headers) @?= (p, 200, Just "application/cbor")
      j <- either (assertFailure . show) pure (cborToJson r.body)
      pure ("\"" <> LBS.fromStrict (Text.Encoding.encodeUtf8 p) <> "\":" <> j)
    limitTwo d = d {pairFiles = pairListed defaultSettings {renameLimit = RenameLimit 2}}
    crashing d = d {pairFiles = \_ -> ioError (userError "boom")}

-- | What 'runRenames' saw.
data RenamesRun = RenamesRun
  { status :: Int,
    contentType :: Maybe BS.ByteString,
    -- | With a newline, for the goldens.
    body :: LBS.ByteString,
    headTree :: LBS.ByteString,
    baseTree :: LBS.ByteString,
    console :: [Text],
    spans :: [Text]
  }

-- | @renames.sh@ with these extra args, served with @--diff@ at pkg/.
runRenames :: [String] -> String -> (Deps -> Deps) -> IO RenamesRun
runRenames extra diff tweak = fst <$> runRenamesWith extra diff tweak (\_ -> pure ())

-- | Like 'runRenames', with more requests first.
runRenamesWith ::
  [String] -> String -> (Deps -> Deps) -> (Application -> IO a) -> IO (RenamesRun, a)
runRenamesWith extra diff tweak more = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "repo"
  fixtureRepo "renames" (repo : extra)
  Dir.createDirectory (tmp </> "home")
  setup <- gitSetup (tmp </> "home") gitProbeTimeLimit
  let change _ d =
        tweak
          d
            { checkRoot = \p -> pure (Right (Root p)),
              probeFiles = probeFilesWith setup,
              readEntry = readSourceEntry
            }
      args = [repo </> "pkg", "--idle-exit", "0", "--verbose", "--diff", diff]
      args' = args <> ["--trace-file", "t.jsonl"]
  withRunningProbe args' change $ \fake app -> do
    extraResult <- more app
    r <- getSidePath app ["diff-files"]
    h <- getSide app "modules" "head"
    b <- getSide app "modules" "base"
    -- The pairing line and span come after the fill, so maybe after the
    -- answers. The workers start after them: wait for the done line.
    _ <- waitFor 10 (any ("indexing: done" `Text.isPrefixOf`) <$> readIORef fake.console)
    out <- readIORef fake.console
    names <- map (.name) <$> readIORef fake.spans
    pure . (,extraResult) $
      RenamesRun
        { status = r.status,
          contentType = lookup "Content-Type" r.headers,
          body = r.body <> "\n",
          headTree = h.body,
          baseTree = b.body,
          console = out,
          spans = names
        }

-- | What 'runCabalChange' saw.
data CabalChange = CabalChange
  { headTree :: LBS.ByteString,
    baseTree :: LBS.ByteString,
    treeStatus :: Int,
    -- | Module reads by the workers, in order: side and path.
    claims :: [(Text, Text)],
    console :: [Text]
  }

runCabalChange :: IO CabalChange
runCabalChange = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "repo"
  fixtureRepo "cabal-change" [repo]
  Dir.createDirectory (tmp </> "home")
  setup <- gitSetup (tmp </> "home") gitProbeTimeLimit
  headId <- Text.strip . Text.pack <$> gitOut repo ["rev-parse", "HEAD"]
  claimed <- newIORef []
  let record src p = case src of
        CommitEntries c ->
          let ObjectId o = c.commit
              side = if o == headId then "head" else "base"
           in atomicModifyIORef' claimed (\rs -> (rs <> [(side, relPathText p)], ()))
        DiskEntries _ -> pure ()
        WorkTreeEntries _ -> pure ()
      change _ d =
        d
          { checkRoot = \p -> pure (Right (Root p)),
            probeFiles = probeFilesWith setup,
            readEntry = \src p k -> record src p >> readSourceEntry src p k,
            indexWorkers = pure 1
          }
      args = [repo, "--idle-exit", "0", "--verbose", "--diff", "HEAD~1..HEAD"]
  withRunningProbe args change $ \fake app -> do
    -- The parsed/shared line comes right after the done line.
    let shares l = "indexing: " `Text.isPrefixOf` l && " shared" `Text.isSuffixOf` l
    ok <- waitFor 10 (any shares <$> readIORef fake.console)
    assertBool "not indexed" ok
    rs <- readIORef claimed
    h <- getSide app "modules" "head"
    b <- getSide app "modules" "base"
    t <- getSide app "modules" "tree"
    (h.status, b.status) @?= (200, 200)
    out <- readIORef fake.console
    pure
      CabalChange
        { headTree = h.body,
          baseTree = b.body,
          treeStatus = t.status,
          claims = rs,
          console = out
        }

-- | Head has the first module, base the second; both have A.
checkSides :: Application -> Text -> Text -> Assertion
checkSides app headOnly baseOnly = do
  h <- getSide app "files" "head"
  b <- getSide app "files" "base"
  (h.status, b.status) @?= (200, 200)
  assertBool "same lists" (h.body /= b.body)
  t <- getSide app "files" "tree"
  t.status @?= 404
  let status path = (.status) <$> getSidePath app path
  status ["module", "head", headOnly] >>= (@?= 200)
  status ["module", "base", baseOnly] >>= (@?= 200)
  status ["module", "head", baseOnly] >>= (@?= 404)
  status ["module", "base", headOnly] >>= (@?= 404)
  status ["module", "tree", "A"] >>= (@?= 404)
  forM_ ["head", "base"] $ \side -> do
    r <- getSide app "indexing" side
    let tag = "\"side\":\"" <> Text.Encoding.encodeUtf8 side <> "\""
    assertBool (show r.body) (tag `inBody` r)

-- | A project dir with one library of these modules.
makeProject :: FilePath -> [Text] -> IO FilePath
makeProject dir names = do
  Dir.createDirectoryIfMissing True (dir </> "src")
  writeFile (dir </> "p.cabal") $
    "cabal-version: 3.0\nname: p\nversion: 0.1\n\nlibrary\n  exposed-modules: "
      <> unwords (map Text.unpack names)
      <> "\n  hs-source-dirs: src\n"
  forM_ names $ \n -> do
    let name = Text.unpack n
    writeFile (dir </> "src" </> name <> ".hs") ("module " <> name <> " where\n")
  Dir.canonicalizePath dir

-- | GET api/<what>/<side>.
getSide :: Application -> Text -> Text -> IO ApiResult
getSide app what side = getSidePath app [what, side]

-- | GET api/<path...>.
getSidePath :: Application -> [Text] -> IO ApiResult
getSidePath app path =
  callApp app "GET" "127.0.0.1:4321" (tokenText fakeToken : "api" : path)

inBody :: BS.ByteString -> ApiResult -> Bool
inBody needle r = needle `BS.isInfixOf` LBS.toStrict r.body

-- | Read the event stream until it holds every text (5 s at most).
streamUntil :: Application -> [BS.ByteString] -> IO BS.ByteString
streamUntil app needles = do
  seen <- newIORef BS.empty
  let req =
        defaultRequest
          { requestMethod = "GET",
            requestHeaderHost = Just "127.0.0.1:4321",
            pathInfo = [tokenText fakeToken, "api", "events"]
          }
      write b = do
        modifyIORef' seen (<> LBS.toStrict (Builder.toLazyByteString b))
        bytes <- readIORef seen
        when (all (`BS.isInfixOf` bytes) needles) (throwIO StreamSeen)
      go = app req $ \resp -> do
        let (_, _, withBody) = responseToStream resp
        withBody (\body -> body write (pure ()))
        pure ResponseReceived
  r <- timeout 5_000_000 (try @StreamSeen go)
  bytes <- readIORef seen
  case r of
    Just (Left StreamSeen) -> pure bytes
    _ -> assertFailure ("not seen in the stream: " <> show bytes)

-- | Thrown to end the stream once the texts are there.
data StreamSeen = StreamSeen
  deriving (Show)

instance Exception StreamSeen

quiet :: Logger
quiet = newLogger LevelInfo (\_ -> pure ())

-- | GET api/files/tree on the app.
getFiles :: Application -> IO ApiResult
getFiles app =
  callApp app "GET" "127.0.0.1:4321" [tokenText fakeToken, "api", "files", "tree"]

-- * Idle exit

-- | A "minute" here is 100 ms: short, but with room for slow machines.
-- The URL line must come before the timer fires.
idleTests :: TestTree
idleTests =
  testGroup
    "idle exit"
    [ testCase "no tab: exit 0 with a line, all closed" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        code <-
          timeout 5_000_000 (run deps ["--idle-exit", "1", "--trace-file", "t.jsonl"])
        code @?= Just ExitSuccess
        readIORef fake.console
          >>= ( @?=
                  [ "haskell-reviewer: serving at " <> fakeUrl,
                    "haskell-reviewer: no browser tab for 1 minute, exiting"
                  ]
              )
        readIORef fake.closed >>= (@?= True)
        readIORef fake.listenerClosed >>= (@?= True),
      testCase "plural minutes" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        code <- timeout 5_000_000 (run deps ["--idle-exit", "3"])
        code @?= Just ExitSuccess
        out <- readIORef fake.console
        drop (length out - 1) out
          @?= ["haskell-reviewer: no browser tab for 3 minutes, exiting"],
      testCase "--idle-exit 0: no idle exit, a signal still stops it" $ do
        fake <- newFake
        stop <- newEmptyMVar
        let deps = idleDeps fake stop (\ready _ -> ready >> blockForever)
        running <- async (run deps ["--idle-exit", "0"])
        threadDelay 1_000_000
        poll running >>= assertRunning
        putMVar stop ()
        code <- timeout 5_000_000 (wait running)
        code @?= Just ExitSuccess
        readIORef fake.console >>= (@?= ["haskell-reviewer: serving at " <> fakeUrl]),
      testCase "an open stream keeps it alive; closing it starts the timer" $ do
        fake <- newFake
        stop <- newEmptyMVar
        opened <- newEmptyMVar
        streamThread <- newEmptyMVar
        let serve ready app = do
              ready
              t <- forkIO (openStream app opened)
              putMVar streamThread t
              blockForever
            deps = idleDeps fake stop serve
        -- 5 "minutes" (500 ms) to get the stream open.
        running <- async (run deps ["--idle-exit", "5"])
        t <- takeMVar streamThread
        counted <- timeout 5_000_000 (takeMVar opened)
        counted @?= Just ()
        threadDelay 1_500_000
        poll running >>= assertRunning
        killThread t
        code <- timeout 5_000_000 (wait running)
        code @?= Just ExitSuccess
        out <- readIORef fake.console
        drop 1 out @?= ["haskell-reviewer: no browser tab for 5 minutes, exiting"]
    ]

-- | Fake deps with a 100 ms minute, the given serve, and a stop signal
-- the test sends.
idleDeps :: Fake -> MVar () -> (IO () -> Application -> IO ()) -> Deps
idleDeps fake stop serve =
  (fake.deps)
    { idleMinute = Microseconds 100_000,
      listen = \_ _ -> pure (Right fake.listener {serve = serve}),
      installShutdown = \_ ->
        pure
          Shutdown
            { wait = readMVar stop,
              stopped = not <$> isEmptyMVar stop
            }
    }

-- | Open the event stream on the app, like a browser tab, and keep it
-- open. Writes go nowhere. The MVar is filled at the first write: the
-- stream is counted as open by then.
openStream :: Application -> MVar () -> IO ()
openStream app opened = do
  let req =
        defaultRequest
          { requestMethod = "GET",
            requestHeaderHost = Just "127.0.0.1:4321",
            pathInfo = [tokenText fakeToken, "api", "events"]
          }
  ResponseReceived <- app req $ \resp -> do
    let (_, _, withBody) = responseToStream resp
    withBody (\body -> body (\_ -> void (tryPutMVar opened ())) (pure ()))
    pure ResponseReceived
  pure ()

-- | A probe with these listers, in browse mode or diff mode. In diff
-- mode, @HEAD~1..HEAD@ at made-up commits.
fakeProbe :: Sides Lister -> Probe
fakeProbe = \case
  BrowseSides l -> BrowseProbe l
  DiffSides p -> DiffProbe info (\k -> k p)
  where
    oid c = ObjectId (Text.replicate 40 c)
    info =
      CommitsDiff
        CommitsInfo
          { range = Range TwoDot "HEAD~1" "HEAD",
            sides = Pair {head = oid "1", base = oid "2"},
            baseFrom = BaseTyped
          }

-- | A probe for @--diff HEAD@ with these listers, at a made-up commit.
fakeRevProbe :: Pair Lister -> Probe
fakeRevProbe p = DiffProbe info (\k -> k p)
  where
    commit = ObjectId (Text.replicate 40 "2")
    info = WorkTreeDiff WorkTreeInfo {rev = "HEAD", base = commit}

-- | A listing that never ends.
neverDone :: IO (Either ListError FileList)
neverDone = Left (ListCrashed "") <$ blockForever

-- | A lister for a fake ROOT.
fakeLister :: IO (Either ListError FileList) -> Lister
fakeLister action =
  Lister {list = fmap listed <$> action}
  where
    listed fl = Listed {files = fl, source = fakeRoot}
    fakeRoot = DiskEntries (CanonicalRoot "/abs/x" "/abs/x")

-- | Poll every 20 ms, up to the given seconds.
waitFor :: Double -> IO Bool -> IO Bool
waitFor secs check = go (ceiling (secs * 50) :: Int)
  where
    go n = do
      ok <- check
      if ok || n <= 0 then pure ok else threadDelay 20_000 >> go (n - 1)

-- | Skipped on GHC 9.12: a 9.12.4 RTS bug crashes it now and then
-- (Q-RTS-BLACKHOLE), likely
-- https://gitlab.haskell.org/ghc/ghc/-/issues/27432 (fixed in 9.12.5).
-- Re-enable on 9.12 once 9.12.5 is the tested 9.12. 9.10 and 9.14 run it.
skipOnGhc912 :: TestName -> Assertion -> TestTree
skipOnGhc912 name body
  | versionBranch compilerVersion == [9, 12] =
      testCase (name <> " (skipped on GHC 9.12, Q-RTS-BLACKHOLE)") (pure ())
  | otherwise = testCase name body

blockForever :: IO ()
blockForever = forever (threadDelay 1_000_000)

assertRunning :: Maybe a -> Assertion
assertRunning = \case
  Nothing -> pure ()
  Just _ -> assertFailure "run exited too early"

data Fake = Fake
  { deps :: Deps,
    listener :: Listener,
    console :: IORef [Text],
    spans :: IORef [Span],
    closed :: IORef Bool,
    listenerClosed :: IORef Bool,
    -- | URLs passed to openBrowser.
    opened :: IORef [Text]
  }

-- | A signal came before 'run' even started.
alreadyStopped :: Shutdown
alreadyStopped = Shutdown {wait = pure (), stopped = pure True}

fakeToken :: Token
fakeToken = tokenFromBytes (BS.replicate 24 0)

fakeUrl :: Text
fakeUrl = "http://127.0.0.1:4321/" <> tokenText fakeToken <> "/"

fakeWallTime :: UTCTime
fakeWallTime = UTCTime (fromGregorian 2026 10 20) 0

-- | Fake deps. The monotonic clock goes up by 4000 ns at each call.
--
-- The fake server calls ready, then sends the stop signal itself and
-- blocks, like warp would.
newFake :: IO Fake
newFake = do
  console <- newIORef []
  spans <- newIORef []
  closed <- newIORef False
  listenerClosed <- newIORef False
  opened <- newIORef []
  stop <- newEmptyMVar
  ticks <- newIORef 1000
  let tick = atomicModifyIORef' ticks (\t -> (t + 4000, MonotonicTime t))
      tracer =
        Tracer
          { record = \s -> atomicModifyIORef' spans (\ss -> (ss <> [s], ())),
            close = modifyIORef' closed (const True)
          }
      listener =
        Listener
          { port = Port 4321,
            serve = \ready _ -> do
              ready
              void (tryPutMVar stop ())
              forever (threadDelay 1000000),
            close = modifyIORef' listenerClosed (const True)
          }
      deps =
        Deps
          { -- Atomic: request threads and the listing thread log at once.
            console = \t -> atomicModifyIORef' console (\ls -> (ls <> [t], ())),
            clock = Clock {now = pure fakeWallTime, monotonic = tick},
            checkRoot = \p -> pure (Right (Root ("/abs/" <> p))),
            openTracer = \_ _ -> pure (Right tracer),
            newToken = pure fakeToken,
            listen = \_ _ -> pure (Right listener),
            loadConfig = \_ -> pure builtinDefaults,
            loadLangs = \_ -> pure noLangs,
            -- In memory: never the real ui.json.
            prefsPath = pure (Left HomeUnset),
            -- Never done: no line or span from the listing.
            probeFiles = \_ _ _ ->
              pure (Right (fakeProbe (BrowseSides (fakeLister neverDone)))),
            readProject = Session.Project.readProject,
            -- No real parse here: see Note [Shared values in tests] in
            -- HaskellReviewer.Core.PackageGlobTest.
            warmUp = [],
            diffWarmUp = pure (),
            readEntry = \_ _ _ -> pure (Right (TextContent "module A where\n")),
            parse = \_ _ -> pure textOnly,
            pairFiles = pairListed defaultSettings,
            indexWorkers = pure 1,
            openBrowser = \_ _ url -> modifyIORef' opened (<> [url]),
            installShutdown = \_ ->
              pure
                Shutdown
                  { wait = readMVar stop,
                    stopped = not <$> isEmptyMVar stop
                  },
            -- A real minute: the default idle exit (5 min) never
            -- fires in these tests.
            idleMinute = Microseconds 60_000_000,
            spawnSetup = Marked 0 (KeptAt (ResourceLimit 1024))
          }
  pure
    Fake
      { deps = deps,
        listener = listener,
        console = console,
        spans = spans,
        closed = closed,
        listenerClosed = listenerClosed,
        opened = opened
      }

assertOneLineWith :: Text -> [Text] -> Assertion
assertOneLineWith prefix out = case out of
  [line] ->
    assertBool
      (show line <> " should start with " <> show prefix)
      (prefix `Text.isPrefixOf` line)
  _ -> assertFailure ("expected one line, got " <> show out)
