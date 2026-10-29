-- | Indexing with fake reads and parses: priority, waiting requests,
-- failures, progress. See Note [Indexing] in
-- "HaskellReviewer.Session.Index".
--
-- No sleeps for ordering: tests wait on the state they need, with a
-- timeout only as a safety net.
module HaskellReviewer.Session.IndexTest (tests, parsingTests) where

import Control.Concurrent.Async (AsyncCancelled (..), cancel, poll, waitCatch, withAsync)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
  )
import Control.Exception (AsyncException (..), fromException, throwIO)
import Control.Monad (when)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent (FileContent (..), FileSize (..))
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Modules (ModuleId (..), buildModuleIndex)
import HaskellReviewer.Core.NameTable (NameEntry (..))
import HaskellReviewer.Core.Parser (ParseSettings)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger (..), newLogger)
import HaskellReviewer.Effects.Parser (parseGuarded)
import HaskellReviewer.Session.Index
  ( Index,
    IndexDeps (..),
    IndexFailure (..),
    Indexed (..),
    Progress (..),
    SlotState (..),
    awaitModule,
    bigFileSize,
    newIndex,
    readNames,
    readProgress,
    readStates,
    requestModule,
    runWorkers,
    slotState,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Index"
    [ testCase "a request jumps the queue" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B", "C", "D"]
        withAsync (runWorkers fake.deps 1 index) $ \_ -> do
          waitState index "A" isParsing
          atomically (requestModule index (m "D"))
          release fake "A"
          waitDone index
        readTVarIO fake.parsed >>= (@?= ["A", "D", "B", "C"]),
      testCase "a request waits for the module, then gets it" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B"]
        withAsync (runWorkers fake.deps 1 index) $ \_ ->
          withAsync (awaitModule index (m "A")) $ \request -> do
            waitState index "A" isParsing
            -- Can't be done: A's parse is still held.
            poll request >>= assertBool "answered before the parse" . isNothing
            release fake "A"
            r <- within (waitCatch request)
            either (assertFailure . show) (@?= Just (Right (IndexedParse textOnly))) r,
      testCase "a done module: answered at once; no slot: Nothing" $ do
        fake <- newFake []
        index <- indexOf ["A"]
        within (runWorkers fake.deps 2 index)
        r <- within (awaitModule index (m "A"))
        r @?= Just (Right (IndexedParse textOnly))
        none <- within (awaitModule index (m "Z"))
        none @?= Nothing,
      testCase "two requests for one module: one parse" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B", "C"]
        withAsync (runWorkers fake.deps 2 index) $ \_ ->
          withAsync (awaitModule index (m "C")) $ \r1 ->
            withAsync (awaitModule index (m "C")) $ \r2 -> do
              release fake "A"
              mapM_ (within . waitCatch) [r1, r2]
              waitDone index
        parsed <- readTVarIO fake.parsed
        length (filter (== "C") parsed) @?= 1
        length parsed @?= 3,
      testCase "failures: gone, unreadable, crashed; the others finish" $ do
        fake <- newFake []
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "B.hs" -> pure (Left ReadGone)
                    "C.hs" -> pure (Left (ReadFailed "Permission denied"))
                    _ -> fake.deps.readModule p,
                  parse = \s t ->
                    if t == "D"
                      then ioError (userError "boom")
                      else fake.deps.parse s t
                }
        index <- indexOf ["A", "B", "C", "D", "E"]
        within (runWorkers deps 2 index)
        states <- atomically (readStates index)
        Map.toList states
          @?= [ (m "A", Ready (IndexedParse textOnly)),
                (m "B", Failed SourceGone),
                (m "C", Failed (SourceUnreadable "Permission denied")),
                (m "D", Failed (IndexCrashed "user error (boom)")),
                (m "E", Ready (IndexedParse textOnly))
              ]
        atomically (readProgress index)
          >>= (@?= Progress {total = 5, ready = 2, failed = 3})
        logged <- readTVarIO fake.logged
        assertBool (show logged) $
          "haskell-reviewer: cannot read C.hs: Permission denied" `elem` logged
        assertBool (show logged) $
          "haskell-reviewer: cannot index D.hs: user error (boom)" `elem` logged,
      testCase "not text, too large, no longer a file" $ do
        fake <- newFake []
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "A.hs" -> pure (Right (Binary (FileSize 10)))
                    "B.hs" -> pure (Right NestedRepo)
                    _ -> fake.deps.readModule p
                }
        index <- indexOf ["A", "B", "C"]
        within (runWorkers deps 1 index)
        states <- atomically (readStates index)
        Map.elems states
          @?= [ Ready (IndexedNotText (FileSize 10)),
                Failed SourceGone,
                Ready (IndexedParse textOnly)
              ],
      testCase "progress counts at each step" $ do
        fake <- newFake ["A", "B", "C"]
        let deps =
              fake.deps
                { readModule = \p ->
                    if relPathText p == "D.hs"
                      then pure (Left ReadGone)
                      else fake.deps.readModule p
                }
        index <- indexOf ["A", "B", "C", "D"]
        let progress = atomically (readProgress index)
        withAsync (runWorkers deps 1 index) $ \workers -> do
          waitState index "A" isParsing
          progress >>= (@?= Progress {total = 4, ready = 0, failed = 0})
          release fake "A"
          waitState index "B" isParsing
          progress >>= (@?= Progress {total = 4, ready = 1, failed = 0})
          release fake "B"
          waitState index "C" isParsing
          progress >>= (@?= Progress {total = 4, ready = 2, failed = 0})
          release fake "C"
          -- The workers exit once every module is done.
          r <- within (waitCatch workers)
          either (assertFailure . show) pure r
        progress >>= (@?= Progress {total = 4, ready = 3, failed = 1})
        logged <- readTVarIO fake.logged
        -- 4 modules: a line at each one.
        filter ("indexing: " `Text.isPrefixOf`) logged
          @?= ["indexing: 1/4", "indexing: 2/4", "indexing: 3/4", "indexing: 4/4"],
      testCase "no modules: workers exit at once" $ do
        fake <- newFake []
        index <- indexOf []
        within (runWorkers fake.deps 4 index)
        atomically (readProgress index)
          >>= (@?= Progress {total = 0, ready = 0, failed = 0}),
      testCase "a cancel goes through; nothing is left Pending or Parsing" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B"]
        withAsync (runWorkers fake.deps 1 index) $ \workers -> do
          waitState index "A" isParsing
          cancel workers
          r <- within (waitCatch workers)
          case r of
            Left e | Just AsyncCancelled <- fromException e -> pure ()
            other -> assertFailure ("not cancelled: " <> show other)
        states <- atomically (readStates index)
        Map.elems states @?= replicate 2 (Failed (IndexCrashed "indexing was stopped"))
        atomically (readProgress index)
          >>= (@?= Progress {total = 2, ready = 0, failed = 2}),
      testCase "workers die: waiting requests get Failed, nothing left Pending" $ do
        fake <- newFake ["A"]
        -- A heap overflow is async: it ends the workers, not the module.
        let deps =
              fake.deps
                { parse = \st t ->
                    if t == "C" then throwIO HeapOverflow else fake.deps.parse st t
                }
        index <- indexOf ["A", "B", "C", "D"]
        withAsync (runWorkers deps 1 index) $ \workers -> do
          waitState index "A" isParsing
          -- C first, then B: B is never parsed.
          atomically (requestModule index (m "C") >> requestModule index (m "B"))
          withAsync (awaitModule index (m "B")) $ \waiting -> do
            release fake "A"
            r <- within (waitCatch workers)
            case r of
              Left e | Just HeapOverflow <- fromException e -> pure ()
              other -> assertFailure ("workers didn't die: " <> show other)
            w <- within (waitCatch waiting)
            either (assertFailure . show) (@?= Just (Left crashed)) w
        states <- atomically (readStates index)
        Map.elems states
          @?= Ready (IndexedParse textOnly) : replicate 3 (Failed crashed)
        atomically (readProgress index)
          >>= (@?= Progress {total = 4, ready = 1, failed = 3}),
      testCase "a log line that can't be written doesn't stop indexing" $ do
        fake <- newFake []
        let deps =
              fake.deps
                { readModule = \p ->
                    if relPathText p == "B.hs"
                      then pure (Left (ReadFailed "Permission denied"))
                      else fake.deps.readModule p,
                  logger = Logger {emit = \_ _ -> ioError (userError "stderr gone")}
                }
        index <- indexOf ["A", "B", "C"]
        within (runWorkers deps 1 index)
        states <- atomically (readStates index)
        Map.elems states
          @?= [ Ready (IndexedParse textOnly),
                Failed (SourceUnreadable "Permission denied"),
                Ready (IndexedParse textOnly)
              ],
      testCase "big files: one at a time, never in the way of small ones" $ do
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "C", big "D", "E"]
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "C.hs" -> pure (Right (TextContent (big "C")))
                    "D.hs" -> pure (Right (TextContent (big "D")))
                    _ -> fake.deps.readModule p
                }
            bigRunning = atomically (readTVar fake.bigRunning >>= check . (>= 1))
        index <- indexOf ["C", "D", "E", "F"]
        withAsync (runWorkers deps 2 index) $ \_ -> do
          -- One big file parses; the other worker skips the other big
          -- one and takes the small ones.
          within bigRunning
          waitState index "E" isParsing
          withAsync (awaitModule index (m "D")) $ \requested -> do
            release fake "E"
            waitState index "F" isReady
            readTVarIO fake.bigRunning >>= (@?= 1)
            release fake (big "C")
            release fake (big "D")
            r <- within (waitCatch requested)
            either (assertFailure . show) (@?= Just (Right (IndexedParse textOnly))) r
          waitDone index
        readTVarIO fake.bigMost >>= (@?= 1)
        sort <$> readTVarIO fake.parsed >>= (@?= [big "C", big "D", "E", "F"]),
      testCase "a request during the first read of a big file keeps its priority" $ do
        -- A and B are big and wait in their parse; whichever gets the
        -- lock runs, the other goes back to the queue, before C. C's
        -- first read waits on a gate; C is requested meanwhile.
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "A", big "B"]
        cEntered <- newEmptyMVar
        cGate <- newEmptyMVar
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "C.hs" -> do
                      _ <- tryPutMVar cEntered ()
                      readMVar cGate
                      pure (Right (TextContent (big "C")))
                    other -> pure (Right (TextContent (big (Text.dropEnd 3 other))))
                }
        index <- indexOf ["A", "B", "C"]
        withAsync (runWorkers deps 2 index) $ \_ -> do
          within (takeMVar cEntered)
          within (atomically (readTVar fake.bigRunning >>= check . (>= 1)))
          -- The one whose parse started holds the lock.
          started <- readTVarIO fake.parsed
          let (running, waiting)
                | big "A" `elem` started = ("A", "B")
                | otherwise = ("B", "A")
          waitState index waiting (== Pending)
          atomically (requestModule index (m "C"))
          putMVar cGate ()
          waitState index "C" (== Pending)
          -- The lock frees: C goes first. Without its priority the
          -- other big one would, and wait in its parse for ever.
          release fake (big running)
          waitState index "C" isReady
          release fake (big waiting)
          waitDone index
    ]
  where
    crashed = IndexCrashed "heap overflow"

-- | With the real parser: in the "Parsing" group (Q-RTS-BLACKHOLE).
parsingTests :: TestTree
parsingTests =
  testGroup
    "Index names"
    [ testCase "the name table grows as modules are done" $ do
        fake <- newFake ["module B where\nb = 1\n"]
        let deps =
              fake.deps
                { readModule = \p -> pure . Right . TextContent $ case relPathText p of
                    "A.hs" -> "module A (a) where\ndata T = K\na = 1\n"
                    _ -> "module B where\nb = 1\n",
                  parse = \s t -> do
                    _ <- fake.deps.parse s t
                    parseGuarded s t
                }
        index <- indexOf ["A", "B"]
        withAsync (runWorkers deps 1 index) $ \_ -> do
          waitState index "B" isParsing
          names <- atomically (readNames index)
          fmap (map (\e -> (e.name, e.exported))) names
            @?= Map.fromList [(m "A", [("T", False), ("K", False), ("a", True)])]
          release fake "module B where\nb = 1\n"
          waitDone index
        names <- atomically (readNames index)
        Map.keys names @?= [m "A", m "B"]
    ]

-- * Fakes

data Fake = Fake
  { deps :: IndexDeps,
    -- | Parse calls, by text, in order.
    parsed :: TVar [Text],
    -- | Parses running now, and the most at once.
    running :: TVar Int,
    most :: TVar Int,
    -- | The same, for texts over 'bigFileSize'.
    bigRunning :: TVar Int,
    bigMost :: TVar Int,
    gates :: Map.Map Text (MVar ()),
    logged :: TVar [Text]
  }

-- | Each module's text is its name. A parse of a text in the list waits
-- until it's released.
newFake :: [Text] -> IO Fake
newFake held = do
  parsed <- newTVarIO []
  running <- newTVarIO 0
  most <- newTVarIO 0
  bigRunning <- newTVarIO 0
  bigMost <- newTVarIO 0
  logged <- newTVarIO []
  gates <- Map.fromList <$> mapM (\t -> (t,) <$> newEmptyMVar) held
  let parse :: ParseSettings -> Text -> IO ModuleParse
      parse _ t = do
        let FileSize limit = bigFileSize
            isBig = Text.length t > limit
            enter r mx = do
              modifyTVar' r (+ 1)
              n <- readTVar r
              modifyTVar' mx (max n)
        atomically $ do
          modifyTVar' parsed (<> [t])
          enter running most
          when isBig (enter bigRunning bigMost)
        mapM_ readMVar (Map.lookup t gates)
        atomically $ do
          modifyTVar' running (subtract 1)
          when isBig (modifyTVar' bigRunning (subtract 1))
        pure textOnly
      deps =
        IndexDeps
          { readModule = \p ->
              pure (Right (TextContent (Text.dropEnd 3 (relPathText p)))),
            parse = parse,
            logger =
              newLogger LevelVerbose (\l -> atomically (modifyTVar' logged (<> [l])))
          }
  pure
    Fake
      { deps = deps,
        parsed = parsed,
        running = running,
        most = most,
        bigRunning = bigRunning,
        bigMost = bigMost,
        gates = gates,
        logged = logged
      }

-- | Let a held parse finish.
release :: Fake -> Text -> IO ()
release fake t = case Map.lookup t fake.gates of
  Just gate -> putMVar gate ()
  Nothing -> assertFailure ("not held: " <> show t)

textOnly :: ModuleParse
textOnly = TextOnly TextOnlyModule {reason = TooSlow, warnings = []}

-- | An index of loose modules, one per name (@A@ is @A.hs@).
indexOf :: [Text] -> IO Index
indexOf ns =
  newIndex
    ( buildModuleIndex
        project
        (mkFileList FromDisk 0 [(path (n <> ".hs"), EntryFile) | n <- ns])
    )
  where
    project =
      Project
        { source = ImplicitRootCabalFiles,
          withCompiler = Nothing,
          ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
          packages = [],
          brokenPackages = [],
          warnings = []
        }

m :: Text -> ModuleId
m n = LooseModule (path (n <> ".hs"))

path :: Text -> RelPath
path p = fromMaybe (error ("bad path " <> show p)) (relPathFromSegments [p])

isParsing :: SlotState -> Bool
isParsing = \case
  Parsing -> True
  _ -> False

isReady :: SlotState -> Bool
isReady = \case
  Ready _ -> True
  _ -> False

-- | Wait until the module's slot is in this state.
waitState :: Index -> Text -> (SlotState -> Bool) -> Assertion
waitState index n ok =
  within . atomically $
    slotState index (m n) >>= check . maybe False ok

-- | Wait until no slot is Pending or Parsing.
waitDone :: Index -> Assertion
waitDone index = within (atomically (done index >>= check))

done :: Index -> STM Bool
done index = do
  p <- readProgress index
  pure (p.ready + p.failed == p.total)

-- | 5 s, a safety net.
within :: IO a -> IO a
within action =
  timeout 5_000_000 action >>= \case
    Just a -> pure a
    Nothing -> assertFailure "not within 5 s"
