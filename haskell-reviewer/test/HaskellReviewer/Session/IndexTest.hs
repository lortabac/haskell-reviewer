-- | Indexing with fake reads and parses: priority, waiting requests,
-- failures, progress. See Note [Indexing] in
-- "HaskellReviewer.Session.Index".
--
-- No sleeps for ordering: tests wait on the state they need, with a
-- timeout only as a safety net.
module HaskellReviewer.Session.IndexTest (tests, parsingTests) where

import Control.Concurrent (myThreadId, threadDelay)
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
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
  )
import Control.Exception
  ( AsyncException (..),
    ErrorCall (..),
    IOException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (join, void, when)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (BlockReason (..), ThreadStatus (..), threadStatus)
import HaskellReviewer.Core.FileContent (FileContent (..), FileSize (..))
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule,
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (textOnlyModule)
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
import HaskellReviewer.Core.References (occKeys)
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), Logger (..), newLogger)
import HaskellReviewer.Effects.Parser (parseGuarded)
import HaskellReviewer.Session.Index
  ( Index,
    IndexDeps (..),
    IndexFailure (..),
    IndexJob (..),
    Indexed (..),
    ParsedSource (..),
    Pool,
    Progress (..),
    ShareCounts (..),
    SlotState (..),
    awaitModule,
    bigFileSize,
    cacheKeys,
    newIndex,
    newPool,
    prioritise,
    pruneCache,
    readNames,
    readProgress,
    readStates,
    requestCount,
    requestModule,
    runWorkers,
    shareCounts,
    slotState,
    withRequest,
  )
import Hedgehog (Property, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import IndexSupport (isParsing, isReady, waitDone, waitState, within)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Index"
    [ gateTests,
      poolTests,
      shareTests,
      testCase "a request jumps the queue" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B", "C", "D"]
        withAsync (runWorkers 1 [IndexJob fake.deps index]) $ \_ -> do
          waitState index (m "A") isParsing
          atomically (requestModule index (m "D"))
          release fake "A"
          waitDone index
        readTVarIO fake.parsed >>= (@?= ["A", "D", "B", "C"]),
      testCase "a request waits for the module, then gets it" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B"]
        withAsync (runWorkers 1 [IndexJob fake.deps index]) $ \_ ->
          withAsync (awaitModule index (m "A")) $ \request -> do
            waitState index (m "A") isParsing
            -- Can't be done: A's parse is still held.
            poll request >>= assertBool "answered before the parse" . isNothing
            release fake "A"
            r <- within (waitCatch request)
            either (assertFailure . show) (@?= Just (Right textOnlyResult)) r,
      testCase "a done module: answered at once; no slot: Nothing" $ do
        fake <- newFake []
        index <- indexOf ["A"]
        within (runWorkers 2 [IndexJob fake.deps index])
        r <- within (awaitModule index (m "A"))
        r @?= Just (Right (IndexedTextOnly textOnlyModule))
        none <- within (awaitModule index (m "Z"))
        none @?= Nothing,
      testCase "two requests for one module: one parse" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B", "C"]
        withAsync (runWorkers 2 [IndexJob fake.deps index]) $ \_ ->
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
        within (runWorkers 2 [IndexJob deps index])
        states <- atomically (readStates index)
        Map.toList states
          @?= [ (m "A", Ready (IndexedTextOnly textOnlyModule)),
                (m "B", Failed SourceGone),
                (m "C", Failed (SourceUnreadable "Permission denied")),
                (m "D", Failed (IndexCrashed "user error (boom)")),
                (m "E", Ready (IndexedTextOnly textOnlyModule))
              ]
        atomically (readProgress index)
          >>= (@?= Progress {total = 5, ready = 2, failed = 3})
        logged <- readTVarIO fake.logged
        assertBool (show logged) $
          "haskell-reviewer: cannot read C.hs: Permission denied" `elem` logged
        assertBool (show logged) $
          "haskell-reviewer: cannot index D.hs: user error (boom)" `elem` logged,
      testCase "a blob not in the local repo: SourceNotLocal, logged (D-343)" $ do
        fake <- newFake []
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "B.hs" -> pure (Left (ReadMissing (ObjectId "ee")))
                    _ -> fake.deps.readModule p
                }
        index <- indexOf ["A", "B"]
        within (runWorkers 1 [IndexJob deps index])
        states <- atomically (readStates index)
        Map.lookup (m "B") states @?= Just (Failed SourceNotLocal)
        logged <- readTVarIO fake.logged
        assertBool (show logged) $
          "haskell-reviewer: cannot read B.hs: object ee is not in the local repo"
            `elem` logged,
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
        within (runWorkers 1 [IndexJob deps index])
        states <- atomically (readStates index)
        Map.elems states
          @?= [ Ready (IndexedNotText (FileSize 10)),
                Failed SourceGone,
                Ready (IndexedTextOnly textOnlyModule)
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
        withAsync (runWorkers 1 [IndexJob deps index]) $ \workers -> do
          waitState index (m "A") isParsing
          progress >>= (@?= Progress {total = 4, ready = 0, failed = 0})
          release fake "A"
          waitState index (m "B") isParsing
          progress >>= (@?= Progress {total = 4, ready = 1, failed = 0})
          release fake "B"
          waitState index (m "C") isParsing
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
        within (runWorkers 4 [IndexJob fake.deps index])
        atomically (readProgress index)
          >>= (@?= Progress {total = 0, ready = 0, failed = 0}),
      testCase "a cancel goes through; nothing is left Pending or Parsing" $ do
        fake <- newFake ["A"]
        index <- indexOf ["A", "B"]
        withAsync (runWorkers 1 [IndexJob fake.deps index]) $ \workers -> do
          waitState index (m "A") isParsing
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
        withAsync (runWorkers 1 [IndexJob deps index]) $ \workers -> do
          waitState index (m "A") isParsing
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
          @?= Ready (IndexedTextOnly textOnlyModule) : replicate 3 (Failed crashed)
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
        within (runWorkers 1 [IndexJob deps index])
        states <- atomically (readStates index)
        Map.elems states
          @?= [ Ready (IndexedTextOnly textOnlyModule),
                Failed (SourceUnreadable "Permission denied"),
                Ready (IndexedTextOnly textOnlyModule)
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
        withAsync (runWorkers 2 [IndexJob deps index]) $ \_ -> do
          -- One big file parses; the other worker skips the other big
          -- one and takes the small ones.
          within bigRunning
          waitState index (m "E") isParsing
          withAsync (awaitModule index (m "D")) $ \requested -> do
            release fake "E"
            waitState index (m "F") isReady
            readTVarIO fake.bigRunning >>= (@?= 1)
            release fake (big "C")
            release fake (big "D")
            r <- within (waitCatch requested)
            either (assertFailure . show) (@?= Just (Right textOnlyResult)) r
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
        withAsync (runWorkers 2 [IndexJob deps index]) $ \_ -> do
          within (takeMVar cEntered)
          within (atomically (readTVar fake.bigRunning >>= check . (>= 1)))
          -- The one whose parse started holds the lock.
          started <- readTVarIO fake.parsed
          let (running, waiting)
                | big "A" `elem` started = ("A", "B")
                | otherwise = ("B", "A")
          waitState index (m waiting) (== Pending)
          atomically (requestModule index (m "C"))
          putMVar cGate ()
          waitState index (m "C") (== Pending)
          -- The lock frees: C goes first. Without its priority the
          -- other big one would, and wait in its parse for ever.
          release fake (big running)
          waitState index (m "C") isReady
          release fake (big waiting)
          waitDone index
    ]
  where
    crashed = IndexCrashed "heap overflow"

-- | Workers make room for requests. See Note [Indexing].
gateTests :: TestTree
gateTests =
  testGroup
    "gate"
    [ testCase "a request computing parks one of two workers" $ do
        fake <- newFake ["A", "B", "C"]
        index <- indexOf ["A", "B", "C", "D"]
        withHeldRequest index $ \done ->
          withAsync (runWorkers 2 [IndexJob fake.deps index]) $ \_ -> do
            waitState index (m "A") isParsing
            release fake "A"
            waitState index (m "B") isParsing
            -- One at a time while the request runs.
            readTVarIO fake.most >>= (@?= 1)
            -- It ends: the parked worker takes C next to B.
            done
            -- B and C held: both in their parse.
            within (atomically (readTVar fake.running >>= check . (== 2)))
            release fake "B"
            release fake "C"
            waitDone index,
      testCase "one worker never parks" $ do
        fake <- newFake []
        index <- indexOf ["A", "B"]
        withHeldRequest index $ \_ -> within (runWorkers 1 [IndexJob fake.deps index])
        readTVarIO fake.parsed >>= (@?= ["A", "B"]),
      testCase "a throw or a cancel gives the gate back" $ do
        fake <- newFake ["B", "C"]
        index <- indexOf ["B", "C"]
        r <- try @IOException (withRequest index (ioError (userError "boom") :: IO ()))
        r @?= Left (userError "boom")
        atomically (requestCount index) >>= (@?= 0)
        entered <- newEmptyMVar
        never <- newEmptyMVar
        withAsync (withRequest index (putMVar entered () >> takeMVar never)) $ \req -> do
          within (takeMVar entered)
          atomically (requestCount index) >>= (@?= 1)
          cancel req
          void (within (waitCatch req))
        atomically (requestCount index) >>= (@?= 0)
        -- So both workers run.
        withAsync (runWorkers 2 [IndexJob fake.deps index]) $ \_ -> do
          waitState index (m "B") isParsing
          waitState index (m "C") isParsing
          release fake "B"
          release fake "C"
          waitDone index,
      testCase "a big file put back gives its place back" $ do
        -- A and B are big and wait in their parse: one runs, the other
        -- is put back. The worker that put it back takes C.
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "A", big "B", "C"]
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "C.hs" -> pure (Right (TextContent "C"))
                    other -> pure (Right (TextContent (big (Text.dropEnd 3 other))))
                }
        index <- indexOf ["A", "B", "C"]
        withAsync (runWorkers 2 [IndexJob deps index]) $ \_ -> do
          within (atomically (readTVar fake.bigRunning >>= check . (>= 1)))
          waitState index (m "C") isParsing
          readTVarIO fake.bigRunning >>= (@?= 1)
          started <- readTVarIO fake.parsed
          let (running, waiting)
                | big "A" `elem` started = (big "A", big "B")
                | otherwise = (big "B", big "A")
          release fake "C"
          release fake running
          release fake waiting
          waitDone index
    ]

-- | One pool, two indexes: @head@ first, then @base@. Their module
-- names differ, so the fake tells them apart. See Note [Indexing].
poolTests :: TestTree
poolTests =
  testGroup
    "pool"
    [ testCase "head's background before base's" $ do
        fake <- newFake []
        (h, b) <- twoIndexes ["A", "B"] ["C", "D"]
        within (runWorkers 1 (headBase fake.deps h b))
        readTVarIO fake.parsed >>= (@?= ["A", "B", "C", "D"]),
      testCase "changed first: head's, base's, then background" $ do
        fake <- newFake []
        (h, b) <- twoIndexes ["A", "B", "C"] ["D", "E", "F"]
        atomically $ do
          prioritise h [m "C", m "nope"]
          prioritise b [m "F", m "E"]
        within (runWorkers 1 (headBase fake.deps h b))
        readTVarIO fake.parsed >>= (@?= ["C", "F", "E", "A", "B", "D"]),
      testCase "a request beats changed; a module taken already is not pushed" $ do
        fake <- newFake ["C"]
        (h, b) <- twoIndexes ["A", "B", "C"] ["D", "E"]
        atomically (prioritise h [m "C"])
        withAsync (runWorkers 1 (headBase fake.deps h b)) $ \_ -> do
          waitState h (m "C") isParsing
          atomically $ do
            prioritise b [m "D"]
            requestModule b (m "E")
            prioritise h [m "C"]
          release fake "C"
          waitDone h
          waitDone b
        readTVarIO fake.parsed >>= (@?= ["C", "E", "D", "A", "B"]),
      testCase "a request on base beats head's background" $ do
        fake <- newFake ["A"]
        (h, b) <- twoIndexes ["A", "B", "C"] ["D", "E"]
        withAsync (runWorkers 1 (headBase fake.deps h b)) $ \_ -> do
          waitState h (m "A") isParsing
          atomically (requestModule b (m "E"))
          release fake "A"
          waitDone h
          waitDone b
        readTVarIO fake.parsed >>= (@?= ["A", "E", "B", "C", "D"]),
      testCase "head's requests before base's, whatever came first" $ do
        fake <- newFake ["A"]
        (h, b) <- twoIndexes ["A", "B", "C"] ["D", "E"]
        withAsync (runWorkers 1 (headBase fake.deps h b)) $ \_ -> do
          waitState h (m "A") isParsing
          atomically (requestModule b (m "E"))
          atomically (requestModule h (m "C"))
          release fake "A"
          waitDone h
          waitDone b
        readTVarIO fake.parsed >>= (@?= ["A", "C", "E", "B", "D"]),
      testCase "only big modules left, lock busy: the other worker waits" $ do
        -- Both big, both held in their parse. The worker without the
        -- lock must wait in STM, not exit.
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "A", big "B"]
        readers <- newTVarIO []
        parsers <- newTVarIO []
        let deps =
              fake.deps
                { readModule = \p -> do
                    me <- myThreadId
                    atomically (modifyTVar' readers (me :))
                    pure (Right (TextContent (big (Text.dropEnd 3 (relPathText p))))),
                  parse = \st t -> do
                    me <- myThreadId
                    atomically (modifyTVar' parsers (me :))
                    fake.deps.parse st t
                }
        (h, b) <- twoIndexes ["A"] ["B"]
        withAsync (runWorkers 2 (headBase deps h b)) $ \_ -> do
          -- Both workers read; one parses with the lock.
          (holder, other) <- within . atomically $ do
            rs <- readTVar readers
            ps <- readTVar parsers
            case (ps, nub rs) of
              ([p], [x, y]) -> pure (p, if x == p then y else x)
              _ -> retry
          let settled = \case
                ThreadBlocked BlockedOnSTM -> True
                ThreadFinished -> True
                _ -> False
          st <- within (waitFor settled (threadStatus other))
          st @?= ThreadBlocked BlockedOnSTM
          assertBool "the holder is the other one" (holder /= other)
          started <- readTVarIO fake.parsed
          let (running, waiting)
                | big "A" `elem` started = (big "A", big "B")
                | otherwise = (big "B", big "A")
          release fake running
          release fake waiting
          waitDone h
          waitDone b
        readTVarIO fake.bigMost >>= (@?= 1),
      testCase "big files on head and base never parse at once" $ do
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "C", big "D"]
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "C.hs" -> pure (Right (TextContent (big "C")))
                    "D.hs" -> pure (Right (TextContent (big "D")))
                    _ -> fake.deps.readModule p
                }
        (h, b) <- twoIndexes ["C", "E"] ["D", "F"]
        withAsync (runWorkers 2 (headBase deps h b)) $ \_ -> do
          within (atomically (readTVar fake.bigRunning >>= check . (>= 1)))
          -- The other worker skips the other big one and does the
          -- small ones.
          waitState h (m "E") isReady
          waitState b (m "F") isReady
          readTVarIO fake.bigRunning >>= (@?= 1)
          started <- readTVarIO fake.parsed
          let (running, waiting)
                | big "C" `elem` started = (big "C", big "D")
                | otherwise = (big "D", big "C")
          release fake running
          release fake waiting
          waitDone h
          waitDone b
        readTVarIO fake.bigMost >>= (@?= 1)
        sort <$> readTVarIO fake.parsed >>= (@?= [big "C", big "D", "E", "F"]),
      testCase "a request on base parks a worker that takes head's modules" $ do
        fake <- newFake ["A", "B", "C"]
        (h, b) <- twoIndexes ["A", "B", "C", "D"] ["E"]
        withHeldRequest b $ \done ->
          withAsync (runWorkers 2 (headBase fake.deps h b)) $ \_ -> do
            atomically (requestCount h) >>= (@?= 1)
            waitState h (m "A") isParsing
            release fake "A"
            waitState h (m "B") isParsing
            readTVarIO fake.most >>= (@?= 1)
            done
            within (atomically (readTVar fake.running >>= check . (== 2)))
            release fake "B"
            release fake "C"
            waitDone h
            waitDone b,
      testCase "a cancel fails what is not done, on both" $ do
        fake <- newFake ["A"]
        (h, b) <- twoIndexes ["A", "B"] ["C"]
        withAsync (runWorkers 1 (headBase fake.deps h b)) $ \workers -> do
          waitState h (m "A") isParsing
          cancel workers
          void (within (waitCatch workers))
        let stopped = Failed (IndexCrashed "indexing was stopped")
        Map.elems <$> atomically (readStates h) >>= (@?= [stopped, stopped])
        Map.elems <$> atomically (readStates b) >>= (@?= [stopped])
        atomically (readProgress b)
          >>= (@?= Progress {total = 1, ready = 0, failed = 1}),
      testCase "workers die: what is not done fails, on both" $ do
        fake <- newFake []
        let deps =
              fake.deps
                { parse = \st t ->
                    if t == "B" then throwIO HeapOverflow else fake.deps.parse st t
                }
        (h, b) <- twoIndexes ["A", "B"] ["C"]
        r <- try @AsyncException (within (runWorkers 1 (headBase deps h b)))
        r @?= Left HeapOverflow
        Map.elems <$> atomically (readStates h)
          >>= (@?= [Ready textOnlyResult, Failed crashed])
        Map.elems <$> atomically (readStates b) >>= (@?= [Failed crashed]),
      testCase "an empty index finishes" $ do
        fake <- newFake []
        (h, b) <- twoIndexes ["A"] []
        within (runWorkers 2 [IndexJob fake.deps b, IndexJob fake.deps h])
        within (runWorkers 2 [IndexJob fake.deps b])
        atomically (readProgress h)
          >>= (@?= Progress {total = 1, ready = 1, failed = 0})
        atomically (readProgress b)
          >>= (@?= Progress {total = 0, ready = 0, failed = 0})
        within (runWorkers 2 [])
        readTVarIO fake.parsed >>= (@?= ["A"]),
      testCase "an index from another pool: throws, and nothing is left to do" $ do
        fake <- newFake []
        h <- indexOf ["A"]
        b <- indexOf ["B"]
        r <- try @ErrorCall (within (runWorkers 1 (headBase fake.deps h b)))
        let why = "runWorkers: indexes from different pools"
        r @?= Left (ErrorCall why)
        let failed i = Map.elems <$> atomically (readStates i)
        mapM_ (\i -> failed i >>= (@?= [Failed (IndexCrashed (Text.pack why))])) [h, b]
        readTVarIO fake.parsed >>= (@?= []),
      testProperty "random requests on both: all done, each parsed once" poolProperty
    ]
  where
    crashed = IndexCrashed "heap overflow"

-- | Two indexes in one pool share parses by key (D-320). Each file's
-- text is its blob, or its name with no blob, so the fake's parse
-- list shows what was parsed.
shareTests :: TestTree
shareTests =
  testGroup
    "sharing"
    [ testCase "same blob and settings: one parse, both get it" $ do
        fake <- newFake []
        pool <- newPool
        let hf = [("A.hs", Just "x"), ("B.hs", Just "y")]
            bf = [("Moved.hs", Just "x"), ("C.hs", Just "z")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        within (runWorkers 1 (headBase (blobReads fake (hf <> bf)) h b))
        sort <$> readTVarIO fake.parsed >>= (@?= ["x", "y", "z"])
        atomically (slotState b (m "Moved")) >>= (@?= Just (Ready textOnlyResult))
        atomically (shareCounts h) >>= (@?= ShareCounts {parses = 3, shares = 1})
        atomically (readProgress b)
          >>= (@?= Progress {total = 2, ready = 2, failed = 0}),
      testCase "same blob, other settings: two parses" $ do
        fake <- newFake []
        pool <- newPool
        let hf = [("A.hs", Just "x")]
            bf = [("A.lhs", Just "x")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        within (runWorkers 1 (headBase (blobReads fake (hf <> bf)) h b))
        readTVarIO fake.parsed >>= (@?= ["x", "x"])
        atomically (shareCounts h) >>= (@?= ShareCounts {parses = 2, shares = 0}),
      testCase "no blob: two parses" $ do
        fake <- newFake []
        pool <- newPool
        let hf = [("A.hs", Nothing)]
            bf = [("A.hs", Nothing)]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        within (runWorkers 2 (headBase (blobReads fake (hf <> bf)) h b))
        readTVarIO fake.parsed >>= (@?= ["A", "A"])
        atomically (shareCounts h) >>= (@?= ShareCounts {parses = 2, shares = 0}),
      testCase "same key twice in one index: one parse" $ do
        fake <- newFake []
        pool <- newPool
        let hf = [("A.hs", Just "x"), ("B.hs", Just "x")]
        h <- indexBlobs pool hf
        within (runWorkers 2 [IndexJob (blobReads fake hf) h])
        readTVarIO fake.parsed >>= (@?= ["x"])
        Map.elems <$> atomically (readStates h)
          >>= (@?= [Ready textOnlyResult, Ready textOnlyResult]),
      testCase "a request for a twin being parsed waits, then gets it" $ do
        fake <- newFake ["x"]
        pool <- newPool
        let hf = [("A.hs", Just "x"), ("B.hs", Just "y")]
            bf = [("Moved.hs", Just "x")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        withAsync (runWorkers 2 (headBase (blobReads fake (hf <> bf)) h b)) $ \_ -> do
          waitState h (m "A") isParsing
          withAsync (awaitModule b (m "Moved")) $ \request -> do
            -- The other worker does B, then waits: Moved is A's twin.
            waitState h (m "B") isReady
            poll request >>= assertBool "answered before the parse" . isNothing
            atomically (slotState b (m "Moved")) >>= (@?= Just Pending)
            release fake "x"
            r <- within (waitCatch request)
            either (assertFailure . show) (@?= Just (Right textOnlyResult)) r
          waitDone h
          waitDone b
        sort <$> readTVarIO fake.parsed >>= (@?= ["x", "y"]),
      testCase "the owner can't read it: the twin reads it" $ do
        fake <- newFake []
        gate <- newEmptyMVar
        let hf = [("A.hs", Just "x")]
            bf = [("Moved.hs", Just "x")]
            deps =
              (blobReads fake (hf <> bf))
                { readModule = \p -> case relPathText p of
                    "A.hs" -> do
                      readMVar gate
                      pure (Left (ReadFailed "Permission denied"))
                    _ -> (blobReads fake (hf <> bf)).readModule p
                }
        pool <- newPool
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        withAsync (runWorkers 2 (headBase deps h b)) $ \_ -> do
          waitState h (m "A") isParsing
          -- Moved waits for A's owner.
          atomically (slotState b (m "Moved")) >>= (@?= Just Pending)
          putMVar gate ()
          waitDone h
          waitDone b
        atomically (slotState h (m "A"))
          >>= (@?= Just (Failed (SourceUnreadable "Permission denied")))
        atomically (slotState b (m "Moved")) >>= (@?= Just (Ready textOnlyResult))
        readTVarIO fake.parsed >>= (@?= ["x"]),
      testCase "the owner finds it big while the lock is busy: no hang, one parse" $ do
        -- B1 holds the lock. A is read only then, so it is found big
        -- and put back; then its twin Moved too. Both wait for the lock.
        let FileSize limit = bigFileSize
            big n = n <> Text.replicate limit "x"
        fake <- newFake [big "B1"]
        let deps =
              fake.deps
                { readModule = \p -> case relPathText p of
                    "B1.hs" -> pure (Right (TextContent (big "B1")))
                    "A.hs" -> do
                      -- Once B1's parse has started.
                      atomically (readTVar fake.parsed >>= check . elem (big "B1"))
                      pure (Right (TextContent (big "X")))
                    _ -> pure (Right (TextContent (big "X")))
                }
        pool <- newPool
        let hf = [("B1.hs", Just "b1"), ("A.hs", Just "x")]
            bf = [("Moved.hs", Just "x")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        withAsync (runWorkers 2 (headBase deps h b)) $ \_ -> do
          within (atomically (readTVar fake.bigRunning >>= check . (>= 1)))
          release fake (big "B1")
          waitDone h
          waitDone b
        sort <$> readTVarIO fake.parsed >>= (@?= [big "B1", big "X"])
        readTVarIO fake.bigMost >>= (@?= 1)
        atomically (slotState b (m "Moved")) >>= (@?= Just (Ready textOnlyResult)),
      testCase "a cancel while the owner parses: the next run parses the key" $ do
        fake <- newFake ["x"]
        pool <- newPool
        let hf = [("A.hs", Just "x")]
            bf = [("Moved.hs", Just "x")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        let deps = blobReads fake (hf <> bf)
        withAsync (runWorkers 1 (headBase deps h b)) $ \workers -> do
          waitState h (m "A") isParsing
          cancel workers
          void (within (waitCatch workers))
        let stopped = Failed (IndexCrashed "indexing was stopped")
        Map.elems <$> atomically (readStates h) >>= (@?= [stopped])
        Map.elems <$> atomically (readStates b) >>= (@?= [stopped])
        -- A later run on the same pool: the key is not left owned.
        release fake "x"
        h2 <- indexBlobs pool hf
        within (runWorkers 1 [IndexJob (blobReads fake hf) h2])
        Map.elems <$> atomically (readStates h2) >>= (@?= [Ready textOnlyResult])
        readTVarIO fake.parsed >>= (@?= ["x", "x"]),
      testProperty "random keys: one parse per key, same results" shareProperty,
      testCase "prune: only the live indexes' blobs and settings stay (D-352)" $ do
        fake <- newFake []
        pool <- newPool
        let hf = [("A.hs", Just "x"), ("B.hs", Just "y"), ("C.hs", Nothing)]
            bf = [("A.lhs", Just "x"), ("D.hs", Just "z")]
        h <- indexBlobs pool hf
        b <- indexBlobs pool bf
        within (runWorkers 1 (headBase (blobReads fake (hf <> bf)) h b))
        let keys = Map.toList <$> atomically (cacheKeys pool)
        keys >>= (@?= [(ObjectId "x", 2), (ObjectId "y", 1), (ObjectId "z", 1)])
        -- The next generation: head's A edited (no blob now), B kept;
        -- base as it was, in a new index.
        let hf2 = [("A.hs", Nothing), ("B.hs", Just "y")]
        h2 <- indexBlobs pool hf2
        b2 <- indexBlobs pool bf
        pruneCache pool [h2, b2] >>= (@?= 1)
        keys >>= (@?= [(ObjectId "x", 1), (ObjectId "y", 1), (ObjectId "z", 1)])
        -- The kept results are hits: only A is parsed again.
        within (runWorkers 1 (headBase (blobReads fake (hf2 <> bf)) h2 b2))
        sort <$> readTVarIO fake.parsed >>= (@?= ["A", "C", "x", "x", "y", "z"])
        pruneCache pool [] >>= (@?= 3)
        keys >>= (@?= []),
      testCase "prune: an Owned key stays" $ do
        fake <- newFake ["x"]
        pool <- newPool
        let hf = [("A.hs", Just "x")]
        h <- indexBlobs pool hf
        withAsync (runWorkers 1 [IndexJob (blobReads fake hf) h]) $ \_ -> do
          waitState h (m "A") isParsing
          pruneCache pool [] >>= (@?= 0)
          Map.toList <$> atomically (cacheKeys pool) >>= (@?= [(ObjectId "x", 1)])
          release fake "x"
          waitDone h
    ]

-- | The fake's deps, reading each file's blob as its text, or its
-- name if it has none. Give every file of 'indexBlobs'.
blobReads :: Fake -> [(Text, Maybe Text)] -> IndexDeps
blobReads fake fs =
  fake.deps
    { readModule = \p ->
        let f = relPathText p
         in pure . Right . TextContent $
              fromMaybe (Text.dropEnd 3 f) (join (lookup f fs))
    }

-- | Random blobs on two indexes, random workers and requests.
shareProperty :: Property
shareProperty = property $ do
  let blob = Gen.element [Nothing, Just "x", Just "y", Just "z"]
  hs <- forAll (Gen.list (Range.linear 0 6) blob)
  bs <- forAll (Gen.list (Range.linear 0 6) blob)
  workers <- forAll (Gen.int (Range.linear 1 4))
  let files tag ks =
        [(tag <> Text.pack (show i) <> ".hs", k) | (i, k) <- zip [1 :: Int ..] ks]
      hf = files "h" hs
      bf = files "b" bs
      names = map fst (hf <> bf) <> ["none.hs"]
  asks <- forAll (Gen.list (Range.linear 0 8) (Gen.element names))
  (parsed, states, counts) <- evalIO $ do
    fake <- newFake []
    pool <- newPool
    h <- indexBlobs pool hf
    b <- indexBlobs pool bf
    let deps = blobReads fake (hf <> bf)
        onIndex f = if "h" `Text.isPrefixOf` f then h else b
        idOf f = LooseModule (path f)
    withAsync (runWorkers workers (headBase deps h b)) $ \run -> do
      mapM_ (\f -> within (awaitModule (onIndex f) (idOf f))) asks
      within (waitCatch run) >>= either throwIO pure
    states <- atomically ((<>) <$> readStates h <*> readStates b)
    parsed <- readTVarIO fake.parsed
    counts <- atomically (shareCounts h)
    pure (parsed, states, counts)
  let keyed = nub [k | Just k <- hs <> bs]
      unkeyed = [Text.dropEnd 3 f | (f, Nothing) <- hf <> bf]
  sort parsed === sort (keyed <> unkeyed)
  Map.elems states === replicate (length hs + length bs) (Ready textOnlyResult)
  let total = length hs + length bs
  counts === ShareCounts {parses = length parsed, shares = total - length parsed}

-- | Poll until the value is right. For a thread's status, which has no
-- STM to wait on.
waitFor :: (a -> Bool) -> IO a -> IO a
waitFor ok get = do
  x <- get
  if ok x then pure x else threadDelay 1_000 >> waitFor ok get

-- | Jobs for two indexes, head first, with the same deps.
headBase :: IndexDeps -> Index -> Index -> [IndexJob]
headBase deps h b = [IndexJob deps h, IndexJob deps b]

-- | Two indexes in one pool.
twoIndexes :: [Text] -> [Text] -> IO (Index, Index)
twoIndexes hs bs = do
  pool <- newPool
  h <- indexIn pool hs
  b <- indexIn pool bs
  pure (h, b)

-- | Some workers on two indexes; requests on either, while they run.
poolProperty :: Property
poolProperty = property $ do
  nh <- forAll (Gen.int (Range.linear 0 6))
  nb <- forAll (Gen.int (Range.linear 0 6))
  workers <- forAll (Gen.int (Range.linear 1 3))
  let hs = [Text.pack ('h' : show i) | i <- [1 .. nh]]
      bs = [Text.pack ('b' : show i) | i <- [1 .. nb]]
  asks <- forAll (Gen.list (Range.linear 0 8) (Gen.element (hs <> bs <> ["none"])))
  (parsed, states) <- evalIO $ do
    fake <- newFake []
    (h, b) <- twoIndexes hs bs
    let onIndex n = if "h" `Text.isPrefixOf` n then h else b
    withAsync (runWorkers workers (headBase fake.deps h b)) $ \run -> do
      mapM_ (\n -> within (awaitModule (onIndex n) (m n))) asks
      within (waitCatch run) >>= either throwIO pure
    states <- atomically ((<>) <$> readStates h <*> readStates b)
    parsed <- readTVarIO fake.parsed
    pure (parsed, states)
  sort parsed === sort (hs <> bs)
  Map.elems states === replicate (nh + nb) (Ready textOnlyResult)

-- | Hold a request in 'withRequest'. The action gets a way to end it;
-- it ends anyway on return.
withHeldRequest :: Index -> (IO () -> IO a) -> IO a
withHeldRequest index act = do
  entered <- newEmptyMVar
  done <- newEmptyMVar
  withAsync (withRequest index (putMVar entered () >> readMVar done)) $ \req -> do
    within (takeMVar entered)
    act $ do
      _ <- tryPutMVar done ()
      void (within (waitCatch req))

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
        withAsync (runWorkers 1 [IndexJob deps index]) $ \_ -> do
          waitState index (m "B") isParsing
          names <- atomically (readNames index)
          fmap (map (\e -> (e.name, e.exported))) names
            @?= Map.fromList [(m "A", [("T", False), ("K", False), ("a", True)])]
          release fake "module B where\nb = 1\n"
          waitDone index
        names <- atomically (readNames index)
        Map.keys names @?= [m "A", m "B"]
        -- A parsed module keeps its text, for the docs page, and the
        -- keys of find references.
        atomically (slotState index (m "A")) >>= \case
          Just (Ready (IndexedParsed p)) -> do
            p.text @?= "module A (a) where\ndata T = K\na = 1\n"
            p.occKeys @?= occKeys p.text p.parsed
          other -> assertFailure (take 300 (show other)),
      testCase "a shared parse: the twin gets the result and the names" $ do
        fake <- newFake []
        let text = "module A (a) where\na = 1\n"
            deps =
              fake.deps
                { readModule = \_ -> pure (Right (TextContent text)),
                  parse = \st t -> do
                    _ <- fake.deps.parse st t
                    parseGuarded st t
                }
        pool <- newPool
        h <- indexBlobs pool [("A.hs", Just "x")]
        b <- indexBlobs pool [("Moved.hs", Just "x")]
        within (runWorkers 1 (headBase deps h b))
        readTVarIO fake.parsed >>= (@?= [text])
        hs <- atomically (slotState h (m "A"))
        bs <- atomically (slotState b (m "Moved"))
        bs @?= hs
        names <- atomically (readNames b)
        fmap (map (.name)) names @?= Map.fromList [(m "Moved", ["a"])]
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
              newLogger LevelVerbose (\l -> atomically (modifyTVar' logged (<> [l]))),
            label = ""
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
textOnly = TextOnly textOnlyModule

textOnlyModule :: TextOnlyModule
textOnlyModule = Model.textOnlyModule TooSlow []

textOnlyResult :: Indexed
textOnlyResult = IndexedTextOnly textOnlyModule

-- | An index of loose modules, one per name (@A@ is @A.hs@).
indexOf :: [Text] -> IO Index
indexOf ns = do
  pool <- newPool
  indexIn pool ns

-- | An index of loose modules, one per name, in this pool.
indexIn :: Pool -> [Text] -> IO Index
indexIn pool ns = indexBlobs pool [(n <> ".hs", Nothing) | n <- ns]

-- | An index of loose modules, by file name, with each file's blob, in
-- this pool.
indexBlobs :: Pool -> [(Text, Maybe Text)] -> IO Index
indexBlobs pool fs =
  newIndex
    pool
    (\p -> ObjectId <$> join (lookup (relPathText p) fs))
    ( buildModuleIndex
        project
        (mkFileList FromDisk 0 [(path f, EntryFile) | (f, _) <- fs])
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
