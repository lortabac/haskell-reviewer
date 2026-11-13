-- | Resolution in the session, with real parses and fake reads: partial
-- results, the memo, waiting for imports. See Note [Session
-- resolution] in "HaskellReviewer.Session.Resolve".
--
-- In the "Parsing" group (Q-RTS-BLACKHOLE). Waits have a 5 s safety
-- net; timings are lower bounds only (D-125).
module HaskellReviewer.Session.ResolveTest (tests) where

import Control.Concurrent.Async (forConcurrently, poll, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
  )
import Control.Monad (forM, forM_)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.FileContent (FileContent (..))
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.ModuleGraph (ImportTarget (..), mapImport)
import HaskellReviewer.Core.ModuleModel (Extensions (..), ParsedModule (..))
import HaskellReviewer.Core.ModuleModel qualified as Model (Import (..), Located (..))
import HaskellReviewer.Core.Modules (ModuleIndex, buildModuleIndex)
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.References (RefsAnswer (..))
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.Resolve
  ( ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    Reason (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    homeImports,
    isPartial,
  )
import HaskellReviewer.Core.Resolve qualified as Core (resolveModule)
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Effects.Parser (parseWithin)
import HaskellReviewer.Session.Index
  ( Index,
    IndexDeps (..),
    IndexFailure (..),
    Indexed (..),
    Progress (..),
    SlotState (..),
    newIndex,
    readProgress,
    runWorkers,
    slotState,
  )
import HaskellReviewer.Session.Resolve
  ( Resolver,
    awaitImports,
    findReferences,
    memoKeys,
    moduleState,
    newResolver,
    resolveModule,
    warmUpStates,
  )
import IndexSupport
  ( isParsing,
    isReady,
    libFile,
    libProject,
    modId,
    waitDone,
    waitState,
    within,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Resolve session"
    [ testCase "partial while an import isn't ready; exact once it is" partialThenExact,
      testCase "the memo gets final sets only" memoFinalOnly,
      testCase "awaitImports moves the imports up, then gives up" awaitBumps,
      testCase "awaitImports returns once the imports are done" awaitEarly,
      testCase "awaitImports: a failed import, or one with no slot, is done" $
        awaitFailedOrNoSlot,
      testCase "both wait for the module itself, Pending or Parsing" waitForModule,
      testCase "nothing for text only, no slot, failed" nothingCases,
      testCase "a new resolver has its own memo, same results" newResolverCase,
      testCase "many threads while indexing: same as one thread after" concurrent,
      testCase "find references: partial while indexing; the memo grows" findRefsCase,
      testCase "moduleState of each slot state" moduleStateCase,
      testCase "the warm-up covers every slot state and import path" warmUpCovers
    ]

-- * Cases

-- | A imports nothing, B imports A and C.
abc :: [(Text, Text)]
abc =
  [ ("A", "module A (a) where\na = 1\n"),
    ("B", "module B where\nimport A\nimport C\nb = a + c\n"),
    ("C", "module C (c) where\nc = 2\n")
  ]

partialThenExact :: Assertion
partialThenExact = do
  w <- newWorld ["A"] abc
  withAsync (runWorkers w.deps 2 w.index) $ \_ -> do
    waitState w.index (modId "B") isReady
    waitState w.index (modId "C") isReady
    partial <- resolved w "B"
    assertBool "partial" (isPartial partial)
    partial.notReady @?= [modId "A"]
    assertBool (show (results partial)) $
      Heuristic (NotReady :| []) (ModuleTarget (modId "A")) `elem` results partial
    release w "A"
    waitDone w.index
  complete <- resolved w "B"
  assertBool "complete" (not (isPartial complete))
  assertBool (show (results complete)) $
    any (isExactIn (modId "A")) (results complete)
  where
    isExactIn m = \case
      Exact (DeclTarget m' _) -> m == m'
      _ -> False

memoFinalOnly :: Assertion
memoFinalOnly = do
  w <- newWorld ["A"] abc
  withAsync (runWorkers w.deps 2 w.index) $ \_ -> do
    waitState w.index (modId "B") isReady
    waitState w.index (modId "C") isReady
    _ <- resolved w "B"
    -- Nothing that read A; C is final at once.
    memoKeys w.resolver >>= (@?= [modId "C"])
    release w "A"
    waitDone w.index
  first <- resolved w "B"
  memoKeys w.resolver >>= (@?= [modId "A", modId "C"])
  again <- resolved w "B"
  results again @?= results first
  again.punFields @?= first.punFields
  -- Already in the memo: nothing new.
  Map.keys again.newFinal @?= []
  memoKeys w.resolver >>= (@?= [modId "A", modId "C"])

awaitBumps :: Assertion
awaitBumps = do
  w <-
    newWorld
      ["B"]
      [ ("A", "module A where\nimport D\n"),
        ("B", "module B where\n"),
        ("C", "module C where\n"),
        ("D", "module D where\n")
      ]
  withAsync (runWorkers w.deps 1 w.index) $ \_ -> do
    waitState w.index (modId "B") isParsing
    t0 <- getMonotonicTime
    within (awaitImports w.resolver (Microseconds 20_000) (modId "A"))
    t1 <- getMonotonicTime
    assertBool ("waited " <> show (t1 - t0)) (t1 - t0 >= 0.02)
    atomically (slotState w.index (modId "D")) >>= (@?= Just Pending)
    release w "B"
    waitDone w.index
  readTVarIO w.order >>= (@?= ["A", "B", "D", "C"])

awaitEarly :: Assertion
awaitEarly = do
  w <-
    newWorld
      ["D"]
      [ ("A", "module A where\nimport D\nimport E\n"),
        ("D", "module D where\n"),
        ("E", "module E where\n")
      ]
  withAsync (runWorkers w.deps 2 w.index) $ \_ -> do
    waitState w.index (modId "A") isReady
    -- 10 s: only the imports can end it within the safety net.
    withAsync (awaitImports w.resolver (Microseconds 10_000_000) (modId "A")) $ \a -> do
      waitState w.index (modId "D") isParsing
      poll a >>= assertBool "returned with D parsing" . isNothing
      release w "D"
      within (waitCatch a) >>= either (assertFailure . show) pure
      -- It returned because both imports are done.
      forM_ ["D", "E"] $ \n ->
        atomically (slotState w.index (modId n))
          >>= assertBool (Text.unpack n) . maybe False isReady
    -- All done: at once.
    waitDone w.index
    within (awaitImports w.resolver (Microseconds 10_000_000) (modId "A"))

awaitFailedOrNoSlot :: Assertion
awaitFailedOrNoSlot = do
  w <-
    newWorld
      []
      [ ("A", "module A where\nimport G\nimport N\n"),
        ("G", gone),
        ("N", noFile)
      ]
  within (runWorkers w.deps 1 w.index)
  atomically (slotState w.index (modId "G")) >>= (@?= Just (Failed SourceGone))
  atomically (slotState w.index (modId "N")) >>= (@?= Nothing)
  -- 10 s: only "done" can end it within the safety net.
  within (awaitImports w.resolver (Microseconds 10_000_000) (modId "A"))

waitForModule :: Assertion
waitForModule = do
  let long = Microseconds 10_000_000
  w <-
    newWorld
      ["A", "B"]
      [ ("A", "module A (a) where\na = 1\n"),
        ("B", "module B where\nimport A\nb = a\n")
      ]
  withAsync (runWorkers w.deps 1 w.index) $ \_ -> do
    -- One worker, held on A: B stays Pending.
    waitState w.index (modId "A") isParsing
    withAsync (resolveModule w.resolver (modId "B")) $ \res ->
      withAsync (awaitImports w.resolver long (modId "B")) $ \imps -> do
        let waiting why = do
              poll res >>= assertBool ("resolved, " <> why) . isNothing
              poll imps >>= assertBool ("imports, " <> why) . isNothing
        atomically (slotState w.index (modId "B")) >>= (@?= Just Pending)
        waiting "B pending"
        release w "A"
        -- B is held in its parse now.
        waitState w.index (modId "B") isParsing
        waiting "B parsing"
        release w "B"
        within (waitCatch imps) >>= either (assertFailure . show) pure
        within (waitCatch res) >>= \case
          Right (Just r) -> assertBool "complete" (not (isPartial r))
          other -> assertFailure (take 300 (show (fmap (fmap results) other)))

nothingCases :: Assertion
nothingCases = do
  w <-
    newWorld
      []
      [ ("T", "module T where\nx = = 1\n"),
        ("G", gone),
        ("A", "module A where\n")
      ]
  within (runWorkers w.deps 2 w.index)
  atomically (slotState w.index (modId "G")) >>= (@?= Just (Failed SourceGone))
  forM_ ["T", "G", "Z"] $ \n -> do
    r <- within (resolveModule w.resolver (modId n))
    assertBool (Text.unpack n) (isNothing r)
    within (awaitImports w.resolver (Microseconds 10_000_000) (modId n))
  r <- within (resolveModule w.resolver (modId "A"))
  assertBool "A" (isJust r)

newResolverCase :: Assertion
newResolverCase = do
  w1 <- newWorld [] abc
  w2 <- newWorld [] abc
  within (runWorkers w1.deps 2 w1.index)
  within (runWorkers w2.deps 2 w2.index)
  r1 <- resolved w1 "B"
  memoKeys w1.resolver >>= (@?= [modId "A", modId "C"])
  memoKeys w2.resolver >>= (@?= [])
  r2 <- resolved w2 "B"
  results r2 @?= results r1
  -- Same index, new resolver: an empty memo too.
  r3 <- newResolver w1.modules w1.index
  memoKeys r3 >>= (@?= [])

concurrent :: Assertion
concurrent = do
  let n = 30 :: Int
      name k = "M" <> Text.pack (show k)
      -- Each module imports the one before and M1.
      src k =
        "module "
          <> name k
          <> " (f"
          <> Text.pack (show k)
          <> ", module M1) where\n"
          <> (if k > 1 then "import " <> name (k - 1) <> "\nimport M1\n" else "")
          <> "f"
          <> Text.pack (show k)
          <> " = "
          <> (if k > 1 then "f" <> Text.pack (show (k - 1)) else "1")
          <> "\n"
      mods = [(name k, src k) | k <- [1 .. n]]
      wanted = map name [10, 20, n]
  -- M1 is held until a partial result is seen, so the requests really
  -- run while indexing.
  w <- newWorld ["M1"] mods
  partials <- newTVarIO (0 :: Int)
  finals <- withAsync (runWorkers w.deps 2 w.index) $ \_ ->
    withAsync
      (forConcurrently [1 .. 8 :: Int] $ \_ -> forM wanted (untilComplete w partials))
      $ \requests -> do
        within (atomically (readTVar partials >>= check . (> 0)))
        release w "M1"
        within (wait requests)
  waitDone w.index
  single <- newWorld [] mods
  within (runWorkers single.deps 1 single.index)
  expected <- forM wanted (resolved single)
  mapM_ (\got -> map results got @?= map results expected) finals
  mapM_ (\got -> map (.punFields) got @?= map (.punFields) expected) finals
  -- A complete result may use sets that aren't final yet (from a
  -- module done later whose names it didn't need). So the memo may have
  -- fewer keys, never more. One more round fills it.
  keys1 <- memoKeys single.resolver
  keys <- memoKeys w.resolver
  assertBool (show keys) (all (`elem` keys1) keys)
  mapM_ (resolved w) wanted
  memoKeys w.resolver >>= (@?= keys1)
  where
    -- Partial: try again once one more module is done. No busy loop:
    -- it would starve the workers.
    untilComplete w partials m = do
      before <- doneCount <$> atomically (readProgress w.index)
      r <- resolved w m
      if isPartial r
        then do
          atomically (modifyTVar' partials (+ 1))
          within . atomically $
            readProgress w.index >>= check . (> before) . doneCount
          untilComplete w partials m
        else pure r
    doneCount p = p.ready + p.failed

moduleStateCase :: Assertion
moduleStateCase = do
  let tag = \case
        ModuleParsed _ -> "parsed"
        ModuleNotParsed -> "not parsed"
        ModuleNotReady -> "not ready" :: Text
      expected = \case
        Ready (IndexedParsed _) -> "parsed"
        Ready _ -> "not parsed"
        Failed _ -> "not parsed"
        Pending -> "not ready"
        Parsing -> "not ready"
  mapM_
    (\(i, st) -> (show i, tag (moduleState warmUpStates i)) @?= (show i, expected st))
    (Map.toList warmUpStates)
  tag (moduleState warmUpStates (modId "NoSlot")) @?= "not parsed"
  -- The text comes through.
  case [p | i <- resolveWarmUpModules, ModuleParsed p <- [moduleState warmUpStates i]] of
    p : _ -> assertBool "text" (not (Text.null p.text))
    [] -> assertFailure "nothing parsed"

warmUpCovers :: Assertion
warmUpCovers = do
  let stateTag = \case
        Pending -> "pending"
        Parsing -> "parsing"
        Ready (IndexedParsed _) -> "parsed"
        Ready (IndexedTextOnly _) -> "text only"
        Ready (IndexedNotText _) -> "not text"
        Ready (IndexedTooLarge _) -> "too large"
        Failed _ -> "failed" :: Text
  Set.toList (Set.fromList (map stateTag (Map.elems warmUpStates)))
    @?= sort
      ["pending", "parsing", "parsed", "text only", "not text", "too large", "failed"]
  -- Through the slot states: the same results as the core warm-up.
  let input = resolveWarmUpInput
      viaSlots = input {state = moduleState warmUpStates}
      summary inp i = case Core.resolveModule inp i of
        Nothing -> Nothing
        Just r ->
          Just (results r, IntMap.elems r.punFields, r.notReady, Map.keys r.newFinal)
  mapM_
    (\i -> (show i, summary viaSlots i) @?= (show i, summary input i))
    resolveWarmUpModules
  -- homeImports: with and without the implicit Prelude, an external
  -- import, a repeat.
  let parsed =
        [(i, p.parsed) | i <- resolveWarmUpModules, ModuleParsed p <- [input.state i]]
      targets i p =
        [mapImport input.graph i imp.package imp.name.value | imp <- p.imports]
      homeMapped i p = [() | HomeModule _ _ <- targets i p]
      external i p = [() | ExternalImport _ <- targets i p]
      implicit p = "ImplicitPrelude" `elem` p.extensions.enabled
  assertBool "a repeat" $
    or [length (homeMapped i p) > length (homeImports input.graph i p) | (i, p) <- parsed]
  assertBool "an external import" (or [not (null (external i p)) | (i, p) <- parsed])
  Set.toList (Set.fromList [implicit p | (_, p) <- parsed]) @?= [False, True]

findRefsCase :: Assertion
findRefsCase = do
  w <-
    newWorld
      ["C"]
      [ ("A", "module A (a) where\na = 1\n"),
        ("B", "module B where\nimport A\nb = a\n"),
        ("C", "module C where\nimport A\nc = a\n")
      ]
  let find = within (findReferences w.resolver show (modId "A") (Named DVal "a"))
  withAsync (runWorkers w.deps 2 w.index) $ \_ -> do
    waitState w.index (modId "A") isReady
    waitState w.index (modId "B") isReady
    during <- find
    during.partial @?= True
    -- A's export and equation, B's use.
    during.total @?= 3
    during.resolved @?= 2
    memoKeys w.resolver >>= (@?= [modId "A"])
    release w "C"
    waitDone w.index
  after <- find
  after.partial @?= False
  after.total @?= 4
  -- Already in the memo: nothing new.
  Map.keys after.newFinal @?= []

-- * A world: an index of one library, with real parses

data World = World
  { modules :: ModuleIndex,
    index :: Index,
    resolver :: Resolver,
    deps :: IndexDeps,
    -- | Modules parsed, in order.
    order :: TVar [Text],
    gates :: Map.Map Text (MVar ())
  }

-- | A file read gives 'ReadGone'.
gone :: Text
gone = "<gone>"

-- | The module is listed, but has no file.
noFile :: Text
noFile = "<no file>"

-- | One library in @src@, every module exposed: (module name, text).
-- The parses of the modules in the first list wait until released.
newWorld :: [Text] -> [(Text, Text)] -> IO World
newWorld held mods = do
  order <- newTVarIO []
  gates <- Map.fromList <$> mapM (\t -> (t,) <$> newEmptyMVar) held
  let sources = Map.fromList mods
      byText = Map.fromList [(t, n) | (n, t) <- mods]
      nameOf p = Text.dropEnd 3 (Text.drop 4 (relPathText p))
      idx = buildModuleIndex (libProject (map fst mods)) fl
      -- Listed, with no file: no slot.
      fl = mkFileList FromDisk 0 [(libFile n, EntryFile) | (n, t) <- mods, t /= noFile]
      deps =
        IndexDeps
          { readModule = \p -> case Map.lookup (nameOf p) sources of
              Just t | t /= gone -> pure (Right (TextContent t))
              _ -> pure (Left ReadGone),
            parse = \s t -> do
              let n = Map.findWithDefault "?" t byText
              atomically (modifyTVar' order (<> [n]))
              mapM_ readMVar (Map.lookup n gates)
              -- Not the 2 s limit: on a loaded machine a module could be
              -- text only (too slow), and the results would change.
              parseWithin 60_000_000 s t,
            logger = newLogger LevelInfo (\_ -> pure ())
          }
  index <- newIndex idx
  resolver <- newResolver idx index
  pure
    World
      { modules = idx,
        index = index,
        resolver = resolver,
        deps = deps,
        order = order,
        gates = gates
      }

-- | Let a held parse finish.
release :: World -> Text -> IO ()
release w n = case Map.lookup n w.gates of
  Just gate -> putMVar gate ()
  Nothing -> assertFailure ("not held: " <> show n)

-- | A module's resolution; it must have one.
resolved :: World -> Text -> IO ModuleResolution
resolved w n =
  within (resolveModule w.resolver (modId n)) >>= \case
    Just r -> pure r
    Nothing -> assertFailure ("no resolution for " <> Text.unpack n)

-- | The free names' results, by token.
results :: ModuleResolution -> [Resolved]
results r = IntMap.elems r.names
