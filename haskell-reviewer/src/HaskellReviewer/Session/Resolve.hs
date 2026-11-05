{-# LANGUAGE StrictData #-}

-- | Resolution in the session: one 'Resolver' per snapshot, over its
-- index, with a memo of final export sets (SPEC §11.4.5). See Note
-- [Session resolution].
module HaskellReviewer.Session.Resolve
  ( -- * Types
    Resolver,

    -- * Building
    newResolver,

    -- * Requests
    resolveModule,
    awaitImports,
    importWait,

    -- * Warm-up
    warmUpResolve,

    -- * Internal, for tests
    moduleState,
    memoKeys,
    warmUpStates,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    registerDelay,
  )
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HaskellReviewer.Core.Entities (ExportSet, forceExportSets)
import HaskellReviewer.Core.FileContent (FileSize (..), TooLargeInfo (..))
import HaskellReviewer.Core.ModuleGraph (ModuleGraph, buildModuleGraph, forceModuleGraph)
import HaskellReviewer.Core.ModuleModel (TextOnlyReason (..), textOnlyModule)
import HaskellReviewer.Core.Modules (ModuleId (..), ModuleIndex)
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments)
import HaskellReviewer.Core.Resolve
  ( ExportSets (..),
    ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
    exportSets,
    forceModuleResolution,
    homeImports,
  )
import HaskellReviewer.Core.Resolve qualified as Core (resolveModule)
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Session.Index
  ( Index,
    IndexFailure (..),
    Indexed (..),
    ParsedSource (..),
    SlotState (..),
    awaitModule,
    readStates,
    requestModule,
    slotState,
  )

{- Note [Session resolution]

Resolution runs on request, one module at a time (D-252, D-262). Each
snapshot has one 'Resolver': its index, the module graph, and a memo of
final export sets (Note [Export set walk] in
"HaskellReviewer.Core.Resolve"). A Reload makes a new snapshot, so a
new Resolver with an empty memo. Within a snapshot nothing is ever
dropped from the memo: slots only move forward, so a final set never
goes stale.

'resolveModule' waits for the module, then reads every slot and the
memo in one transaction, so they agree. It resolves with no lock held.
New final sets go in the memo with a left-biased union: two requests
may build the same set at once; the first one in wins, the other copy
is dropped. Both are equal anyway.

A result that read a module not ready yet is partial ('isPartial'):
the client asks again later. To make that rarer, 'awaitImports' first
moves the module's direct imports of ours to the front of the queue and
waits for them, up to 'importWait' (G2).

Q-RTS-BLACKHOLE: request threads share the Resolver. The graph is
forced in 'newResolver', before the snapshot is filled. Sets are forced
before they go in the memo, and the memo is a strict map, so the TVar
holds no thunk. The result is forced before it's returned. The shared
values of resolution are built by 'warmUpResolve', in the listing
thread, before the fill.
-}

-- * Types

-- | Resolution state of one snapshot. See Note [Session resolution].
data Resolver = Resolver
  { index :: Index,
    -- | Forced in 'newResolver'.
    graph :: ModuleGraph,
    -- | Final export sets only, forced.
    memo :: TVar (Map ModuleId ExportSet)
  }

-- * Building

-- | A resolver with an empty memo. The graph is built and forced here.
newResolver :: ModuleIndex -> Index -> IO Resolver
newResolver idx index = do
  graph <- evaluate (buildModuleGraph idx)
  _ <- evaluate (forceModuleGraph graph)
  memo <- newTVarIO Map.empty
  pure Resolver {index = index, graph = graph, memo = memo}

-- * Requests

-- | Resolve a module's free names: wait for the module, resolve, keep
-- the new final sets. 'Nothing' if it has no slot, or didn't parse, or
-- failed. The result is fully forced. See Note [Session resolution].
resolveModule :: Resolver -> ModuleId -> IO (Maybe ModuleResolution)
resolveModule r i =
  awaitModule r.index i >>= \case
    Just (Right (IndexedParsed _)) -> do
      (states, memo) <- atomically $ (,) <$> readStates r.index <*> readTVar r.memo
      let input = ResolveInput {graph = r.graph, state = moduleState states, memo = memo}
      case Core.resolveModule input i of
        Nothing -> pure Nothing
        Just res -> do
          _ <- evaluate (forceModuleResolution res)
          -- What's in the memo already wins.
          unless (Map.null res.newFinal) $
            atomically (modifyTVar' r.memo (`Map.union` res.newFinal))
          pure (Just res)
    _ -> pure Nothing

-- | Wait for the module, move its direct imports of ours to the front
-- of the queue, and wait until they are done or the time is up (G2).
-- The time starts once the module itself is done. Imports in CPP
-- branches that are skipped are waited for too. Returns after the wait
-- for the module if it didn't parse, at once if it has no slot.
awaitImports :: Resolver -> Microseconds -> ModuleId -> IO ()
awaitImports r (Microseconds d) i =
  awaitModule r.index i >>= \case
    Just (Right (IndexedParsed s)) -> do
      let deps = homeImports r.graph i s.parsed
          allDone = mapM (isDone r.index) deps >>= check . and
      ready <- atomically $ do
        mapM_ (requestModule r.index) deps
        (True <$ allDone) <|> pure False
      unless ready $ do
        timer <- registerDelay d
        atomically (allDone <|> (readTVar timer >>= check))
    _ -> pure ()

-- | How long a request waits for the imports: 100 ms (G2).
importWait :: Microseconds
importWait = Microseconds 100_000

-- * Warm-up

-- | Resolve the warm-up project once, through 'moduleState', so the
-- shared values are built before request threads use them
-- (Q-RTS-BLACKHOLE). Every result and export set is forced. Run in the
-- listing thread, with the parser's warm-up.
warmUpResolve :: IO ()
warmUpResolve = do
  let states = warmUpStates
      input = resolveWarmUpInput {state = moduleState states}
      sets = exportSets input resolveWarmUpModules
  _ <- evaluate (forceModuleGraph input.graph)
  -- Every branch of moduleState, and no slot.
  mapM_ (evaluate . stateTag . moduleState states) (noSlot : Map.keys states)
  _ <- evaluate (forceExportSets sets.sets)
  _ <- evaluate (forceExportSets sets.newFinal)
  mapM_ (warmUpOne input) resolveWarmUpModules
  where
    stateTag = \case
      ModuleParsed p -> p.text `seq` ()
      ModuleNotParsed -> ()
      ModuleNotReady -> ()

-- * Internal, for tests

-- | A slot's state, as resolution sees it. No slot (generated, no
-- source): not parsed.
moduleState :: Map ModuleId SlotState -> ModuleId -> ModuleState
moduleState states i = case Map.lookup i states of
  Just (Ready (IndexedParsed s)) ->
    ModuleParsed ParsedInput {text = s.text, parsed = s.parsed}
  Just (Ready _) -> ModuleNotParsed
  Just (Failed _) -> ModuleNotParsed
  Just Pending -> ModuleNotReady
  Just Parsing -> ModuleNotReady
  Nothing -> ModuleNotParsed

-- | The modules in the memo.
memoKeys :: Resolver -> IO [ModuleId]
memoKeys r = Map.keys <$> readTVarIO r.memo

-- | The warm-up project as slot states, with one more module for each
-- state it doesn't have. Resolving through them gives the same results
-- as 'resolveWarmUpInput'.
warmUpStates :: Map ModuleId SlotState
warmUpStates =
  Map.fromList $
    [(i, slotOf (resolveWarmUpInput.state i)) | i <- resolveWarmUpModules]
      <> [ (extra "Parsing", Parsing),
           (extra "Failed", Failed SourceGone),
           (extra "NotText", Ready (IndexedNotText (FileSize 1))),
           ( extra "TooLarge",
             Ready (IndexedTooLarge TooLargeInfo {size = FileSize 2, limit = FileSize 1})
           )
         ]
  where
    slotOf = \case
      ModuleParsed p ->
        Ready (IndexedParsed ParsedSource {text = p.text, parsed = p.parsed})
      ModuleNotParsed -> Ready (IndexedTextOnly (textOnlyModule TooSlow []))
      ModuleNotReady -> Pending
    extra n = LooseModule (pathOf ["warm-up", n <> ".hs"])

-- * Internal

-- | Ready, failed or no slot.
isDone :: Index -> ModuleId -> STM Bool
isDone index i =
  slotState index i >>= \case
    Just Pending -> pure False
    Just Parsing -> pure False
    _ -> pure True

-- | One warm-up module: its imports of ours, and its resolution.
warmUpOne :: ResolveInput -> ModuleId -> IO ()
warmUpOne input i = do
  case input.state i of
    ModuleParsed p -> () <$ evaluate (length (homeImports input.graph i p.parsed))
    _ -> pure ()
  evaluate (maybe () forceModuleResolution (Core.resolveModule input i))

-- | A module with no slot.
noSlot :: ModuleId
noSlot = LooseModule (pathOf ["warm-up", "NoSlot.hs"])

pathOf :: [Text] -> RelPath
pathOf segs = fromMaybe (error "warm-up: bad path") (relPathFromSegments segs)
