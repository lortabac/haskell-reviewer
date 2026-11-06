-- | A steady run (plan §2.2): after indexing, every module page once,
-- back to back; then pages with idle gaps; then search. With @+RTS -S@,
-- for the GC pauses. The first run also does the fallback census, in a
-- pass of its own before the timed one.
module Bench.Steady
  ( -- * Types
    SteadyPlan (..),
    CensusPass (..),
    TimedPass (..),
    PageTime (..),
    SteadyRun (..),

    -- * Running
    runSteady,
  )
where

import Bench.Fallbacks (Category (..), Census, Hit (..), Slot (..), classifyPage)
import Bench.GcLog (GcLog, GcLogError)
import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.IndexLog (IndexDone)
import Bench.Machine (LoadAvg, getLoadAvg)
import Bench.ModuleList (IdleStep (..), ModuleItem (..), idlePlan, isSlot)
import Bench.Queries (queries)
import Bench.RtsStats (RtsStats)
import Bench.Run
  ( DoneAt (..),
    Ended (..),
    ProcMemory (..),
    RunError,
    Session,
    SessionConfig,
    Timed (..),
    awaitDone,
    failWith,
    getApi,
    procMemory,
    requireRts,
    withSession,
  )
import Bench.SearchDuring (QueryTime, timeQuery)
import Bench.Url (modulePath)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, replicateM)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)

-- * Types

-- | What a steady run does.
data SteadyPlan = SteadyPlan
  { census :: CensusPass,
    timed :: TimedPass,
    -- | Pages with idle gaps (@--nav-idle@).
    idlePages :: Int,
    -- | Rounds of every query after indexing.
    searchRounds :: Int,
    -- | Picks the idle pages and gaps: the run index.
    seed :: Int
  }

-- | Decode every page and bucket the fallbacks? A pass of its own, so
-- the decoding doesn't slow down the timed pages.
data CensusPass = CensusPass | NoCensusPass
  deriving (Eq, Show)

-- | Every page once, back to back, timed? @--census-only@ skips it.
data TimedPass = TimedPass | NoTimedPass
  deriving (Eq, Show)

-- | One module page.
data PageTime = PageTime
  { ref :: Text,
    -- | From request to the last byte.
    us :: Integer,
    wireBytes :: Int
  }

-- | Everything one steady run measures.
data SteadyRun = SteadyRun
  { doneUs :: Integer,
    done :: IndexDone,
    -- | Back to back, modules we parse only (the others answer from
    -- the module index, no slot to wait for).
    pages :: [PageTime],
    idlePages :: [PageTime],
    searchTimes :: [QueryTime],
    -- | Only if the plan asked for it.
    census :: Maybe Census,
    -- | See 'Ended'.
    gc :: Maybe (Either GcLogError GcLog),
    rts :: RtsStats,
    -- | At the end; Linux only.
    rssBytes :: Maybe Integer,
    hwmBytes :: Maybe Integer,
    loadAvg :: Maybe LoadAvg
  }

-- * Running

-- | One run over the listed modules (tree order).
runSteady :: SessionConfig -> [ModuleItem] -> SteadyPlan -> IO (Either RunError SteadyRun)
runSteady cfg items plan = (>>= finish) <$> withSession cfg (steps items plan)
  where
    finish (build, ended) = build ended <$> requireRts ended

steps :: [ModuleItem] -> SteadyPlan -> Session -> IO (Ended -> RtsStats -> SteadyRun)
steps items plan s = do
  doneAt <- awaitDone s

  census <- case plan.census of
    NoCensusPass -> pure Nothing
    CensusPass -> do
      hits <- forM items $ \m -> getApi s Gzip (modulePath m.ref) >>= classify m
      pure (Just (Map.fromListWith (flip (<>)) [(c, [h]) | Just (c, h) <- hits]))

  let slots = filter isSlot items
  pages <- case plan.timed of
    NoTimedPass -> pure []
    TimedPass -> mapM (timePage s) slots

  idle <- forM (idlePlan plan.seed plan.idlePages (length slots)) $ \step -> do
    threadDelay (step.gapMs * 1000)
    timePage s (slots !! step.index)

  searched <-
    concatMap NE.toList <$> replicateM plan.searchRounds (mapM (timeQuery s) queries)

  mem <- procMemory s
  load <- getLoadAvg
  pure $ \ended rts ->
    SteadyRun
      { doneUs = doneAt.atUs,
        done = doneAt.line,
        pages = pages,
        idlePages = idle,
        searchTimes = searched,
        census = census,
        gc = ended.gc,
        rts = rts,
        rssBytes = mem.rssBytes,
        hwmBytes = mem.hwmBytes,
        loadAvg = load
      }

-- | Ask for one module page and time it. Any status is fine here: the
-- census looks at statuses.
timePage :: Session -> ModuleItem -> IO PageTime
timePage s m = do
  t <- getApi s Gzip (modulePath m.ref)
  pure PageTime {ref = m.ref, us = t.us, wireBytes = t.response.wireBytes}

-- | The fallback category of one module page, if any. A 200 must be a
-- module page; anything else is 'Failed', with the body as the detail.
classify :: ModuleItem -> Timed -> IO (Maybe (Category, Hit))
classify m t
  | t.response.status /= 200 =
      pure (Just (Failed, Hit {ref = m.ref, detail = statusDetail}))
  | otherwise = case cborToJson (LBS.fromStrict t.response.body) of
      Left e -> failWith (what <> Text.unpack (renderCborJsonError e))
      Right json -> case Aeson.eitherDecode json of
        Left e -> failWith (what <> e)
        Right v -> either (failWith . (what <>) . show) pure (classifyPage m.ref slot v)
  where
    what = Text.unpack m.ref <> ": "
    slot = if isSlot m then HasSlot else NoSlot
    statusDetail =
      "HTTP "
        <> Text.pack (show t.response.status)
        <> ": "
        <> Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode t.response.body
