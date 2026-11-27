-- | A steady run (plan §2.2, D-303): after indexing, every module page
-- once, back to back, then every source page; then module pages with
-- idle gaps, then the same source pages with the same gaps; then
-- search, then find references, then in diff mode the diffs (D-358),
-- then text search (D-308). With @+RTS -S@, for the GC pauses. The
-- first run also does the fallback census, in a pass of its own before
-- the timed one. In diff mode (D-357) each phase does head, then base.
module Bench.Steady
  ( -- * Types
    SteadyPlan (..),
    DiffPlan (..),
    CensusPass (..),
    TimedPass (..),
    PageTime (..),
    Route (..),
    TextTime (..),
    DiffTime (..),
    SteadyRun (..),

    -- * Running
    runSteady,
    timeDiff,
    getDiffFiles,
  )
where

import Bench.DiffAnswer (DiffKind, parseDiffAnswer)
import Bench.DiffFiles
  ( Candidate (..),
    ChangedFile (..),
    DiffFilesAnswer,
    parseDiffFiles,
  )
import Bench.Fallbacks (Category (..), Census, Hit (..), Slot (..), classifyPage)
import Bench.GcLog (GcLog, GcLogError, PhaseMark (..))
import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.IndexLog (IndexDone)
import Bench.Machine (LoadAvg, getLoadAvg)
import Bench.Mode (Side, sideText)
import Bench.ModuleList
  ( IdleStep (..),
    ModuleItem (..),
    Queued (..),
    SideModules (..),
    idlePlan,
    isSlot,
    queueOrder,
  )
import Bench.Queries
  ( RefsQuery,
    TextAnswer (..),
    parseTextAnswer,
    queries,
    textLimitBox,
    textLimitPage,
    textQueries,
  )
import Bench.Refs (RefsTime)
import Bench.RefsDuring (timeRefs)
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
    elapsedUs,
    failWith,
    getApi,
    procMemory,
    requireRts,
    withSession,
  )
import Bench.SearchDuring (QueryTime, timeQuery)
import Bench.Url (diffFilesPath, diffPath, modulePath, sourcePath, textSearchPath)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, replicateM)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty)
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
    -- | Pages with idle gaps (@--nav-idle@), per side.
    idlePages :: Int,
    -- | Rounds of every query after indexing.
    searchRounds :: Int,
    -- | Refs queries, and rounds of them after the search.
    refs :: [RefsQuery],
    refsRounds :: Int,
    -- | Rounds of every text query (limit 8) after the refs. With 0,
    -- no text search at all.
    textRounds :: Int,
    -- | Picks the idle pages and gaps: the run index.
    seed :: Int,
    -- | Diff mode only.
    diff :: Maybe DiffPlan
  }

-- | The @diff@ phase (D-358): @api\/diff-files@ a few times, then
-- rounds of @api\/diff@ over the picked files.
data DiffPlan = DiffPlan
  { filesRounds :: Int,
    files :: [Candidate],
    rounds :: Int
  }

-- | Decode every page and bucket the fallbacks? A pass of its own, so
-- the decoding doesn't slow down the timed pages.
data CensusPass = CensusPass | NoCensusPass
  deriving (Eq, Show)

-- | Every page once, back to back, timed? @--census-only@ skips it.
data TimedPass = TimedPass | NoTimedPass
  deriving (Eq, Show)

-- | Which page of a module.
data Route = ModuleRoute | SourceRoute
  deriving (Eq, Show)

-- | One module or source page.
data PageTime = PageTime
  { side :: Side,
    route :: Route,
    -- | The module's ref, also for a source page.
    ref :: Text,
    -- | From request to the last byte.
    us :: Integer,
    wireBytes :: Int
  }

-- | One text search request.
data TextTime = TextTime
  { side :: Side,
    query :: Text,
    -- | From request to the last byte.
    us :: Integer,
    stopped :: Bool,
    total :: Int,
    files :: Int,
    sent :: Int,
    wireBytes :: Int
  }

-- | One @api\/diff@ request.
data DiffTime = DiffTime
  { path :: Text,
    -- | From request to the last byte.
    us :: Integer,
    wireBytes :: Int,
    -- | Too-slow (D-331) is the one to look at.
    kind :: DiffKind
  }

-- | Everything one steady run measures.
data SteadyRun = SteadyRun
  { doneUs :: Integer,
    done :: IndexDone,
    -- | Back to back, modules we parse only (the others answer from
    -- the module index, no slot to wait for).
    pages :: [PageTime],
    -- | The source pages of the same modules, back to back.
    sources :: [PageTime],
    idlePages :: [PageTime],
    -- | The source pages of 'idlePages', with the same gaps.
    idleSources :: [PageTime],
    searchTimes :: [QueryTime],
    refsTimes :: [RefsTime],
    -- | The first text search of each side: it loads the store. Limit
    -- 8.
    textFirst :: [TextTime],
    -- | Text searches after it, limit 8.
    textTimes :: [TextTime],
    -- | One round with the results page's limit (1000).
    textPages :: [TextTime],
    -- | @api\/diff-files@ request times; diff mode only.
    diffFilesTimes :: [Integer],
    -- | @api\/diff@ requests; diff mode only.
    diffTimes :: [DiffTime],
    -- | When each phase started, the first at the done line. Named like
    -- the JSON keys.
    phases :: [PhaseMark],
    -- | Only if the plan asked for it.
    census :: Maybe Census,
    -- | See 'Ended'.
    gc :: Maybe (Either GcLogError GcLog),
    rts :: RtsStats,
    -- | Linux only. RSS 1 s after the last request, the peak RSS right
    -- after the done line, and the peak at the end.
    idleRssBytes :: Maybe Integer,
    hwmAtDoneBytes :: Maybe Integer,
    hwmBytes :: Maybe Integer,
    loadAvg :: Maybe LoadAvg
  }

-- * Running

-- | One run over the listed modules of each side (tree order).
runSteady ::
  SessionConfig -> NonEmpty SideModules -> SteadyPlan -> IO (Either RunError SteadyRun)
runSteady cfg trees plan = (>>= finish) <$> withSession cfg (steps trees plan)
  where
    finish (build, ended) = build ended <$> requireRts ended

steps ::
  NonEmpty SideModules -> SteadyPlan -> Session -> IO (Ended -> RtsStats -> SteadyRun)
steps trees plan s = do
  doneAt <- awaitDone s
  atDone <- procMemory s
  let mark name = (\t -> PhaseMark {name = name, startUs = t}) <$> elapsedUs s
      -- From the done line: the phases cover all of "after indexing".
      m0 = PhaseMark {name = "census", startUs = doneAt.atUs}

  census <- case plan.census of
    NoCensusPass -> pure Nothing
    CensusPass -> do
      hits <- forM (NE.toList trees) $ \t -> forM t.items $ \m ->
        getApi s Gzip (modulePath t.side m.ref) >>= classify m
      pure (Just (Map.fromListWith (flip (<>)) [(c, [h]) | Just (c, h) <- concat hits]))

  let sides = NE.map (.side) trees
      perSide :: (Side -> IO [a]) -> IO [a]
      perSide f = concat <$> mapM f (NE.toList sides)
      sideSlots = [queueOrder t.side t.items | t <- NE.toList trees]
      timed f = case plan.timed of
        NoTimedPass -> pure []
        TimedPass -> mapM f (concat sideSlots)
      -- The same seed on each side: the sides' lists differ anyway.
      idlePass f = fmap concat $ forM sideSlots $ \slots ->
        forM (idlePlan plan.seed plan.idlePages (length slots)) $ \step -> do
          threadDelay (step.gapMs * 1000)
          f (slots !! step.index)
  m1 <- mark "pages"
  pages <- timed (timePage s)
  m2 <- mark "sources"
  sources <- timed (timeSource s)
  m3 <- mark "pages_idle"
  idle <- idlePass (timePage s)
  m4 <- mark "sources_idle"
  idleSources <- idlePass (timeSource s)

  m5 <- mark "search"
  searched <- perSide $ \side ->
    concatMap NE.toList <$> replicateM plan.searchRounds (mapM (timeQuery s side) queries)
  m6 <- mark "refs"
  refsTimes <- perSide $ \side ->
    concat <$> replicateM plan.refsRounds (mapM (timeRefs s side) plan.refs)
  -- No mark in browse mode: its phases stay as before.
  (mDiff, diffFilesTimes, diffTimes) <- case plan.diff of
    Nothing -> pure ([], [], [])
    Just d -> do
      m <- mark "diff"
      fs <- replicateM d.filesRounds ((.us) . fst <$> getDiffFiles s)
      ds <- concat <$> replicateM d.rounds (mapM (timeDiff s . (.file.path)) d.files)
      pure ([m], fs, ds)
  -- Last: the store it loads would change the memory of the phases
  -- before.
  m7 <- mark "text"
  (textFirst, textRounds, textPages) <-
    if plan.textRounds <= 0
      then pure ([], [], [])
      else fmap unzip3 $ forM (NE.toList sides) $ \side -> do
        first <- timeText s side textLimitBox (NE.head textQueries)
        rounds <-
          replicateM plan.textRounds (mapM (timeText s side textLimitBox) textQueries)
        page <- mapM (timeText s side textLimitPage) textQueries
        pure (first, concatMap NE.toList rounds, NE.toList page)
  m8 <- mark "end_idle"

  -- Over the RTS idle-GC delay (0.3 s), so this is the idle size.
  threadDelay 1_000_000
  mem <- procMemory s
  -- Till the run ends: the stop, and the RTS's last GC.
  m9 <- mark "stop"
  load <- getLoadAvg
  pure $ \ended rts ->
    SteadyRun
      { doneUs = doneAt.atUs,
        done = doneAt.line,
        pages = pages,
        sources = sources,
        idlePages = idle,
        idleSources = idleSources,
        searchTimes = searched,
        refsTimes = refsTimes,
        textFirst = textFirst,
        textTimes = concat textRounds,
        textPages = concat textPages,
        diffFilesTimes = diffFilesTimes,
        diffTimes = diffTimes,
        phases = [m0, m1, m2, m3, m4, m5, m6] <> mDiff <> [m7, m8, m9],
        census = census,
        gc = ended.gc,
        rts = rts,
        idleRssBytes = mem.rssBytes,
        hwmAtDoneBytes = atDone.hwmBytes,
        hwmBytes = mem.hwmBytes,
        loadAvg = load
      }

-- | Ask for one module page and time it. Any status is fine here: the
-- census looks at statuses.
timePage :: Session -> Queued -> IO PageTime
timePage s m = do
  t <- getApi s Gzip (modulePath m.side m.ref)
  pure
    PageTime
      { side = m.side,
        route = ModuleRoute,
        ref = m.ref,
        us = t.us,
        wireBytes = t.response.wireBytes
      }

-- | Ask for one module's source page and time it. Fails the run unless
-- it's a 200: every parsed module's file is in the file list.
timeSource :: Session -> Queued -> IO PageTime
timeSource s m = do
  t <- getApi s Gzip (sourcePath m.side m.path)
  if t.response.status /= 200
    then
      failWith
        ( "source "
            <> Text.unpack (sideText m.side)
            <> " "
            <> m.path
            <> ": expected 200, got "
            <> show t.response.status
        )
    else
      pure
        PageTime
          { side = m.side,
            route = SourceRoute,
            ref = m.ref,
            us = t.us,
            wireBytes = t.response.wireBytes
          }

-- | @api\/diff-files@, timed and read. Fails the run unless it's a 200
-- with a diff-files answer.
getDiffFiles :: Session -> IO (Timed, DiffFilesAnswer)
getDiffFiles s = do
  t <- getApi s Gzip diffFilesPath
  if t.response.status /= 200
    then failWith ("diff-files: expected 200, got " <> show t.response.status)
    else case parseDiffFiles t.response.body of
      Left e -> failWith ("diff-files: " <> show e)
      Right a -> pure (t, a)

-- | One file's diff, timed. Fails the run unless it's a 200 with a
-- diff answer. Any kind is fine here: the pick decides, the report
-- warns about too-slow.
timeDiff :: Session -> Text -> IO DiffTime
timeDiff s p = do
  t <- getApi s Gzip (diffPath p)
  let what = "diff " <> Text.unpack p
  if t.response.status /= 200
    then failWith (what <> ": expected 200, got " <> show t.response.status)
    else case parseDiffAnswer t.response.body of
      Left e -> failWith (what <> ": " <> show e)
      Right k ->
        pure
          DiffTime
            { path = p,
              us = t.us,
              wireBytes = t.response.wireBytes,
              kind = k
            }

-- | One text search, timed. Fails the run unless it's a 200 with a text
-- search answer.
timeText :: Session -> Side -> Int -> Text -> IO TextTime
timeText s side limit q = do
  t <- getApi s Gzip (textSearchPath side q limit)
  let what = "text search " <> show q
  if t.response.status /= 200
    then failWith (what <> ": expected 200, got " <> show t.response.status)
    else case parseTextAnswer t.response.body of
      Left e -> failWith (what <> ": " <> show e)
      Right a ->
        pure
          TextTime
            { side = side,
              query = q,
              us = t.us,
              stopped = a.stopped,
              total = a.total,
              files = a.files,
              sent = a.sent,
              wireBytes = t.response.wireBytes
            }

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
