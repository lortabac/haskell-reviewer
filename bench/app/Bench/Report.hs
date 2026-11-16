-- | Builds the benchmark's two outputs: the stdout table, and the JSON
-- report (schema 4).
module Bench.Report
  ( -- * Types
    Report (..),
    Settings (..),
    ToolInfo (..),
    ProjectReport (..),
    FallbackCheck (..),
    NavPattern (..),

    -- * Targets
    projectVerdicts,

    -- * Rendering
    renderTable,
    encodeReportJson,
  )
where

import Bench.Fallbacks
  ( Category,
    Census,
    Diff (..),
    Expected (..),
    FallbackVerdict (..),
    Hit (..),
    Lookup (..),
    NotChecked (..),
    Strictness,
    categoryText,
    countCensus,
    deterministic,
    failsBench,
    renderDiff,
  )
import Bench.GcLog
  ( GcRun (..),
    GcSummary (..),
    PhaseGc (..),
    phaseMajors,
    renderGcLogError,
    summarizeGc,
  )
import Bench.IndexLog (IndexDone (..))
import Bench.Machine (LoadAvg (..), MachineInfo (..))
import Bench.ModuleList (OnDemandStatus (..), Sample (..), sampleKindText)
import Bench.Projects (Project (..), Role (..))
import Bench.Queries (RefsQuery (..), textLimitBox, textLimitPage)
import Bench.Refs
  ( RefsSummary (..),
    RefsTime (..),
    byQuery,
    slowdownPercent,
    startedBefore,
  )
import Bench.RefsDuring (RefsDuringRun (..))
import Bench.RtsStats (RtsStats (..))
import Bench.SearchDuring (QueryTime (..), SearchDuringRun (..))
import Bench.Startup
  ( FileList (..),
    FileSource (..),
    OnDemand (..),
    StartupRun (..),
    UiParts (..),
  )
import Bench.Stats (Latency (..), Summary (..), latency, summarize)
import Bench.Steady (PageTime (..), Route (..), SteadyRun (..), TextTime (..))
import Bench.Targets
  ( Target (..),
    Verdict (..),
    memoryBudgetBytes,
    navigation,
    navigationIdle,
    navigationSource,
    navigationSourceIdle,
    onDemandModule,
    refsAfterP95,
    renderVerdict,
    searchAfterMedian,
    searchAfterP95,
    statText,
    textSearchAfterMedian,
    textSearchAfterP95,
    uiUsable,
    urlAndAccepting,
    verdict,
  )
import Data.Aeson ((.=))
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Key qualified as Aeson.Key
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.Printf (printf)

-- * Types

-- | Everything one benchmark invocation produces.
data Report = Report
  { started :: UTCTime,
    label :: Maybe Text,
    settings :: Settings,
    tool :: ToolInfo,
    machine :: MachineInfo,
    loadAtStart :: Maybe LoadAvg,
    -- | Only the projects that ran to completion. A project that failed
    -- (bad pin, a run that errored) is reported on stderr and left out
    -- here; see "Main".
    projects :: [ProjectReport]
  }

-- | The CLI settings that change the numbers.
data Settings = Settings
  { runs :: Int,
    warmup :: Int,
    steadyRuns :: Int,
    searchRuns :: Int,
    refsRuns :: Int,
    navIdle :: Int,
    -- | @--rts@, split on spaces.
    rtsOpts :: [String],
    censusOnly :: Bool,
    -- | @--fallbacks@, for messages.
    fallbacksFile :: FilePath
  }

-- | What we know about the binary under test.
data ToolInfo = ToolInfo
  { exe :: FilePath,
    -- | First line of @--version@.
    version :: Text,
    gitSha :: Maybe Text,
    gitDirty :: Maybe Bool,
    ghc :: Maybe Text,
    rtsWay :: Maybe Text
  }

-- | One project's runs. Warm-up runs are already dropped. Any list may
-- be empty (@--runs 0@, @--census-only@, ...).
data ProjectReport = ProjectReport
  { project :: Project,
    -- | Listed modules, and those we parse (with a slot).
    modulesListed :: Int,
    modulesParsed :: Int,
    samples :: [Sample],
    startup :: [StartupRun],
    steady :: [SteadyRun],
    searchDuring :: [SearchDuringRun],
    -- | What the refs runs asked (none for an extra project).
    refsQueries :: [RefsQuery],
    refsDuring :: [RefsDuringRun],
    fallbacks :: FallbackCheck,
    -- | Things the human should look at, e.g. a text-only count that
    -- isn't the expected one.
    warnings :: [Text]
  }

-- | The census against @bench\/fallbacks.txt@. See "Bench.Fallbacks".
data FallbackCheck = FallbackCheck
  { -- | 'Nothing' if no run did the census.
    got :: Maybe Census,
    expected :: Lookup,
    diffs :: [Diff],
    verdict :: FallbackVerdict,
    strictness :: Strictness
  }

-- | How the navigation pages were asked for.
data NavPattern = BackToBack | IdleGaps
  deriving (Eq, Show)

-- * Targets

-- | The targets this project's runs can say something about.
projectVerdicts :: ProjectReport -> [Verdict]
projectVerdicts pr =
  catMaybes
    [ medianOf urlAndAccepting (map (.serveUs) pr.startup),
      medianOf (uiUsable pr.project.role) (map (.uiUsableUs) pr.startup),
      p95Of onDemandModule (onDemandTimes pr),
      p95Of navigation (map (.us) (concatMap (.pages) pr.steady)),
      p95Of navigationIdle (map (.us) (concatMap (.idlePages) pr.steady)),
      p95Of navigationSource (map (.us) (concatMap (.sources) pr.steady)),
      p95Of navigationSourceIdle (map (.us) (concatMap (.idleSources) pr.steady)),
      medianOf searchAfterMedian (searchAfterTimes pr),
      p95Of searchAfterP95 (searchAfterTimes pr),
      p95Of refsAfterP95 (map (.us) (refsAfter pr)),
      medianOf textSearchAfterMedian (map (.us) (textAfter pr)),
      p95Of textSearchAfterP95 (map (.us) (textAfter pr))
    ]
  where
    medianOf t xs = do
      s <- summaryUs xs
      Just (verdict t s.median (Just s.max))
    p95Of t xs = do
      l <- latencyUs xs
      Just (verdict t l.p95 (Just l.max))

onDemandTimes :: ProjectReport -> [Integer]
onDemandTimes pr = [o.us | r <- pr.startup, Just o <- [r.onDemand], o.status == Counted]

searchAfterTimes :: ProjectReport -> [Integer]
searchAfterTimes pr = map (.us) (concatMap (.searchTimes) pr.steady)

refsAfter :: ProjectReport -> [RefsTime]
refsAfter pr = concatMap (.refsTimes) pr.steady

-- | Text searches with the box's limit, after the first (the load).
textAfter :: ProjectReport -> [TextTime]
textAfter pr = concatMap (.textTimes) pr.steady

-- | The first text search of each run: it loads the store.
textFirsts :: ProjectReport -> [TextTime]
textFirsts pr = mapMaybe (.textFirst) pr.steady

-- | Text searches with the results page's limit.
textPagesOf :: ProjectReport -> [TextTime]
textPagesOf pr = concatMap (.textPages) pr.steady

-- | Latency over all samples of one query; counts from the last sample
-- (they don't change after indexing).
data TextSummary = TextSummary
  { query :: Text,
    latency :: Latency,
    total :: Int,
    files :: Int,
    sent :: Int,
    wireBytes :: Int
  }

-- | One summary per query, in first-seen order.
textByQuery :: [TextTime] -> [TextSummary]
textByQuery ts = mapMaybe one (nub (map (.query) ts))
  where
    one q = do
      mine <- NE.nonEmpty [t | t <- ts, t.query == q]
      let lastOne = NE.last mine
      Just
        TextSummary
          { query = q,
            latency = latency (NE.map (fromInteger . (.us)) mine),
            total = lastOne.total,
            files = lastOne.files,
            sent = lastOne.sent,
            wireBytes = lastOne.wireBytes
          }

-- * Rendering: the table

-- | The table printed on stdout.
renderTable :: Report -> Text
renderTable report =
  Text.unlines $
    [heading, settingsLine, nofileLine, "load average at start: " <> loadText start]
      <> concatMap (projectSection st.fallbacksFile) report.projects
      <> targetLines
      <> smallRunsNote
  where
    st = report.settings
    start = report.loadAtStart
    heading = "haskell-reviewer bench" <> maybe "" (" - " <>) report.label
    settingsLine
      | st.censusOnly = "census only"
      | otherwise =
          "runs: "
            <> showT st.runs
            <> " start-up + "
            <> showT st.warmup
            <> " warm-up, "
            <> showT st.steadyRuns
            <> " steady, "
            <> showT st.searchRuns
            <> " search-during, "
            <> showT st.refsRuns
            <> " refs-during; RTS: "
            <> (if null st.rtsOpts then "product defaults" else Text.pack (unwords st.rtsOpts))
    -- A high RLIMIT_NOFILE makes every process spawn slower (D-85).
    nofileLine =
      "RLIMIT_NOFILE: soft="
        <> maybe "unlimited" showT report.machine.nofileSoft
        <> " hard="
        <> maybe "unlimited" showT report.machine.nofileHard
    targetLines = case concatMap verdictLines report.projects of
      [] -> []
      ls ->
        [ "",
          "Targets (SPEC §10.1, D-208, D-303, D-308; a MISS never fails the bench, \
          \§10.3):"
        ]
          <> ls
    verdictLines pr = map (("  " <>) . renderVerdict pr.project.name) (projectVerdicts pr)
    smallRunsNote
      | not st.censusOnly && st.runs < 3 = ["", "with --runs < 3, medians mean little"]
      | otherwise = []

projectSection :: FilePath -> ProjectReport -> [Text]
projectSection fallbacksFile pr =
  [ "",
    "== "
      <> pr.project.name
      <> " ("
      <> roleText pr.project.role
      <> ", "
      <> showT pr.modulesListed
      <> " modules listed, "
      <> showT pr.modulesParsed
      <> " parsed)"
  ]
    <> startupLines pr
    <> steadyLines pr
    <> searchDuringLines pr
    <> refsDuringLines pr
    <> fallbackLines fallbacksFile pr.fallbacks
    <> memoryLines pr
    <> map ("warning: " <>) pr.warnings

startupLines :: ProjectReport -> [Text]
startupLines pr = case NE.nonEmpty pr.startup of
  Nothing -> []
  Just runs ->
    [ "start-up, " <> plural (length runs) "run" <> ", ms, median [min-max]:",
      "  URL " <> ms1 (.urlUs) runs <> " | accepting " <> ms1 (.serveUs) runs,
      "  UI usable "
        <> ms1 (.uiUsableUs) runs
        <> " ("
        <> fmtKiB (NE.head runs).uiWireBytes
        <> " KiB on the wire, first run)",
      "    shell "
        <> msMedian (.serveUs) runs
        <> ", assets "
        <> msMedian (.parts.assetsUs) runs
        <> ", prefs "
        <> msMedian (.parts.prefsUs) runs
        <> ", modules "
        <> msMedian (.parts.modulesUs) runs
        <> ", search "
        <> msMedian (.parts.searchUs) runs
        <> maybe
          ""
          (\s -> ", module " <> fmtMs s.median)
          (summaryUs (mapMaybe (.parts.moduleUs) (NE.toList runs)))
        <> ", files "
        <> msMedian (.filesUs) runs
        <> " (first search partial in "
        <> showT (length (NE.filter (.searchPartial) runs))
        <> ")",
      "  on-demand module: " <> onDemandText,
      "  indexing done (harness) "
        <> ms1 (.doneUs) runs
        <> " | product "
        <> fmt1 (summarize (NE.map (fromInteger . (.done.ms)) runs))
        <> maybe
          ""
          (\s -> " | span " <> fmtMs s.median)
          (summaryUs (mapMaybe (.indexingSpanUs) (NE.toList runs))),
      "  done line: "
        <> showT d.modules
        <> " modules, "
        <> showT d.ready
        <> " ready, "
        <> showT d.failed
        <> " failed, "
        <> showT d.textOnly
        <> " text only (first run)",
      "  stop " <> ms1 (.stopUs) runs
    ]
    where
      d = (NE.head runs).done
      onDemandText = case latencyUs (onDemandTimes pr) of
        Nothing -> "no sample (indexing always done first)" <> excluded
        Just l -> latencyText l <> excluded <> "; " <> sampleText
      excluded = case statusCount IndexedBefore of
        0 -> ""
        n -> " (" <> showT n <> " asked after indexing, left out)"
      statusCount st =
        length [() | r <- NE.toList runs, Just o <- [r.onDemand], o.status == st]
      sampleText =
        Text.intercalate ", " [sampleKindText s.kind <> " " <> s.ref | s <- pr.samples]

steadyLines :: ProjectReport -> [Text]
steadyLines pr = case pr.steady of
  [] -> []
  runs ->
    [ "steady, " <> plural (length runs) "run" <> " after indexing, ms:",
      "  navigation back to back: " <> maybe "-" latencyText (latencyUs (map (.us) pages)),
      "  navigation with idle gaps: " <> maybe "-" latencyText (latencyUs (map (.us) idle)),
      "  source back to back: " <> maybe "-" latencyText (latencyUs (map (.us) sources)),
      "  source with idle gaps: "
        <> maybe "-" latencyText (latencyUs (map (.us) idleSources)),
      "  search: " <> maybe "-" latencyText (latencyUs (searchAfterTimes pr))
    ]
      <> refsLines
      <> textLines
      <> slowLines
      <> gcLines
      <> phaseLines
      <> gcWarnings
  where
    pages = concatMap (.pages) pr.steady
    idle = concatMap (.idlePages) pr.steady
    sources = concatMap (.sources) pr.steady
    idleSources = concatMap (.idleSources) pr.steady
    refsLines
      | null pr.refsQueries = []
      | otherwise =
          ("  refs: " <> maybe "-" latencyText (latencyUs (map (.us) (refsAfter pr))))
            : map refsQueryLine (byQuery pr.refsQueries (refsAfter pr))
    refsQueryLine q =
      "    "
        <> q.query.moduleRef
        <> " "
        <> q.query.decl
        <> ": "
        <> shortLatency q.latency
        <> "; total "
        <> showT q.total
        <> ", sent "
        <> showT q.sent
        <> ", "
        <> fmtKiB q.wireBytes
        <> " KiB"
    textLines = case textAfter pr of
      [] -> []
      ts ->
        [ "  text search (limit "
            <> showT textLimitBox
            <> ", after the load): "
            <> maybe "-" latencyText (latencyUs (map (.us) ts)),
          "    first (loads the store): "
            <> Text.intercalate ", " [fmtMs (fromInteger t.us) | t <- textFirsts pr]
            <> "; page (limit "
            <> showT textLimitPage
            <> "): "
            <> maybe "-" shortLatency (latencyUs (map (.us) (textPagesOf pr)))
        ]
          <> map textQueryLine (textByQuery ts)
          <> [ "    page "
                 <> quoted q.query
                 <> ": "
                 <> fmtMs q.latency.median
                 <> ", sent "
                 <> showT q.sent
                 <> ", "
                 <> fmtKiB q.wireBytes
                 <> " KiB"
             | q <- textByQuery (textPagesOf pr)
             ]
    textQueryLine q =
      "    "
        <> quoted q.query
        <> ": "
        <> shortLatency q.latency
        <> "; total "
        <> plural q.total "line"
        <> " in "
        <> plural q.files "file"
    quoted q = "\"" <> q <> "\""
    phaseLines = case phaseSummary pr.steady of
      [] -> []
      ps -> ["  major GC by phase: " <> Text.intercalate ", " (map phaseText ps)]
    phaseText p =
      p.name
        <> " "
        <> showT p.majors
        <> maybe "" (\m -> " (max " <> fmtMs (fromInteger m) <> ")") p.maxMajorUs
    slow = slowPages pr
    slowLines
      | null slow = []
      | otherwise =
          ("  pages over 100 ms: " <> showT (length slow))
            : map slowLine (take 10 slow)
    slowLine p =
      "    "
        <> navPatternText p.pattern
        <> " "
        <> routeText p.route
        <> " "
        <> p.ref
        <> " "
        <> fmtMs (fromInteger p.us)
    gcWarnings =
      [ "  warning: steady run " <> showT i <> ": " <> Text.pack (renderGcLogError e)
      | (i, Just (Left e)) <- zip [1 :: Int ..] (map (.gc) pr.steady)
      ]
    gcLines = case gcSummary pr.steady of
      Nothing -> []
      Just g ->
        [ "  GC: "
            <> showT g.collections
            <> " pauses, "
            <> showT g.majors
            <> " major ("
            <> showT g.majorsAfter
            <> " after indexing); max "
            <> fmtMs (fromInteger g.maxPauseUs)
            <> ", max major "
            <> fmtMs (fromInteger g.maxMajorPauseUs)
            <> ", p99 "
            <> fmtMs (fromInteger g.p99PauseUs)
            <> "; max major after indexing "
            <> maybe "-" (fmtMs . fromInteger) g.maxMajorAfterUs
            <> ", max live after indexing "
            <> maybe "-" fmtMiB g.maxLiveAfterBytes
            <> " MiB",
          "      max during indexing "
            <> maybe "-" (fmtMs . fromInteger) g.maxDuringUs
            <> ", after "
            <> maybe "-" (fmtMs . fromInteger) g.maxAfterUs
            <> "; paused "
            <> fmtMs (fromInteger g.pausedUs)
            <> " in total"
        ]

searchDuringLines :: ProjectReport -> [Text]
searchDuringLines pr = case pr.searchDuring of
  [] -> []
  runs ->
    [ "search during indexing, "
        <> plural (length runs) "run"
        <> ", report only, ms: "
        <> maybe "-" latencyText (latencyUs [q.us | q <- qs, q.partial])
        <> "; "
        <> showT (length qs)
        <> " queries, "
        <> showT (length (filter (.partial) qs))
        <> " partial"
    ]
    where
      qs = concatMap (.queryTimes) runs

refsDuringLines :: ProjectReport -> [Text]
refsDuringLines pr = case pr.refsDuring of
  [] -> []
  runs ->
    [ "refs during indexing, "
        <> plural (length runs) "run"
        <> ", report only, ms: "
        <> maybe "-" latencyText (latencyUs (map (.us) during))
        <> "; "
        <> showT (length (filter (.partial) during))
        <> " of them partial",
      "  first answer median "
        <> maybe "-" fmtMs (median (firstTimes runs))
        <> "; rounds "
        <> Text.intercalate ", " (map (showT . (.rounds)) runs)
        <> maybe "" (\x -> "; indexing " <> signed x <> "% vs start-up") slowdown
    ]
    where
      during = refsDuringTimes runs
      median xs = (.median) <$> summaryUs xs
      slowdown = do
        base <- median (map (.done.ms) pr.startup)
        slow <- median (map (.done.ms) runs)
        slowdownPercent base slow
      signed x = (if x >= 0 then "+" else "") <> fmtDouble x

-- | Refs answers of refs-during runs whose request started before the
-- done line.
refsDuringTimes :: [RefsDuringRun] -> [RefsTime]
refsDuringTimes runs = [t | r <- runs, t <- r.refsTimes, startedBefore r.doneUs t]

-- | The first answer of each run: it waits for its home module.
firstTimes :: [RefsDuringRun] -> [Integer]
firstTimes runs = [t.us | r <- runs, t : _ <- [r.refsTimes]]

fallbackLines :: FilePath -> FallbackCheck -> [Text]
fallbackLines file fc =
  ( "fallbacks: "
      <> verdictText
      <> (if failsBench fc.strictness fc.verdict then " (fails the bench)" else "")
      <> maybe "" (\c -> "; got: " <> countsText c) fc.got
  )
    : map ("  " <>) (concatMap renderDiff fc.diffs)
  where
    path = Text.pack file
    verdictText = case fc.verdict of
      NotCheckedBecause NoRowForProject -> "not checked, no row in " <> path
      NotCheckedBecause (OtherShasOnly shas) ->
        "not checked, "
          <> path
          <> " has this project only at "
          <> Text.intercalate ", " shas
          <> ", not at the pinned sha: add a row (--census-only)"
      NotCheckedBecause NoCensusRun -> "not checked, no census ran (--steady-runs 0)"
      AsExpected -> "as expected"
      Mismatch -> "MISMATCH"
      TooSlowDiffers -> "too-slow differs (warning)"
    countsText census
      | Map.null census = "none"
      | otherwise =
          Text.intercalate
            ", "
            [categoryText c <> " " <> showT n | (c, n) <- Map.toList (countCensus census)]

memoryLines :: ProjectReport -> [Text]
memoryLines pr =
  case NE.nonEmpty (map (.rts) pr.startup <> map (.rts) pr.steady) of
    Nothing -> []
    Just rs ->
      [ "memory, MiB, max over runs: max_mem_in_use start-up "
          <> orDash (peakStartup pr)
          <> " / steady "
          <> orDash (peakSteady pr)
          <> ", max_live "
          <> orDash (maxOf (mapMaybe (.maxLiveBytes) (NE.toList rs)))
          <> ", idle RSS after indexing "
          <> orDash (maxOf (mapMaybe (.idleRssBytes) pr.startup))
          <> " / after the steady work "
          <> orDash (maxOf (mapMaybe (.idleRssBytes) pr.steady))
          <> ", peak RSS "
          <> orDash (maxOf (hwms pr))
          <> " (at done "
          <> orDash (maxOf (mapMaybe (.hwmAtDoneBytes) pr.steady))
          <> ")"
      ]
        <> budgetLine
      where
        peak = maximum (NE.map (.peakMemBytes) rs)
        orDash = maybe "-" fmtMiB
        budgetLine = case memoryBudgetBytes pr.project.role of
          Nothing -> []
          Just b ->
            [ "  §10.2 budget "
                <> fmtMiB b
                <> " (max_mem_in_use): "
                <> (if peak < b then "ok" else "MISS → §10.3: record, decide, log")
            ]

-- | max_mem_in_use of the start-up runs (@-t@, bytes) and of the
-- steady ones (@-S@, MiB steps).
peakStartup, peakSteady :: ProjectReport -> Maybe Integer
peakStartup pr = maxOf (map (.rts.peakMemBytes) pr.startup)
peakSteady pr = maxOf (map (.rts.peakMemBytes) pr.steady)

-- | Peak RSS of every run that has one.
hwms :: ProjectReport -> [Integer]
hwms pr = mapMaybe (.hwmBytes) pr.startup <> mapMaybe (.hwmBytes) pr.steady

-- | A page over the navigation limit.
data SlowPage = SlowPage
  { pattern :: NavPattern,
    route :: Route,
    ref :: Text,
    us :: Integer
  }

-- | Pages over the navigation limit, worst first.
slowPages :: ProjectReport -> [SlowPage]
slowPages pr =
  sortOn
    (Down . (.us))
    ( [ SlowPage BackToBack p.route p.ref p.us
      | p <- concatMap (\r -> r.pages <> r.sources) pr.steady,
        slow p
      ]
        <> [ SlowPage IdleGaps p.route p.ref p.us
           | p <- concatMap (\r -> r.idlePages <> r.idleSources) pr.steady,
             slow p
           ]
    )
  where
    slow p = p.us >= navigation.limitUs

navPatternText :: NavPattern -> Text
navPatternText BackToBack = "back-to-back"
navPatternText IdleGaps = "idle"

routeText :: Route -> Text
routeText ModuleRoute = "module"
routeText SourceRoute = "source"

-- | Pooled over the steady runs whose @-S@ file had GC lines.
gcSummary :: [SteadyRun] -> Maybe GcSummary
gcSummary runs = summarizeGc [GcRun r.doneUs g | r <- runs, Just (Right g) <- [r.gc]]

-- | Major collections per phase of one run, if its @-S@ file had GC
-- lines.
runPhases :: SteadyRun -> [PhaseGc]
runPhases r = case r.gc of
  Just (Right g) -> phaseMajors r.phases g
  _ -> []

-- | 'runPhases' pooled over runs: counts added, the max of the maxes.
-- In the first run's order: every run has the same phases.
phaseSummary :: [SteadyRun] -> [PhaseGc]
phaseSummary runs = case filter (not . null) (map runPhases runs) of
  [] -> []
  firstRun : _ -> map (pool . (.name)) firstRun
  where
    everyPhase = concatMap runPhases runs
    pool n =
      PhaseGc
        { name = n,
          majors = sum (map (.majors) same),
          maxMajorUs = maxOf (mapMaybe (.maxMajorUs) same)
        }
      where
        same = [p | p <- everyPhase, p.name == n]

-- * Rendering: helpers

summaryUs :: [Integer] -> Maybe Summary
summaryUs xs = summarize . NE.map fromInteger <$> NE.nonEmpty xs

latencyUs :: [Integer] -> Maybe Latency
latencyUs xs = latency . NE.map fromInteger <$> NE.nonEmpty xs

-- | Median [min-max] of a microseconds field, in ms.
ms1 :: (StartupRun -> Integer) -> NonEmpty StartupRun -> Text
ms1 field runs = fmt1 (summarize (NE.map ((/ 1000) . fromInteger . field) runs))

msMedian :: (StartupRun -> Integer) -> NonEmpty StartupRun -> Text
msMedian field runs = fmtMs (summarize (NE.map (fromInteger . field) runs)).median

-- | Median, p95 and max, no count.
shortLatency :: Latency -> Text
shortLatency l =
  "median " <> fmtMs l.median <> ", p95 " <> fmtMs l.p95 <> ", max " <> fmtMs l.max

latencyText :: Latency -> Text
latencyText l =
  "median "
    <> fmtMs l.median
    <> ", p95 "
    <> fmtMs l.p95
    <> ", max "
    <> fmtMs l.max
    <> " ("
    <> showT l.count
    <> ")"

-- | @median [min-max]@, one decimal place.
fmt1 :: Summary -> Text
fmt1 s = fmtDouble s.median <> " [" <> fmtDouble s.min <> "-" <> fmtDouble s.max <> "]"

-- | Microseconds as ms, one decimal.
fmtMs :: Double -> Text
fmtMs us = fmtDouble (us / 1000)

fmtKiB :: Int -> Text
fmtKiB b = fmtDouble (fromIntegral b / 1024)

fmtMiB :: Integer -> Text
fmtMiB b = fmtDouble (fromInteger b / (1024 * 1024))

fmtDouble :: Double -> Text
fmtDouble x = Text.pack (printf "%.1f" x)

loadText :: Maybe LoadAvg -> Text
loadText Nothing = "unknown"
loadText (Just l) = Text.pack (printf "%.2f %.2f %.2f" l.one l.five l.fifteen)

maxOf :: (Ord a) => [a] -> Maybe a
maxOf [] = Nothing
maxOf xs = Just (maximum xs)

-- | "1 run", "3 runs".
plural :: Int -> Text -> Text
plural n word = showT n <> " " <> word <> (if n == 1 then "" else "s")

showT :: (Show a) => a -> Text
showT = Text.pack . show

roleText :: Role -> Text
roleText Small = "small"
roleText Typical = "typical"
roleText Large = "large"
roleText Extra = "extra"

-- * JSON (schema 4)

-- | Encode the whole report. Built by hand with 'Data.Aeson.Encoding',
-- like "HaskellReviewer.Effects.Trace", to keep the key order fixed and
-- readable. Times are microseconds, sizes bytes.
encodeReportJson :: Report -> LBS.ByteString
encodeReportJson report = Aeson.Encoding.encodingToLazyByteString (reportEncoding report)

type Encoding = Aeson.Encoding.Encoding

-- | JSON null for 'Nothing'.
nullable :: (a -> Encoding) -> Maybe a -> Encoding
nullable = maybe Aeson.Encoding.null_

reportEncoding :: Report -> Encoding
reportEncoding report =
  Aeson.Encoding.pairs $
    ("schema" .= (4 :: Int))
      <> ("started" .= iso8601Show report.started)
      <> ("label" .= report.label)
      <> Aeson.Encoding.pair "settings" (settingsEncoding report.settings)
      <> Aeson.Encoding.pair "tool" (toolEncoding report.tool)
      <> Aeson.Encoding.pair "machine" (machineEncoding report.machine)
      <> Aeson.Encoding.pair "loadavg_start" (loadEncoding report.loadAtStart)
      <> Aeson.Encoding.pair "projects" (Aeson.Encoding.list projectEncoding report.projects)

settingsEncoding :: Settings -> Encoding
settingsEncoding s =
  Aeson.Encoding.pairs $
    ("runs" .= s.runs)
      <> ("warmup" .= s.warmup)
      <> ("steady_runs" .= s.steadyRuns)
      <> ("search_runs" .= s.searchRuns)
      <> ("refs_runs" .= s.refsRuns)
      <> ("nav_idle" .= s.navIdle)
      <> ("rts_opts" .= s.rtsOpts)
      <> ("census_only" .= s.censusOnly)

toolEncoding :: ToolInfo -> Encoding
toolEncoding tool =
  Aeson.Encoding.pairs $
    ("exe" .= tool.exe)
      <> ("version" .= tool.version)
      <> ("git_sha" .= tool.gitSha)
      <> ("git_dirty" .= tool.gitDirty)
      <> ("ghc" .= tool.ghc)
      <> ("rts_way" .= tool.rtsWay)

machineEncoding :: MachineInfo -> Encoding
machineEncoding machine =
  Aeson.Encoding.pairs $
    ("os" .= machine.os)
      <> ("arch" .= machine.arch)
      <> ("kernel" .= machine.kernel)
      <> ("cpu" .= machine.cpu)
      <> ("cores" .= machine.cores)
      <> ("memory_bytes" .= machine.memoryBytes)
      <> ("nofile_soft" .= machine.nofileSoft)
      <> ("nofile_hard" .= machine.nofileHard)

loadEncoding :: Maybe LoadAvg -> Encoding
loadEncoding Nothing = Aeson.Encoding.null_
loadEncoding (Just l) = Aeson.Encoding.list Aeson.Encoding.double [l.one, l.five, l.fifteen]

projectEncoding :: ProjectReport -> Encoding
projectEncoding pr =
  Aeson.Encoding.pairs $
    ("name" .= pr.project.name)
      <> ("role" .= roleText pr.project.role)
      <> ("sha" .= pr.project.sha)
      <> ("files" .= fmap (.files.count) first)
      <> ("source" .= fmap (sourceText . (.files.source)) first)
      <> ("modules_listed" .= pr.modulesListed)
      <> ("modules_parsed" .= pr.modulesParsed)
      <> Aeson.Encoding.pair "samples" (Aeson.Encoding.list sampleEncoding pr.samples)
      <> Aeson.Encoding.pair
        "refs_queries"
        (Aeson.Encoding.list refsQueryEncoding pr.refsQueries)
      <> Aeson.Encoding.pair "startup_runs" (Aeson.Encoding.list startupEncoding pr.startup)
      <> Aeson.Encoding.pair "summary" (startupSummaryEncoding pr.startup)
      <> Aeson.Encoding.pair "on_demand" (maybeLatency (latencyUs (onDemandTimes pr)))
      <> Aeson.Encoding.pair "steady" (steadyEncoding pr)
      <> Aeson.Encoding.pair "search_during" (searchDuringEncoding pr.searchDuring)
      <> Aeson.Encoding.pair "refs_during" (refsDuringEncoding pr.refsDuring)
      <> Aeson.Encoding.pair "fallbacks" (fallbacksEncoding pr.fallbacks)
      <> Aeson.Encoding.pair "memory" (memoryEncoding pr)
      <> Aeson.Encoding.pair "targets" targetsEncoding
      <> ("warnings" .= pr.warnings)
  where
    targetsEncoding = Aeson.Encoding.list verdictEncoding (projectVerdicts pr)
    first = case pr.startup of
      r : _ -> Just r
      [] -> Nothing

sourceText :: FileSource -> Text
sourceText FromGit = "git"
sourceText FromDisk = "disk"

refsQueryEncoding :: RefsQuery -> Encoding
refsQueryEncoding q =
  Aeson.Encoding.pairs (("module" .= q.moduleRef) <> ("decl" .= q.decl))

sampleEncoding :: Sample -> Encoding
sampleEncoding s = Aeson.Encoding.pairs (("kind" .= sampleKindText s.kind) <> ("ref" .= s.ref))

-- | The numeric fields of a start-up run, by JSON key. One list for the
-- per-run objects and the summary, so they can't drift apart.
startupFields :: [(Aeson.Key.Key, StartupRun -> Maybe Integer)]
startupFields =
  [ ("url_us", Just . (.urlUs)),
    ("serve_us", Just . (.serveUs)),
    ("files_us", Just . (.filesUs)),
    ("ui_usable_us", Just . (.uiUsableUs)),
    ("ui_assets_us", Just . (.parts.assetsUs)),
    ("ui_prefs_us", Just . (.parts.prefsUs)),
    ("ui_modules_us", Just . (.parts.modulesUs)),
    ("ui_search_us", Just . (.parts.searchUs)),
    ("ui_module_us", (.parts.moduleUs)),
    ("ui_wire_bytes", Just . toInteger . (.uiWireBytes)),
    ("done_us", Just . (.doneUs)),
    ("indexing_ms", Just . (.done.ms)),
    ("indexing_span_us", (.indexingSpanUs)),
    ("startup_span_us", (.startupSpanUs)),
    ("files_span_us", (.filesSpanUs)),
    ("stop_us", Just . (.stopUs)),
    ("peak_mem_bytes", Just . (.rts.peakMemBytes)),
    ("max_live_bytes", (.rts.maxLiveBytes)),
    ("allocated_bytes", (.rts.allocatedBytes)),
    ("gc_cpu_us", (.rts.gcCpuUs)),
    ("mutator_cpu_us", (.rts.mutatorCpuUs)),
    ("idle_rss_bytes", (.idleRssBytes)),
    ("hwm_bytes", (.hwmBytes))
  ]

startupEncoding :: StartupRun -> Encoding
startupEncoding r =
  Aeson.Encoding.pairs $
    foldMap (\(k, f) -> k .= f r) startupFields
      <> ("search_partial" .= r.searchPartial)
      <> Aeson.Encoding.pair "on_demand" (nullable onDemandEncoding r.onDemand)
      <> Aeson.Encoding.pair "done" (doneEncoding r.done)
      <> Aeson.Encoding.pair "loadavg" (loadEncoding r.loadAvg)
  where
    onDemandEncoding o =
      Aeson.Encoding.pairs $
        ("sample" .= sampleKindText o.sample.kind)
          <> ("ref" .= o.sample.ref)
          <> ("us" .= o.us)
          <> ("status" .= onDemandStatusText o.status)
          <> ("done_before" .= o.doneBefore)
          <> ("total" .= o.total)
    onDemandStatusText :: OnDemandStatus -> Text
    onDemandStatusText Counted = "counted"
    onDemandStatusText IndexedBefore = "indexed-before"

doneEncoding :: IndexDone -> Encoding
doneEncoding d =
  Aeson.Encoding.pairs $
    ("modules" .= d.modules)
      <> ("ready" .= d.ready)
      <> ("failed" .= d.failed)
      <> ("text_only" .= d.textOnly)
      <> ("ms" .= d.ms)

-- | One 'Summary' (median, min, max) per start-up field, null if no run
-- has it.
startupSummaryEncoding :: [StartupRun] -> Encoding
startupSummaryEncoding runs = Aeson.Encoding.pairs (foldMap one startupFields)
  where
    one (k, f) = Aeson.Encoding.pair k (nullable summaryEncoding (summaryUs (mapMaybe f runs)))

summaryEncoding :: Summary -> Encoding
summaryEncoding s =
  Aeson.Encoding.pairs $
    ("median" .= s.median) <> ("min" .= s.min) <> ("max" .= s.max)

maybeLatency :: Maybe Latency -> Encoding
maybeLatency Nothing = Aeson.Encoding.null_
maybeLatency (Just l) =
  Aeson.Encoding.pairs $
    ("count" .= l.count)
      <> ("median_us" .= l.median)
      <> ("p95_us" .= l.p95)
      <> ("max_us" .= l.max)

steadyEncoding :: ProjectReport -> Encoding
steadyEncoding pr =
  Aeson.Encoding.pairs $
    Aeson.Encoding.pair "runs" (Aeson.Encoding.list runEncoding pr.steady)
      <> Aeson.Encoding.pair "pages" (maybeLatency (latencyUs (map (.us) pages)))
      <> ("pages_wire_bytes" .= sum (map (.wireBytes) pages))
      <> Aeson.Encoding.pair "pages_idle" (maybeLatency (latencyUs (map (.us) idle)))
      <> Aeson.Encoding.pair "sources" (maybeLatency (latencyUs (map (.us) sources)))
      <> ("sources_wire_bytes" .= sum (map (.wireBytes) sources))
      <> Aeson.Encoding.pair
        "sources_idle"
        (maybeLatency (latencyUs (map (.us) idleSources)))
      <> Aeson.Encoding.pair "search_after" (maybeLatency (latencyUs (searchAfterTimes pr)))
      <> Aeson.Encoding.pair "refs_after" refsAfterEncoding
      <> Aeson.Encoding.pair "text_search_after" textAfterEncoding
      <> Aeson.Encoding.pair "gc" (nullable gcEncoding (gcSummary pr.steady))
      <> Aeson.Encoding.pair "slow_pages" (Aeson.Encoding.list slowEncoding (slowPages pr))
  where
    pages = concatMap (.pages) pr.steady
    idle = concatMap (.idlePages) pr.steady
    sources = concatMap (.sources) pr.steady
    idleSources = concatMap (.idleSources) pr.steady
    refs = refsAfter pr
    refsAfterEncoding =
      Aeson.Encoding.pairs $
        Aeson.Encoding.pair "latency" (maybeLatency (latencyUs (map (.us) refs)))
          <> ("partial" .= length (filter (.partial) refs))
          <> Aeson.Encoding.pair
            "by_query"
            ( Aeson.Encoding.list
                refsSummaryEncoding
                (byQuery pr.refsQueries refs)
            )
    textAfterEncoding =
      Aeson.Encoding.pairs $
        Aeson.Encoding.pair "latency" (maybeLatency (latencyUs (map (.us) texts)))
          <> ("first_us" .= map (.us) (textFirsts pr))
          <> Aeson.Encoding.pair
            "page_latency"
            (maybeLatency (latencyUs (map (.us) (textPagesOf pr))))
          <> ("stopped" .= length (filter (.stopped) allText))
          <> Aeson.Encoding.pair
            "by_query"
            (Aeson.Encoding.list textSummaryEncoding (textByQuery texts))
          <> Aeson.Encoding.pair
            "page_by_query"
            (Aeson.Encoding.list textSummaryEncoding (textByQuery (textPagesOf pr)))
    texts = textAfter pr
    allText = textFirsts pr <> texts <> textPagesOf pr
    runEncoding r =
      Aeson.Encoding.pairs $
        ("done_us" .= r.doneUs)
          <> Aeson.Encoding.pair "done" (doneEncoding r.done)
          <> ("pages" .= length r.pages)
          <> ("pages_idle" .= length r.idlePages)
          <> ("sources" .= length r.sources)
          <> ("sources_idle" .= length r.idleSources)
          <> ("searches" .= length r.searchTimes)
          <> ("refs" .= length r.refsTimes)
          <> ("text_searches" .= length r.textTimes)
          <> ("text_first_us" .= fmap (.us) r.textFirst)
          <> ("peak_mem_bytes" .= r.rts.peakMemBytes)
          <> ("max_live_bytes" .= r.rts.maxLiveBytes)
          <> ("idle_rss_bytes" .= r.idleRssBytes)
          <> ("hwm_at_done_bytes" .= r.hwmAtDoneBytes)
          <> ("hwm_bytes" .= r.hwmBytes)
          <> Aeson.Encoding.pair "gc" (nullable gcEncoding (gcSummary [r]))
          <> Aeson.Encoding.pair
            "gc_phases"
            (Aeson.Encoding.list phaseEncoding (runPhases r))
          <> ("gc_log_error" .= gcLogError r)
          <> Aeson.Encoding.pair "loadavg" (loadEncoding r.loadAvg)
    gcLogError r = case r.gc of
      Just (Left e) -> Just (renderGcLogError e)
      _ -> Nothing
    slowEncoding p =
      Aeson.Encoding.pairs
        ( ("pattern" .= navPatternText p.pattern)
            <> ("route" .= routeText p.route)
            <> ("ref" .= p.ref)
            <> ("us" .= p.us)
        )
    phaseEncoding p =
      Aeson.Encoding.pairs
        (("name" .= p.name) <> ("majors" .= p.majors) <> ("max_major_us" .= p.maxMajorUs))

refsSummaryEncoding :: RefsSummary -> Encoding
refsSummaryEncoding q =
  Aeson.Encoding.pairs $
    ("module" .= q.query.moduleRef)
      <> ("decl" .= q.query.decl)
      <> ("count" .= q.latency.count)
      <> ("median_us" .= q.latency.median)
      <> ("p95_us" .= q.latency.p95)
      <> ("max_us" .= q.latency.max)
      <> ("total" .= q.total)
      <> ("sent" .= q.sent)
      <> ("wire_bytes" .= q.wireBytes)

textSummaryEncoding :: TextSummary -> Encoding
textSummaryEncoding q =
  Aeson.Encoding.pairs $
    ("query" .= q.query)
      <> ("count" .= q.latency.count)
      <> ("median_us" .= q.latency.median)
      <> ("p95_us" .= q.latency.p95)
      <> ("max_us" .= q.latency.max)
      <> ("total" .= q.total)
      <> ("files" .= q.files)
      <> ("sent" .= q.sent)
      <> ("wire_bytes" .= q.wireBytes)

gcEncoding :: GcSummary -> Encoding
gcEncoding g =
  Aeson.Encoding.pairs $
    ("collections" .= g.collections)
      <> ("majors" .= g.majors)
      <> ("majors_after" .= g.majorsAfter)
      <> ("max_major_after_us" .= g.maxMajorAfterUs)
      <> ("max_live_after_bytes" .= g.maxLiveAfterBytes)
      <> ("max_pause_us" .= g.maxPauseUs)
      <> ("max_major_pause_us" .= g.maxMajorPauseUs)
      <> ("p99_pause_us" .= g.p99PauseUs)
      <> ("max_during_us" .= g.maxDuringUs)
      <> ("max_after_us" .= g.maxAfterUs)
      <> ("paused_us" .= g.pausedUs)

searchDuringEncoding :: [SearchDuringRun] -> Encoding
searchDuringEncoding runs =
  Aeson.Encoding.pairs $
    ("runs" .= length runs)
      <> ("rounds" .= map (.rounds) runs)
      <> ("queries" .= length qs)
      <> ("partial" .= length (filter (.partial) qs))
      <> Aeson.Encoding.pair "latency_partial" (maybeLatency (latencyUs partialTimes))
      <> Aeson.Encoding.pair "loadavg" (Aeson.Encoding.list (loadEncoding . (.loadAvg)) runs)
  where
    qs = concatMap (.queryTimes) runs
    partialTimes = [q.us | q <- qs, q.partial]

refsDuringEncoding :: [RefsDuringRun] -> Encoding
refsDuringEncoding runs =
  Aeson.Encoding.pairs $
    ("runs" .= length runs)
      <> ("rounds" .= map (.rounds) runs)
      <> ("done_us" .= map (.doneUs) runs)
      <> ("indexing_ms" .= map (.done.ms) runs)
      <> ("answers_during" .= length during)
      <> ("partial_during" .= length (filter (.partial) during))
      <> Aeson.Encoding.pair "latency_during" (maybeLatency (latencyUs duringUs))
      <> ("first_us" .= firstTimes runs)
      <> Aeson.Encoding.pair
        "loadavg"
        (Aeson.Encoding.list (loadEncoding . (.loadAvg)) runs)
  where
    during = refsDuringTimes runs
    duringUs = map (.us) during

fallbacksEncoding :: FallbackCheck -> Encoding
fallbacksEncoding fc =
  Aeson.Encoding.pairs $
    ("checked" .= checked)
      <> ("fails_bench" .= failsBench fc.strictness fc.verdict)
      <> Aeson.Encoding.pair "got" (nullable (countsEncoding . countCensus) fc.got)
      <> Aeson.Encoding.pair "expected" expectedEncoding
      <> Aeson.Encoding.pair "diffs" (Aeson.Encoding.list diffEncoding fc.diffs)
  where
    checked :: Text
    checked = case fc.verdict of
      NotCheckedBecause NoCensusRun -> "no-census"
      NotCheckedBecause NoRowForProject -> "no-row"
      NotCheckedBecause (OtherShasOnly _) -> "other-sha"
      Mismatch -> "mismatch"
      AsExpected -> "ok"
      TooSlowDiffers -> "too-slow-differs"
    expectedEncoding = case fc.expected of
      Found e -> countsEncoding e.counts
      _ -> Aeson.Encoding.null_
    countsEncoding :: Map.Map Category Int -> Encoding
    countsEncoding m =
      Aeson.Encoding.pairs (foldMap countPair (Map.toList m))
    countPair (c, n) = Aeson.Key.fromText (categoryText c) .= n
    diffEncoding d =
      Aeson.Encoding.pairs $
        ("category" .= categoryText d.category)
          <> ("expected" .= d.expected)
          <> ("got" .= d.got)
          <> ("deterministic" .= deterministic d.category)
          <> Aeson.Encoding.pair "modules" (Aeson.Encoding.list hitEncoding d.hits)
    hitEncoding h = Aeson.Encoding.pairs (("ref" .= h.ref) <> ("detail" .= h.detail))

memoryEncoding :: ProjectReport -> Encoding
memoryEncoding pr =
  Aeson.Encoding.pairs $
    ("peak_mem_bytes" .= peak)
      <> ("peak_mem_startup_bytes" .= peakStartup pr)
      <> ("peak_mem_steady_bytes" .= peakSteady pr)
      <> ("max_live_bytes" .= maxOf (mapMaybe (.maxLiveBytes) rtss))
      <> ("idle_rss_bytes" .= maxOf (mapMaybe (.idleRssBytes) pr.startup))
      <> ("idle_rss_after_bytes" .= maxOf (mapMaybe (.idleRssBytes) pr.steady))
      <> ("hwm_at_done_bytes" .= maxOf (mapMaybe (.hwmAtDoneBytes) pr.steady))
      <> ("hwm_bytes" .= maxOf (hwms pr))
      <> ("budget_bytes" .= budget)
      <> ("budget_met" .= ((<) <$> peak <*> budget))
  where
    rtss = map (.rts) pr.startup <> map (.rts) pr.steady
    peak = maxOf (map (.peakMemBytes) rtss)
    budget = memoryBudgetBytes pr.project.role

verdictEncoding :: Verdict -> Encoding
verdictEncoding v =
  Aeson.Encoding.pairs $
    ("name" .= v.target.key)
      <> ("stat" .= statText v.target.stat)
      <> ("limit_us" .= v.target.limitUs)
      <> ("value_us" .= v.valueUs)
      <> ("max_us" .= v.maxUs)
      <> ("met" .= v.met)
