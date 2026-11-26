-- | The bench's JSON report, schema 5 (D-357; schema 4 was D-308).
-- One entry per project and mode; a browse entry has every schema-4
-- key with the same meaning, plus the new ones.
module Bench.Report.Json
  ( encodeReportJson,
  )
where

import Bench.Fallbacks
  ( Category,
    Diff (..),
    Expected (..),
    FallbackVerdict (..),
    Hit (..),
    Lookup (..),
    NotChecked (..),
    categoryText,
    countCensus,
    deterministic,
    failsBench,
  )
import Bench.GcLog (GcSummary (..), PhaseGc (..), renderGcLogError)
import Bench.IndexLog (IndexDone (..))
import Bench.Machine (LoadAvg (..), MachineInfo (..))
import Bench.Mode (Mode (..), Side (..), modeText, sideText)
import Bench.ModuleList (OnDemandStatus (..), Sample (..), sampleKindText)
import Bench.Projects (Project (..))
import Bench.Queries (RefsQuery (..))
import Bench.Refs (RefsSummary (..), RefsTime (..), byQuery)
import Bench.RefsDuring (RefsDuringRun (..))
import Bench.Report
  ( FallbackCheck (..),
    ProjectReport (..),
    Report (..),
    Settings (..),
    SideCount (..),
    SideTimes (..),
    SlowPage (..),
    TextSummary (..),
    ToolInfo (..),
    firstTimes,
    gcSummary,
    hwms,
    latencyUs,
    maxOf,
    navPatternText,
    onDemandTimes,
    peakStartup,
    peakSteady,
    projectSideTimes,
    projectVerdicts,
    refsAfter,
    refsDuringTimes,
    roleText,
    routeText,
    runPhases,
    searchAfterTimes,
    slowPages,
    summaryUs,
    textAfter,
    textByQuery,
    textFirsts,
    textPagesOf,
  )
import Bench.RtsStats (RtsStats (..))
import Bench.SearchDuring (QueryTime (..), SearchDuringRun (..))
import Bench.Startup
  ( FileList (..),
    FileSource (..),
    OnDemand (..),
    StartupRun (..),
    UiParts (..),
  )
import Bench.Stats (Latency (..), Summary (..))
import Bench.Steady (PageTime (..), SteadyRun (..), TextTime (..))
import Bench.Targets (Target (..), Verdict (..), memoryBudgetBytes, statText)
import Data.Aeson ((.=))
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Key qualified as Aeson.Key
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)

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
    ("schema" .= (5 :: Int))
      <> ("started" .= iso8601Show report.started)
      <> ("label" .= report.label)
      <> Aeson.Encoding.pair "settings" (settingsEncoding report.settings)
      <> Aeson.Encoding.pair "tool" (toolEncoding report.tool)
      <> Aeson.Encoding.pair "machine" (machineEncoding report.machine)
      <> Aeson.Encoding.pair "loadavg_start" (loadEncoding report.loadAtStart)
      <> Aeson.Encoding.pair
        "projects"
        (Aeson.Encoding.list projectEncoding report.projects)
      <> ("notes" .= report.notes)

settingsEncoding :: Settings -> Encoding
settingsEncoding s =
  Aeson.Encoding.pairs $
    ("modes" .= map modeText s.modes)
      <> ("runs" .= s.runs)
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
loadEncoding (Just l) =
  Aeson.Encoding.list Aeson.Encoding.double [l.one, l.five, l.fifteen]

projectEncoding :: ProjectReport -> Encoding
projectEncoding pr =
  Aeson.Encoding.pairs $
    ("name" .= pr.project.name)
      <> ("role" .= roleText pr.project.role)
      <> ("sha" .= pr.project.sha)
      <> ("mode" .= modeText pr.mode)
      <> ("base_sha" .= if pr.mode == Browse then Nothing else pr.project.base)
      <> ("diff_spec" .= pr.diffSpec)
      <> ("files" .= fmap (.files.count) first)
      <> ("source" .= fmap (sourceText . (.files.source)) first)
      <> ("modules_listed" .= pr.modulesListed)
      <> ("modules_parsed" .= pr.modulesParsed)
      <> Aeson.Encoding.pair
        "sides"
        (Aeson.Encoding.list sideCountEncoding (NE.toList pr.sides))
      <> Aeson.Encoding.pair "samples" (Aeson.Encoding.list sampleEncoding pr.samples)
      <> Aeson.Encoding.pair
        "refs_queries"
        (Aeson.Encoding.list refsQueryEncoding pr.refsQueries)
      <> Aeson.Encoding.pair
        "startup_runs"
        (Aeson.Encoding.list startupEncoding pr.startup)
      <> Aeson.Encoding.pair "summary" (startupSummaryEncoding pr.startup)
      <> Aeson.Encoding.pair "on_demand" (maybeLatency (latencyUs (onDemandTimes pr)))
      <> Aeson.Encoding.pair "steady" (steadyEncoding pr)
      <> Aeson.Encoding.pair "search_during" (searchDuringEncoding pr.searchDuring)
      <> Aeson.Encoding.pair "refs_during" (refsDuringEncoding pr.refsDuring)
      <> Aeson.Encoding.pair "fallbacks" (nullable fallbacksEncoding pr.fallbacks)
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

sideCountEncoding :: SideCount -> Encoding
sideCountEncoding c =
  Aeson.Encoding.pairs $
    ("side" .= sideText c.side)
      <> ("modules_listed" .= c.listed)
      <> ("modules_parsed" .= c.parsed)

sampleEncoding :: Sample -> Encoding
sampleEncoding s =
  Aeson.Encoding.pairs $
    ("kind" .= sampleKindText s.kind)
      <> ("side" .= sideText s.side)
      <> ("ref" .= s.ref)

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
    ("ui_session_us", (.parts.sessionUs)),
    ("diff_files_us", (.parts.diffFilesUs)),
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
          <> ("side" .= sideText o.sample.side)
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
    one (k, f) =
      Aeson.Encoding.pair k (nullable summaryEncoding (summaryUs (mapMaybe f runs)))

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
      <> Aeson.Encoding.pair
        "search_after"
        (maybeLatency (latencyUs (searchAfterTimes pr)))
      <> Aeson.Encoding.pair "refs_after" refsAfterEncoding
      <> Aeson.Encoding.pair "text_search_after" textAfterEncoding
      <> Aeson.Encoding.pair "gc" (nullable gcEncoding (gcSummary pr.steady))
      <> Aeson.Encoding.pair
        "slow_pages"
        (Aeson.Encoding.list slowEncoding (slowPages pr))
      <> Aeson.Encoding.pair
        "by_side"
        (Aeson.Encoding.list bySideEncoding (projectSideTimes pr))
  where
    bySideEncoding st =
      Aeson.Encoding.pairs $
        ("side" .= sideText st.side)
          <> lat "pages" st.pages
          <> lat "pages_idle" st.idlePages
          <> lat "sources" st.sources
          <> lat "sources_idle" st.idleSources
          <> lat "search_after" st.search
          <> lat "refs_after" (map (.us) st.refs)
          <> lat "text_search_after" (map (.us) st.text)
      where
        lat k xs = Aeson.Encoding.pair k (maybeLatency (latencyUs xs))
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
                [ (st.side, q)
                | st <- projectSideTimes pr,
                  q <- byQuery pr.refsQueries st.refs
                ]
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
          <> ("text_first_us" .= fmap (.us) (listToMaybe r.textFirst))
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
        ( ("side" .= sideText p.side)
            <> ("pattern" .= navPatternText p.pattern)
            <> ("route" .= routeText p.route)
            <> ("ref" .= p.ref)
            <> ("us" .= p.us)
        )
    phaseEncoding p =
      Aeson.Encoding.pairs
        (("name" .= p.name) <> ("majors" .= p.majors) <> ("max_major_us" .= p.maxMajorUs))

-- | With the side: 'byQuery' runs per side, see 'steadyEncoding'.
refsSummaryEncoding :: (Side, RefsSummary) -> Encoding
refsSummaryEncoding (side, q) =
  Aeson.Encoding.pairs $
    ("side" .= sideText side)
      <> ("module" .= q.query.moduleRef)
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
    ("side" .= sideText q.side)
      <> ("query" .= q.query)
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
      <> Aeson.Encoding.pair
        "loadavg"
        (Aeson.Encoding.list (loadEncoding . (.loadAvg)) runs)
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
      <> ("side" .= fmap sideText v.side)
      <> ("stat" .= statText v.target.stat)
      <> ("limit_us" .= v.target.limitUs)
      <> ("value_us" .= v.valueUs)
      <> ("max_us" .= v.maxUs)
      <> ("met" .= v.met)
