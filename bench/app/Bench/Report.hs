-- | Builds the benchmark's two outputs: the stdout table, and the JSON
-- report (schema 2).
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
import Bench.GcLog (GcRun (..), GcSummary (..), renderGcLogError, summarizeGc)
import Bench.IndexLog (IndexDone (..))
import Bench.Machine (LoadAvg (..), MachineInfo (..))
import Bench.ModuleList (OnDemandStatus (..), Sample (..), sampleKindText)
import Bench.Projects (Project (..), Role (..))
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
import Bench.Steady (PageTime (..), SteadyRun (..))
import Bench.Targets
  ( Target (..),
    Verdict (..),
    memoryBudgetBytes,
    navigation,
    navigationIdle,
    onDemandModule,
    renderVerdict,
    searchAfterMedian,
    searchAfterP95,
    statText,
    uiUsable,
    urlAndAccepting,
    verdict,
  )
import Data.Aeson ((.=))
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Key qualified as Aeson.Key
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
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
      medianOf searchAfterMedian (searchAfterTimes pr),
      p95Of searchAfterP95 (searchAfterTimes pr)
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
            <> " search-during; RTS: "
            <> (if null st.rtsOpts then "product defaults" else Text.pack (unwords st.rtsOpts))
    -- A high RLIMIT_NOFILE makes every process spawn slower (D-85).
    nofileLine =
      "RLIMIT_NOFILE: soft="
        <> maybe "unlimited" showT report.machine.nofileSoft
        <> " hard="
        <> maybe "unlimited" showT report.machine.nofileHard
    targetLines = case concatMap verdictLines report.projects of
      [] -> []
      ls -> ["", "Targets (SPEC §10.1, D-208; a MISS never fails the bench, §10.3):"] <> ls
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
      "  search: " <> maybe "-" latencyText (latencyUs (searchAfterTimes pr))
    ]
      <> slowLines
      <> gcLines
      <> gcWarnings
  where
    pages = concatMap (.pages) pr.steady
    idle = concatMap (.idlePages) pr.steady
    slow = slowPages pr
    slowLines
      | null slow = []
      | otherwise =
          ("  pages over 100 ms: " <> showT (length slow))
            : map slowLine (take 10 slow)
    slowLine p =
      "    " <> navPatternText p.pattern <> " " <> p.ref <> " " <> fmtMs (fromInteger p.us)
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
            <> maybe "-" (fmtMs . fromInteger) g.maxMajorAfterUs,
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
      [ "memory, MiB, max over runs: max_mem_in_use "
          <> fmtMiB peak
          <> ", max_live "
          <> maybe "-" fmtMiB (maxOf (mapMaybe (.maxLiveBytes) (NE.toList rs)))
          <> ", idle RSS "
          <> maybe "-" fmtMiB (maxOf (mapMaybe (.idleRssBytes) pr.startup))
          <> ", peak RSS "
          <> maybe "-" fmtMiB (maxOf (hwms pr))
      ]
        <> budgetLine
      where
        peak = maximum (NE.map (.peakMemBytes) rs)
        budgetLine = case memoryBudgetBytes pr.project.role of
          Nothing -> []
          Just b ->
            [ "  §10.2 budget "
                <> fmtMiB b
                <> " (max_mem_in_use): "
                <> (if peak < b then "ok" else "MISS → §10.3: record, decide, log")
            ]

-- | Peak RSS of every run that has one.
hwms :: ProjectReport -> [Integer]
hwms pr = mapMaybe (.hwmBytes) pr.startup <> mapMaybe (.hwmBytes) pr.steady

-- | A page over the navigation limit.
data SlowPage = SlowPage
  { pattern :: NavPattern,
    ref :: Text,
    us :: Integer
  }

-- | Pages over the navigation limit, worst first.
slowPages :: ProjectReport -> [SlowPage]
slowPages pr =
  sortOn
    (Down . (.us))
    ( [SlowPage BackToBack p.ref p.us | p <- concatMap (.pages) pr.steady, slow p]
        <> [SlowPage IdleGaps p.ref p.us | p <- concatMap (.idlePages) pr.steady, slow p]
    )
  where
    slow p = p.us >= navigation.limitUs

navPatternText :: NavPattern -> Text
navPatternText BackToBack = "back-to-back"
navPatternText IdleGaps = "idle"

-- | Pooled over the steady runs whose @-S@ file had GC lines.
gcSummary :: [SteadyRun] -> Maybe GcSummary
gcSummary runs = summarizeGc [GcRun r.doneUs g | r <- runs, Just (Right g) <- [r.gc]]

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

-- * JSON (schema 2)

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
    ("schema" .= (2 :: Int))
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
      <> Aeson.Encoding.pair "startup_runs" (Aeson.Encoding.list startupEncoding pr.startup)
      <> Aeson.Encoding.pair "summary" (startupSummaryEncoding pr.startup)
      <> Aeson.Encoding.pair "on_demand" (maybeLatency (latencyUs (onDemandTimes pr)))
      <> Aeson.Encoding.pair "steady" (steadyEncoding pr)
      <> Aeson.Encoding.pair "search_during" (searchDuringEncoding pr.searchDuring)
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
      <> Aeson.Encoding.pair "search_after" (maybeLatency (latencyUs (searchAfterTimes pr)))
      <> Aeson.Encoding.pair "gc" (nullable gcEncoding (gcSummary pr.steady))
      <> Aeson.Encoding.pair "slow_pages" (Aeson.Encoding.list slowEncoding (slowPages pr))
  where
    pages = concatMap (.pages) pr.steady
    idle = concatMap (.idlePages) pr.steady
    runEncoding r =
      Aeson.Encoding.pairs $
        ("done_us" .= r.doneUs)
          <> Aeson.Encoding.pair "done" (doneEncoding r.done)
          <> ("pages" .= length r.pages)
          <> ("pages_idle" .= length r.idlePages)
          <> ("searches" .= length r.searchTimes)
          <> ("peak_mem_bytes" .= r.rts.peakMemBytes)
          <> ("max_live_bytes" .= r.rts.maxLiveBytes)
          <> ("rss_bytes" .= r.rssBytes)
          <> ("hwm_bytes" .= r.hwmBytes)
          <> Aeson.Encoding.pair "gc" (nullable gcEncoding (gcSummary [r]))
          <> ("gc_log_error" .= gcLogError r)
          <> Aeson.Encoding.pair "loadavg" (loadEncoding r.loadAvg)
    gcLogError r = case r.gc of
      Just (Left e) -> Just (renderGcLogError e)
      _ -> Nothing
    slowEncoding p =
      Aeson.Encoding.pairs
        (("pattern" .= navPatternText p.pattern) <> ("ref" .= p.ref) <> ("us" .= p.us))

gcEncoding :: GcSummary -> Encoding
gcEncoding g =
  Aeson.Encoding.pairs $
    ("collections" .= g.collections)
      <> ("majors" .= g.majors)
      <> ("majors_after" .= g.majorsAfter)
      <> ("max_major_after_us" .= g.maxMajorAfterUs)
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
      <> ("max_live_bytes" .= maxOf (mapMaybe (.maxLiveBytes) rtss))
      <> ("idle_rss_bytes" .= maxOf (mapMaybe (.idleRssBytes) pr.startup))
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
