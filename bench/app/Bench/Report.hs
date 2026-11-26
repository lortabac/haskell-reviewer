-- | The benchmark's report, and the stdout table. The JSON is in
-- "Bench.Report.Json".
module Bench.Report
  ( -- * Types
    Report (..),
    Settings (..),
    ToolInfo (..),
    ProjectReport (..),
    SideCount (..),
    FallbackCheck (..),
    NavPattern (..),
    SideTimes (..),
    TextSummary (..),
    SlowPage (..),

    -- * Targets
    projectVerdicts,

    -- * Rendering
    renderTable,

    -- * For "Bench.Report.Json"
    projectSideTimes,
    onDemandTimes,
    searchAfterTimes,
    refsAfter,
    textAfter,
    textFirsts,
    textPagesOf,
    textByQuery,
    refsDuringTimes,
    firstTimes,
    peakStartup,
    peakSteady,
    hwms,
    slowPages,
    navPatternText,
    routeText,
    gcSummary,
    runPhases,
    summaryUs,
    latencyUs,
    maxOf,
    roleText,
  )
where

import Bench.Fallbacks
  ( Census,
    Diff (..),
    FallbackVerdict (..),
    Lookup (..),
    NotChecked (..),
    Strictness,
    categoryText,
    countCensus,
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
import Bench.Mode (Mode (..), Side (..), diffSide, modeText, sideText)
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
  ( OnDemand (..),
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
    sideVerdicts,
    textSearchAfterMedian,
    textSearchAfterP95,
    uiUsable,
    urlAndAccepting,
    verdictOf,
  )
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
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
    -- | One per project and mode, only those that ran to completion. A
    -- failed one (bad pin, a run that errored) is reported on stderr and
    -- left out here; see "Main".
    projects :: [ProjectReport],
    -- | Things to say once, e.g. a mode a project can't run. In the
    -- table and the JSON (@notes@).
    notes :: [Text]
  }

-- | The CLI settings that change the numbers.
data Settings = Settings
  { modes :: [Mode],
    runs :: Int,
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

-- | One project's runs in one mode. Warm-up runs are already dropped.
-- Any list may be empty (@--runs 0@, @--census-only@, ...).
data ProjectReport = ProjectReport
  { project :: Project,
    mode :: Mode,
    -- | @--diff@'s argument; 'Nothing' in browse mode.
    diffSpec :: Maybe Text,
    -- | Per side, head first.
    sides :: NonEmpty SideCount,
    -- | Listed modules, and those we parse (with a slot), over all
    -- sides.
    modulesListed :: Int,
    modulesParsed :: Int,
    samples :: [Sample],
    startup :: [StartupRun],
    steady :: [SteadyRun],
    searchDuring :: [SearchDuringRun],
    -- | What the refs runs asked (none for an extra project).
    refsQueries :: [RefsQuery],
    refsDuring :: [RefsDuringRun],
    -- | Browse mode only (D-357).
    fallbacks :: Maybe FallbackCheck,
    -- | Things the human should look at, e.g. a text-only count that
    -- isn't the expected one.
    warnings :: [Text]
  }

-- | One side's module tree.
data SideCount = SideCount
  { side :: Side,
    listed :: Int,
    parsed :: Int
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

-- | One side's samples over all steady runs: the per-side split for the
-- targets, the table and the JSON, in one place. Times in microseconds.
data SideTimes = SideTimes
  { side :: Side,
    pages :: [Integer],
    idlePages :: [Integer],
    sources :: [Integer],
    idleSources :: [Integer],
    search :: [Integer],
    refs :: [RefsTime],
    -- | Text searches after the load, limit 8.
    text :: [TextTime],
    -- | The first text search of each run: it loads the store.
    textFirst :: [TextTime],
    -- | Text searches with the results page's limit.
    textPages :: [TextTime]
  }

-- | 'SideTimes' for each side of the project's mode, head first.
projectSideTimes :: ProjectReport -> [SideTimes]
projectSideTimes pr = map one (NE.toList (NE.map (.side) pr.sides))
  where
    one s =
      SideTimes
        { side = s,
          pages = pageUs s (.pages),
          idlePages = pageUs s (.idlePages),
          sources = pageUs s (.sources),
          idleSources = pageUs s (.idleSources),
          search = [q.us | r <- pr.steady, q <- r.searchTimes, q.side == s],
          refs = [t | t <- refsAfter pr, t.side == s],
          text = [t | t <- textAfter pr, t.side == s],
          textFirst = [t | t <- textFirsts pr, t.side == s],
          textPages = [t | t <- textPagesOf pr, t.side == s]
        }
    pageUs s field = [p.us | r <- pr.steady, p <- field r, p.side == s]

-- * Targets

-- | The targets this project's runs can say something about. The
-- start-up ones have no side; the others one verdict per side (D-357).
projectVerdicts :: ProjectReport -> [Verdict]
projectVerdicts pr =
  catMaybes
    [ verdictOf urlAndAccepting (map (.serveUs) pr.startup),
      verdictOf (uiUsable pr.project.role) (map (.uiUsableUs) pr.startup),
      verdictOf onDemandModule (onDemandTimes pr)
    ]
    <> bySide navigation (.pages)
    <> bySide navigationIdle (.idlePages)
    <> bySide navigationSource (.sources)
    <> bySide navigationSourceIdle (.idleSources)
    <> bySide searchAfterMedian (.search)
    <> bySide searchAfterP95 (.search)
    <> bySide refsAfterP95 (map (.us) . (.refs))
    <> bySide textSearchAfterMedian (map (.us) . (.text))
    <> bySide textSearchAfterP95 (map (.us) . (.text))
  where
    bySide t f = sideVerdicts t [(diffSide st.side, f st) | st <- projectSideTimes pr]

onDemandTimes :: ProjectReport -> [Integer]
onDemandTimes pr = [o.us | r <- pr.startup, Just o <- [r.onDemand], o.status == Counted]

searchAfterTimes :: ProjectReport -> [Integer]
searchAfterTimes pr = map (.us) (concatMap (.searchTimes) pr.steady)

refsAfter :: ProjectReport -> [RefsTime]
refsAfter pr = concatMap (.refsTimes) pr.steady

-- | Text searches with the box's limit, after the first (the load).
textAfter :: ProjectReport -> [TextTime]
textAfter pr = concatMap (.textTimes) pr.steady

-- | The first text search of each run and side: it loads the store.
textFirsts :: ProjectReport -> [TextTime]
textFirsts pr = concatMap (.textFirst) pr.steady

-- | Text searches with the results page's limit.
textPagesOf :: ProjectReport -> [TextTime]
textPagesOf pr = concatMap (.textPages) pr.steady

-- | Latency over all samples of one query on one side; counts from the
-- last sample (they don't change after indexing).
data TextSummary = TextSummary
  { side :: Side,
    query :: Text,
    latency :: Latency,
    total :: Int,
    files :: Int,
    sent :: Int,
    wireBytes :: Int
  }

-- | One summary per side and query, in first-seen order.
textByQuery :: [TextTime] -> [TextSummary]
textByQuery ts = mapMaybe one (nub [(t.side, t.query) | t <- ts])
  where
    one (s, q) = do
      mine <- NE.nonEmpty [t | t <- ts, t.side == s, t.query == q]
      let lastOne = NE.last mine
      Just
        TextSummary
          { side = s,
            query = q,
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
      <> map ("note: " <>) report.notes
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
          "modes: "
            <> Text.intercalate "," (map modeText st.modes)
            <> "; runs: "
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
          "Targets (SPEC §10.1, D-208, D-303, D-308, D-357; a MISS never fails the \
          \bench, §10.3):"
        ]
          <> ls
    verdictLines pr =
      ["  " <> renderVerdict (verdictName pr v) v | v <- projectVerdicts pr]
    smallRunsNote
      | not st.censusOnly && st.runs < 3 = ["", "with --runs < 3, medians mean little"]
      | otherwise = []

-- | A @--diff@ argument with short shas, for the table: the JSON has
-- them in full.
shortSpec :: Text -> Text
shortSpec = Text.intercalate ".." . map (Text.take 8) . Text.splitOn ".."

-- | "base:" before a ref in diff mode, nothing in browse mode.
sidePrefix :: Side -> Text
sidePrefix = maybe "" ((<> ":") . sideText) . diffSide

-- | "pandoc", "pandoc range", "pandoc range base": the project, its mode
-- if not browse, the side if any.
verdictName :: ProjectReport -> Verdict -> Text
verdictName pr v = Text.unwords ([pr.project.name] <> modeWord <> sideWord)
  where
    modeWord = [modeText pr.mode | pr.mode /= Browse]
    sideWord = maybe [] (pure . sideText) v.side

projectSection :: FilePath -> ProjectReport -> [Text]
projectSection fallbacksFile pr =
  [ "",
    "== "
      <> pr.project.name
      <> " ("
      <> roleText pr.project.role
      <> ", "
      <> modeText pr.mode
      <> maybe "" (\d -> " --diff " <> shortSpec d) pr.diffSpec
      <> ", "
      <> showT pr.modulesListed
      <> " modules listed, "
      <> showT pr.modulesParsed
      <> " parsed"
      <> sidesText
      <> ")"
  ]
    <> startupLines pr
    <> steadyLines pr
    <> searchDuringLines pr
    <> refsDuringLines pr
    <> maybe [] (fallbackLines fallbacksFile) pr.fallbacks
    <> memoryLines pr
    <> map ("warning: " <>) pr.warnings
  where
    sidesText = case pr.mode of
      Browse -> ""
      _ ->
        "; "
          <> Text.intercalate
            ", "
            [ sideText c.side <> " " <> showT c.listed <> "/" <> showT c.parsed
            | c <- NE.toList pr.sides
            ]

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
        <> maybeMedian ", session " (.parts.sessionUs)
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
        <> maybeMedian ", diff-files (not in UI usable) " (.parts.diffFilesUs)
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
        Text.intercalate
          ", "
          [sampleKindText s.kind <> " " <> sampleSide s <> s.ref | s <- pr.samples]
      sampleSide s = sidePrefix s.side
      maybeMedian label field =
        maybe
          ""
          (\x -> label <> fmtMs x.median)
          (summaryUs (mapMaybe field (NE.toList runs)))

steadyLines :: ProjectReport -> [Text]
steadyLines pr = case pr.steady of
  [] -> []
  runs ->
    ["steady, " <> plural (length runs) "run" <> " after indexing, ms:"]
      <> concatMap sideLines (projectSideTimes pr)
      <> slowLines
      <> gcLines
      <> phaseLines
      <> gcWarnings
  where
    -- The same lines for each side; " (base)" after the name in diff
    -- mode.
    sideLines st =
      [ "  navigation back to back" <> sfx <> ": " <> lat st.pages,
        "  navigation with idle gaps" <> sfx <> ": " <> lat st.idlePages,
        "  source back to back" <> sfx <> ": " <> lat st.sources,
        "  source with idle gaps" <> sfx <> ": " <> lat st.idleSources,
        "  search" <> sfx <> ": " <> lat st.search
      ]
        <> refsLines
        <> textLines
      where
        sfx = maybe "" (\d -> " (" <> sideText d <> ")") (diffSide st.side)
        lat = maybe "-" latencyText . latencyUs
        refsLines
          | null pr.refsQueries = []
          | otherwise =
              ("  refs" <> sfx <> ": " <> lat (map (.us) st.refs))
                : map refsQueryLine (byQuery pr.refsQueries st.refs)
        textLines = case st.text of
          [] -> []
          ts ->
            [ "  text search (limit "
                <> showT textLimitBox
                <> ", after the load)"
                <> sfx
                <> ": "
                <> lat (map (.us) ts),
              "    first (loads the store): "
                <> Text.intercalate
                  ", "
                  [fmtMs (fromInteger t.us) | t <- st.textFirst]
                <> "; page (limit "
                <> showT textLimitPage
                <> "): "
                <> maybe "-" shortLatency (latencyUs (map (.us) st.textPages))
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
                 | q <- textByQuery st.textPages
                 ]
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
        <> sidePrefix p.side
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
  { side :: Side,
    pattern :: NavPattern,
    route :: Route,
    ref :: Text,
    us :: Integer
  }

-- | Pages over the navigation limit, worst first.
slowPages :: ProjectReport -> [SlowPage]
slowPages pr =
  sortOn
    (Down . (.us))
    ( [ SlowPage p.side BackToBack p.route p.ref p.us
      | p <- concatMap (\r -> r.pages <> r.sources) pr.steady,
        slow p
      ]
        <> [ SlowPage p.side IdleGaps p.route p.ref p.us
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
