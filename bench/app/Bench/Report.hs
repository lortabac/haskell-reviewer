-- | Builds the benchmark's two outputs: the stdout table, and the JSON
-- report (schema 1).
module Bench.Report
  ( -- * Types
    Report (..),
    ToolInfo (..),
    ProjectRun (..),

    -- * Rendering
    renderTable,
    encodeReportJson,
  )
where

import Bench.Machine (MachineInfo (..))
import Bench.Projects (Project (..), Role (..))
import Bench.Run (FileSource (..), RunResult (..))
import Bench.Stats (Summary (..), summarize)
import Data.Aeson ((.=))
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
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
    runs :: Int,
    warmup :: Int,
    tool :: ToolInfo,
    machine :: MachineInfo,
    -- | Only the projects that ran to completion. A project that failed
    -- (bad pin, a run that errored) is reported on stderr and left out
    -- here; see "Main".
    projects :: [ProjectRun]
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

-- | One project's timed runs. Warm-up runs are already dropped by the
-- time this is built.
--
-- 'filesCount' and 'source' live on each 'RunResult', not here, since
-- they come from the same @api\/files\/tree@ check as the timings; we
-- read them from 'NE.head' below, trusting they don't change run to
-- run (same project, same pinned commit).
data ProjectRun = ProjectRun
  { project :: Project,
    runResults :: NonEmpty RunResult
  }

-- * Rendering

-- | SPEC §10.1: URL printed and accepting connections, under this.
targetLimitUs :: Integer
targetLimitUs = 300000

-- | The table printed on stdout.
renderTable :: Report -> Text
renderTable report =
  Text.unlines $
    [heading, nofileLine, "", columnsLine]
      <> map projectRow report.projects
      <> [""]
      <> map targetLine report.projects
      <> smallRunsNote
  where
    heading =
      "haskell-reviewer bench: "
        <> Text.pack (show report.runs)
        <> " runs + "
        <> Text.pack (show report.warmup)
        <> " warm-up, median [min-max]"
    -- A high RLIMIT_NOFILE makes every process spawn slower (D-85):
    -- the product closes every fd up to the limit at start-up.
    nofileLine =
      "RLIMIT_NOFILE: soft="
        <> maybe "unlimited" showInt report.machine.nofileSoft
        <> " hard="
        <> maybe "unlimited" showInt report.machine.nofileHard
    showInt = Text.pack . show
    columnsLine =
      Text.intercalate
        " | "
        [ "project",
          "files",
          "URL ms",
          "serve ms",
          "files ms",
          "peak MiB",
          "max live MiB",
          "stop ms"
        ]
    projectRow pr =
      Text.intercalate
        " | "
        [ pr.project.name,
          Text.pack (show (NE.head pr.runResults).filesCount),
          fmt1 (msSummary (.urlUs) pr.runResults),
          fmt1 (msSummary (.serveUs) pr.runResults),
          fmt1 (msSummary (.filesUs) pr.runResults),
          fmt1 (mibSummary (.peakMemBytes) pr.runResults),
          maybe "-" fmt1 (maybeMibSummary (.maxLiveBytes) pr.runResults),
          fmt1 (msSummary (.stopUs) pr.runResults)
        ]
    targetLine pr =
      "§10.1 URL + accepting < 300 ms (median serve): "
        <> pr.project.name
        <> ": "
        <> (if met then "ok" else "MISS")
        <> " ("
        <> fmtDouble serveSummary.median
        <> " ms)"
      where
        serveSummary = msSummary (.serveUs) pr.runResults
        met = serveSummary.median < usToMs targetLimitUs
    smallRunsNote
      | report.runs < 3 = ["", "with --runs < 3, medians mean little"]
      | otherwise = []

-- | @median [min-max]@, one decimal place.
fmt1 :: Summary -> Text
fmt1 s = fmtDouble s.median <> " [" <> fmtDouble s.min <> "-" <> fmtDouble s.max <> "]"

fmtDouble :: Double -> Text
fmtDouble x = Text.pack (printf "%.1f" x)

usToMs :: Integer -> Double
usToMs us = fromInteger us / 1000

bytesToMiB :: Integer -> Double
bytesToMiB b = fromInteger b / (1024 * 1024)

-- | Summarise a microseconds field, in milliseconds.
msSummary :: (RunResult -> Integer) -> NonEmpty RunResult -> Summary
msSummary field = summarize . NE.map (usToMs . field)

-- | Summarise a bytes field, in MiB.
mibSummary :: (RunResult -> Integer) -> NonEmpty RunResult -> Summary
mibSummary field = summarize . NE.map (bytesToMiB . field)

-- | Like 'mibSummary', but the field may be missing on some runs.
-- 'Nothing' if it's missing on every run.
maybeMibSummary :: (RunResult -> Maybe Integer) -> NonEmpty RunResult -> Maybe Summary
maybeMibSummary field runs = nonEmptySummary bytesToMiB (mapMaybe field (NE.toList runs))

nonEmptySummary :: (Integer -> Double) -> [Integer] -> Maybe Summary
nonEmptySummary _ [] = Nothing
nonEmptySummary conv (x : xs) = Just (summarize (NE.map conv (x :| xs)))

-- * JSON (schema 1)

-- | Encode the whole report. Built by hand with 'Data.Aeson.Encoding',
-- like "HaskellReviewer.Effects.Trace", to keep the key order fixed and
-- readable.
encodeReportJson :: Report -> LBS.ByteString
encodeReportJson report = Aeson.Encoding.encodingToLazyByteString (reportEncoding report)

reportEncoding :: Report -> Aeson.Encoding.Encoding
reportEncoding report =
  Aeson.Encoding.pairs $
    ("schema" .= (1 :: Int))
      <> ("started" .= iso8601Show report.started)
      <> ("label" .= report.label)
      <> Aeson.Encoding.pair "settings" settingsEncoding
      <> Aeson.Encoding.pair "tool" (toolEncoding report.tool)
      <> Aeson.Encoding.pair "machine" (machineEncoding report.machine)
      <> Aeson.Encoding.pair "projects" projectsEncoding
  where
    settingsEncoding =
      Aeson.Encoding.pairs $
        ("runs" .= report.runs) <> ("warmup" .= report.warmup)
    projectsEncoding = Aeson.Encoding.list projectEncoding report.projects

toolEncoding :: ToolInfo -> Aeson.Encoding.Encoding
toolEncoding tool =
  Aeson.Encoding.pairs $
    ("exe" .= tool.exe)
      <> ("version" .= tool.version)
      <> ("git_sha" .= tool.gitSha)
      <> ("git_dirty" .= tool.gitDirty)
      <> ("ghc" .= tool.ghc)
      <> ("rts_way" .= tool.rtsWay)

machineEncoding :: MachineInfo -> Aeson.Encoding.Encoding
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

projectEncoding :: ProjectRun -> Aeson.Encoding.Encoding
projectEncoding pr =
  Aeson.Encoding.pairs $
    ("name" .= pr.project.name)
      <> ("role" .= roleText pr.project.role)
      <> ("sha" .= pr.project.sha)
      <> ("files" .= (NE.head pr.runResults).filesCount)
      <> ("source" .= sourceText (NE.head pr.runResults).source)
      <> Aeson.Encoding.pair "runs" runsEncoding
      <> Aeson.Encoding.pair "summary" (summaryMapEncoding pr.runResults)
      <> Aeson.Encoding.pair "targets" (Aeson.Encoding.list targetEncoding [pr])
  where
    runsEncoding = Aeson.Encoding.list runEncoding (NE.toList pr.runResults)

roleText :: Role -> Text
roleText Small = "small"
roleText Typical = "typical"
roleText Large = "large"
roleText Extra = "extra"

sourceText :: FileSource -> Text
sourceText FromGit = "git"
sourceText FromDisk = "disk"

runEncoding :: RunResult -> Aeson.Encoding.Encoding
runEncoding r =
  Aeson.Encoding.pairs $
    ("url_us" .= r.urlUs)
      <> ("serve_us" .= r.serveUs)
      <> ("files_us" .= r.filesUs)
      <> ("startup_span_us" .= r.startupSpanUs)
      <> ("files_span_us" .= r.filesSpanUs)
      <> ("stop_us" .= r.stopUs)
      <> ("exit_code" .= r.exitCode)
      <> ("peak_mem_bytes" .= r.peakMemBytes)
      <> ("max_live_bytes" .= r.maxLiveBytes)
      <> ("allocated_bytes" .= r.allocatedBytes)
      <> ("gc_cpu_us" .= r.gcCpuUs)
      <> ("mutator_cpu_us" .= r.mutatorCpuUs)

-- | One 'Summary' per run field, in its own raw unit (microseconds or
-- bytes: whatever 'runEncoding' uses), keyed by that field's JSON name.
summaryMapEncoding :: NonEmpty RunResult -> Aeson.Encoding.Encoding
summaryMapEncoding runs =
  Aeson.Encoding.pairs (foldMap toPair fields)
  where
    toPair (name, Nothing) = Aeson.Encoding.pair name Aeson.Encoding.null_
    toPair (name, Just s) = Aeson.Encoding.pair name (summaryEncoding s)
    fields =
      [ ("url_us", Just (rawSummary (.urlUs))),
        ("serve_us", Just (rawSummary (.serveUs))),
        ("files_us", Just (rawSummary (.filesUs))),
        ("stop_us", Just (rawSummary (.stopUs))),
        ("peak_mem_bytes", Just (rawSummary (.peakMemBytes))),
        ("max_live_bytes", rawMaybeSummary (.maxLiveBytes)),
        ("allocated_bytes", rawMaybeSummary (.allocatedBytes)),
        ("gc_cpu_us", rawMaybeSummary (.gcCpuUs)),
        ("mutator_cpu_us", rawMaybeSummary (.mutatorCpuUs)),
        ("startup_span_us", rawMaybeSummary (.startupSpanUs)),
        ("files_span_us", rawMaybeSummary (.filesSpanUs))
      ]
    rawSummary field = summarize (NE.map (fromInteger . field) runs)
    rawMaybeSummary field = nonEmptySummary fromInteger (mapMaybe field (NE.toList runs))

summaryEncoding :: Summary -> Aeson.Encoding.Encoding
summaryEncoding s =
  Aeson.Encoding.pairs $
    ("median" .= s.median) <> ("min" .= s.min) <> ("max" .= s.max)

targetEncoding :: ProjectRun -> Aeson.Encoding.Encoding
targetEncoding pr =
  Aeson.Encoding.pairs $
    ("name" .= ("url_and_accepting" :: Text))
      <> ("limit_us" .= targetLimitUs)
      <> ("median_us" .= medianUs)
      <> ("met" .= (medianUs < fromInteger targetLimitUs))
  where
    medianUs = (summarize (NE.map (fromInteger . (.serveUs)) pr.runResults)).median
