module Bench.GcLogTest (tests) where

import Bench.GcLog
  ( GcLog (..),
    GcLogError (..),
    GcRun (..),
    GcSummary (..),
    GenTotal (..),
    Pause (..),
    PauseKind (..),
    PhaseGc (..),
    PhaseMark (..),
    parseGcLog,
    phaseMajors,
    summarizeGc,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Tests run from the package dir.
fixture :: String -> IO GcLog
fixture name = do
  raw <- readFile ("../fixtures/bench/gc-" <> name <> ".txt")
  either (assertFailure . show) pure (parseGcLog raw)

tests :: TestTree
tests =
  testGroup
    "Bench.GcLog"
    ( [ testCase ("parses " <> v <> " " <> gc) $ do
          l <- fixture (v <> "-" <> gc)
          length [() | Pause {kind = Collection} <- l.pauses] @?= 6
          length [() | Pause {kind = Sync} <- l.pauses] @?= syncs gc
          map (\t -> (t.kind, t.gen)) l.totals @?= totalKeys gc
      | v <- ["9.10", "9.12", "9.14"],
        gc <- ["copying", "nonmoving"]
      ]
        <> [ testCase "a collection line" $ do
               l <- fixture "9.14-copying"
               take 1 l.pauses
                 @?= [ Pause
                         { kind = Collection,
                           gen = 0,
                           durationUs = 0,
                           atUs = 1000,
                           liveBytes = Just 665488
                         }
                     ],
             testCase "a sync has no live bytes" $ do
               l <- fixture "9.12-nonmoving"
               [p.liveBytes | p@Pause {kind = Sync} <- l.pauses] @?= [Nothing, Nothing],
             testCase "summary max pauses are read in microseconds" $ do
               l <- fixture "9.14-copying"
               map (.maxPauseUs) l.totals @?= [900, 8100],
             testCase "a sync takes the time of the line before" $ do
               l <- fixture "9.12-nonmoving"
               [p.atUs | p@Pause {kind = Sync} <- l.pauses] @?= [6000, 6000],
             testCase "not a -S file" $
               parseGcLog "hello\n" @?= Left NoGcLines,
             testCase "summary: during and after the done time" $
               summarizeGc [GcRun {doneUs = 10_000, gcLog = sample}]
                 @?= Just
                   GcSummary
                     { collections = 4,
                       majors = 2,
                       maxPauseUs = 40_000,
                       maxMajorPauseUs = 40_000,
                       p99PauseUs = 30_000,
                       maxDuringUs = Just 5_000,
                       maxAfterUs = Just 30_000,
                       majorsAfter = 1,
                       maxMajorAfterUs = Just 30_000,
                       maxLiveAfterBytes = Just 300,
                       pausedUs = 38_000
                     },
             testCase "summary: no pauses" $
               summarizeGc [GcRun {doneUs = 0, gcLog = GcLog {pauses = [], totals = []}}]
                 @?= Nothing,
             testCase "majors by phase: (start, end], minors left out" $
               phaseMajors
                 [ PhaseMark "pages" 0,
                   PhaseMark "sources" 10_000,
                   PhaseMark "pages_idle" 20_000,
                   PhaseMark "stop" 60_000
                 ]
                 sample
                 @?= [ PhaseGc {name = "pages", majors = 1, maxMajorUs = Just 2_000},
                       PhaseGc {name = "sources", majors = 0, maxMajorUs = Nothing},
                       PhaseGc
                         { name = "pages_idle",
                           majors = 1,
                           maxMajorUs = Just 30_000
                         },
                       PhaseGc {name = "stop", majors = 0, maxMajorUs = Nothing}
                     ],
             testCase "majors by phase: the last phase has no end" $
               phaseMajors [PhaseMark "stop" 0] sample
                 @?= [PhaseGc {name = "stop", majors = 2, maxMajorUs = Just 30_000}],
             testCase "majors by phase from the done time add up to majors after" $ do
               let marks = [PhaseMark "census" 10_000, PhaseMark "pages" 30_000]
                   phases = phaseMajors marks sample
                   summary = summarizeGc [GcRun {doneUs = 10_000, gcLog = sample}]
               Just (sum (map (.majors) phases)) @?= fmap (.majorsAfter) summary
           ]
    )
  where
    syncs "nonmoving" = 2
    syncs _ = 0
    totalKeys "nonmoving" = [(Collection, 0), (Collection, 1), (Sync, 1)]
    totalKeys _ = [(Collection, 0), (Collection, 1)]
    pause g d at live =
      Pause {kind = Collection, gen = g, durationUs = d, atUs = at, liveBytes = live}
    sample =
      GcLog
        { pauses =
            [ pause 0 5_000 5_000 (Just 100),
              -- At the done time: during. Its live bytes don't count.
              pause 1 2_000 10_000 (Just 900),
              pause 1 30_000 50_000 (Just 300),
              -- A minor after done: its live bytes don't count either.
              pause 0 1_000 55_000 (Just 5_000)
            ],
          -- The summary saw a longer one than the lines: it wins the max.
          totals = [GenTotal {kind = Collection, gen = 1, count = 2, maxPauseUs = 40_000}]
        }
