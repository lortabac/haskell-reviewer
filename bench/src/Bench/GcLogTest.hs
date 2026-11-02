module Bench.GcLogTest (tests) where

import Bench.GcLog
  ( GcLog (..),
    GcLogError (..),
    GcRun (..),
    GcSummary (..),
    GenTotal (..),
    Pause (..),
    PauseKind (..),
    parseGcLog,
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
                 @?= [Pause {kind = Collection, gen = 0, durationUs = 0, atUs = 1000}],
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
                     { collections = 3,
                       majors = 2,
                       maxPauseUs = 40_000,
                       maxMajorPauseUs = 40_000,
                       p99PauseUs = 30_000,
                       maxDuringUs = Just 5_000,
                       maxAfterUs = Just 30_000,
                       majorsAfter = 1,
                       maxMajorAfterUs = Just 30_000,
                       pausedUs = 37_000
                     },
             testCase "summary: no pauses" $
               summarizeGc [GcRun {doneUs = 0, gcLog = GcLog {pauses = [], totals = []}}]
                 @?= Nothing
           ]
    )
  where
    syncs "nonmoving" = 2
    syncs _ = 0
    totalKeys "nonmoving" = [(Collection, 0), (Collection, 1), (Sync, 1)]
    totalKeys _ = [(Collection, 0), (Collection, 1)]
    sample =
      GcLog
        { pauses =
            [ Pause {kind = Collection, gen = 0, durationUs = 5_000, atUs = 5_000},
              Pause {kind = Collection, gen = 1, durationUs = 2_000, atUs = 10_000},
              Pause {kind = Collection, gen = 1, durationUs = 30_000, atUs = 50_000}
            ],
          -- The summary saw a longer one than the lines: it wins the max.
          totals = [GenTotal {kind = Collection, gen = 1, count = 2, maxPauseUs = 40_000}]
        }
