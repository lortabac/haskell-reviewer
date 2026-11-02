module Bench.RtsStatsTest (tests) where

import Bench.RtsStats
  ( RtsStats (..),
    RtsStatsError (..),
    checkUserRtsOpts,
    parseRtsStats,
    parseRtsSummary,
  )
import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | A sample @-t ... --machine-readable@ file, trimmed to the keys we
-- read. Real output has many more rows; extra ones are just ignored.
sampleFile :: String
sampleFile =
  "'./haskell-reviewer' +RTS '-t/tmp/rts.txt' '--machine-readable' \n\
  \ [(\"bytes allocated\", \"50144\")\n\
  \ ,(\"num_GCs\", \"1\")\n\
  \ ,(\"mut_cpu_seconds\", \"0.000079\")\n\
  \ ,(\"GC_cpu_seconds\", \"0.000139\")\n\
  \ ,(\"allocated_bytes\", \"50144\")\n\
  \ ,(\"max_live_bytes\", \"44328\")\n\
  \ ,(\"max_mem_in_use_bytes\", \"6291456\")\n\
  \ ]\n"

tests :: TestTree
tests =
  testGroup
    "Bench.RtsStats"
    [ testCase "a normal file parses, seconds become microseconds" $
        case parseRtsStats sampleFile of
          Right s ->
            s
              @?= RtsStats
                { peakMemBytes = 6291456,
                  maxLiveBytes = Just 44328,
                  allocatedBytes = Just 50144,
                  gcCpuUs = Just 139,
                  mutatorCpuUs = Just 79
                }
          Left e -> assertFailure (show e),
      testCase "missing peak key is an error" $
        parseRtsStats " [(\"max_live_bytes\", \"1\")]\n"
          @?= Left (MissingKey "max_mem_in_use_bytes"),
      testCase "peak outside the sane range is rejected" $
        parseRtsStats " [(\"max_mem_in_use_bytes\", \"1\")]\n"
          @?= Left (PeakOutOfRange 1),
      testCase "the summary of a -S file (9.14, copying)" $ do
        raw <- readFile "../fixtures/bench/gc-9.14-copying.txt"
        parseRtsSummary raw
          @?= Right
            RtsStats
              { peakMemBytes = 58 * 1024 * 1024,
                maxLiveBytes = Just 20740984,
                allocatedBytes = Just 163884200,
                gcCpuUs = Just 96000,
                mutatorCpuUs = Just 19000
              },
      testCase "the summary of a -S file (9.10, nonmoving)" $ do
        raw <- readFile "../fixtures/bench/gc-9.10-nonmoving.txt"
        fmap (.peakMemBytes) (parseRtsSummary raw) @?= Right (108 * 1024 * 1024),
      testCase "a -S file with no summary is an error" $
        parseRtsSummary "  1 2 3 0.0 0.0 0.0 0.0 0 0 (Gen:  0)\n"
          @?= Left (MissingKey "MiB total memory in use"),
      testCase "user RTS options: GC ones pass" $
        checkUserRtsOpts ["--nonmoving-gc", "-I0", "-A32m", "-N4", "-c"] @?= Right (),
      testCase "user RTS options: stats file ones are refused" $
        mapM_
          (\o -> assertBool o (isLeft (checkUserRtsOpts ["-I0", o])))
          ["-s", "-S/tmp/x", "-t", "-tfile", "--machine-readable", "-RTS", "+RTS"],
      testCase "no list literal at all is an error" $
        case parseRtsStats "garbage, no brackets here" of
          Left (CannotParse _) -> pure ()
          other -> assertFailure ("expected CannotParse, got " <> show other)
    ]
