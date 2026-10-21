module Bench.RtsStatsTest (tests) where

import Bench.RtsStats (RtsStats (..), RtsStatsError (..), parseRtsStats)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
      testCase "no list literal at all is an error" $
        case parseRtsStats "garbage, no brackets here" of
          Left (CannotParse _) -> pure ()
          other -> assertFailure ("expected CannotParse, got " <> show other)
    ]
