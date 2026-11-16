module Bench.TargetsTest (tests) where

import Bench.Projects (Role (..))
import Bench.Targets
  ( Bound (..),
    Stat (..),
    Target (..),
    Verdict (..),
    navigation,
    navigationSource,
    navigationSourceIdle,
    refsAfterP95,
    renderVerdict,
    searchAfterMedian,
    uiUsable,
    urlAndAccepting,
    verdict,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Targets"
    [ testCase "< 300 ms: exactly 300 ms is a miss" $ do
        (verdict urlAndAccepting 299_999 Nothing).met @?= True
        (verdict urlAndAccepting 300_000 Nothing).met @?= False,
      testCase "≤ 30 ms: exactly 30 ms is ok" $ do
        (verdict searchAfterMedian 30_000 Nothing).met @?= True
        (verdict searchAfterMedian 30_001 Nothing).met @?= False,
      testCase "UI usable: 10 s for large, 1 s otherwise" $ do
        (uiUsable Large).limitUs @?= 10_000_000
        (uiUsable Typical).limitUs @?= 1_000_000
        (uiUsable Small).limitUs @?= 1_000_000,
      testCase "an ok line" $
        renderVerdict "pandoc" (verdict (uiUsable Typical) 312_400 (Just 340_100))
          @?= "§10.1 UI usable < 1 s (median): pandoc: ok (312.4 ms; max 340.1)",
      testCase "a MISS line" $
        renderVerdict "wire-server" (verdict navigation 120_000 Nothing)
          @?= "§10.1 navigation < 100 ms (back to back) (p95): wire-server: MISS (120.0 ms) \
              \→ §10.3: record, decide, log",
      testCase "source targets: p95 < 100 ms" $
        mapM_
          ( \t -> do
              (t.stat, t.bound, t.limitUs) @?= (P95, Below, 100_000)
              (verdict t 99_999 Nothing).met @?= True
              (verdict t 100_000 Nothing).met @?= False
          )
          [navigationSource, navigationSourceIdle],
      testCase "refs after indexing: p95 < 300 ms" $ do
        (refsAfterP95.stat, refsAfterP95.bound) @?= (P95, Below)
        (verdict refsAfterP95 299_999 Nothing).met @?= True
        (verdict refsAfterP95 300_000 Nothing).met @?= False,
      testCase "a refs line" $
        renderVerdict "wire-server" (verdict refsAfterP95 216_000 (Just 250_000))
          @?= "D-303 find references after indexing < 300 ms (p95): wire-server: ok \
              \(216.0 ms; max 250.0)"
    ]
