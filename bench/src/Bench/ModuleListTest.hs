module Bench.ModuleListTest (tests) where

import Bench.ModuleList
  ( IdleStep (..),
    ModuleItem (..),
    OnDemandStatus (..),
    OnDemandTimes (..),
    Queued (..),
    Sample (..),
    SampleKind (..),
    Sized (..),
    Source (..),
    idlePlan,
    onDemandStatus,
    parseModuleList,
    pickSamples,
    queueOrder,
  )
import Data.ByteString qualified as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Tests run from the package dir.
modulesFixture :: IO [ModuleItem]
modulesFixture = do
  body <- BS.readFile "../fixtures/api/modules.json"
  either (assertFailure . show) pure (parseModuleList body)

tests :: TestTree
tests =
  testGroup
    "Bench.ModuleList"
    [ testCase "every module of the fixture, in tree order, loose ones last" $ do
        ms <- modulesFixture
        map (.ref) ms
          @?= [ "Foo",
                "Gen",
                "Gone",
                "Lexer",
                "Linked",
                "Lit",
                "Paths_pkg",
                "Sig",
                "pkg:exe:app:Main",
                "pkg:exe:outside:Main",
                "pkg:test:spec:Main",
                "Tests",
                "pkg:bench:slow:Main",
                "pkg/src/Lexer.hs",
                "Tool",
                "scripts/build-docs.hs"
              ],
      testCase "sources" $ do
        ms <- modulesFixture
        map (.source) (take 5 ms)
          @?= [ Parsed "pkg/src/Foo.hs",
                NoSource "autogen",
                NoSource "missing",
                Generated,
                NoSource "symlink"
              ]
        lookup "pkg:test:spec:Main" [(m.ref, m.source) | m <- ms] @?= Just PreprocessedByFlags,
      testCase "queue order: only the parsed ones" $ do
        ms <- modulesFixture
        map (.ref) (queueOrder ms)
          @?= [ "Foo",
                "Lit",
                "Sig",
                "pkg:exe:app:Main",
                "Tests",
                "pkg:bench:slow:Main",
                "pkg/src/Lexer.hs",
                "Tool",
                "scripts/build-docs.hs"
              ],
      testCase "samples: last, largest, middle" $
        pickSamples (sized [("A", 10), ("B", 99), ("C", 5), ("D", 7), ("E", 1)])
          @?= [Sample LastQueued "E", Sample Largest "B", Sample Middle "C"],
      testCase "samples: a module picked twice comes once" $
        pickSamples (sized [("A", 10), ("B", 1)])
          @?= [Sample LastQueued "B", Sample Largest "A"],
      testCase "samples: nothing parsed" $
        pickSamples [] @?= [],
      testCase "idle plan: n steps, in range, same for the same seed" $ do
        let plan = idlePlan 1 50 7
        length plan @?= 50
        assertBool "module index in range" (all (\st -> st.index >= 0 && st.index < 7) plan)
        assertBool "gap in 200..1000" (all (\st -> st.gapMs >= 200 && st.gapMs <= 1000) plan)
        idlePlan 1 50 7 @?= plan
        assertBool "another seed, another plan" (idlePlan 2 50 7 /= plan),
      testCase "on demand: asked during indexing counts, even if it ends after done" $ do
        onDemandStatus False OnDemandTimes {startUs = 100, doneUs = 500} @?= Counted
        onDemandStatus False OnDemandTimes {startUs = 500, doneUs = 500} @?= Counted,
      testCase "on demand: asked after indexing is left out" $ do
        onDemandStatus True OnDemandTimes {startUs = 100, doneUs = 500} @?= IndexedBefore
        onDemandStatus False OnDemandTimes {startUs = 501, doneUs = 500} @?= IndexedBefore,
      testCase "idle plan: no modules" $
        idlePlan 1 50 0 @?= []
    ]
  where
    sized = map (\(r, b) -> Sized {ref = r, bytes = b})
