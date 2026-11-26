module Bench.ModuleListTest (tests) where

import Bench.Mode (Side (..))
import Bench.ModuleList
  ( IdleStep (..),
    ModuleItem (..),
    OnDemandStatus (..),
    OnDemandTimes (..),
    Queued (..),
    Sample (..),
    SampleKind (..),
    SideModules (..),
    Sized (..),
    Source (..),
    diffQueueOrder,
    idlePlan,
    isChanged,
    modeQueueOrder,
    onDemandStatus,
    parseModuleList,
    pickDiffSamples,
    pickSamples,
    queueOrder,
  )
import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Tests run from the package dir.
modulesFixture :: IO [ModuleItem]
modulesFixture = fixture "modules.json"

-- | The same tree with diff markers (D-325).
diffFixture :: IO [ModuleItem]
diffFixture = fixture "modules-diff.json"

fixture :: FilePath -> IO [ModuleItem]
fixture name = do
  body <- BS.readFile ("../fixtures/api/" <> name)
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
        map (.ref) (queueOrder Tree ms)
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
      testCase "browse mode: no markers" $ do
        ms <- modulesFixture
        assertBool "none changed" (not (any isChanged ms)),
      testCase "diff markers: change and settings" $ do
        ms <- diffFixture
        [(m.ref, m.change, m.settingsChanged) | m <- take 3 ms]
          @?= [ ("Foo", Nothing, False),
                ("Gen", Just "added", False),
                ("Gone", Just "removed", True)
              ]
        [m.ref | m <- ms, isChanged m, m.ref `elem` map (.ref) (queueOrder Head ms)]
          @?= [ "Lit",
                "Sig",
                "pkg:exe:app:Main",
                "pkg:bench:slow:Main",
                "pkg/src/Lexer.hs",
                "Tool"
              ],
      testCase "diff queue order: changed first, head before base" $ do
        headMs <- diffFixture
        baseMs <- diffFixture
        let q = diffQueueOrder headMs baseMs
            changed =
              [ "Lit",
                "Sig",
                "pkg:exe:app:Main",
                "pkg:bench:slow:Main",
                "pkg/src/Lexer.hs",
                "Tool"
              ]
            rest = ["Foo", "Tests", "scripts/build-docs.hs"]
        [(x.side, x.ref) | x <- q]
          @?= map (Head,) changed
          <> map (Base,) changed
          <> map (Head,) rest
          <> map (Base,) rest,
      testCase "mode queue order: tree order, or the diff order" $ do
        headMs <- diffFixture
        baseMs <- modulesFixture
        modeQueueOrder (SideModules Tree baseMs :| [])
          @?= queueOrder Tree baseMs
        modeQueueOrder (SideModules Head headMs :| [SideModules Base baseMs])
          @?= diffQueueOrder headMs baseMs,
      testCase "diff queue order: an unchanged base is last" $ do
        headMs <- diffFixture
        baseMs <- modulesFixture
        let q = diffQueueOrder headMs baseMs
        length q @?= 18
        [x.ref | x <- q, x.side == Base] @?= map (.ref) (queueOrder Tree baseMs),
      testCase "diff samples: head's three, then base's last-queued" $ do
        headMs <- diffFixture
        baseMs <- diffFixture
        let q = diffQueueOrder headMs baseMs
            headSized =
              [ Sized {side = x.side, ref = x.ref, bytes = size x.ref}
              | x <- q,
                x.side == Head
              ]
            size r = if r == "Sig" then 99 else 1
        pickDiffSamples headSized q
          @?= [ Sample LastQueued Head "scripts/build-docs.hs",
                Sample Largest Head "Sig",
                Sample Middle Head "pkg/src/Lexer.hs",
                Sample LastQueued Base "scripts/build-docs.hs"
              ],
      testCase "samples: last, largest, middle" $
        pickSamples (sized [("A", 10), ("B", 99), ("C", 5), ("D", 7), ("E", 1)])
          @?= [ Sample LastQueued Tree "E",
                Sample Largest Tree "B",
                Sample Middle Tree "C"
              ],
      testCase "samples: a module picked twice comes once" $
        pickSamples (sized [("A", 10), ("B", 1)])
          @?= [Sample LastQueued Tree "B", Sample Largest Tree "A"],
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
    sized = map (\(r, b) -> Sized {side = Tree, ref = r, bytes = b})
