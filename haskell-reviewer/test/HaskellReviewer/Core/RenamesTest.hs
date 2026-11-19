-- | File pairing (D-324): properties on generated cases, units on the
-- steps and the budget. The git oracle is in RenamesOracleTest.
module HaskellReviewer.Core.RenamesTest (tests) where

import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import HaskellReviewer.Core.RelPath (RelPath, relPathName)
import HaskellReviewer.Core.Renames
  ( Blob (..),
    Change (..),
    DiffEntry (..),
    Limit (..),
    ModifiedFile (..),
    Needed (..),
    Pairing (..),
    RenameLimit (..),
    RenameStep (..),
    RenamedFile (..),
    Settings (..),
    Step (..),
    defaultSettings,
    gitTableReversed,
    pairFiles,
    startPairing,
    unknownBlobs,
    warmUpRenames,
  )
import HaskellReviewer.Core.Sides (Pair (..), Side (..))
import HaskellReviewer.Core.Similarity
  ( Score (..),
    Spans,
    basenameScore,
    maxScore,
    renameScore,
  )
import HaskellReviewer.Core.WorkTree (Exec (..))
import Hedgehog
  ( Property,
    PropertyT,
    annotate,
    assert,
    cover,
    failure,
    forAll,
    property,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Internal.Tree (treeValue)
import IndexSupport (rel)
import RenamesSupport (Case (..), Item (..), caseContents, caseEntries, genCase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Renames"
    [ testProperty "same result from shuffled input" propShuffled,
      testProperty "every path once, one to one" propOneToOne,
      testProperty "exact first" propExactFirst,
      testProperty "scores and steps" propScores,
      testProperty "past the limit: exact and basename only" propLimit,
      testProperty "only files pair inexactly" propOnlyFiles,
      testProperty "contents asked in at most 2 rounds, files only, once" propRounds,
      testCase "generated cases hit every step (the oracle uses them)" shares,
      testCase "git's hash table turns around on its last insert" $ do
        map gitTableReversed [0, 1, 51, 52, 204, 205, 206, 3276, 3277, 3278, 52429]
          @?= [False, False, False, False, False, True, False, False, True, False, True],
      testCase "a small read budget stops before the matrix" $ do
        let c = simpleCase
            p = pairingOf defaultSettings {readBudget = 10} c
        p.limit @?= OverWorkBudget
        [r | Renamed r <- p.changes, r.step /= ExactStep] @?= [],
      testCase "the read budget never drops basename renames" $ do
        let p = pairingOf defaultSettings {readBudget = 1} basenameCase
        p.limit @?= OverWorkBudget
        [(r.from, r.step) | Renamed r <- p.changes]
          @?= [(rel "d1/Same.hs", BasenameStep)],
      testCase "unknown sizes count against the read budget once read" $ do
        -- 20 bytes in all, none known up front.
        let noSize = \case
              File x b -> File x b {size = Nothing}
              other -> other
            unsized = Map.map noSize
            e = caseEntries simpleCase
            entries = Pair {head = unsized e.head, base = unsized e.base}
            contents = caseContents simpleCase
            run budget = pairFiles defaultSettings {readBudget = budget} entries contents
        (run 19).limit @?= OverWorkBudget
        (run 20).limit @?= WithinLimits
        [r.step | Renamed r <- (run 20).changes] @?= [MatrixStep],
      testCase "a small merge budget stops the matrix" $ do
        let c = simpleCase
            p = pairingOf defaultSettings {mergeBudget = 1} c
        p.limit @?= OverWorkBudget,
      testCase "the simple case pairs by the matrix" $ do
        let c = simpleCase
            p = pairingOf defaultSettings c
        p.limit @?= WithinLimits
        [(r.step, r.score) | Renamed r <- p.changes] @?= [(MatrixStep, Score 48000)],
      testCase "files over the size cap get no score" $ do
        let c = simpleCase
            p = pairingOf defaultSettings {sizeCap = 9} c
        [() | Renamed _ <- p.changes] @?= [],
      testCase "unknown blobs that matter" $ do
        let e = File NotExecutable Blob {oid = Nothing, size = Just 1}
            entries =
              Pair
                { head = Map.fromList [(rel "both", e), (rel "new", e)],
                  base = Map.fromList [(rel "both", e)]
                }
        -- No source, so "new" can't pair.
        unknownBlobs entries @?= Pair {head = [rel "both"], base = [rel "both"]},
      testCase "unknown blobs: same path is modified" $ do
        let e = File NotExecutable Blob {oid = Nothing, size = Just 1}
            one = Map.fromList [(rel "f", e)]
            sides = Pair {head = one, base = one}
        (pairFiles defaultSettings sides (Pair Map.empty Map.empty)).changes
          @?= [Modified ModifiedFile {path = rel "f", note = Nothing}],
      testCase "warm-up runs" warmUpRenames
    ]

-- | With fixed seeds (no flake): of 2,000 generated cases, enough have
-- an exact, a basename, a matrix rename, or hit a limit of 2. Measured
-- (2026-11-19): 769, 288, 587 and 334.
shares :: IO ()
shares = do
  let cs =
        [ treeValue t
        | i <- [0 .. 1999 :: Int],
          Just t <- [evalGen 50 (Seed.from (fromIntegral i)) genCase]
        ]
      ps = map (pairingOf defaultSettings) cs
      lim = map (pairingOf defaultSettings {renameLimit = RenameLimit 2}) cs
      with st = length [() | p <- ps, st `elem` [r.step | Renamed r <- p.changes]]
      limitHits = length [() | p <- lim, p.limit == OverRenameLimit]
  assertBool "exact" (with ExactStep >= 500)
  assertBool "basename" (with BasenameStep >= 160)
  assertBool "matrix" (with MatrixStep >= 400)
  assertBool "limit" (limitHits >= 200)

-- | old.txt -> new.txt at 80%, by the matrix.
simpleCase :: Case
simpleCase =
  Case
    { base = [("old.txt", file "1\n2\n3\n4\n5\n")],
      head = [("new.txt", file "1\n2\n3\n4\n6\n")]
    }
  where
    file = IFile NotExecutable

-- | d1/Same.hs -> d2/Same.hs at 90% (basename step), and old.txt ->
-- new.txt at 80% (matrix).
basenameCase :: Case
basenameCase =
  Case
    { base = ("d1/Same.hs", file same) : simpleCase.base,
      head = ("d2/Same.hs", file (same <> "x\n")) : simpleCase.head
    }
  where
    file = IFile NotExecutable
    same = "a\nb\nc\nd\ne\nf\ng\nh\ni\n"

pairingOf :: Settings -> Case -> Pairing
pairingOf s c = pairFiles s (caseEntries c) (caseContents c)

renames :: Pairing -> [RenamedFile]
renames p = [r | Renamed r <- p.changes]

propShuffled :: Property
propShuffled = property $ do
  c <- forAll genCase
  b <- forAll (Gen.shuffle c.base)
  h <- forAll (Gen.shuffle c.head)
  pairingOf defaultSettings c === pairingOf defaultSettings Case {base = b, head = h}

propOneToOne :: Property
propOneToOne = property $ do
  c <- forAll genCase
  let p = pairingOf defaultSettings c
      e = caseEntries c
      basePaths = [r.from | r <- renames p] <> [a | Removed a <- p.changes] <> both p
      headPaths = [r.to | r <- renames p] <> [a | Added a <- p.changes] <> both p
      both q = [m.path | Modified m <- q.changes]
      unchanged =
        Set.difference
          (Map.keysSet (Map.intersection e.base e.head))
          (Set.fromList (both p))
  sort basePaths === Set.toAscList (Map.keysSet e.base `Set.difference` unchanged)
  sort headPaths === Set.toAscList (Map.keysSet e.head `Set.difference` unchanged)
  -- Same path is never a rename.
  for_ (renames p) $ \r ->
    assert (r.from /= r.to && Map.notMember r.from e.head && Map.notMember r.to e.base)

-- | No destination with an identical unused source pairs inexactly.
propExactFirst :: Property
propExactFirst = property $ do
  c <- forAll genCase
  let p = pairingOf defaultSettings c
      e = caseEntries c
      unused = Set.fromList [a | Removed a <- p.changes]
      key = \case
        Submodule x -> Just (Left x)
        File _ b -> Right <$> b.oid
        Symlink b -> Right <$> b.oid
  for_ (renames p) $ \r -> unless (r.step == ExactStep) $ do
    let k = key (e.head Map.! r.to)
        same =
          [ s
          | s <- Set.toList unused,
            key (e.base Map.! s) == k,
            compatible (e.base Map.! s) (e.head Map.! r.to)
          ]
    annotate (show r)
    same === []
  cover 10 "an exact rename" (any ((== ExactStep) . (.step)) (renames p))
  where
    compatible a b = case (a, b) of
      (File _ _, File _ _) -> True
      (Symlink _, Symlink _) -> True
      (Submodule _, Submodule _) -> True
      _ -> False

-- | Each step's own rule. How often each step runs is checked by
-- 'shares', with fixed seeds: a cover here (5% against a rate of about
-- 14%) flaked.
propScores :: Property
propScores = property $ do
  c <- forAll genCase
  let rs = renames (pairingOf defaultSettings c)
  for_ rs $ \r -> case r.step of
    ExactStep -> r.score === maxScore
    BasenameStep -> do
      assert (r.score >= basenameScore)
      relPathName r.from === relPathName r.to
    MatrixStep -> assert (r.score >= renameScore)

propLimit :: Property
propLimit = property $ do
  c <- forAll genCase
  let p = pairingOf defaultSettings {renameLimit = RenameLimit 1} c
      full = pairingOf defaultSettings c
  case p.limit of
    OverRenameLimit -> [r | r <- renames p, r.step == MatrixStep] === []
    WithinLimits -> p === full
    OverWorkBudget -> failure
  -- The exact and basename renames are the same either way.
  let early q = [r | r <- renames q, r.step /= MatrixStep]
  early p === early full

propOnlyFiles :: Property
propOnlyFiles = property $ do
  c <- forAll genCase
  let e = caseEntries c
  for_ (renames (pairingOf defaultSettings c)) $ \r -> unless (r.step == ExactStep) $ do
    isFile (e.base Map.! r.from) === True
    isFile (e.head Map.! r.to) === True
  where
    isFile = \case
      File _ _ -> True
      _ -> False

propRounds :: Property
propRounds = property $ do
  c <- forAll genCase
  let e = caseEntries c
  rounds <- drive e (caseContents c) Set.empty 0 (startPairing defaultSettings e)
  assert (rounds <= 2)
  where
    drive ::
      Pair (Map RelPath DiffEntry) ->
      Pair (Map RelPath Spans) ->
      Set.Set (Side, RelPath) ->
      Int ->
      Step ->
      PropertyT IO Int
    drive e cs seen n = \case
      Done _ -> pure n
      NeedContent need k -> do
        let asked =
              [(HeadSide, p) | p <- need.files.head]
                <> [(BaseSide, p) | p <- need.files.base]
        annotate (show asked)
        assert (all (`Set.notMember` seen) asked)
        assert (all (isFileIn e.head) need.files.head)
        assert (all (isFileIn e.base) need.files.base)
        assert (not (null asked))
        let keep m ps = Map.restrictKeys m (Set.fromList ps)
            got = Pair (keep cs.head need.files.head) (keep cs.base need.files.base)
        drive e cs (Set.union seen (Set.fromList asked)) (n + 1) (k got)
    isFileIn m p = case Map.lookup p m of
      Just (File _ _) -> True
      _ -> False
