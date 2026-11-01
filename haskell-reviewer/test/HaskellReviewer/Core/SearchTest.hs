-- | The search scorer and ranking. See Note [Search] in
-- "HaskellReviewer.Core.Search".
module HaskellReviewer.Core.SearchTest (tests) where

import Control.Monad (forM_)
import Data.Char (isUpper, toLower)
import Data.List (isSubsequenceOf, nubBy, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Modules (ModuleId (..))
import HaskellReviewer.Core.NameTable (NameEntry (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..), renderDeclId)
import HaskellReviewer.Core.RelPath (RelPath, relPathFromBytes)
import HaskellReviewer.Core.Search
  ( Hit (..),
    MatchCase (..),
    ModuleCandidate (..),
    Pattern (..),
    Qualified (..),
    Query (..),
    SearchResult (..),
    makePattern,
    parseQuery,
    scoreText,
    scoreV1,
    scoreV2,
    search,
    searchLimit,
  )
import Hedgehog (Gen, Property, annotate, assert, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Search"
    [ parseTests,
      orderTests,
      testGroup
        "properties"
        [ testProperty "V2 matches iff the folded query is a subsequence" v2Matches,
          testProperty "V1 matches iff the folded query is a subsequence" v1Matches,
          testProperty "a name equal to the query comes first" exactFirst,
          testProperty "the order of names doesn't change the hits" orderFree,
          testProperty "the best k are the first k of all hits" bestK,
          testProperty "one char, up to 1024 chars: what V2 gives" oneCharV2,
          testProperty "appending to the text never lowers the V2 score" appendV2,
          testProperty "any unicode, long names and queries: no crash" noCrash
        ]
    ]

-- * parseQuery

-- | Plain text and case, and the qualified parts if any.
type Parsed = (Text, Bool, Maybe (Text, Text, Bool))

parseTests :: TestTree
parseTests =
  testCase "parseQuery" $
    mapM_
      (\(q, expected) -> (q, fmap describe (parseQuery q)) @?= (q, expected))
      [ ("insert", Just ("insert", False, Nothing)),
        ( "Data.Map.insert",
          Just ("Data.Map.insert", True, Just ("Data.Map", "insert", False))
        ),
        ("Data.Map.Map", Just ("Data.Map.Map", True, Just ("Data.Map", "Map", True))),
        ("Data.Map", Just ("Data.Map", True, Just ("Data", "Map", True))),
        ("M.insert", Just ("M.insert", True, Just ("M", "insert", False))),
        ("m.insert", Just ("m.insert", False, Just ("m", "insert", False))),
        ("Map.!", Just ("Map.!", True, Just ("Map", "!", False))),
        ("Prelude..", Just ("Prelude..", True, Just ("Prelude", ".", False))),
        ("a'_1.b", Just ("a'_1.b", False, Just ("a'_1", "b", False))),
        (".", Just (".", False, Nothing)),
        ("<$>", Just ("<$>", False, Nothing)),
        ("a", Just ("a", False, Nothing)),
        ("Map.", Just ("Map.", True, Nothing)),
        ("Data.Map.", Just ("Data.Map.", True, Nothing)),
        ("1.x", Just ("1.x", False, Nothing)),
        (".x", Just (".x", False, Nothing)),
        (" Data . Map\t", Just ("Data.Map", True, Just ("Data", "Map", True))),
        ("Été.x", Just ("Été.x", True, Just ("Été", "x", False))),
        ("ünï", Just ("ünï", False, Nothing)),
        ("", Nothing),
        ("  \t\n", Nothing),
        (Text.replicate 128 "a", Just (Text.replicate 128 "a", False, Nothing)),
        (Text.replicate 129 "a", Nothing),
        ( " " <> Text.replicate 128 "a" <> " ",
          Just (Text.replicate 128 "a", False, Nothing)
        )
      ]
  where
    sensitive p = p.matchCase == CaseSensitive
    describe :: Query -> Parsed
    describe q =
      ( q.plain.text,
        sensitive q.plain,
        (\x -> (x.qual.text, x.base.text, sensitive x.base)) <$> q.qualified
      )

-- * Ordering

orderTests :: TestTree
orderTests =
  testGroup
    "order"
    [ testCase "map: exact, same ignoring case, prefix, word start, camel, middle" $
        results "map" [mapModule]
          @?= [ "val:map@Data.Map",
                "type:Map@Data.Map",
                "val:mapM@Data.Map",
                "val:mapMaybe@Data.Map",
                "module Data.Map",
                "type:IntMap@Data.Map",
                "val:fmap@Data.Map"
              ],
      testCase "Map: case-sensitive, so no mapM" $
        results "Map" [mapModule]
          @?= ["type:Map@Data.Map", "module Data.Map", "type:IntMap@Data.Map"],
      testCase "fM: camel humps" $
        results "fM" [("Base", [val "foldMap", val "forM", val "fmap", val "fromMaybe"])]
          @?= ["val:forM@Base", "val:foldMap@Base", "val:fromMaybe@Base"],
      testCase "Data.Map.insert: Data.Map's before Data.IntMap's" $
        results "Data.Map.insert" twoInserts
          @?= ["val:insert@Data.Map", "val:insert@Data.IntMap"],
      testCase "M.insert: a short qualifier" $
        results "M.insert" twoInserts
          @?= ["val:insert@Data.Map", "val:insert@Data.IntMap"],
      testCase "Data.Map: the module, then its type" $
        results "Data.Map" [("Data.Map", [ty "Map", val "map"])]
          @?= ["module Data.Map", "type:Map@Data.Map"],
      testCase "operators" $
        results "<$>" [("Base", [val "<$>", val "<$", val "<*>", val "$", val "<$$>"])]
          @?= ["val:<$>@Base", "val:<$$>@Base"],
      testCase "Map.!: an operator in a module" $
        results "Map.!" [("Data.IntMap", [val "!"]), ("Data.Map", [val "!?", val "!"])]
          @?= ["val:!@Data.Map", "val:!@Data.IntMap", "val:!?@Data.Map"],
      testCase "Prelude..: the dot operator" $
        results "Prelude.." [("Prelude", [val "..", val "."])]
          @?= ["val:.@Prelude", "val:..@Prelude"],
      testCase "fields, constructors, classes, pattern synonyms" $
        results
          "port"
          [("Util", [field "Config" "port", con "Port", cls "Ports", pat "Portal"])]
          @?= [ "field:Config.port@Util",
                "con:Port@Util",
                "class:Ports@Util",
                "pat:Portal@Util"
              ],
      testCase "ties: exported first; equal ignoring case after" $ do
        let (mods, names) =
              fixture
                [ ("A", [private (val "foo"), ty "Foo"]),
                  ("B", [val "foo"])
                ]
        map render (hitsFor "foo" mods names)
          @?= ["val:foo@B", "val:foo@A", "type:Foo@A"],
      testCase "the same file in two components: two hits" $ do
        let a =
              ModuleCandidate
                { id = LooseModule (path "A.hs"),
                  name = "A",
                  ref = "pkg:lib:A"
                }
            b = a {id = LooseModule (path "b/A.hs"), ref = "pkg:exe:x:A"}
            mods = Map.fromList [(a.id, a), (b.id, b)]
            names = Map.fromList [(a.id, [entry (val "go")]), (b.id, [entry (val "go")])]
        map refOf (hitsFor "go" mods names) @?= ["pkg:exe:x:A", "pkg:lib:A"],
      testCase "the best 50, and how many matched" $ do
        let xs = [val ("x" <> Text.pack (show i)) | i <- [0 :: Int .. 119]]
            (mods, names) = fixture [("M", xs)]
            r = searchFor "x" mods names
        r.matches @?= 120
        length r.hits @?= searchLimit
        map render (take 3 r.hits) @?= ["val:x0@M", "val:x1@M", "val:x2@M"],
      testCase "a long name goes through V1" $ do
        let long = Text.replicate 2000 "a" <> "Map"
            p = makePattern CaseSensitive "aMap"
        assertBool "V1 used" (scoreText p long == scoreV1 p long)
        assertBool "matches" (isJust (scoreText p long))
        results "aMap" [("M", [val long, val "aMap"])]
          @?= ["val:aMap@M", "val:" <> long <> "@M"],
      testCase "no match, no hit; an empty pattern matches nothing" $ do
        results "zzz" [mapModule] @?= []
        map (\f -> f (makePattern IgnoreCase "") "abc") [scoreText, scoreV1, scoreV2]
          @?= [Nothing, Nothing, Nothing],
      -- Checked by hand against fzf's algo.go.
      testCase "exact scores" $
        [ (q, t, scoreV2 (patternFor q) t, scoreV1 (patternFor q) t)
        | (q, t) <-
            [ ("map", "map"),
              ("map", "fmap"),
              ("fM", "forM"),
              ("fM", "foldMap"),
              ("aab", "aaab")
            ]
        ]
          @?= [ ("map", "map", Just 88, Just 88),
                ("map", "fmap", Just 56, Just 56),
                ("fM", "forM", Just 55, Just 55),
                ("fM", "foldMap", Just 54, Just 54),
                -- V2 finds the best path, V1 scores the first window.
                ("aab", "aaab", Just 75, Just 56)
              ],
      testCase "one char: the best occurrence, any length" $ do
        let p = makePattern IgnoreCase "a"
        -- After a dot (9) beats mid-word (0); at the start (10) beats both.
        scoreText p "xa.a" @?= Just 34
        scoreText p "a.a" @?= Just 36
        scoreText p (Text.replicate 3000 "x" <> ".a") @?= Just 34,
      testCase "many long names that match: bounded time" $ do
        let numbered k =
              [val (Text.replicate k "a" <> Text.pack (show i)) | i <- [1 :: Int .. 500]]
            -- V1 for the long ones; V2 near its cell cap for the short ones.
            (mods, names) = fixture [("M", numbered 1000), ("N", numbered 60)]
        t0 <- getMonotonicTime
        let queries =
              [ (Text.replicate 65 "a", 500),
                (Text.replicate 60 "a", 1000),
                ("aaaa", 1000)
              ]
        forM_ queries $
          \(q, n) -> do
            let r = searchFor q mods names
            (q, r.matches) @?= (q, n)
        t1 <- getMonotonicTime
        -- About 0.1 s here; generous for slow, busy CI machines.
        assertBool ("took " <> show (t1 - t0) <> " s") (t1 - t0 < 3)
    ]
  where
    patternFor q =
      makePattern (if Text.any isUpper q then CaseSensitive else IgnoreCase) q
    twoInserts = [("Data.IntMap", [val "insert"]), ("Data.Map", [val "insert"])]
    mapModule =
      ( "Data.Map",
        [ val "insert",
          val "fmap",
          ty "IntMap",
          val "mapMaybe",
          val "mapM",
          ty "Map",
          val "map"
        ]
      )

-- | A declaration and whether it's exported.
type Decl = (DeclId, Bool)

val, ty, con, cls, pat :: Text -> Decl
val n = (Named DVal n, True)
ty n = (Named DType n, True)
con n = (Named DCon n, True)
cls n = (Named DClass n, True)
pat n = (Named DPat n, True)

field :: Text -> Text -> Decl
field p n = (Field p n, True)

private :: Decl -> Decl
private (d, _) = (d, False)

-- | Loose modules, @A.B@ at @A/B.hs@, with their names.
fixture :: [(Text, [Decl])] -> (Map ModuleId ModuleCandidate, Map ModuleId [NameEntry])
fixture ms =
  ( Map.fromList [(c.id, c) | (c, _) <- cands],
    Map.fromList [(c.id, map entry ds) | (c, ds) <- cands]
  )
  where
    cands = [(candidate m, ds) | (m, ds) <- ms]
    candidate m =
      ModuleCandidate
        { id = LooseModule (path (Text.unpack (Text.replace "." "/" m) <> ".hs")),
          name = m,
          ref = m
        }

entry :: Decl -> NameEntry
entry (d, e) = NameEntry {name = nameOf d, decl = d, exported = e}
  where
    nameOf = \case
      Named _ n -> n
      Field _ n -> n
      Inst _ -> ""

path :: String -> RelPath
path p = case relPathFromBytes (Text.Encoding.encodeUtf8 (Text.pack p)) of
  Just r -> r
  Nothing -> error ("bad path " <> p)

-- | Rendered hits for a query, at most 'searchLimit'.
results :: Text -> [(Text, [Decl])] -> [Text]
results q ms = let (mods, names) = fixture ms in map render (hitsFor q mods names)

hitsFor :: Text -> Map ModuleId ModuleCandidate -> Map ModuleId [NameEntry] -> [Hit]
hitsFor q mods names = (searchFor q mods names).hits

searchFor ::
  Text -> Map ModuleId ModuleCandidate -> Map ModuleId [NameEntry] -> SearchResult
searchFor q mods names = case parseQuery q of
  Just query -> search searchLimit query mods names
  Nothing -> SearchResult {matches = 0, hits = []}

render :: Hit -> Text
render = \case
  ModuleHit m -> "module " <> m.name
  DeclHit m e -> renderDeclId e.decl <> "@" <> m.name

refOf :: Hit -> Text
refOf = \case
  ModuleHit m -> m.ref
  DeclHit m _ -> m.ref

-- * Properties

-- | Short texts from a small alphabet: matches are common.
genSmall :: Gen Text
genSmall = Gen.text (Range.linear 0 30) genChar

genChar :: Gen Char
genChar = Gen.element ("abAB_'.1<$é É" <> unusualCase)

-- | ǅ (title case), İ, the Kelvin sign and Roman numeral one: their
-- case maps are odd.
unusualCase :: String
unusualCase = "\x01C5\x0130\x212A\x2160"

genPattern :: Gen Pattern
genPattern = do
  t <- Gen.text (Range.linear 1 5) genChar
  mc <- Gen.element [CaseSensitive, IgnoreCase]
  pure (makePattern mc t)

-- | The plain subsequence check, on folded strings.
isFoldedSubsequence :: Pattern -> Text -> Bool
isFoldedSubsequence p t = f (Text.unpack p.text) `isSubsequenceOf` f (Text.unpack t)
  where
    f = case p.matchCase of
      CaseSensitive -> id
      IgnoreCase -> map toLower

v2Matches :: Property
v2Matches = property $ do
  p <- forAll genPattern
  t <- forAll genSmall
  isJust (scoreV2 p t) === isFoldedSubsequence p t
  isJust (scoreText p t) === isFoldedSubsequence p t

v1Matches :: Property
v1Matches = property $ do
  p <- forAll genPattern
  t <- forAll genSmall
  isJust (scoreV1 p t) === isFoldedSubsequence p t

-- | No dots: a qualified query could put an equal base name first.
genName :: Gen Text
genName =
  Gen.text (Range.linear 1 8) (Gen.element ("abAB_'1<$é" <> unusualCase))

genModules :: Gen [(Text, [Decl])]
genModules = do
  n <- Gen.int (Range.linear 1 4)
  mapM
    ( \i -> do
        ns <- Gen.list (Range.linear 0 15) genDecl
        pure ("M" <> Text.pack (show i), nubBy (\a b -> fst a == fst b) ns)
    )
    [1 .. n]
  where
    genDecl = do
      n <- genName
      mk <- Gen.element [val, ty, con, \x -> field "P" x]
      e <- Gen.bool
      let (d, _) = mk n
      pure (d, e)

exactFirst :: Property
exactFirst = property $ do
  ms <- forAll genModules
  let all' = [n | (_, ds) <- ms, (d, _) <- ds, Just n <- [declText d]]
  if null all'
    then pure ()
    else do
      q <- forAll (Gen.element all')
      let (mods, names) = fixture ms
      case hitsFor q mods names of
        top : _ -> hitName top === q
        [] -> annotate "no hit" >> failure

orderFree :: Property
orderFree = property $ do
  ms <- forAll genModules
  q <- forAll genName
  let (mods, names) = fixture ms
      (_, reversed) = fixture [(m, reverse ds) | (m, ds) <- ms]
      run ns = case parseQuery q of
        Just query -> search maxBound query mods ns
        Nothing -> SearchResult {matches = 0, hits = []}
      a = run names
      b = run reversed
  a.matches === b.matches
  sort (map render a.hits) === sort (map render b.hits)

bestK :: Property
bestK = property $ do
  ms <- forAll genModules
  q <- forAll (Gen.choice [genName, Gen.element ["a", "A", "M1.a", "M2.b"]])
  k <- forAll (Gen.int (Range.linear 0 12))
  let (mods, names) = fixture ms
  case parseQuery q of
    Nothing -> pure ()
    Just query -> do
      let capped = search k query mods names
          full = search maxBound query mods names
      capped.matches === full.matches
      map render capped.hits === map render (take k full.hits)

oneCharV2 :: Property
oneCharV2 = property $ do
  mc <- forAll (Gen.element [CaseSensitive, IgnoreCase])
  c <- forAll genChar
  let p = makePattern mc (Text.singleton c)
  t <- forAll (Gen.choice [genSmall, Gen.text (Range.linear 900 1024) genChar])
  scoreText p t === scoreV2 p t

appendV2 :: Property
appendV2 = property $ do
  p <- forAll genPattern
  t <- forAll genSmall
  s <- forAll genSmall
  assert (scoreV2 p (t <> s) >= scoreV2 p t)

noCrash :: Property
noCrash = property $ do
  let any' = Gen.text (Range.linear 0 40) Gen.unicode
  names <- forAll (Gen.list (Range.linear 0 20) any')
  long <- forAll (Gen.text (Range.linear 1000 3000) (Gen.element ("aA.b_" :: String)))
  q <- forAll (Gen.choice [any', Gen.text (Range.linear 100 140) Gen.unicode])
  let (mods, ns) = fixture [("M", [val n | n <- long : names, not (Text.null n)])]
      r = searchFor q mods ns
  assert (length (map render r.hits) <= searchLimit)
  assert (r.matches >= length r.hits)

declText :: DeclId -> Maybe Text
declText = \case
  Named _ n -> Just n
  Field _ n -> Just n
  Inst _ -> Nothing

hitName :: Hit -> Text
hitName = \case
  ModuleHit m -> m.name
  DeclHit _ e -> e.name
