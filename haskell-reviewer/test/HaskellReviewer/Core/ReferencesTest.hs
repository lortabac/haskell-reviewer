-- | Find references (Note [Find references] in
-- "HaskellReviewer.Core.References"): the filtered resolution, the
-- keys, the finder against a full resolution, and the lines.
--
-- In the "Parsing" group (Q-RTS-BLACKHOLE).
module HaskellReviewer.Core.ReferencesTest (tests) where

import Control.Monad (forM_)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel (ParsedModule (..))
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.NameTable (NameEntry (..), moduleNames)
import HaskellReviewer.Core.Names (DeclId)
import HaskellReviewer.Core.Occurrences
  ( FreeContext,
    Occurrence (..),
    Resolution (..),
    TokenIx (..),
    occurrenceList,
  )
import HaskellReviewer.Core.References
  ( RefKind (..),
    RefLine (..),
    RefMatch (..),
    Reference (..),
    RefsAnswer (..),
    RefsInput (..),
    findRefs,
    hasKey,
    keyHash,
    lineCap,
    mayRefer,
    occKeyCount,
    occKeys,
    refLine,
  )
import HaskellReviewer.Core.Resolve
  ( ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    isPartial,
    occKey,
    resolveModule,
    resolveModuleOnly,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
import HaskellReviewer.Core.SourceLinks (LinkReason (..))
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenSpanAt)
import IndexSupport (memInput, modId)
import ResolveDump (Fixture (..), cases, loadFixture, resolveInput)
import ResolveDump qualified as Dump (fixtureDir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "References"
    [ testCase "filtered resolution: same results for the tokens kept" $
        forEachProject $
          \input mods -> mapM_ (filteredSame input) mods,
      testCase "every free token's key is in the module's keys" $
        forEachProject $
          \input mods -> mapM_ (keysCover input) mods,
      testCase "the finder agrees with a full resolution" $
        forEachProject $
          \input mods -> mapM_ (finderAgrees input mods) mods,
      testCase "kinds, guesses; locals and doc names don't count" kindsCase,
      testCase "a backtick use, tabs before it; a guess of the home module" extrasCase,
      testCase "partial: a module read isn't ready, or a slot isn't done" partialCase,
      testCase "lines: short, long, near the end, over lines, not ASCII" linesCase
    ]

-- * Over the fixtures

-- | The warm-up project, then each resolve fixture.
forEachProject :: (ResolveInput -> [ModuleId] -> Assertion) -> Assertion
forEachProject check = do
  check resolveWarmUpInput resolveWarmUpModules
  forM_ cases $ \c -> do
    fx <- loadFixture (Dump.fixtureDir <> c)
    check (resolveInput fx) fx.modules

-- | Each key of a module, alone: the results of its tokens are those of
-- the full resolution, and only they are there.
filteredSame :: ResolveInput -> ModuleId -> Assertion
filteredSame input i = case (input.state i, resolveModule input i) of
  (ModuleParsed p, Just full) ->
    forM_ (Set.toList (Set.fromList (map snd (freeKeys p)))) $ \k ->
      case resolveModuleOnly input i (== k) of
        Nothing -> assertBool "no result" False
        Just only -> do
          let wanted = [t | (t, k') <- freeKeys p, k' == k]
              restrict m = IntMap.filterWithKey (\t _ -> t `elem` wanted) m
          (show i, k, only.names) @?= (show i, k, restrict full.names)
          (show i, k, only.punFields) @?= (show i, k, restrict full.punFields)
          (show i, k, Map.size only.docNames) @?= (show i, k, 0)
          assertBool "partial only if the full one is" $
            not (isPartial only) || isPartial full
  _ -> pure ()

keysCover :: ResolveInput -> ModuleId -> Assertion
keysCover input i = case input.state i of
  ModuleParsed p -> do
    let keys = occKeys p.text p.parsed
        ks = map snd (freeKeys p)
    forM_ ks $ \k -> assertBool (show (i, k)) (hasKey keys (keyHash k))
    occKeyCount keys @?= Set.size (Set.fromList (map keyHash ks))
  _ -> pure ()

-- | For every named declaration of the module: the references are the
-- tokens a full resolution of every module links to it, plus its
-- module's own; asking only the candidates the keys give changes
-- nothing.
finderAgrees :: ResolveInput -> [ModuleId] -> ModuleId -> Assertion
finderAgrees input mods home = case input.state home of
  ModuleParsed p ->
    forM_ [e.decl | e <- moduleNames p.parsed.declarations] $ \d -> do
      let parsedMods = [m | m <- mods, ModuleParsed _ <- [input.state m]]
          others = sortOn show [m | m <- parsedMods, m /= home]
          named m = case input.state m of
            ModuleParsed q -> mayRefer d (occKeys q.text q.parsed)
            _ -> False
          ask cs =
            findRefs
              RefsInput
                { resolve = input,
                  home = home,
                  decl = d,
                  others = cs,
                  indexing = False
                }
          byKeys = ask (filter named others)
          everyone = ask others
          spans a = [(r.match.module_, r.match.span) | r <- a.results]
      (show d, spans byKeys) @?= (show d, spans everyone)
      (show d, byKeys.total) @?= (show d, everyone.total)
      let expected =
            Set.fromList $
              [(home, s) | s <- ownSpans p d]
                <> [(m, s) | m <- parsedMods, s <- resolvedSpans input m home d]
      (show d, Set.fromList (spans byKeys)) @?= (show d, expected)
  _ -> pure ()

-- | The tokens of a module the occurrence table links to a declaration.
ownSpans :: ParsedInput -> DeclId -> [Span]
ownSpans p d =
  [ tokenSpanAt p.parsed.tokens i
  | (TokenIx i, o) <- occurrenceList p.parsed.occurrences,
    any isD (o.resolution : maybe [] pure o.punField)
  ]
  where
    isD = \case
      TopLevelRef d' _ -> d' == d
      _ -> False

-- | The tokens and pun fields of a module a full resolution links to
-- the declaration.
resolvedSpans :: ResolveInput -> ModuleId -> ModuleId -> DeclId -> [Span]
resolvedSpans input m home d = case (input.state m, resolveModule input m) of
  (ModuleParsed p, Just r) ->
    [ tokenSpanAt p.parsed.tokens i
    | (i, x) <- IntMap.toList r.names <> IntMap.toList r.punFields,
      hits x
    ]
  _ -> []
  where
    hits = \case
      Exact (DeclTarget m' d') -> m' == home && d' == d
      Heuristic _ (DeclTarget m' d') -> m' == home && d' == d
      _ -> False

-- | Each free token and pun field, with its key.
freeKeys :: ParsedInput -> [(Int, Text)]
freeKeys p =
  [ (i, occKey c (sliceSpan ls (tokenSpanAt p.parsed.tokens i)))
  | (TokenIx i, o) <- occurrenceList p.parsed.occurrences,
    c <- free o.resolution <> maybe [] free o.punField
  ]
  where
    ls = sourceLines p.text
    free :: Resolution -> [FreeContext]
    free = \case
      FreeRef c -> [c]
      _ -> []

-- * Units

-- | A: the declarations; B uses them every way; C has a local of the
-- same name and a doc name; D guesses between A and E.
kindsCase :: Assertion
kindsCase = do
  let input =
        memInput
          Set.empty
          [ ( "A",
              "module A (C (..), f) where\n\
              \class C a where\n\
              \  m :: a -> Int\n\
              \f :: Int\n\
              \f = 1\n"
            ),
            ( "B",
              "module B (f) where\n\
              \import A (C (m), f)\n\
              \import qualified A as Q\n\
              \data T = T\n\
              \instance Q.C T where\n\
              \  m _ = f\n\
              \g = Q.f + m T\n"
            ),
            ( "C",
              "module C where\n\
              \import A\n\
              \-- | Not 'f'.\n\
              \h f = f\n"
            ),
            ("D", "module D where\nimport A\nimport E\nk = f\n"),
            ("E", "module E (f) where\nf = 2\n")
          ]
      refs d = findRefs (refsInput input (modId "A") d)
      shown ans =
        [ (r.match.module_, r.match.span.startLine, r.match.kind, r.match.reasons)
        | r <- ans.results
        ]
      a = modId "A"
      b = modId "B"
      dm = modId "D"
      fOf = head' [e.decl | e <- namesOf input a, e.name == "f"]
      mOf = head' [e.decl | e <- namesOf input a, e.name == "m"]
  shown (refs fOf)
    @?= [ (a, Line 1, RefExport, []),
          (a, Line 4, RefBinding, []),
          (a, Line 5, RefBinding, []),
          (b, Line 1, RefExport, []),
          (b, Line 2, RefImport, []),
          (b, Line 6, RefUse, []),
          (b, Line 7, RefUse, []),
          (dm, Line 4, RefUse, [ReasonAmbiguous])
        ]
  -- The method in an instance is a binding.
  shown (refs mOf)
    @?= [ (a, Line 3, RefBinding, []),
          (b, Line 2, RefImport, []),
          (b, Line 6, RefBinding, []),
          (b, Line 7, RefUse, [])
        ]
  -- C's f are locals, E's its own: neither is a candidate.
  (refs fOf).resolved @?= 3
  where
    head' = \case
      x : _ -> x
      [] -> error "no such name"

-- | F uses A's f in backticks, after a tab, on a short line and on a
-- long one. A guesses its own field through a record dot.
extrasCase :: Assertion
extrasCase = do
  let input =
        memInput
          Set.empty
          [ ( "A",
              "{-# LANGUAGE OverloadedRecordDot #-}\n\
              \module A (f) where\n\
              \data R = P {x :: Int} | Q {x :: Int}\n\
              \f :: Int -> Int -> Int\n\
              \f a _ = a\n\
              \g r = r.x\n"
            ),
            ( "F",
              "module F where\nimport A\nh =\t"
                <> Text.replicate 200 " "
                <> "1 `f` 2\nk =\t"
                <> Text.replicate 300 " "
                <> "f 1 2\n"
            )
          ]
      a = modId "A"
      fRefs = findRefs (refsInput input a (declOf input "A" "f"))
      xRefs = findRefs (refsInput input a (declOf input "A" "x"))
      inF = [r | r <- fRefs.results, r.match.module_ == modId "F"]
      -- The token, cut out of the line sent.
      tokenOf r =
        let Col c = r.match.span.startCol
            Col l = r.line.col
         in Text.take 1 (Text.drop (fromIntegral c - fromIntegral l) r.line.text)
  [(r.match.span.startLine, r.match.kind) | r <- inF]
    @?= [(Line 3, RefUse), (Line 4, RefUse)]
  map tokenOf inF @?= ["f", "f"]
  -- Short: the whole line. Long (309 code points): its last lineCap.
  map (.line.col) inF @?= [Col 1, Col (fromIntegral (309 - lineCap + 1))]
  assertBool "the tab is sent" ("\t" `Text.isInfixOf` (head' inF).line.text)
  [ (r.match.span.startLine, r.match.kind, r.match.reasons)
    | r <- xRefs.results,
      r.match.kind == RefUse
    ]
    @?= [(Line 6, RefUse, [ReasonThisModule])]
  where
    head' = \case
      x : _ -> x
      [] -> error "no references"

partialCase :: Assertion
partialCase = do
  let input =
        memInput
          (Set.fromList ["C"])
          [ ("A", "module A (a) where\na = 1\n"),
            ("B", "module B where\nimport A\nimport C\nb = a\n"),
            ("C", "module C where\n")
          ]
      ask indexing =
        findRefs
          (refsInput input (modId "A") (declOf input "A" "a")) {indexing = indexing}
  -- B read C, which isn't ready.
  (ask False).partial @?= True
  let done =
        memInput
          Set.empty
          [ ("A", "module A (a) where\na = 1\n"),
            ("B", "module B where\nimport A\nb = a\n")
          ]
      askDone indexing =
        findRefs (refsInput done (modId "A") (declOf done "A" "a")) {indexing = indexing}
  (askDone False).partial @?= False
  (askDone True).partial @?= True
  (askDone False).total @?= 3
  -- New final sets, for the memo: A's.
  Map.keys (askDone False).newFinal @?= [modId "A"]

linesCase :: Assertion
linesCase = do
  let line1 t s = refLine (sourceLines t) s
      at l c1 c2 = Span (Line l) (Col c1) (Line l) (Col c2)
      long = Text.replicate 300 "a" <> "XY" <> Text.replicate 300 "b"
  -- Short: the whole line.
  line1 "x = y\r\nfoo bar\n" (at 2 5 8) @?= RefLine (Col 1) "foo bar"
  -- Long: 240 code points, the match in the middle.
  let mid = line1 long (at 1 301 303)
  Text.length mid.text @?= lineCap
  mid.col @?= Col (301 - 119)
  assertBool "has the match" ("XY" `Text.isInfixOf` mid.text)
  -- Near the start or the end: shifted to stay on the line.
  line1 long (at 1 2 3) @?= RefLine (Col 1) (Text.take lineCap long)
  let end = line1 long (at 1 600 602)
  end @?= RefLine (Col (fromIntegral (602 - lineCap + 1))) (Text.takeEnd lineCap long)
  -- Over two lines: from its start to the end of the first line.
  let two = line1 (long <> "\nnext\n") (Span (Line 1) (Col 301) (Line 2) (Col 3))
  two.col @?= Col 301
  Text.length two.text @?= lineCap
  -- Not ASCII: code points, not bytes.
  let wide = Text.replicate 300 "é" <> "λ" <> Text.replicate 10 "é"
      w = line1 wide (at 1 301 302)
  Text.length w.text @?= lineCap
  assertBool "has λ" ("λ" `Text.isInfixOf` w.text)
  Text.take 1 (Text.drop (301 - fromCol w.col) w.text) @?= "λ"
  where
    fromCol (Col c) = fromIntegral c

-- * Helpers

refsInput :: ResolveInput -> ModuleId -> DeclId -> RefsInput
refsInput input home d =
  RefsInput
    { resolve = input,
      home = home,
      decl = d,
      others =
        [ m
        | m <- map modId ["A", "B", "C", "D", "E", "F", "G"],
          ModuleParsed q <- [input.state m],
          mayRefer d (occKeys q.text q.parsed)
        ],
      indexing = False
    }

namesOf :: ResolveInput -> ModuleId -> [NameEntry]
namesOf input i = case input.state i of
  ModuleParsed p -> moduleNames p.parsed.declarations
  _ -> []

declOf :: ResolveInput -> Text -> Text -> DeclId
declOf input m n = case [e.decl | e <- namesOf input (modId m), e.name == n] of
  d : _ -> d
  [] -> error "no such name"
