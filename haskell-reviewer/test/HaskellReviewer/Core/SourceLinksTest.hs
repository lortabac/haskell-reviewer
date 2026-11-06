-- | The source view's links and outline (Note [Source links] in
-- "HaskellReviewer.Core.SourceLinks"), with the runs they go into.
module HaskellReviewer.Core.SourceLinksTest (tests, linearTests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word32)
import HaskellReviewer.Core.Highlight
  ( Run (..),
    highlightLinked,
    linkedRunCount,
    linkedRunList,
  )
import HaskellReviewer.Core.ModuleModel (ParsedModule (..))
import HaskellReviewer.Core.Modules
  ( ModuleId,
    ModuleIndex,
    modulePath,
    renderModuleId,
  )
import HaskellReviewer.Core.Names (ExternalModule (..), renderDeclId)
import HaskellReviewer.Core.Occurrences
  ( Occurrence (..),
    Resolution (..),
    TokenIx (..),
    noOccurrences,
    occurrenceList,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RelPath (relPathText)
import HaskellReviewer.Core.Resolve
  ( ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
    Resolved (..),
    forceModuleResolution,
    isPartial,
    resolveModule,
  )
import HaskellReviewer.Core.ResolveWarmUp
  ( resolveWarmUpIndex,
    resolveWarmUpInput,
    resolveWarmUpModules,
  )
import HaskellReviewer.Core.SourceLinks
  ( DeclLink (..),
    ExternalLink (..),
    GuessHow (..),
    LinkReason (..),
    LinkTarget (..),
    LinksInput (..),
    LocalLink (..),
    ModuleLink (..),
    OutlineEntry (..),
    OutlineKind (..),
    SourceLinks (..),
    forceSourceLinks,
    linkAt,
    linksOf,
    sourceLinks,
  )
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..), renderSpan)
import HaskellReviewer.Core.Tokens (tokenCount, tokenSpanAt)
import HaskellReviewer.Server.Wire.Source
  ( SourceBody (..),
    SourceView (..),
    encodeSource,
    howName,
    parsedSource,
    reasonName,
  )
import HaskellReviewer.Session.Snapshot (Generation (..))
import IndexSupport (MemProject (..), libFile, memProject, modId)
import ResolveDump (Fixture (..), cases, loadFixture, resolveInput)
import ResolveDump qualified as Dump (fixtureDir)
import System.Directory qualified as Dir
import System.FilePath (dropExtension)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (getAllocationCounter)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Source links"
    [ testGroup "links" unitTests,
      testCase "outline: kinds, ids and lines" outlineCase,
      testCase "no occurrence table: runs, no links" noTableCase,
      testCase "each linked module's file is looked up once" pathLookups,
      testCase "invariants: warm-up project" (mapM_ invariants warmUpModules),
      testCase "invariants: resolve fixtures" $ do
        mods <- concat <$> mapM fixtureModules cases
        mapM_ invariants mods,
      testGroup "goldens" $
        testCase "every golden has a test, and the other way round" noStale
          : [goldenVsString c (goldenDir <> c <> ".txt") (golden c) | c <- goldenCases]
    ]

-- * A small project

-- | One module of a project, resolved and linked.
data Linked = Linked
  { self :: ModuleId,
    source :: ParsedInput,
    resolution :: Maybe ModuleResolution,
    links :: SourceLinks
  }

linkedIn :: ModuleIndex -> ResolveInput -> ModuleId -> Maybe Linked
linkedIn idx input i = case input.state i of
  ModuleParsed p ->
    let res = resolveModule input i
     in Just
          Linked
            { self = i,
              source = p,
              resolution = res,
              links = linksOf idx input.state i p res
            }
  _ -> Nothing

linkedOf :: MemProject -> Text -> IO Linked
linkedOf w n =
  maybe
    (assertFailure ("not parsed: " <> show n))
    pure
    (linkedIn w.index w.input (modId n))

-- | Each token with a link: its text and target.
linkedTokens :: Linked -> [(Text, LinkTarget)]
linkedTokens l =
  [ (sliceSpan ls (tokenSpanAt l.source.parsed.tokens i), ts !! (fromIntegral w - 1))
  | i <- [0 .. tokenCount l.source.parsed.tokens - 1],
    let w = linkAt l.links i,
    w /= 0
  ]
  where
    ls = sourceLines l.source.text
    ts = l.links.targets

-- | The link word of each token with this text.
wordsOf :: Linked -> Text -> [Word32]
wordsOf l t =
  [ linkAt l.links i
  | i <- [0 .. tokenCount l.source.parsed.tokens - 1],
    sliceSpan ls (tokenSpanAt l.source.parsed.tokens i) == t
  ]
  where
    ls = sourceLines l.source.text

-- | The external targets of each token with this text.
externalsOf :: Linked -> Text -> [(ModuleName, GuessHow, Int, Text)]
externalsOf l t =
  [(x.module_.name, x.how, x.count, x.query) | ToExternal x <- targetsOf l t]

targetsOf :: Linked -> Text -> [LinkTarget]
targetsOf l t = [x | (s, x) <- linkedTokens l, s == t]

-- * Unit tests

unitTests :: [TestTree]
unitTests =
  [ testCase "a local: definition and uses share one target" $ do
      l <- linkedOf (memProject Set.empty [("A", "module A where\nf x = x + x\n")]) "A"
      let ws = wordsOf l "x"
      length ws @?= 3
      Set.size (Set.fromList ws) @?= 1
      targetsOf l "x" @?= replicate 3 (ToLocal LocalLink {line = Line 2, col = Col 3}),
    testCase "a field of two types here: this-module" $ do
      l <-
        linkedOf
          ( memProject
              Set.empty
              [ ( "A",
                  "{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}\n\
                  \module A where\n\
                  \data S = S {fld :: Int}\n\
                  \data T = T {fld :: Int}\n\
                  \g r = fld r\n"
                )
              ]
          )
          "A"
      let reasonsOf = \case
            ToDecl d -> d.reasons
            _ -> []
      assertBool (show (map reasonsOf (targetsOf l "fld"))) $
        [ReasonThisModule] `elem` map reasonsOf (targetsOf l "fld"),
    testCase "another module's declaration: exact, with its line" $ do
      let w =
            memProject
              Set.empty
              [ ("A", "module A (x) where\n\nx :: Int\nx = 1\n"),
                ("B", "module B where\nimport A\ny = x\n")
              ]
      l <- linkedOf w "B"
      case targetsOf l "x" of
        [ToDecl d] -> do
          (d.ref, relPathText d.path, d.line, renderDeclId d.id)
            @?= ("A", "src/A.hs", Just (Line 3), "val:x")
          (d.reasons, d.candidates) @?= ([], 1)
        other -> assertFailure (show other),
    testCase "an import not ready: a module target, partial" $ do
      let w =
            memProject
              (Set.fromList ["A"])
              [ ("A", "module A (x) where\nx = 1\n"),
                ( "B",
                  "{-# LANGUAGE NoImplicitPrelude #-}\nmodule B where\nimport A\ny = x\n"
                )
              ]
      l <- linkedOf w "B"
      fmap isPartial l.resolution @?= Just True
      case targetsOf l "x" of
        [ToModule m] ->
          (m.ref, fmap relPathText m.path, m.reasons)
            @?= ("A", Just "src/A.hs", [ReasonNotReady])
        other -> assertFailure (show other),
    testCase "external: listed, the only open import, one of two" $ do
      let w =
            memProject
              Set.empty
              [ ( "A",
                  "{-# LANGUAGE NoImplicitPrelude #-}\n\
                  \module A where\n\
                  \import Data.List (sort)\n\
                  \a = sort\n"
                ),
                ( "B",
                  "module B where\nimport Data.Maybe\nb = fromMaybe\nc = (<|>)\n"
                )
              ]
      a <- linkedOf w "A"
      let ext = externalsOf a
      -- The import item and the use.
      ext "sort"
        @?= replicate 2 (ModuleName "Data.List", HowListed, 1, "sort +module:Data.List")
      b <- linkedOf w "B"
      let extB = externalsOf b
      extB "fromMaybe" @?= [(ModuleName "Prelude", HowOneOf, 2, "fromMaybe")]
      extB "<|>" @?= [(ModuleName "Prelude", HowOneOf, 2, "<|>")],
    testCase "external: the only open import, a qualified name" $ do
      let w =
            memProject
              Set.empty
              [ ( "A",
                  "{-# LANGUAGE NoImplicitPrelude #-}\n\
                  \module A where\n\
                  \import qualified Data.Map as M\n\
                  \a = M.lookup\n"
                )
              ]
      a <- linkedOf w "A"
      [(x.how, x.query) | ToExternal x <- targetsOf a "M.lookup"]
        @?= [(HowOnlyOpen, "lookup +module:Data.Map")],
    testCase "unresolved, built-ins and module words: no link" $ do
      l <-
        linkedOf
          ( memProject
              Set.empty
              [ ( "A",
                  "{-# LANGUAGE NoImplicitPrelude #-}\n\
                  \module A where\n\
                  \y = mystery : []\n"
                )
              ]
          )
          "A"
      wordsOf l "mystery" @?= [0]
      wordsOf l ":" @?= [0]
      wordsOf l "A" @?= [0]
      map fst (linkedTokens l) @?= ["y"],
    testCase "a pun links its variable, not the field" $ do
      let w =
            memProject
              Set.empty
              [ ("A", "module A (R (..)) where\ndata R = R {f :: Int}\n"),
                ( "B",
                  "{-# LANGUAGE NamedFieldPuns #-}\n\
                  \module B where\nimport A\ng R {f} = f\n"
                )
              ]
      l <- linkedOf w "B"
      targetsOf l "f"
        @?= replicate 2 (ToLocal LocalLink {line = Line 4, col = Col 6}),
    testCase "a quasi-quote links its quoter, over the whole token" $ do
      let w =
            memProject
              Set.empty
              [ ("A", "module A (qq) where\nqq = undefined\n"),
                ( "B",
                  "{-# LANGUAGE QuasiQuotes #-}\nmodule B where\n\
                  \import A\nq = [qq|x y|]\n"
                )
              ]
      l <- linkedOf w "B"
      [renderDeclId d.id | ToDecl d <- targetsOf l "[qq|x y|]"] @?= ["val:qq"],
    testCase "DataKinds: a constructor in a type" $ do
      let w =
            memProject
              Set.empty
              [ ("A", "module A (Nat (..)) where\ndata Nat = Z | S Nat\n"),
                ( "B",
                  "{-# LANGUAGE DataKinds #-}\nmodule B where\nimport A\ntype K = Z\n"
                )
              ]
      l <- linkedOf w "B"
      [(renderDeclId d.id, d.reasons) | ToDecl d <- targetsOf l "Z"]
        @?= [("con:Z", [ReasonDataKinds])],
    testCase "two uses of a name share a target; different reasons don't" $ do
      let w =
            memProject
              Set.empty
              [ ("A", "module A (x) where\nx = 1\n"),
                ("C", "module C (x) where\nx = 2\n"),
                ("B", "module B where\nimport A\nimport C\ny = x + x\nz = y\n")
              ]
      l <- linkedOf w "B"
      let ws = wordsOf l "x"
      length ws @?= 2
      Set.size (Set.fromList ws) @?= 1
      [(d.ref, d.reasons, d.candidates) | ToDecl d <- take 1 (targetsOf l "x")]
        @?= [("A", [ReasonAmbiguous], 2)]
  ]

outlineCase :: Assertion
outlineCase = do
  l <-
    linkedOf
      ( memProject
          Set.empty
          [ ( "A",
              "{-# LANGUAGE PatternSynonyms, TypeFamilies, TemplateHaskell #-}\n\
              \module A where\n\
              \data T = T\n\
              \class C a where\n\
              \  m :: a\n\
              \instance C T where\n\
              \  m = T\n\
              \f :: Int\n\
              \f = 1\n\
              \pattern P = T\n\
              \type family F a\n\
              \type instance F T = Int\n\
              \$(pure [])\n"
            )
          ]
      )
      "A"
  [(e.kind, renderDeclId e.id, e.line, e.firstLine, e.lastLine) | e <- l.links.outline]
    @?= [ (OutlineType, "type:T", Line 3, Line 3, Line 3),
          (OutlineClass, "class:C", Line 4, Line 4, Line 5),
          (OutlineInstance, "inst:C:T", Line 6, Line 6, Line 7),
          (OutlineValue, "val:f", Line 8, Line 8, Line 9),
          (OutlinePatSyn, "pat:P", Line 10, Line 10, Line 10),
          (OutlineType, "type:F", Line 11, Line 11, Line 11),
          (OutlineFamInst, "inst:F:T", Line 12, Line 12, Line 12)
        ]

noTableCase :: Assertion
noTableCase = do
  let w = memProject Set.empty [("A", "module A where\nf x = x\n")]
  l <- linkedOf w "A"
  let p0 = l.source.parsed
      p = p0 {occurrences = noOccurrences}
      input = ParsedInput {text = l.source.text, parsed = p}
      links = linksOf w.index (const ModuleNotParsed) l.self input l.resolution
      runs = linkedRunList (highlightLinked (linkAt links) (Just p.tokens) p.regions)
  links.targets @?= []
  -- = is plain: no run.
  map snd runs @?= replicate 6 0

-- | Each module linked to has its file looked up once, however many
-- targets it has: no scan of the index per target.
pathLookups :: Assertion
pathLookups = do
  let n = 50
      i k = tshow (k :: Int)
      mods =
        [ ("K" <> i k, "module K" <> i k <> " where\n" <> defs (i k))
        | k <- [1 .. n]
        ]
          <> [ ( "U",
                 "module U where\n"
                   <> Text.concat ["import K" <> i k <> "\n" | k <- [1 .. n]]
                   <> Text.concat
                     [uses (i k) | k <- [1 .. n]]
               )
             ]
      defs j = "x" <> j <> " = ()\ny" <> j <> " = ()\n"
      uses j = "u" <> j <> " = (x" <> j <> ", y" <> j <> ")\n"
      w = memProject Set.empty mods
      u = modId "U"
  p <- case w.input.state u of
    ModuleParsed p -> pure p
    _ -> assertFailure "U not parsed"
  calls <- newIORef (0 :: Int)
  let links =
        sourceLinks
          LinksInput
            { self = u,
              index = w.index,
              text = p.text,
              parsed = p.parsed,
              resolution = resolveModule w.input u,
              state = w.input.state,
              pathOf = counted calls (modulePath w.index)
            }
  _ <- evaluate (forceSourceLinks links)
  length [() | ToDecl _ <- links.targets] @?= 3 * n
  -- The n modules K, and U itself.
  readIORef calls >>= (@?= n + 1)

-- | Count the calls of a function.
counted :: IORef Int -> (a -> b) -> a -> b
counted ref f x = unsafePerformIO $ do
  atomicModifyIORef' ref (\c -> (c + 1, ()))
  pure (f x)
{-# NOINLINE counted #-}

-- * Invariants

-- | The parsed modules of the warm-up project, linked.
warmUpModules :: [(ModuleIndex, Linked)]
warmUpModules =
  [ (resolveWarmUpIndex, l)
  | i <- resolveWarmUpModules,
    Just l <- [linkedIn resolveWarmUpIndex resolveWarmUpInput i]
  ]

-- | The parsed modules of one resolve fixture, linked.
fixtureModules :: FilePath -> IO [(ModuleIndex, Linked)]
fixtureModules c = do
  fx <- loadFixture (Dump.fixtureDir <> c)
  let input = resolveInput fx
  pure [(fx.index, l) | i <- fx.modules, Just l <- [linkedIn fx.index input i]]

-- | Links point into the table; every token that can link does; a
-- linked token is exactly one run, with its span; runs are sorted and
-- don't overlap; every target is used.
invariants :: (ModuleIndex, Linked) -> Assertion
invariants (_, l) = do
  let name = Text.unpack (renderModuleId l.self)
      p = l.source.parsed
      n = length l.links.targets
      ws = [linkAt l.links i | i <- [0 .. tokenCount p.tokens - 1]]
  assertBool (name <> ": link past the table") (all (<= fromIntegral n) ws)
  -- Every target used.
  Set.fromList (filter (/= 0) ws) @?= Set.fromList [1 .. fromIntegral n]
  -- Every token that can link does, and only those.
  forM_ (occurrenceList p.occurrences) $ \(TokenIx i, o) ->
    let canLink = case o.resolution of
          LocalRef _ -> True
          TopLevelRef _ _ -> True
          FreeRef _ -> case IntMap.lookup i . (.names) =<< l.resolution of
            Just (Unresolved _) -> False
            Just _ -> True
            Nothing -> False
          BuiltInRef -> False
          NotAName _ -> False
     in (name, i, linkAt l.links i /= 0) @?= (name, i, canLink)
  -- Runs.
  let runs = linkedRunList (highlightLinked (linkAt l.links) (Just p.tokens) p.regions)
      pos (Line a) (Col b) = (a, b)
      start (r, _) = pos r.span.startLine r.span.startCol
      end (r, _) = pos r.span.endLine r.span.endCol
      linkedRuns = sort [(start r, end r, snd r) | r <- runs, snd r /= 0]
      linkedToks =
        sort
          [ (pos s.startLine s.startCol, pos s.endLine s.endCol, w)
          | i <- [0 .. tokenCount p.tokens - 1],
            let w = linkAt l.links i,
            w /= 0,
            let s = tokenSpanAt p.tokens i
          ]
  forM_ runs $ \r -> assertBool (name <> ": empty run " <> show r) (start r < end r)
  forM_ (zip runs (drop 1 runs)) $ \(a, b) ->
    assertBool (name <> ": overlap at " <> show b) (end a <= start b)
  (name, linkedRuns) @?= (name, linkedToks)

-- * Goldens

-- | Readable dumps of a few resolve fixtures.
goldenCases :: [FilePath]
goldenCases = ["contexts", "partial", "th-cpp"]

goldenDir :: FilePath
goldenDir = "../fixtures/source/"

noStale :: Assertion
noStale = do
  files <- Dir.listDirectory goldenDir
  sort (map dropExtension files) @?= sort goldenCases

-- | Per module: its targets, its linked tokens, its outline.
golden :: FilePath -> IO LBS.ByteString
golden c = do
  mods <- fixtureModules c
  pure . LBS.fromStrict . Text.Encoding.encodeUtf8 . Text.unlines $
    concatMap dumpModule mods

dumpModule :: (ModuleIndex, Linked) -> [Text]
dumpModule (_, l) =
  ("== " <> renderModuleId l.self <> partial)
    : [ "#" <> tshow k <> " " <> renderTarget t
      | (k, t) <- zip [1 :: Int ..] l.links.targets
      ]
      <> [ renderSpan (tokenSpanAt p.tokens i)
             <> " "
             <> tshow (sliceSpan ls (tokenSpanAt p.tokens i))
             <> " -> #"
             <> tshow w
         | i <- [0 .. tokenCount p.tokens - 1],
           let w = linkAt l.links i,
           w /= 0
         ]
      <> [ "outline "
             <> tshow e.kind
             <> " "
             <> renderDeclId e.id
             <> " "
             <> lineText e.line
             <> " "
             <> lineText e.firstLine
             <> "-"
             <> lineText e.lastLine
         | e <- l.links.outline
         ]
      <> [""]
  where
    p = l.source.parsed
    ls = sourceLines l.source.text
    partial = if maybe False isPartial l.resolution then " (partial)" else ""

renderTarget :: LinkTarget -> Text
renderTarget = \case
  ToLocal x -> "local " <> lineText x.line <> ":" <> let Col c = x.col in tshow c
  ToDecl d ->
    Text.unwords $
      [ "decl",
        d.ref,
        relPathText d.path <> ":" <> maybe "?" lineText d.line,
        renderDeclId d.id
      ]
        <> reasons d.reasons d.candidates
  ToModule m ->
    Text.unwords $
      ["module", m.ref, maybe "-" relPathText m.path] <> reasons m.reasons m.candidates
  ToExternal x ->
    let ModuleName name = x.module_.name
     in Text.unwords
          [ "external",
            maybe "" (\pk -> tshow pk <> ":") x.module_.package <> name,
            howName x.how,
            tshow x.count,
            tshow x.query
          ]
  where
    reasons rs k =
      [Text.intercalate "," (map reasonName rs) | not (null rs)]
        <> ["x" <> tshow k | k /= 1]

-- | One line per i in 1 .. k.
each :: Int -> (Text -> Text) -> Text
each k f = Text.concat [f (tshow i) | i <- [1 .. k]]

lineText :: Line -> Text
lineText (Line l) = tshow l

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

-- * Linear time

-- | Links, runs and the body grow about linearly with n (build and
-- encode; parse and resolution done and forced first, D-125), and the
-- body stays within a bound per run and target. See Note [Linear time
-- tests] in "HaskellReviewer.Core.DeclsTest".
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: source links"
      [ testCase "n locals, each used" $
          linear 2000 $ \k ->
            [ ( "U",
                "module U where\nf = g\n  where\n"
                  <> each k (\i -> "    x" <> i <> " = x" <> i <> "\n")
                  <> "    g = ()\n"
              )
            ],
        testCase "n top-level declarations, each used" $
          linear 2000 $ \k ->
            [ ( "U",
                "module U where\n"
                  <> Text.concat ["x" <> tshow i <> " = ()\n" | i <- [1 .. k]]
                  <> each k (\i -> "u" <> i <> " = x" <> i <> "\n")
              )
            ],
        testCase "n external names from one open import" $
          linear 2000 $ \k ->
            [ ( "U",
                "module U where\nimport Data.Maybe\n"
                  <> each k (\i -> "u" <> i <> " = v" <> i <> "\n")
              )
            ],
        testCase "n home imports, one name each" $
          linear 300 $ \k ->
            [ ("K" <> i, "module K" <> i <> " (x" <> i <> ") where\nx" <> i <> " = ()\n")
            | i <- map tshow [1 .. k]
            ]
              <> [ ( "U",
                     "module U where\n"
                       <> Text.concat ["import K" <> tshow i <> "\n" | i <- [1 .. k]]
                       <> each k (\i -> "u" <> i <> " = x" <> i <> "\n")
                   )
                 ],
        testCase "one line of n tokens, columns past 65535" $
          linear 24000 $ \k ->
            [ ( "U",
                "module U where\nu x = "
                  <> Text.intercalate " + " (replicate k "x")
                  <> "\n"
              )
            ]
      ]

-- | Bytes allocated at n / 4 and n: the ratio must stay under 6. At n
-- the body must be within 16 bytes a run, 64 a target and 64 an
-- outline entry, plus 200.
linear :: Int -> (Int -> [(Text, Text)]) -> Assertion
linear big make = do
  (small, _) <- allocated (make (big `div` 4))
  (large, (bytes, bound)) <- allocated (make big)
  let ratio = fromIntegral large / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, large) <> ", ratio " <> show ratio) (ratio < 6)
  assertBool ("body " <> show bytes <> ", bound " <> show bound) (bytes <= bound)

allocated :: [(Text, Text)] -> IO (Int, (Int, Int))
allocated mods = do
  let w = memProject Set.empty mods
      u = modId "U"
  (p, res) <- case w.input.state u of
    ModuleParsed p -> do
      let res = resolveModule w.input u
      _ <- evaluate (Text.length p.text)
      _ <- evaluate (maybe () forceModuleResolution res)
      -- Every other module's declarations too: links read them.
      _ <-
        evaluate . sum $
          [ length q.parsed.declarations
          | (n, _) <- mods,
            ModuleParsed q <- [w.input.state (modId n)]
          ]
      pure (p, res)
    _ -> assertFailure "U not parsed"
  a0 <- getAllocationCounter
  let links = linksOf w.index w.input.state u p res
  _ <- evaluate (forceSourceLinks links)
  let lexed = parsedSource p.parsed links
      body =
        encodeSource
          (Generation 1)
          w.index
          SourceView
            { path = libFile "U",
              moduleId = Just u,
              partial = False,
              body = LexedBody lexed
            }
  bytes <- evaluate (fromIntegral (LBS.length body))
  a1 <- getAllocationCounter
  when (null links.targets) (assertFailure "no targets")
  let runs =
        linkedRunCount
          (highlightLinked (linkAt links) (Just p.parsed.tokens) p.parsed.regions)
      bound = 16 * runs + 64 * length links.targets + 64 * length links.outline + 200
  pure (fromIntegral (a0 - a1), (bytes, bound))
