-- | Links out of the module on its page (Note [Page links] in
-- "HaskellReviewer.Core.SourceLinks"): the same targets as the source
-- view, no local ones, every doc name sent, linear size.
module HaskellReviewer.Server.PageLinksTest (tests, linearTests) where

import ApiSupport (arr, cborValue, field, int, str)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.DocLinks (docNames, docRef)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.Modules (ModuleId, ModuleIndex, renderModuleId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Resolve
  ( ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
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
    DeclRole (..),
    LinkTarget (..),
    ModuleLink (..),
    PageLinks (..),
    SourceLinks (..),
    forcePageLinks,
    linkAt,
    linksInput,
    noPageLinks,
    pageLinks,
    sourceLinks,
  )
import HaskellReviewer.Effects.Parser (declsWarmUp, haddockWarmUp)
import HaskellReviewer.Server.Wire.Page
  ( ModuleAnswer (..),
    ModuleView (..),
    encodeModulePage,
  )
import HaskellReviewer.Session.Index (ParsedSource (..))
import HaskellReviewer.Session.Snapshot (Generation (..))
import IndexSupport (MemProject (..), libFile, memProject, modId)
import ResolveDump (Fixture (..), cases, loadFixture, resolveInput)
import ResolveDump qualified as Dump (fixtureDir)
import System.Mem (getAllocationCounter)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Page links"
    [ testCase "code and docs link out; locals stay handles" unitCase,
      testCase "invariants: warm-up project" $
        mapM_ (invariants resolveWarmUpIndex resolveWarmUpInput) resolveWarmUpModules,
      testCase "invariants: resolve fixtures" $
        mapM_
          ( \c -> do
              fx <- loadFixture (Dump.fixtureDir <> c)
              mapM_ (invariants fx.index (resolveInput fx)) fx.modules
          )
          cases,
      testCase "every doc name is on the page once" docNamesCase
    ]

-- * Unit

unitCase :: Assertion
unitCase = do
  let w =
        memProject
          Set.empty
          [ ("A", "module A (T, g) where\ndata T = T\ng :: Int\ng = 1\n"),
            ( "B",
              "module B where\n\
              \import A\n\
              \import qualified A as Q\n\
              \-- | 'g', 'Q.T', 'h', 'Nope.x'.\n\
              \h :: T -> Maybe U\n\
              \h = undefined\n\
              \data U = U T\n"
            )
          ]
  page <- pageJson w "B"
  let parsed = field "parse" page
      targets = map arr (arr (field "targets" parsed))
      target n = targets !! (n - 1)
      decls = arr (field "declarations" parsed)
  [h] <- pure [d | d <- decls, field "name" d == String "h"]
  [u] <-
    pure
      [ d
      | d <- decls,
        field "name" d == String "U",
        field "kind" d == String "type"
      ]
  let code = map arr (arr (field "code" (field "signature" h)))
      names =
        [ (str (field "written" o), field "target" o, field "link" o)
        | o@(Object m) <- everything (field "docs" h),
          KeyMap.lookup "kind" m == Just (String "name")
        ]
  -- No local targets, no target in B, every def false.
  assertBool "no local" (all (\t -> kindOf t /= "local") targets)
  assertBool "not in B" (all (\t -> str (t !! 1) /= "B") targets)
  [t !! 8 | t <- targets, kindOf t == "decl"]
    @?= [Bool False | t <- targets, kindOf t == "decl"]
  -- T links out, U to its handle, Maybe to Hoogle.
  let tokenOf t = [tok | tok <- code, str (tok !! 1) == t]
  [tokT] <- pure (tokenOf "T")
  (tokT !! 2, kindOf (target (int (tokT !! 3))), str (target (int (tokT !! 3)) !! 4))
    @?= (Null, "decl", "type:T")
  [tokU] <- pure (tokenOf "U")
  (length tokU, tokU !! 2) @?= (3, field "n" u)
  [tokMaybe] <- pure (tokenOf "Maybe")
  kindOf (target (int (tokMaybe !! 3))) @?= "external"
  -- Docs: g and Q.T link out (Q.T shares T's target), h is local, Nope.x
  -- is nothing.
  [(n, t /= Null, l /= Null) | (n, t, l) <- names]
    @?= [ ("g", False, True),
          ("Q.T", False, True),
          ("h", True, False),
          ("Nope.x", False, False)
        ]
  [l | ("Q.T", _, l) <- names] @?= [tokT !! 3]
  [str (target (int l) !! 4) | ("g", _, l) <- names] @?= ["val:g"]

-- * Invariants

-- | The page links of a module agree with its source view: each code
-- token links to the same target, as a use. No local target, none in
-- this module; each target once, in order of first use, all used.
invariants :: ModuleIndex -> ResolveInput -> ModuleId -> Assertion
invariants idx input i = case input.state i of
  ModuleParsed p -> do
    let res = resolveModule input i
        li = linksInput idx input.state i p res
        page = pageLinks li
        src = sourceLinks li
        n = length page.targets
        name = Text.unpack (renderModuleId i)
        target w = page.targets !! (fromIntegral w - 1)
    forM_ (IntMap.toList page.code) $ \(t, w) -> do
      let sw = linkAt src t
      assertBool (name <> ": no source link at " <> show t) (sw /= 0)
      (name, t, target w) @?= (name, t, asUse (src.targets !! (fromIntegral sw - 1)))
    forM_ page.targets $ \case
      ToLocal _ -> assertFailure (name <> ": a local target")
      ToDecl d -> do
        assertBool (name <> ": a decl here") (d.module_ /= i)
        d.role @?= AtUse
      ToModule m -> assertBool (name <> ": this module") (m.module_ /= i)
      ToExternal _ -> pure ()
    let docOrder = nubOrd (map docRef (docNames p.parsed))
        used =
          map snd (IntMap.toAscList page.code)
            <> [w | r <- docOrder, Just w <- [Map.lookup r page.docs]]
    (name, nubOrd used) @?= (name, [1 .. fromIntegral n])
    (name, length (nub page.targets)) @?= (name, n)
  _ -> pure ()
  where
    asUse = \case
      ToDecl d -> ToDecl d {role = AtUse}
      t -> t

-- | Name nodes on the page: one per name 'docNames' lists.
docNamesCase :: Assertion
docNamesCase = do
  forM_ [declsWarmUp, haddockWarmUp] $ \t -> case parseSource defaultParseSettings t of
    Parsed m -> do
      page <- encoded (memProject Set.empty [("M", t)]) "M" t m noPageLinks
      nameNodes page @?= length (docNames m)
    other -> assertFailure (take 300 (show other))
  forM_ resolveWarmUpModules $ \i -> case resolveWarmUpInput.state i of
    ModuleParsed p -> do
      page <- pageOfInput resolveWarmUpIndex resolveWarmUpInput i p
      let name = renderModuleId i
      (name, nameNodes page) @?= (name, length (docNames p.parsed))
    _ -> pure ()
  where
    nameNodes page =
      length
        [ ()
        | Object o <- everything page,
          KeyMap.lookup "kind" o == Just (String "name")
        ]

-- * Linear time

linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: page links"
      [ testCase "n signatures naming one external type: one target" $
          linear 2000 1 $ \k ->
            [("U", "module U where\n" <> each k (\i -> sig ("x" <> i) "Maybe ()"))],
        testCase "n doc refs to one name: one target" $
          linear 2000 1 $ \k ->
            [ ("K", "module K (foo) where\nfoo :: ()\nfoo = ()\n"),
              ( "U",
                "module U where\nimport K\n-- | "
                  <> Text.unwords (replicate k "'foo'")
                  <> "\nu :: ()\nu = ()\n"
              )
            ],
        testCase "n distinct doc names from n imports" $
          linear 300 300 $ \k ->
            [("K" <> i, "module K" <> i <> " where\nx" <> i <> " = ()\n") | i <- nums k]
              <> [ ( "U",
                     "{-# LANGUAGE NoImplicitPrelude #-}\nmodule U where\n"
                       <> Text.concat ["import K" <> i <> "\n" | i <- nums k]
                       <> "-- | "
                       <> Text.unwords ["'x" <> i <> "'" | i <- nums k]
                       <> "\nu = ()\n"
                   )
                 ],
        testCase "one long signature target, used n times" $
          linear 2000 1 $ \k ->
            [ ( "K",
                "module K where\ntype T = "
                  <> Text.intercalate " -> " (replicate 200 "()")
                  <> "\n"
              ),
              ( "U",
                "{-# LANGUAGE NoImplicitPrelude #-}\nmodule U where\nimport K\n"
                  <> each k (\i -> sig ("x" <> i) "T")
              )
            ]
      ]
  where
    sig x t = x <> " :: " <> t <> "\n" <> x <> " = undefined\n"
    nums k = map tshow [1 .. k]
    each k f = Text.concat (map f (nums k))

-- | Bytes allocated at n / 4 and n, for resolution, links and page:
-- the ratio must stay under 6. At n, the table has at most this many
-- targets.
linear :: Int -> Int -> (Int -> [(Text, Text)]) -> Assertion
linear big most make = do
  (small, _) <- allocated (make (big `div` 4))
  (large, n) <- allocated (make big)
  let ratio = fromIntegral large / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, large) <> ", ratio " <> show ratio) (ratio < 6)
  assertBool ("targets " <> show n) (n <= most)

allocated :: [(Text, Text)] -> IO (Int, Int)
allocated mods = do
  let w = memProject Set.empty mods
      u = modId "U"
  case w.input.state u of
    ModuleParsed p -> do
      _ <- evaluate (Text.length p.text)
      _ <-
        evaluate . sum $
          [ length q.parsed.declarations
          | (n, _) <- mods,
            ModuleParsed q <- [w.input.state (modId n)]
          ]
      -- Resolution too: the doc names are resolved there.
      a0 <- getAllocationCounter
      let res = resolveModule w.input u
      _ <- evaluate (maybe () forceModuleResolution res)
      let links = pageLinks (linksInput w.index w.input.state u p res)
      _ <- evaluate (forcePageLinks links)
      _ <-
        evaluate . LBS.length $
          encodeModulePage (Generation 1) w.index (view u p links)
      a1 <- getAllocationCounter
      when (null links.targets) (assertFailure "no targets")
      pure (fromIntegral (a0 - a1), length links.targets)
    _ -> assertFailure "U not parsed"

-- * Helpers

-- | The page of a module of the project, resolved.
pageJson :: MemProject -> Text -> IO Value
pageJson w n = case w.input.state (modId n) of
  ModuleParsed p -> pageOfInput w.index w.input (modId n) p
  _ -> assertFailure ("not parsed: " <> show n)

-- | A parsed module's page, resolved, partial if resolution says so.
pageOfInput :: ModuleIndex -> ResolveInput -> ModuleId -> ParsedInput -> IO Value
pageOfInput idx input i p = do
  let res = resolveModule input i
      links = pageLinks (linksInput idx input.state i p res)
  cborValue . encodeModulePage (Generation 1) idx $
    (view i p links) {partial = maybe False isPartial res}

-- | A module's page with these links, not partial.
encoded :: MemProject -> Text -> Text -> ParsedModule -> PageLinks -> IO Value
encoded w n t m links =
  cborValue . encodeModulePage (Generation 1) w.index $
    view (modId n) ParsedInput {text = t, parsed = m} links

-- | A parsed module's view with these links, not partial. The path is
-- only shown, so any one does.
view :: ModuleId -> ParsedInput -> PageLinks -> ModuleView
view i p links =
  ModuleView
    { moduleId = i,
      path = Just (libFile "U"),
      partial = False,
      answer = ParsedAnswer ParsedSource {text = p.text, parsed = p.parsed} links
    }

-- | A target's kind.
kindOf :: [Value] -> Text
kindOf = \case
  k : _ -> str k
  [] -> "?"

-- | A value and everything inside it.
everything :: Value -> [Value]
everything v =
  v : case v of
    Object o -> concatMap everything (KeyMap.elems o)
    Array a -> concatMap everything (toList a)
    _ -> []

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
