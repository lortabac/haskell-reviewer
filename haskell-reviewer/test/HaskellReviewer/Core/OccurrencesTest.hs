-- | The occurrence table: packing, scoping on generated modules, the
-- invariants on the fixtures, GHC's AST and collectors as oracles, the
-- warm-up's coverage, and linear time.
module HaskellReviewer.Core.OccurrencesTest (tests, linearTests) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Control.Monad.ST (runST)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Data, cast, gmapQ)
import Data.List (nub, sort, (\\))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Hs
  ( CollectFlag (..),
    GhcPs,
    HsExpr (..),
    HsLocalBinds,
    Match (..),
    StmtLR (..),
    collectLocalBinders,
    collectPatBinders,
    collectPatsBinders,
  )
import GHC.Parser.Annotation (EpAnn, NameAnn, SrcSpanAnnA, locA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, isExact, isOrig, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan, unLoc)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.Occurrences
  ( Binder (..),
    BinderId (..),
    BinderName (..),
    Certainty (..),
    FreeContext (..),
    Namespace (..),
    NotNameKind (..),
    Occurrence (..),
    Occurrences,
    Ref (..),
    Resolution (..),
    Role (..),
    TargetIx,
    TokenIx (..),
    addTarget,
    binderAt,
    binderCount,
    conflicts,
    freezeBuilder,
    hasTopSplices,
    mergeScopes,
    newBinder,
    newBuilder,
    occurrenceAt,
    occurrenceList,
    record,
    recordPun,
  )
import HaskellReviewer.Core.OccurrencesGoldenTest (Case (..), cases, readCase)
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    ParsedAst (..),
    defaultParseSettings,
    parseAst,
    parseSource,
  )
import HaskellReviewer.Core.Parser.Helpers (toSpan)
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens
  ( TokenClass (..),
    Tokens,
    nameLikeClass,
    tokenClassAt,
    tokenCount,
    tokenIndexFrom,
    tokenSpanAt,
  )
import HaskellReviewer.Effects.Parser (occurrencesWarmUp, parserWarmUp)
import Hedgehog (Gen, Property, annotate, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as Dir
import System.FilePath (takeExtension)
import System.Mem (getAllocationCounter)
import Test.Tasty (DependencyType (..), TestTree, after, dependentTestGroup, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Occurrences"
    [ testProperty "pack, then read back" packing,
      testProperty "generated modules: each name finds its binder" generated,
      testCase "fixtures and warm-up: invariants" fixtureInvariants,
      testCase "GHC's AST: every located name has an entry (Data.Data oracle)" astOracle,
      testCase "GHC's collectors: their binders are ours" collectorOracle,
      testCase "the warm-up covers every resolution, role and namespace" warmUpCovers,
      testCase "a text-only module has no table" $
        case parseSource defaultParseSettings "x = = 1" of
          TextOnly _ -> pure ()
          Parsed _ -> assertFailure "parsed"
    ]

-- * Packing

-- | A random table: entries, binders, puns, targets; read back.
packing :: Property
packing = property $ do
  n <- forAll (Gen.int (Range.linear 1 300))
  nTargets <- forAll (Gen.int (Range.linear 1 10))
  binders <- forAll (Gen.list (Range.linear 0 50) genBinder)
  entries <- forAll (Gen.list (Range.linear 0 200) (genEntry n nTargets (length binders)))
  let targets = [Named DVal (Text.pack ("t" <> show k)) | k <- [1 .. nTargets]]
      -- One entry per token: the first wins, as in the builder.
      firsts = nubOrdOn (.token) entries
      occs = runST $ do
        b <- newBuilder n
        ts <- mapM (addTarget b) targets
        bs <- forM binders $ \(s, name, scope) -> newBinder b s name scope
        forM_ entries $ \e -> do
          let r = toRef ts bs e.ref
              i = TokenIx e.token
          case e.pun of
            Nothing -> record b i e.namespace e.role r e.splice
            Just f -> recordPun b i e.namespace e.role r (toRef ts bs f) e.splice
        freezeBuilder b False
  conflicts occs === length entries - length firsts
  [occurrenceAt occs (TokenIx i) | i <- [0 .. n - 1]]
    === [ case [e | e <- firsts, e.token == i] of
            e : _ ->
              Just
                Occurrence
                  { namespace = e.namespace,
                    role = e.role,
                    resolution = toResolution targets e.ref,
                    inSplice = e.splice,
                    punField = toResolution targets <$> e.pun
                  }
            [] -> Nothing
        | i <- [0 .. n - 1]
        ]
  [binderAt occs (BinderId k) | k <- [0 .. length binders - 1]]
    === [ Just
            Binder {span = s, name = maybe AtSpan Stored name, scopes = mergeScopes scope}
        | (s, name, scope) <- binders
        ]
  binderAt occs (BinderId (length binders)) === Nothing

-- | One generated entry.
data Entry = Entry
  { token :: Int,
    namespace :: Namespace,
    role :: Role,
    ref :: TestRef,
    splice :: Bool,
    pun :: Maybe TestRef
  }
  deriving (Show)

-- | A resolution with plain indexes, for the generator.
data TestRef
  = TLocal Int
  | TTop Int Certainty
  | TFree FreeContext
  | TBuiltIn
  | TNotAName NotNameKind
  deriving (Show)

toRef :: [TargetIx] -> [BinderId] -> TestRef -> Ref
toRef ts bs = \case
  TLocal k -> LocalR (bs !! k)
  TTop k c -> TopLevelR (ts !! k) c
  TFree c -> FreeR c
  TBuiltIn -> BuiltInR
  TNotAName k -> NotANameR k

toResolution :: [DeclId] -> TestRef -> Resolution
toResolution targets = \case
  TLocal k -> LocalRef (BinderId k)
  TTop k c -> TopLevelRef (targets !! k) c
  TFree c -> FreeRef c
  TBuiltIn -> BuiltInRef
  TNotAName k -> NotAName k

genEntry :: Int -> Int -> Int -> Gen Entry
genEntry n nTargets nBinders =
  Entry
    <$> Gen.int (Range.linear 0 (n - 1))
    <*> Gen.enumBounded
    <*> Gen.enumBounded
    <*> genRef
    <*> Gen.bool
    <*> Gen.maybe genRef
  where
    genRef =
      Gen.choice $
        [TTop <$> Gen.int (Range.linear 0 (nTargets - 1)) <*> Gen.enumBounded]
          <> [TLocal <$> Gen.int (Range.linear 0 (nBinders - 1)) | nBinders > 0]
          <> [ TFree <$> genContext,
               pure TBuiltIn,
               TNotAName <$> Gen.enumBounded
             ]
    genContext =
      Gen.choice
        [ pure Plain,
          MethodOfClass <$> tok,
          FieldOfCon <$> tok,
          MaybeWildcardOf <$> tok,
          pure QuasiQuoter
        ]
    tok = TokenIx <$> Gen.int (Range.linear 0 100000)

genBinder :: Gen (Span, Maybe Text, [Span])
genBinder =
  (,,)
    <$> genSpan
    <*> Gen.maybe (Gen.text (Range.linear 1 5) Gen.alpha)
    <*> Gen.list (Range.linear 0 3) genSpan

genSpan :: Gen Span
genSpan = do
  l <- Gen.word32 (Range.linear 1 1000)
  c <- Gen.word32 (Range.linear 1 200)
  pure (Span (Line l) (Col c) (Line (l + 1)) (Col c))

-- * Generated modules

-- | A small expression language with a pool of three names, so names
-- shadow each other often.
data Expr
  = Var Text
  | App Expr Expr
  | Lam Text Expr
  | Let Text Expr Expr
  | CaseOf Expr Text Expr
  | Do Text Expr Expr
  | Comp Expr Text Expr
  | Plus Expr Expr
  | Where Text Expr Expr
  | -- | @mdo { a <- e1; rec { b <- e2 }; e3 }@: a and b are in scope
    -- everywhere. The names differ.
    MDo Text Expr Text Expr Expr
  | -- | @do { rec { a <- e1; rec { b <- e2 } }; e3 }@: the same, in a
    -- nested rec.
    RecDo Text Expr Text Expr Expr
  deriving (Show)

genExpr :: Gen Expr
genExpr = Gen.recursive Gen.choice [Var <$> name] rec
  where
    name = Gen.element ["a", "b", "c"]
    rec =
      [ Gen.subterm2 genExpr genExpr App,
        Gen.subtermM genExpr (\e -> Lam <$> name <*> pure e),
        Gen.subtermM2 genExpr genExpr (\e1 e2 -> Let <$> name <*> pure e1 <*> pure e2),
        Gen.subtermM2 genExpr genExpr (\e1 e2 -> (\n -> CaseOf e1 n e2) <$> name),
        Gen.subtermM2 genExpr genExpr (\e1 e2 -> (\n -> Do n e1 e2) <$> name),
        Gen.subtermM2 genExpr genExpr (\h e -> (\n -> Comp h n e) <$> name),
        Gen.subterm2 genExpr genExpr Plus,
        Gen.subtermM2 genExpr genExpr (\b e -> (\n -> Where n b e) <$> name),
        Gen.subtermM3 genExpr genExpr genExpr (\x y z -> recs MDo x y z),
        Gen.subtermM3 genExpr genExpr genExpr (\x y z -> recs RecDo x y z)
      ]
    recs con x y z = do
      a <- name
      b <- Gen.element (filter (/= a) ["a", "b", "c"])
      pure (con a x b y z)

-- | What a piece of text is: a binder (by number), a use (of a binder,
-- or free), or plain.
data Mark = Plain' | Bind Int | UseOf (Maybe Int)
  deriving (Eq, Show)

-- | The text, marked, in one line. Binders are numbered in order.
render :: Expr -> [(Text, Mark)]
render e0 = fst (go [] e0 0)
  where
    p t = [(t, Plain')]
    go env e k = case e of
      Var n -> ([(n, UseOf (lookup n env))], k)
      App f x ->
        let (a, k1) = go env f k
            (b, k2) = go env x k1
         in (p "(" <> a <> p " " <> b <> p ")", k2)
      Plus x y ->
        let (a, k1) = go env x k
            (b, k2) = go env y k1
         in (p "(" <> a <> p " + " <> b <> p ")", k2)
      Lam n x ->
        let (a, k1) = go ((n, k) : env) x (k + 1)
         in (p "(\\" <> [(n, Bind k)] <> p " -> " <> a <> p ")", k1)
      Let n x y ->
        let env' = (n, k) : env
            (a, k1) = go env' x (k + 1)
            (b, k2) = go env' y k1
         in (p "(let { " <> [(n, Bind k)] <> p " = " <> a <> p " } in " <> b <> p ")", k2)
      CaseOf x n y ->
        let (a, k1) = go env x k
            (b, k2) = go ((n, k1) : env) y (k1 + 1)
            bound = [(n, Bind k1)]
         in (p "(case " <> a <> p " of { " <> bound <> p " -> " <> b <> p " })", k2)
      Do n x y ->
        let (a, k1) = go env x k
            (b, k2) = go ((n, k1) : env) y (k1 + 1)
         in (p "(do { " <> [(n, Bind k1)] <> p " <- " <> a <> p "; " <> b <> p " })", k2)
      -- The head sees the generator's binder; the generator doesn't.
      Comp h n x ->
        let (a, k1) = go env x k
            (b, k2) = go ((n, k1) : env) h (k1 + 1)
         in (p "[" <> b <> p " | " <> [(n, Bind k1)] <> p " <- " <> a <> p "]", k2)
      -- A local function with a where clause.
      Where n x y ->
        let env' = (n, k) : env
            (a, k1) = go env' x (k + 1)
            (b, k2) = go env' y k1
            bound = [(n, Bind k)]
            body = a <> p " where { " <> bound <> p " = " <> b <> p " }"
         in (p "(let { g = " <> body <> p " } in g)", k2)
      MDo n1 x n2 y z ->
        let env' = (n2, k + 1) : (n1, k) : env
            (a, k1) = go env' x (k + 2)
            (b, k2) = go env' y k1
            (c, k3) = go env' z k2
         in ( p "(mdo { "
                <> [(n1, Bind k)]
                <> p " <- "
                <> a
                <> p "; rec { "
                <> [(n2, Bind (k + 1))]
                <> p " <- "
                <> b
                <> p " }; "
                <> c
                <> p " })",
              k3
            )
      RecDo n1 x n2 y z ->
        let env' = (n2, k + 1) : (n1, k) : env
            (a, k1) = go env' x (k + 2)
            (b, k2) = go env' y k1
            (c, k3) = go env' z k2
         in ( p "(do { rec { "
                <> [(n1, Bind k)]
                <> p " <- "
                <> a
                <> p "; rec { "
                <> [(n2, Bind (k + 1))]
                <> p " <- "
                <> b
                <> p " } }; "
                <> c
                <> p " })",
              k3
            )

generated :: Property
generated = property $ do
  e <- forAll genExpr
  let pieces = render e
      code = Text.concat (map fst pieces)
      text = "{-# LANGUAGE RecursiveDo #-}\nmodule M where\nx = " <> code <> "\n"
  annotate (Text.unpack text)
  case parseSource defaultParseSettings text of
    TextOnly t -> do
      annotate (show t)
      assert False
    Parsed m -> do
      let occs = m.occurrences
          cols = scanl (+) 5 [Text.length t | (t, _) <- pieces]
          marked = [(c, mk) | ((_, mk), c) <- zip pieces cols, mk /= Plain']
          binderCol = [(b, c) | (c, Bind b) <- marked]
          at c =
            let i = tokenIndexFrom m.tokens (Line 3) (Col (fromIntegral c))
             in occurrenceAt occs (TokenIx i)
          binderStart r = case r of
            Just Occurrence {resolution = LocalRef b} ->
              (\bd -> bd.span.startCol) <$> binderAt occs b
            _ -> Nothing
      forM_ marked $ \(c, mk) -> case mk of
        Bind b -> do
          ((.role) <$> at c) === Just Def
          binderStart (at c) === (Col . fromIntegral <$> lookup b binderCol)
        UseOf (Just b) ->
          binderStart (at c) === (Col . fromIntegral <$> lookup b binderCol)
        UseOf Nothing -> ((.resolution) <$> at c) === Just (FreeRef Plain)
        Plain' -> pure ()
      invariants text m === []

-- * Invariants

-- | What must hold for any table: an entry for every identifier token,
-- no conflict, each local use inside its binder's scope, each binder
-- inside or apart from each of its scopes (@x <- e@ binds from the next
-- statement on; a comprehension's head comes before), and no free or
-- top-level variable inside the scope of a local binder of the same name.
--
-- Skipped by the last one: names in a splice (top-level names are free
-- there), and fields and implicit parameters (not VarNs). Not skipped:
-- arrow tails and the operator of @(| |)@, which see only the scope
-- outside the proc and so could break it; no corpus module has one.
invariants :: Text -> ParsedModule -> [String]
invariants text m =
  [ "no entry: " <> show (spanText (tokenSpanAt ts i))
  | i <- [0 .. tokenCount ts - 1],
    nameLikeClass (tokenClassAt ts i),
    not (isJust (occurrenceAt occs (TokenIx i)))
  ]
    <> ["conflicts: " <> show (conflicts occs) | conflicts occs /= 0]
    <> [ "use out of scope: " <> show (spanText (spanOf i))
       | (i, o) <- occurrenceList occs,
         LocalRef b <- [o.resolution],
         o.role /= Def,
         Just bd <- [binderAt occs b],
         not (any (inside (spanOf i)) bd.scopes)
       ]
    <> [ "binder outside its scope: " <> show bd
       | k <- [0 .. binderCount occs - 1],
         Just bd <- [binderAt occs (BinderId k)],
         not (all (\sc -> inside bd.span sc || apart bd.span sc) bd.scopes)
       ]
    <> [ "shadowed but not local: " <> show (spanText (spanOf i))
       | (i, o) <- occurrenceList occs,
         o.role == Use,
         o.namespace == VarNs,
         not o.inSplice,
         notLocal o.resolution,
         bd <- Map.findWithDefault [] (sliceSpan src (spanOf i)) bindersByName,
         any (inside (spanOf i)) bd.scopes
       ]
  where
    occs = m.occurrences
    ts = m.tokens
    src = sourceLines text
    spanOf (TokenIx i) = tokenSpanAt ts i
    spanText s = (s, sliceSpan src s)
    notLocal = \case
      FreeRef _ -> True
      TopLevelRef _ _ -> True
      _ -> False
    bindersByName =
      Map.fromListWith
        (<>)
        [ (name, [bd])
        | k <- [0 .. binderCount occs - 1],
          Just bd <- [binderAt occs (BinderId k)],
          let name = case bd.name of
                AtSpan -> sliceSpan src bd.span
                Stored t -> t
        ]
    inside a s = start a >= start s && end a <= end s
    apart a s = end a <= start s || start a >= end s
    start s = (s.startLine, s.startCol)
    end s = (s.endLine, s.endCol)

-- | Every module we check: the occurrence fixtures, the other fixture
-- dirs (with plain settings), the warm-up modules.
corpus :: IO [(String, ParseSettings, Text)]
corpus = do
  occ <- forM cases $ \c -> (c.file,c.settings,) <$> readCase c
  others <- fmap concat . forM ["decls", "parse", "highlight"] $ \d -> do
    let dir = "../fixtures/" <> d <> "/"
    files <- sort <$> Dir.listDirectory dir
    forM [f | f <- files, takeExtension f `elem` [".hs", ".lhs", ".hsig"]] $ \f -> do
      t <- Text.Encoding.decodeUtf8Lenient <$> BS.readFile (dir <> f)
      pure (d <> "/" <> f, settingsFor f, t)
  let warmUp =
        [("warm-up " <> show k, s, t) | (k, (s, t)) <- zip [1 :: Int ..] parserWarmUp]
  pure (occ <> others <> warmUp)
  where
    settingsFor f = case takeExtension f of
      ".lhs" -> defaultParseSettings {literate = BirdOrLatex}
      ".hsig" -> defaultParseSettings {kind = SignatureFile}
      _ -> defaultParseSettings

fixtureInvariants :: Assertion
fixtureInvariants = do
  mods <- corpus
  let bad =
        [ (name, v)
        | (name, s, t) <- mods,
          Parsed m <- [parseSource s t],
          v <- invariants t m
        ]
  bad @?= []

-- * Oracles

-- | An expression, located.
type LExpr = GenLocated SrcSpanAnnA (HsExpr GhcPs)

-- | Everything of one type in a value, by Data.Data.
collect :: forall b a. (Data a, Data b) => a -> [b]
collect x = maybe id (:) (cast x) (concat (gmapQ (collect @b) x))

-- | Every located name GHC parsed must have an entry on a token in its
-- span (or the one it is in, for a quasi-quoter), be inside a macro
-- call, or be a built-in (Exact) name.
astOracle :: Assertion
astOracle = do
  mods <- corpus
  let checked =
        [ (name, Text.unpack (sliceSpan (sourceLines t) s), ok)
        | (name, settings, t) <- mods,
          Right p <- [parseAst settings t],
          (l, rdr) <- located p,
          not (maybe False (\r -> isExact r || isOrig r) rdr),
          Just s <- [toSpan p.spans l],
          let ok = covered p.tokens p.model.occurrences s
        ]
  -- Not empty by mistake: the fixtures have thousands of names.
  assertBool ("names checked: " <> show (length checked)) (length checked > 2000)
  [(name, s) | (name, s, False) <- checked] @?= []
  where
    located :: ParsedAst -> [(SrcSpan, Maybe RdrName)]
    located p =
      [(locA l, Just r) | L l r <- collect @(GenLocated (EpAnn NameAnn) RdrName) p.ast]
        <> [ (locA l, Nothing)
           | L l e <- collect @(GenLocated SrcSpanAnnA (HsExpr GhcPs)) p.ast,
             isNamed e
           ]
    isNamed = \case
      HsIPVar {} -> True
      HsOverLabel {} -> True
      _ -> False

-- | A name at this span has an entry.
covered :: Tokens -> Occurrences -> Span -> Bool
covered ts occs s =
  any hasEntry inSpan || maybe False (\i -> hasEntry i || macro i) before
  where
    first = tokenIndexFrom ts s.startLine s.startCol
    inSpan =
      takeWhile
        (\i -> start (tokenSpanAt ts i) < end s)
        [first .. tokenCount ts - 1]
    -- The token the span starts in, when it starts inside one.
    before
      | first > 0, end (tokenSpanAt ts (first - 1)) > start s = Just (first - 1)
      | otherwise = Nothing
    hasEntry i = isJust (occurrenceAt occs (TokenIx i)) || tokenClassAt ts i == Macro
    macro i = tokenClassAt ts i == Macro
    start x = (x.startLine, x.startCol)
    end x = (x.endLine, x.endCol)

-- | The names GHC's collectors find in patterns and local bindings are
-- all binders in our table (we have more: RULES, proc, pattern synonym
-- arguments, quotes).
collectorOracle :: Assertion
collectorOracle = do
  mods <- corpus
  forM_ mods $ \(name, settings, t) -> case parseAst settings t of
    Left _ -> pure ()
    Right p -> do
      let occs = p.model.occurrences
          ours = sort (map (binderName t occs) [0 .. binderCount occs - 1])
          theirs =
            sort . filter (/= "pun-right-hand-side") . map nameText $
              concat
                [ collectPatsBinders CollNoDictBinders (unLoc pats)
                | Match {m_pats = pats} <- collect @(Match GhcPs LExpr) p.ast
                ]
                <> concat
                  [ collectLocalBinders CollNoDictBinders b
                  | b <- collect @(HsLocalBinds GhcPs) p.ast
                  ]
                <> concat
                  [ collectPatBinders CollNoDictBinders pat
                  | BindStmt _ pat _ <- collect @(StmtLR GhcPs GhcPs LExpr) p.ast
                  ]
          missing = theirs \\ ours
      unless (null missing) $
        assertFailure (name <> ": not binders: " <> show missing)
  where
    nameText = Text.pack . occNameString . rdrNameOcc
    binderName t occs k = case binderAt occs (BinderId k) of
      Just Binder {span = s, name = AtSpan} -> sliceSpan (sourceLines t) s
      Just Binder {name = Stored n} -> n
      Nothing -> ""

-- * Warm-up

warmUpCovers :: Assertion
warmUpCovers = case parseSource defaultParseSettings occurrencesWarmUp of
  TextOnly t -> assertFailure (show t)
  Parsed m -> do
    let occs' = m.occurrences
        occs = map snd (occurrenceList m.occurrences)
        kinds = nub (sort (map (kind . (.resolution)) occs))
    invariants occurrencesWarmUp m @?= []
    kinds
      @?= sort
        [ "local",
          "top",
          "top-heuristic",
          "free",
          "free-method",
          "free-field",
          "free-wildcard",
          "free-quasiquoter",
          "builtin",
          "not-a-name module",
          "not-a-name label",
          "not-a-name other"
        ]
    nub (sort (map (.role) occs)) @?= [minBound .. maxBound]
    nub (sort (map (.namespace) occs)) @?= [minBound .. maxBound]
    assertBool "a pun" (any (isJust . (.punField)) occs)
    assertBool "a splice" (any (.inSplice) occs)
    assertBool "top-level splices" (hasTopSplices m.occurrences)
    assertBool
      "a .. binder"
      (any isNamed (mapMaybe (binderAt occs' . BinderId) [0 .. binderCount occs' - 1]))
  where
    isNamed b = case b.name of
      Stored _ -> True
      AtSpan -> False
    kind :: Resolution -> String
    kind = \case
      LocalRef _ -> "local"
      TopLevelRef _ Exact -> "top"
      TopLevelRef _ Heuristic -> "top-heuristic"
      FreeRef Plain -> "free"
      FreeRef (MethodOfClass _) -> "free-method"
      FreeRef (FieldOfCon _) -> "free-field"
      FreeRef (MaybeWildcardOf _) -> "free-wildcard"
      FreeRef QuasiQuoter -> "free-quasiquoter"
      BuiltInRef -> "builtin"
      NotAName ModuleWord -> "not-a-name module"
      NotAName LabelWord -> "not-a-name label"
      NotAName OtherWord -> "not-a-name other"

-- * Linear time

-- | Hostile input: allocation at n and 4n (D-144). The occurrence table
-- is built with the parse.
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    dependentTestGroup
      "Linear time: occurrences"
      AllFinish
      [ testCase "nested lets" $
          linear 10_000 $ \k ->
            "x = "
              <> Text.concat [lets i | i <- [1 .. k]]
              <> "v0\n",
        testCase "a long do block" $
          linear 50_000 $ \k ->
            "x = do\n"
              <> Text.unlines ["  v" <> n i <> " <- f v" <> n (i - 1) | i <- [1 .. k]]
              <> "  pure v1\n",
        testCase "many variables in one pattern" $
          linear 10_000 $ \k ->
            "f (" <> Text.intercalate ", " ["v" <> n i | i <- [1 .. k]] <> ") = v1\n",
        testCase "many equations" $
          linear 50_000 $ \k ->
            Text.unlines ["f " <> n i <> " x = x" | i <- [1 .. k]],
        testCase "deep lambdas" $
          linear 10_000 $ \k ->
            "x = " <> Text.concat ["\\v" <> n i <> " -> " | i <- [1 .. k]] <> "v1\n",
        testCase "a wide record with a wildcard" $
          linear 10_000 $ \k ->
            "{-# LANGUAGE RecordWildCards #-}\ndata R = R {"
              <> Text.intercalate ", " ["f" <> n i <> " :: Int" | i <- [1 .. k]]
              <> "}\ng R {f1 = a, ..} = a + f2\nh r = R {"
              <> Text.intercalate ", " ["f" <> n i <> " = 1" | i <- [1 .. k]]
              <> "}\n",
        testCase "k fields, k wildcards" $
          linear 2_000 $ \k ->
            "{-# LANGUAGE RecordWildCards #-}\ndata R = R {"
              <> Text.intercalate ", " ["f" <> n i <> " :: Int" | i <- [1 .. k]]
              <> "}\n"
              <> Text.unlines ["g" <> n i <> " R {..} = f" <> n i | i <- [1 .. k]],
        testCase "a field in many constructors, used often" $
          linear 10_000 $ \k ->
            "data T = "
              <> Text.intercalate " | " ["C" <> n i <> " {f :: Int}" | i <- [1 .. k]]
              <> "\n"
              <> Text.unlines ["g" <> n i <> " x = f x" | i <- [1 .. 2 * k]],
        testCase "a chain of then f" $
          linear 4_000 $ \k ->
            "{-# LANGUAGE TransformListComp #-}\nx = [v1 | "
              <> Text.intercalate ", " ["v" <> n i <> " <- y, then f" | i <- [1 .. k]]
              <> "]\n",
        testCase "nested rec blocks, braces" $
          linear 4_000 $ \k ->
            "{-# LANGUAGE RecursiveDo #-}\nx = do { "
              <> Text.concat ["rec { v" <> n i <> " <- f; " | i <- [1 .. k]]
              <> "pure v1"
              <> Text.replicate k " }"
              <> "; pure () }\n"
      ]
  where
    n :: Int -> Text
    n = Text.pack . show
    lets i = "let v" <> n i <> " = v" <> n (i - 1) <> " in "

-- | Parse at big / 4 and big: the allocation must grow about linearly.
linear :: Int -> (Int -> Text) -> Assertion
linear big make = do
  small <- allocated (big `div` 4)
  large <- allocated big
  let ratio = fromIntegral large / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, large) <> ", ratio " <> show ratio) (ratio < 6)
  where
    allocated k = do
      -- Forced before the count starts (D-125).
      let src = make k
      _ <- evaluate (Text.length src)
      a0 <- getAllocationCounter
      count <- evaluate $ case parseSource defaultParseSettings src of
        Parsed m -> binderCount m.occurrences + length (occurrenceList m.occurrences)
        TextOnly _ -> 0
      a1 <- getAllocationCounter
      assertBool "no occurrences" (count > 0)
      pure (a0 - a1)
