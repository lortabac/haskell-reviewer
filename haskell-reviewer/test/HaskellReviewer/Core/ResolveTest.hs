{-# LANGUAGE StrictData #-}

-- StrictData: values built with the test tree are built in one thread.
-- See Note [Shared values in tests] in
-- "HaskellReviewer.Core.PackageGlobTest".

-- | Resolution: the warm-up's coverage, partial views and the memo,
-- properties against a naive model, filter laws, and linear time.
module HaskellReviewer.Core.ResolveTest (tests, linearTests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (getAllocationCounter)
import HaskellReviewer.Core.DocLinks (DocRef (..))
import HaskellReviewer.Core.Entities
  ( Avail (..),
    Entity (..),
    EntityKind (..),
    ExportSet (..),
    Names (..),
    OpenPart (..),
    Origin (..),
    everyEntity,
    forceExportSet,
    openParts,
    withKeys,
  )
import HaskellReviewer.Core.ImportScope (ScopeImport (..), ScopeTarget (..), scopeImport)
import HaskellReviewer.Core.ModuleGraph (ImportTarget (..), mapImport)
import HaskellReviewer.Core.ModuleModel
  ( ImportList (..),
    ListKind (..),
    Located (..),
    ModuleParse (..),
    ParsedModule (..),
    forceModuleParse,
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (Import (..))
import HaskellReviewer.Core.Modules
  ( ModuleId (..),
  )
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.Occurrences
  ( FreeContext (..),
    Occurrence (..),
    Resolution (..),
    Role (..),
    TokenIx (..),
    occurrenceList,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.Resolve
  ( ExportSets (..),
    ExternalGuess (..),
    ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    Reason (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    UnresolvedWhy (..),
    exportSets,
    forceModuleResolution,
    homeImports,
    isPartial,
    resolveModule,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
import HaskellReviewer.Core.RichText qualified as Rich (RefNamespace (..))
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenIndexFrom, tokenSpanAt)
import Hedgehog (Gen, Property, PropertyT, annotate, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import IndexSupport (memInput, modId)
import ResolveDump
  ( Fixture (..),
    cases,
    fixtureDir,
    loadFixture,
    renderGuessKind,
    renderReason,
    renderResolved,
    renderSet,
    renderWhy,
    resolveInput,
  )
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Resolve"
    [ testCase "the warm-up covers every path" warmUpCovers,
      testCase "not ready: a guess on the module, partial; exact once ready" notReadyCase,
      testCase "the memo gets final sets only" finalOnly,
      testCase "home imports: implicit Prelude, no externals, no repeats" homeImportsCase,
      testCase "doc names: qualifiers, namespaces, home first" docNamesCase,
      testCase "doc names: a hit beats a guess; DataKinds" docNamesRankCase,
      testCase
        "every free token gets one result; list tokens stay in their import"
        coverage,
      testProperty "same results as a naive model" (Hedgehog.withTests 300 differential),
      testCase "found once: T(.., P) with an ambiguous T bundles per type" $
        regression differentialFor ambiguousBundle,
      testCase "found once: pattern P from a list keeps its type" $
        regression filterLawsFor loneBundle,
      testProperty "the memo doesn't depend on request order" $
        Hedgehog.withTests 100 orderIndependent,
      testProperty
        "exact stays exact once modules are ready"
        (Hedgehog.withTests 100 monotone),
      testProperty "filter laws" (Hedgehog.withTests 100 filterLaws)
    ]

homeImportsCase :: Assertion
homeImportsCase = do
  let input =
        memInput
          Set.empty
          [ ("Prelude", "module Prelude where\n"),
            ("B", "module B where\nx = 1\n"),
            ("C", "module C where\n"),
            ("A", "module A where\nimport B\nimport Data.List\nimport B (x)\nimport C\n"),
            ( "N",
              "{-# LANGUAGE NoImplicitPrelude #-}\nmodule N where\nimport C\nimport B\n"
            ),
            ("P", "module P where\nimport C\nimport Prelude hiding (id)\n")
          ]
      home n = case input.state (modId n) of
        ModuleParsed p -> homeImports input.graph (modId n) p.parsed
        _ -> error "not parsed"
  home "A" @?= map modId ["Prelude", "B", "C"]
  home "N" @?= map modId ["C", "B"]
  home "P" @?= map modId ["C", "Prelude"]

-- * In-memory projects

-- | Each free token of a module: its text and result, in order.
results :: ResolveInput -> Text -> [(Text, Resolved)]
results input n = case (input.state (modId n), resolveModule input (modId n)) of
  (ModuleParsed p, Just r) ->
    let ls = sourceLines p.text
     in [ (sliceSpan ls (tokenSpanAt p.parsed.tokens k), res)
        | (k, res) <- IntMap.toList r.names
        ]
  _ -> []

-- * Warm-up

warmUpCovers :: Assertion
warmUpCovers = do
  let input = resolveWarmUpInput
      ms = resolveWarmUpModules
      rs = [r | m <- ms, Just r <- [resolveModule input m]]
      all' = concat [IntMap.elems r.names <> IntMap.elems r.punFields | r <- rs]
      sets = (exportSets input ms).sets
      ents = concatMap (everyEntity . (.names)) (Map.elems sets)
      parsedMods = [p.parsed | m <- ms, ModuleParsed p <- [input.state m]]
      -- Without the numbers.
      tag = Text.dropWhileEnd (\c -> c == ' ' || c `elem` ['0' .. '9'])
  nubOrd (sort (map kindTag all')) @?= ["exact", "external", "heuristic", "unresolved"]
  nubOrd (sort [tag (renderReason r) | Heuristic why _ <- all', r <- NonEmpty.toList why])
    @?= sort
      [ "ambiguous",
        "not ready",
        "not parsed",
        "made by splice",
        "made by splice here",
        "via splice",
        "wildcard",
        "record dot",
        "datakinds",
        "cpp import",
        "import cycle",
        "ambiguous module",
        "unlisted module",
        "loose import",
        "listed, not found"
      ]
  nubOrd (sort [tag (renderGuessKind g.how) | External g <- all'])
    @?= ["listed", "one of", "only open import"]
  nubOrd (sort [renderWhy w | Unresolved w <- all'])
    @?= ["import excluded by CPP", "no candidate", "not a global"]
  nubOrd (sort [t | r <- all', t <- targetTag r]) @?= ["decl", "module"]
  nubOrd (sort (map (originTag . (.origin)) ents)) @?= ["external", "home", "splice"]
  nubOrd (sort (map (.kind) ents)) @?= [minBound .. maxBound]
  nubOrd (sort [openTag p | s <- Map.elems sets, p <- openParts s])
    @?= ["cycle", "external", "not parsed", "not ready", "splices"]
  let contexts =
        [ c
        | m <- parsedMods,
          (_, o) <- occurrenceList m.occurrences,
          FreeRef c <- [o.resolution]
        ]
  nubOrd (sort (map contextTag contexts))
    @?= ["dot", "field", "method", "plain", "quoter", "wildcard"]
  let imports =
        [ (m, i)
        | m <- ms,
          ModuleParsed p <- [input.state m],
          i <- p.parsed.imports
        ]
  nubOrd (sort [filterTag input m i | (m, i) <- imports])
    @?= sort [(h, l) | h <- ["external", "home"], l <- ["hiding", "none", "only"]]
  -- Bundled pattern synonyms: lone in some sets, back under the type
  -- in another.
  let loneIn n =
        or
          [ c.kind == PatSynKind
          | (ComponentModule _ _ (ModuleName n'), s) <- Map.toList sets,
            n' == n,
            cs <- Map.elems s.names.lone,
            c <- Set.toList cs
          ]
  assertBool "a lone bundled pattern synonym (export item)" (loneIn "Bun3")
  assertBool "a lone bundled pattern synonym (through a hider)" (loneIn "Bun4")
  assertBool "a lone bundled pattern synonym (qualified module export)" (loneIn "Bun5")
  assertBool
    "a bundled pattern synonym put back under its type by a union"
    ( or
        [ c.kind == PatSynKind
        | (ComponentModule _ _ (ModuleName "Bun2"), s) <- Map.toList sets,
          a <- Map.findWithDefault [] "Nat" s.names.types,
          c <- Set.toList a.children
        ]
    )
  assertBool "a partial result" (any isPartial rs)
  assertBool "new final sets" (any (not . Map.null . (.newFinal)) rs)
  where
    openTag = \case
      ReexportsExternal _ -> "external"
      UnparsedModule _ -> "not parsed"
      PendingModule _ -> "not ready"
      SpliceModule _ -> "splices"
      CycleCut _ -> "cycle" :: Text
    kindTag = \case
      Exact _ -> "exact"
      Heuristic _ _ -> "heuristic"
      External _ -> "external"
      Unresolved _ -> "unresolved" :: Text
    targetTag = \case
      Exact t -> [tag t]
      Heuristic _ t -> [tag t]
      _ -> []
      where
        tag = \case
          DeclTarget _ _ -> "decl"
          ModuleTarget _ -> "module" :: Text
    originTag = \case
      HomeDecl _ _ -> "home"
      ExternalName _ -> "external"
      SpliceMade _ -> "splice" :: Text
    contextTag = \case
      Plain -> "plain"
      MethodOfClass _ -> "method"
      FieldOfCon _ -> "field"
      MaybeWildcardOf _ -> "wildcard"
      QuasiQuoter -> "quoter"
      FieldOfDot -> "dot" :: Text
    filterTag input m i =
      ( case mapImport input.graph m i.package i.name.value of
          HomeModule _ _ -> "home"
          ExternalImport _ -> "external" :: Text,
        case i.list of
          Nothing -> "none"
          Just l | l.kind == Only -> "only"
          Just _ -> "hiding" :: Text
      )

-- * Partial views and the memo

notReadyCase :: Assertion
notReadyCase = do
  let mods =
        [ ("A", "module A (a) where\na = ()\n"),
          ("B", "module B where\nimport A\nb = a\n")
        ]
      pending = memInput (Set.singleton "A") mods
      ready = memInput Set.empty mods
  lookup "a" (results pending "B")
    @?= Just (Heuristic (NotReady :| []) (ModuleTarget (modId "A")))
  fmap (.notReady) (resolveModule pending (modId "B")) @?= Just [modId "A"]
  lookup "a" (results ready "B")
    @?= Just (Exact (DeclTarget (modId "A") (Named DVal "a")))
  fmap isPartial (resolveModule ready (modId "B")) @?= Just False

-- | See Note [Resolution] in "HaskellReviewer.Core.Resolve".
docNamesCase :: Assertion
docNamesCase = do
  let input =
        memInput
          Set.empty
          [ ("D", "module D where\ndata T = T\nv = 1\ntype a +++ b = Either a b\n"),
            ( "A",
              "{-# LANGUAGE NoImplicitPrelude #-}\n\
              \-- | 'Q.v' 'D.v' t'T' v'T' 'T' 'sort' 'Nope.x' '+++' 'e'.\n\
              \module A where\n\
              \import qualified D as Q\n\
              \import D hiding (v)\n\
              \import Data.List (sort)\n\
              \import Data.Maybe\n"
            )
          ]
      docs = maybe [] (Map.toList . (.docNames)) (resolveModule input (modId "A"))
      exact d = "exact p:lib:D " <> d
  [(r.namespace, r.qualifier, r.occ, renderResolved x) | (r, x) <- docs]
    @?= [ (Rich.ValueNs, Nothing, "T", exact "con:T"),
          (Rich.TypeNs, Nothing, "T", exact "type:T"),
          (Rich.AnyNs, Nothing, "+++", exact "type:+++"),
          (Rich.AnyNs, Nothing, "T", exact "type:T"),
          (Rich.AnyNs, Nothing, "e", "external Data.Maybe (only open import)"),
          (Rich.AnyNs, Nothing, "sort", "external Data.List (listed)"),
          (Rich.AnyNs, Just (ModuleName "D"), "v", exact "val:v"),
          (Rich.AnyNs, Just (ModuleName "Nope"), "x", "unresolved (no candidate)"),
          (Rich.AnyNs, Just (ModuleName "Q"), "v", exact "val:v")
        ]

-- | A hit beats a guess made from a miss, whatever the order; no
-- DataKinds fallback when constructors come next.
docNamesRankCase :: Assertion
docNamesRankCase = do
  let input =
        memInput
          Set.empty
          [ ("D", "module D where\ndata N = Z\n"),
            ( "A",
              "{-# LANGUAGE DataKinds #-}\n\
              \-- | ':|' 'Z' t'Z'.\n\
              \module A where\n\
              \import Data.List.NonEmpty (NonEmpty ((:|)))\n\
              \import D\n"
            ),
            ( "S",
              "{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}\n\
              \-- | 'Data.Map.insert'.\n\
              \module S where\n\
              \import qualified Data.Map as M (insert)\n\
              \$(pure [])\n"
            )
          ]
      docs m =
        [ (r.namespace, r.occ, renderResolved x)
        | Just res <- [resolveModule input (modId m)],
          (r, x) <- Map.toList res.docNames
        ]
  docs "A"
    @?= [ (Rich.TypeNs, "Z", "heuristic[datakinds] p:lib:D con:Z"),
          (Rich.AnyNs, ":|", "external Data.List.NonEmpty (listed)"),
          (Rich.AnyNs, "Z", "exact p:lib:D con:Z")
        ]
  docs "S" @?= [(Rich.AnyNs, "insert", "external Data.Map (listed)")]

finalOnly :: Assertion
finalOnly = do
  let mods =
        [ ("A", "module A (a) where\na = ()\n"),
          ("B", "module B (module A, b) where\nimport A\nb = ()\n"),
          ("C", "module C (c) where\nc = ()\n"),
          ("D", "module D (module C, module A) where\nimport C\nimport A\n"),
          ("X", "module X (module Y, x) where\nimport {-# SOURCE #-} Y\nx = ()\n"),
          ("Y", "module Y (module X, y) where\nimport X\ny = ()\n")
        ]
      input = memInput (Set.singleton "A") mods
      ids = map (modId . fst) mods
      final = (exportSets input ids).newFinal
  Map.keys final @?= [modId "C"]
  -- With the final sets in the memo, the same results.
  let rendered i = map (renderSet . snd) (Map.toList (exportSets i ids).sets)
  rendered input {memo = final} @?= rendered input

-- * Coverage

-- | On every fixture and the warm-up: one result per free token (and
-- free pun field); a token of an import list points into its own
-- import's module.
coverage :: Assertion
coverage = do
  fxs <- mapM (\c -> loadFixture (fixtureDir <> c)) cases
  let inputs =
        (resolveWarmUpInput, resolveWarmUpModules)
          : [(resolveInput fx, fx.modules) | fx <- fxs]
  forM_ inputs $ \(input, ms) ->
    forM_ ms $ \m -> case (input.state m, resolveModule input m) of
      (ModuleParsed p, Just r) -> do
        let occs = occurrenceList p.parsed.occurrences
        IntMap.keys r.names @?= [i | (TokenIx i, o) <- occs, FreeRef _ <- [o.resolution]]
        IntMap.keys r.punFields
          @?= [ i
              | (TokenIx i, o) <- occs,
                Just (FreeRef _) <- [o.punField]
              ]
        let listTokens =
              [ i
              | (TokenIx i, o) <- occs,
                FreeRef _ <- [o.resolution],
                o.role == Import
              ]
        forM_ listTokens $ \i -> do
          let s = tokenSpanAt p.parsed.tokens i
              owner =
                [ mapImport input.graph m imp.package imp.name.value
                | imp <- p.parsed.imports,
                  within s imp.span
                ]
          case (owner, IntMap.lookup i r.names) of
            ([t], Just res) ->
              unless (inside t res) $
                assertFailure ("outside its import: " <> show (m, s, res))
            (_, other) ->
              assertFailure
                ("no import for a list token: " <> show (m, s, other))
      _ -> pure ()
  where
    within s outer =
      (s.startLine, s.startCol) >= (outer.startLine, outer.startCol)
        && (s.endLine, s.endCol) <= (outer.endLine, outer.endCol)
    inside t res = case (t, res) of
      (HomeModule m _, Exact x) -> targetIn m x
      (HomeModule m _, Heuristic _ x) -> targetIn m x
      (HomeModule _ _, External _) -> True
      (HomeModule _ _, Unresolved _) -> True
      (ExternalImport x, External g) -> g.module_ == x
      _ -> False
    -- Re-exports: the declaration may be elsewhere, so just "ours".
    targetIn m = \case
      ModuleTarget m' -> m == m'
      DeclTarget _ _ -> True

-- * The naive model

{- Generated programs: modules M0 .. Mn-1. Each defines some of:
values v0..v3; types T0..T2 (@data Tk = Tk {fk :: ()}@ or with the
constructor MkTk); classes C0, C1 (an associated type Fk and a method
mk); a @type data@ D0 = DA0 | DB0; pattern synonyms P0, P1. A module
may also not parse (it then brings nothing), or have a top-level splice
and no export list. Modules import lower ones (qualified or not, with
an alias or not, with a list, a hiding list or none), and have an
export list or not (local names, imported names, qualified names,
T(..), T(a, b), bundled pattern synonyms, pattern P, module exports).
A last module U imports some of them and uses names, one per line.

The model is plain sets of entities (module, name, kind), with each
child's parent fixed by its name, computed module by module. A type's
children in scope come from every import, whatever the qualifier.
-}

data GDecl = GValue Int | GData Int ConStyle | GClass Int | GTypeData Int | GPat Int
  deriving (Show)

data ConStyle = SameName | MkName
  deriving (Show)

data ModKind = ModNormal | ModBroken | ModSplices
  deriving (Eq, Show)

data GMod = GMod
  { kind :: ModKind,
    decls :: [GDecl],
    exports :: Maybe [GExport],
    imports :: [GImport]
  }
  deriving (Show)

data GExport
  = EVal (Maybe Text) Text
  | EType (Maybe Text) Text GSubs
  | EPat (Maybe Text) Text
  | EModule Text
  deriving (Show)

data GSubs = SNone | SAll | SSome [Text]
  deriving (Show)

-- | How a generated module imports: down only, or up too (as SOURCE
-- imports, making cycles).
data Imports = DownOnly | WithCycles
  deriving (Eq)

data GImport = GImport
  { target :: Int,
    qual :: Bool,
    alias :: Maybe Text,
    list :: Maybe (ListKind, [GItem]),
    source :: Bool
  }
  deriving (Show)

data GItem = IVal Text | IType Text GSubs | IPat Text
  deriving (Show)

data USpace = UVal | UCon | UType
  deriving (Eq, Show)

data GUse = GUse (Maybe Text) Text USpace
  deriving (Show)

-- | Model kinds: value, method, field, constructor, pattern synonym,
-- type (also families), class, type-data constructor.
data MKind = MV | MM | MF | MC | MP | MT | MCl | MTC
  deriving (Eq, Ord, Show)

-- | A model entity: defining module, name, kind.
data MEnt = MEnt Int Text MKind
  deriving (Eq, Ord, Show)

valueish, conish, typeish :: [MKind]
valueish = [MV, MM, MF]
conish = [MC, MP]
typeish = [MT, MCl, MTC]

data Program = Program
  { mods :: [GMod],
    user :: [GImport],
    uses :: [GUse]
  }
  deriving (Show)

valueNames, fieldNames, typeNames, conNames, childNames, patNames :: [Text]
valueNames = ["v0", "v1", "v2", "v3", "m0", "m1"]
fieldNames = ["f0", "f1", "f2"]
typeNames = ["T0", "T1", "T2", "C0", "C1", "F0", "F1", "D0", "DA0"]
conNames = ["T0", "T1", "T2", "MkT0", "MkT1", "MkT2", "P0", "P1"]
childNames =
  [ "T0",
    "MkT0",
    "f0",
    "T1",
    "MkT1",
    "f1",
    "F0",
    "m0",
    "DA0",
    "DB0",
    "P0",
    "P1"
  ]
patNames = ["P0", "P1", "T0", "MkT1"]

genProgram :: Imports -> Gen Program
genProgram imports = do
  n <- Gen.int (Range.linear 1 7)
  mods <- mapM (genMod n) [0 .. n - 1]
  user <- Gen.list (Range.linear 0 4) (genImport (n - 1) n)
  uses <- Gen.list (Range.linear 1 10) (genUse (aliases user))
  pure Program {mods = mods, user = user, uses = uses}
  where
    genMod n i = do
      kind <-
        Gen.frequency
          [(10, pure ModNormal), (1, pure ModBroken), (1, pure ModSplices)]
      values <- map GValue <$> Gen.subsequence [0 .. 3]
      datas <-
        Gen.subsequence [0 .. 2]
          >>= mapM (\k -> GData k <$> Gen.element [SameName, MkName])
      classes <- map GClass <$> Gen.subsequence [0 .. 1]
      tds <- map GTypeData <$> Gen.subsequence [0]
      pats <- map GPat <$> Gen.subsequence [0 .. 1]
      imports' <-
        if i == 0 && imports == DownOnly
          then pure []
          else
            Gen.list
              (Range.linear 0 3)
              ( genImport
                  (if imports == WithCycles then n - 1 else i - 1)
                  i
              )
      exports <- case kind of
        ModNormal ->
          Gen.frequency
            [ (1, pure Nothing),
              (3, Just <$> Gen.list (Range.linear 0 6) (genExport i (aliases imports')))
            ]
        _ -> pure Nothing
      pure
        GMod
          { kind = kind,
            decls = values <> datas <> classes <> tds <> pats,
            exports = exports,
            imports = imports'
          }
    genImport top self = do
      target <- Gen.int (Range.linear 0 (max 0 top))
      qual <- Gen.bool
      alias <- Gen.maybe (Gen.element ["A", "B"])
      list <-
        Gen.frequency
          [ (2, pure Nothing),
            (2, (\is -> Just (Only, is)) <$> Gen.list (Range.linear 0 4) genItem),
            (1, (\is -> Just (Hiding, is)) <$> Gen.list (Range.linear 0 3) genItem)
          ]
      let source = imports == WithCycles && target >= self
      pure
        GImport
          { target = target,
            qual = qual,
            alias = alias,
            list = list,
            source = source
          }
    genItem =
      Gen.choice
        [ IVal <$> Gen.element (valueNames <> fieldNames),
          IType <$> Gen.element typeNames <*> genSubs,
          IPat <$> Gen.element patNames
        ]
    genSubs =
      Gen.choice [pure SNone, pure SAll, SSome <$> Gen.subsequence (take 6 childNames)]
    genExport i as =
      Gen.choice
        [ EVal <$> genQual i as <*> Gen.element (valueNames <> fieldNames),
          EType <$> genQual i as <*> Gen.element typeNames <*> genExportSubs,
          EPat <$> genQual i as <*> Gen.element patNames,
          EModule <$> Gen.element (("M" <> tshow i) : as)
        ]
    genExportSubs =
      Gen.choice [pure SNone, pure SAll, SSome <$> Gen.subsequence childNames]
    genQual i as =
      Gen.frequency [(4, pure Nothing), (1, Just <$> Gen.element (("M" <> tshow i) : as))]
    genUse as = do
      q <- Gen.frequency [(3, pure Nothing), (1, Just <$> Gen.element ("Z" : as))]
      Gen.choice
        [ (\x -> GUse q x UVal) <$> Gen.element (valueNames <> fieldNames),
          (\x -> GUse q x UCon) <$> Gen.element conNames,
          (\x -> GUse q x UType) <$> Gen.element typeNames
        ]
    aliases is = nubOrd [aliasOf imp | imp <- is]

aliasOf :: GImport -> Text
aliasOf imp = fromMaybe ("M" <> tshow imp.target) imp.alias

-- | The program's modules as text, the user module last, and the line
-- of each use.
render :: Program -> ([(Text, Text)], [Int])
render prog =
  ( [("M" <> tshow i, modText i m) | (i, m) <- zip [0 :: Int ..] prog.mods]
      <> [("U", Text.unlines userLines)],
    [length header + 1 + 2 * k | k <- [0 .. length prog.uses - 1]]
  )
  where
    -- No open external import: what isn't found is dropped, or
    -- unresolved, so the model needs no externals.
    pragmas m =
      "{-# LANGUAGE NoImplicitPrelude, PatternSynonyms, TypeFamilies, TypeData"
        <> (if m.kind == ModSplices then ", TemplateHaskell" else "")
        <> " #-} "
    modText i m =
      Text.unlines $
        [pragmas m <> "module M" <> tshow i <> maybe "" exportList m.exports <> " where"]
          <> map importLine m.imports
          <> case m.kind of
            ModBroken -> ["x = = 1"]
            _ -> concatMap declLines m.decls <> ["$(pure [])" | m.kind == ModSplices]
    declLines = \case
      GValue k -> ["v" <> tshow k <> " = ()"]
      GData k st ->
        ["data T" <> tshow k <> " = " <> conName k st <> " {f" <> tshow k <> " :: ()}"]
      GClass k ->
        [ "class C"
            <> tshow k
            <> " a where { type F"
            <> tshow k
            <> " a; m"
            <> tshow k
            <> " :: a -> () }"
        ]
      GTypeData k ->
        ["type data D" <> tshow k <> " = DA" <> tshow k <> " | DB" <> tshow k]
      GPat k -> ["pattern P" <> tshow k <> " :: ()", "pattern P" <> tshow k <> " = ()"]
    exportList es = " (" <> Text.intercalate ", " (map export es) <> ")"
    export = \case
      EVal q x -> qualified q x
      EType q t s -> qualified q t <> subs s
      EPat q p -> "pattern " <> qualified q p
      EModule a -> "module " <> a
    qualified q x = maybe "" (<> ".") q <> x
    subs = \case
      SNone -> ""
      SAll -> " (..)"
      SSome xs -> " (" <> Text.intercalate ", " xs <> ")"
    importLine imp =
      "import "
        <> (if imp.source then "{-# SOURCE #-} " else "")
        <> (if imp.qual then "qualified " else "")
        <> "M"
        <> tshow imp.target
        <> maybe "" (" as " <>) imp.alias
        <> case imp.list of
          Nothing -> ""
          Just (k, is) ->
            (if k == Hiding then " hiding" else "")
              <> " ("
              <> Text.intercalate ", " (map item is)
              <> ")"
    item = \case
      IVal x -> x
      IType t s -> t <> subs s
      IPat p -> "pattern " <> p
    header =
      "{-# LANGUAGE NoImplicitPrelude, PatternSynonyms #-} module U where"
        : map importLine prog.user
    userLines = header <> concat (zipWith useLines [0 :: Int ..] prog.uses)
    useLines k (GUse q x sp) =
      let name = qualified q x
       in case sp of
            UType -> ["u" <> tshow k <> " :: " <> name, "u" <> tshow k <> " = undefined"]
            _ -> ["u" <> tshow k <> " = " <> name, ""]

conName :: Int -> ConStyle -> Text
conName k = \case
  SameName -> "T" <> tshow k
  MkName -> "MkT" <> tshow k

-- | A module's own entities, and each child's parent.
localsOf :: Int -> GMod -> [(MEnt, Maybe MEnt)]
localsOf i m
  | m.kind == ModBroken = []
  | otherwise = concatMap decl m.decls
  where
    e n k = MEnt i n k
    decl = \case
      GValue k -> [(e ("v" <> tshow k) MV, Nothing)]
      GData k st ->
        let t = e ("T" <> tshow k) MT
         in [(t, Nothing), (e (conName k st) MC, Just t), (e ("f" <> tshow k) MF, Just t)]
      GClass k ->
        let c = e ("C" <> tshow k) MCl
         in [ (c, Nothing),
              (e ("F" <> tshow k) MT, Just c),
              (e ("m" <> tshow k) MM, Just c)
            ]
      GTypeData k ->
        let d = e ("D" <> tshow k) MT
         in [ (d, Nothing),
              (e ("DA" <> tshow k) MTC, Just d),
              (e ("DB" <> tshow k) MTC, Just d)
            ]
      GPat k -> [(e ("P" <> tshow k) MP, Nothing)]

-- | A model set: entities, and pattern synonyms bundled with a type
-- (type, pattern synonym).
data MSet = MSet
  { ents :: Set MEnt,
    bundles :: Set (MEnt, MEnt)
  }

instance Semigroup MSet where
  a <> b = MSet (a.ents <> b.ents) (a.bundles <> b.bundles)

instance Monoid MSet where
  mempty = MSet Set.empty Set.empty

-- | Just these entities, with their bundles. A pattern synonym keeps
-- its bundle without the type (GHC's trimAvail): a union with the type
-- puts it back under it.
restrict :: Set MEnt -> MSet -> MSet
restrict es s =
  MSet
    (Set.intersection es s.ents)
    (Set.filter (\(_, q) -> Set.member q es) s.bundles)

-- | The model: every module's export set, and each child's parent.
data Model = Model
  { exports :: Map Int MSet,
    parents :: Map MEnt MEnt
  }

model :: [GMod] -> Model
model mods = foldl' step (Model Map.empty parents) (zip [0 ..] mods)
  where
    parents =
      Map.fromList
        [ (c, p)
        | (i, m) <- zip [0 ..] mods,
          (c, Just p) <- localsOf i m
        ]
    step md (i, m) =
      Model
        { exports = Map.insert i (exportsOf md i m) md.exports,
          parents = md.parents
        }

-- | The children of these types in a set: by parent, and bundled.
childrenIn :: Model -> Set MEnt -> MSet -> Set MEnt
childrenIn md ts s =
  Set.filter (\c -> maybe False (`Set.member` ts) (Map.lookup c md.parents)) s.ents
    <> Set.fromList
      [ q
      | (t, q) <- Set.toList s.bundles,
        Set.member t ts,
        Set.member q s.ents
      ]

named :: Text -> [MKind] -> Set MEnt -> Set MEnt
named x ks = Set.filter (\(MEnt _ n k) -> n == x && k `elem` ks)

-- | The types, with the children the subs name.
withSubs :: Model -> GSubs -> Set MEnt -> MSet -> MSet
withSubs md s ts set
  | Set.null ts = mempty
  | otherwise =
      -- Kids come under these types only.
      let r = restrict (ts <> kids) set
       in MSet r.ents (Set.filter (\(t, _) -> Set.member t ts) r.bundles)
  where
    kids = case s of
      SNone -> Set.empty
      SAll -> childrenIn md ts set
      SSome ns -> Set.filter (\(MEnt _ n _) -> n `elem` ns) (childrenIn md ts set)

-- | A module's exports, from the export sets of what it imports.
exportsOf :: Model -> Int -> GMod -> MSet
exportsOf md i m = case (m.kind, m.exports) of
  (ModBroken, _) -> mempty
  (_, Nothing) -> locals
  (_, Just es) -> mconcat (map export es) <> marked es
  where
    -- markExported exports a local pattern synonym named in the subs of
    -- an item of ours, whatever else happens (GHC would say no when it
    -- doesn't bundle).
    marked es =
      restrict
        ( Set.unions
            [ named x [MP] locals.ents
            | EType q _ (SSome ns) <- es,
              here q,
              x <- ns
            ]
        )
        locals
    self = "M" <> tshow i
    locals = MSet (Set.fromList (map fst (localsOf i m))) Set.empty
    visibleVia = modelVisible md
    scope q = mconcat [visibleVia imp | imp <- m.imports, matches q imp]
    -- Every import, whatever its qualifier: for a type's children.
    allScope = mconcat (map visibleVia m.imports)
    unqualified = scope Nothing
    here q = maybe True (== self) q
    export = \case
      EVal q x
        | here q,
          not (Set.null (named x valueish locals.ents)) ->
            restrict (named x valueish locals.ents) locals
        | otherwise -> restrict (named x valueish (scope q).ents) (scope q)
      EType q t s
        | here q,
          not (Set.null (named t typeish locals.ents)) ->
            let ts = named t typeish locals.ents
             in withSubs md s ts locals <> bundled ts s (withSubs md s ts locals)
        | otherwise ->
            let ts = named t typeish (scope q).ents
                kids = withSubs md s ts allScope
             in if Set.null ts then mempty else kids <> bundled ts s kids
      EPat q p
        | here q,
          not (Set.null (named p conish locals.ents)) ->
            restrict (named p conish locals.ents) locals
        | otherwise -> restrict (named p conish (scope q).ents) (scope q)
      EModule a
        | a == self -> locals
        | otherwise ->
            mconcat
              [ if imp.qual then restrict unqualified.ents v else v
              | imp <- m.imports,
                aliasOf imp == a,
                let v = visibleVia imp
              ]
    -- @T(.., P)@: pattern synonyms named in the subs, here or in scope,
    -- go under T. Per type: a sub that is a child of another T of that
    -- name (an ambiguous T) still bundles with this one.
    bundled ts s kids = case s of
      SSome ns -> mconcat (map (bundle ns kids) (Set.toList ts))
      _ -> mempty
    bundle ns kids t =
      let own = childrenIn md (Set.singleton t) kids
          ps =
            Set.unions
              [ if Set.null here' then named x [MP] unqualified.ents else here'
              | x <- ns,
                not (any (\(MEnt _ n _) -> n == x) (Set.toList own)),
                -- One here wins over those in scope.
                let here' = named x [MP] locals.ents
              ]
       in MSet ps (Set.fromList [(t, q) | q <- Set.toList ps])

-- | An import qualifies for this qualifier.
matches :: Maybe Text -> GImport -> Bool
matches q imp = case q of
  Nothing -> not imp.qual
  Just a -> aliasOf imp == a

-- | What an import brings.
modelVisible :: Model -> GImport -> MSet
modelVisible md imp = case imp.list of
  Nothing -> set
  Just (Only, is) -> mconcat (map pick is)
  Just (_, is) -> restrict (set.ents `Set.difference` Set.unions (map hide is)) set
  where
    set = Map.findWithDefault mempty imp.target md.exports
    pick = \case
      IVal x -> restrict (named x valueish set.ents) set
      IType t s -> withSubs md s (named t typeish set.ents) set
      IPat p -> restrict (named p conish set.ents) set
    hide = \case
      IVal x -> named x valueish set.ents
      IType t s ->
        let ts = named t typeish set.ents
         in ts <> named t conish set.ents <> (withSubs md s ts set).ents
      IPat p -> named p conish set.ents

-- | The model's candidates for a use in U.
modelUse :: Model -> [GImport] -> GUse -> Set MEnt
modelUse md user (GUse q x sp) =
  named x kinds (mconcat [modelVisible md imp | imp <- user, matches q imp]).ents
  where
    kinds = case sp of
      UVal -> valueish
      UCon -> conish
      UType -> typeish

-- | Our entity as a model one, if it's a declaration of M<i>.
ours :: ModuleId -> DeclId -> EntityKind -> Maybe MEnt
ours m d k = do
  ComponentModule _ _ (ModuleName mn) <- Just m
  i <- case Text.stripPrefix "M" mn of
    Just s
      | not (Text.null s),
        Text.all (`elem` ['0' .. '9']) s ->
          Just (read (Text.unpack s))
    _ -> Nothing
  n <- case d of
    Named _ n -> Just n
    Field _ n -> Just n
    _ -> Nothing
  pure . MEnt i n $ case k of
    ValueKind -> MV
    MethodKind -> MM
    FieldKind -> MF
    BareFieldKind -> MF
    ConKind -> MC
    PatSynKind -> MP
    TypeKind -> MT
    ClassKind -> MCl
    TypeConKind -> MTC

ourEntity :: Entity -> Maybe MEnt
ourEntity x = case x.origin of
  HomeDecl m d -> ours m d x.kind
  _ -> Nothing

-- | Our set as model entities (ours only), and each type's children.
ourSet :: ExportSet -> (Set MEnt, Map MEnt (Set MEnt))
ourSet s =
  ( Set.fromList (mapMaybe ourEntity (everyEntity s.names)),
    Map.fromListWith
      Set.union
      [ (t, Set.fromList (mapMaybe ourEntity (Set.toList a.children)))
      | as <- Map.elems s.names.types,
        a <- as,
        Just t <- [ourEntity a.entity]
      ]
  )

-- | A module that doesn't parse, has top-level splices, or whose set
-- has open parts (from such a module).
openIn :: Map ModuleId ExportSet -> Program -> ModuleId -> Bool
openIn sets prog m =
  Set.member m generated || maybe False (not . Map.null . (.openHome)) (Map.lookup m sets)
  where
    generated =
      Set.fromList
        [ modId ("M" <> tshow i)
        | (i, g) <- zip [0 :: Int ..] prog.mods,
          g.kind /= ModNormal
        ]

differential :: Property
differential = property (forAll (genProgram DownOnly) >>= differentialFor)

-- | The differential check on one program.
differentialFor :: Program -> PropertyT IO ()
differentialFor prog = do
  let (mods, lines') = render prog
      input = memInput Set.empty mods
      n = length prog.mods
      md = model prog.mods
      sets = (exportSets input [modId ("M" <> tshow i) | i <- [0 .. n - 1]]).sets
  annotate (Text.unpack (Text.unlines [t | (_, t) <- mods]))
  -- Export sets, and each type's children.
  forM_ [0 .. n - 1] $ \i -> do
    let (es, kidsOf) =
          maybe (Set.empty, Map.empty) ourSet (Map.lookup (modId ("M" <> tshow i)) sets)
        expected = Map.findWithDefault mempty i md.exports
    (i, es) === (i, expected.ents)
    forM_ (Map.toList kidsOf) $ \(t, ks) ->
      (t, ks) === (t, childrenIn md (Set.singleton t) expected)
  -- Each use.
  case input.state (modId "U") of
    ModuleParsed p -> do
      let r = fromMaybe (error "no result") (resolveModule input (modId "U"))
      forM_ (zip3 [0 :: Int ..] prog.uses lines') $ \(ix, u@(GUse _ _ sp), l) -> do
        let col = case sp of
              UType -> 6 + Text.length (tshow ix)
              _ -> 5 + Text.length (tshow ix)
            k =
              tokenIndexFrom
                p.parsed.tokens
                (Line (fromIntegral l))
                (Col (fromIntegral col))
            got = IntMap.lookup k r.names
            expected = Set.toList (modelUse md prog.user u)
        annotate (show (u, l, col, fmap renderResolved got, expected))
        check (openIn sets prog) prog.user u expected got
    _ -> Hedgehog.failure
  where
    check open user (GUse q x sp) expected got = case (expected, got) of
      ([], Just (External _)) -> pure ()
      -- @Z.x@ with no import as Z.
      ([], Just (Unresolved NoCandidate)) -> pure ()
      -- A module that doesn't parse, or has splices, may have it.
      ([], Just (Heuristic _ (ModuleTarget m))) | open m -> pure ()
      -- GHC would say no: a list names it, its module doesn't export it.
      ([], Just (Heuristic (ListedNotFound :| []) (ModuleTarget m))) ->
        Hedgehog.assert $
          or
            [ m == modId ("M" <> tshow imp.target)
            | imp <- user,
              matches q imp,
              Just (Only, is) <- [imp.list],
              any (lists x sp) is
            ]
      ([e], Just (Exact (DeclTarget m d))) -> Just e === kindless m d e
      (es@(_ : _ : _), Just (Heuristic (Ambiguous k :| _) (DeclTarget m d))) -> do
        k === length es
        Hedgehog.assert (maybe False (`elem` es) (kindless m d (head' es)))
      _ -> Hedgehog.failure
    -- Our DeclId doesn't give the kind: take the model's, if names
    -- match.
    kindless m d (MEnt _ _ k) = ours m d (kindOf k)
    head' = \case
      y : _ -> y
      [] -> error "empty"
    kindOf = \case
      MV -> ValueKind
      MM -> MethodKind
      MF -> FieldKind
      MC -> ConKind
      MP -> PatSynKind
      MT -> TypeKind
      MCl -> ClassKind
      MTC -> TypeConKind
    lists x sp = \case
      IVal y -> sp == UVal && y == x
      IPat y -> sp == UCon && y == x
      IType t s ->
        (sp == UType && t == x)
          || ( sp /= UType
                 && case s of
                   SSome ns -> x `elem` ns
                   _ -> False
             )

-- | Request every module, in a random order, the memo growing: the end
-- memo is the same whatever the order. With cycles.
orderIndependent :: Property
orderIndependent = property $ do
  prog <- forAll (genProgram WithCycles)
  let (mods, _) = render prog
      input = memInput Set.empty mods
      ids = map (modId . fst) mods
  order1 <- forAll (Gen.shuffle ids)
  order2 <- forAll (Gen.shuffle ids)
  let step memo m = Map.union memo (exportSets input {memo = memo} [m]).newFinal
      inOrder = foldl step Map.empty
  fmap renderSet (inOrder order1) === fmap renderSet (inOrder order2)

-- | With some modules not ready: an exact result stays the same once
-- they are (unless the name turns out ambiguous).
monotone :: Property
monotone = property $ do
  prog <- forAll (genProgram DownOnly)
  let (mods, _) = render prog
  pending <- forAll (Set.fromList <$> Gen.subsequence [m | (m, _) <- mods, m /= "U"])
  let partialIn = memInput pending mods
      fullIn = memInput Set.empty mods
      resultsIn inp =
        Map.fromList
          [ p
          | Just r <- [resolveModule inp (modId "U")],
            p <- IntMap.toList r.names
          ]
      before = resultsIn partialIn
      after' = resultsIn fullIn
  -- Code that compiles has no ambiguous name: skip those (a name
  -- found once while another module isn't ready is exact, on purpose).
  forM_ (Map.toList before) $ \(k, res) -> case (res, Map.lookup k after') of
    (_, Just (Heuristic (Ambiguous _ :| _) _)) -> pure ()
    (Exact _, later) -> (k, Just res) === (k, later)
    _ -> pure ()

-- | @hiding ()@ is no list; after @hiding (x)@ x is gone; a list of
-- every exported name is no list.
filterLaws :: Property
filterLaws = property (forAll (genProgram DownOnly) >>= filterLawsFor)

-- | The filter laws on one program's last module.
filterLawsFor :: Program -> PropertyT IO ()
filterLawsFor prog = do
  let (mods, _) = render prog
      n = length prog.mods
      target = "M" <> tshow (n - 1)
      input0 = memInput Set.empty mods
      set =
        fromMaybe
          (error "no set")
          ( Map.lookup
              (modId target)
              (exportSets input0 [modId target]).sets
          )
      values = concat (Map.elems set.names.values)
      isVar e = e.kind `elem` [ValueKind, FieldKind, MethodKind]
      vals = nubOrd (sort [e.occ | e <- values, isVar e])
      pats = nubOrd (sort [e.occ | e <- values, e.kind `elem` [ConKind, PatSynKind]])
      typs = sort (Map.keys set.names.types)
      everything =
        Text.intercalate
          ", "
          ( vals
              <> ["pattern " <> p | p <- pats]
              <> [t <> " (..)" | t <- typs]
          )
      user =
        Text.unlines
          [ "{-# LANGUAGE PatternSynonyms #-} module U2 where",
            "import " <> target,
            "import " <> target <> " hiding ()",
            "import " <> target <> " (" <> everything <> ")"
          ]
          <> Text.unlines ["import " <> target <> " hiding (" <> x <> ")" | x <- vals]
      input = memInput Set.empty (mods <> [("U2", user)])
  case input.state (modId "U2") of
    ModuleParsed p -> do
      let si k imp = scopeImport k imp False [] (TargetHome (modId target) set)
          vis = [(si k imp).visible | (k, imp) <- zip [0 ..] p.parsed.imports]
      case vis of
        plain : hidingNone : allListed : hidden -> do
          norm hidingNone === norm plain
          norm allListed === norm plain
          forM_ (zip vals hidden) $ \(x, v) ->
            Hedgehog.assert (not (any isVar (Map.findWithDefault [] x v.values)))
        _ -> Hedgehog.failure
    _ -> Hedgehog.failure
  where
    norm ns =
      ( Map.map Set.fromList ns.values,
        Map.map (Set.fromList . map (\a -> (a.entity, a.children))) ns.types,
        ns.lone,
        ns.bundles
      )

-- | A check on one program, as a test case.
regression :: (Program -> PropertyT IO ()) -> Program -> Assertion
regression f prog = do
  ok <- Hedgehog.check (Hedgehog.withTests 1 (property (f prog)))
  assertBool "the check fails" ok

-- | Shrunk from a failure: M2 exports @F1 (P0)@, F1 coming from two
-- imports. P0 is a child of one F1 and bundles with the other.
ambiguousBundle :: Program
ambiguousBundle =
  Program
    { mods =
        [ plainMod [GClass 1] Nothing [],
          plainMod [GClass 1, GPat 0] (Just [EType Nothing "F1" (SSome ["P0"])]) [],
          plainMod
            []
            (Just [EType Nothing "F1" (SSome ["P0"])])
            [plainImport 1 (Just "B") Nothing, plainImport 0 (Just "B") Nothing]
        ],
      user = [],
      uses = [GUse Nothing "v0" UVal]
    }

-- | Shrunk from a failure: M3 re-exports M1 hiding D0, so P0 is lone
-- under D0; @import M3 (pattern P0)@ must keep that.
loneBundle :: Program
loneBundle =
  Program
    { mods =
        [ plainMod [] Nothing [],
          plainMod [GTypeData 0, GPat 0] (Just [EType Nothing "D0" (SSome ["P0"])]) [],
          plainMod [] Nothing [],
          plainMod
            []
            (Just [EModule "M1"])
            [plainImport 1 Nothing (Just (Hiding, [IType "D0" SNone]))]
        ],
      user = [],
      uses = [GUse Nothing "v0" UVal]
    }

plainMod :: [GDecl] -> Maybe [GExport] -> [GImport] -> GMod
plainMod ds es is = GMod {kind = ModNormal, decls = ds, exports = es, imports = is}

plainImport :: Int -> Maybe Text -> Maybe (ListKind, [GItem]) -> GImport
plainImport t a l =
  GImport {target = t, qual = False, alias = a, list = l, source = False}

-- * Linear time

-- | Hostile shapes: allocation at n and 4n (D-144), run one at a time
-- after the other tests. One shape, whose slow path barely allocates (a
-- list scanned per name), counts comparisons instead: 'filterCompares'.
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: resolution"
      [ testCase "a re-export chain" $
          linear EveryModule 150 $ \k ->
            modSrc "M0" (Just ["x0"]) [] ["x0 = ()"]
              : [ modSrc
                    (m i)
                    (Just ["module " <> m (i - 1), x i])
                    [m (i - 1)]
                    [x i <> " = ()"]
                | i <- [1 .. k]
                ]
                <> [modSrc "U" Nothing [m k] ["u = x0"]],
        testCase "a qualified re-export chain" $
          linear EveryModule 300 $ \k ->
            modSrc "M0" (Just ["x0"]) [] ["x0 = ()"]
              : [ modSrc
                    (m i)
                    (Just ["module X", x i])
                    ["qualified " <> m (i - 1) <> " as X", m (i - 1)]
                    [x i <> " = ()"]
                | i <- [1 .. k]
                ]
                <> [modSrc "U" Nothing [m k] ["u = x0"]],
        testCase "one export list of n items" $
          linear EveryModule 2000 $ \k ->
            [ modSrc "A" (Just [x i | i <- [1 .. k]]) [] [x i <> " = ()" | i <- [1 .. k]],
              modSrc "U" Nothing ["A"] ["u = x1"]
            ],
        testCase "a module importing n modules, using each" $
          linear EveryModule 300 $ \k ->
            [modSrc (kk i) (Just [x i]) [] [x i <> " = ()"] | i <- [1 .. k]]
              <> [ modSrc
                     "U"
                     Nothing
                     [kk i | i <- [1 .. k]]
                     [use i (x i) | i <- [1 .. k]]
                 ],
        testCase "n imports of one module, each hiding something else" $
          linear EveryModule 300 $ \k ->
            [ modSrc "A" Nothing [] [x i <> " = ()" | i <- [1 .. k]],
              modSrc "U" Nothing ["A hiding (" <> x i <> ")" | i <- [1 .. k]] (uses k)
            ],
        testCase "T(..) with n constructors through a chain" $
          linear EveryModule 2000 $ \k ->
            [ modSrc
                "A"
                (Just ["T (..)"])
                []
                ["data T = " <> Text.intercalate " | " (cons k)],
              modSrc "B" (Just ["T (..)"]) ["A (T (..))"] [],
              modSrc "C" (Just ["module B"]) ["B"] [],
              modSrc "U" Nothing ["C (T (..))"] [use i ("C" <> tshow i) | i <- [1 .. k]]
            ],
        testCase "T(C1, …, Cn) in export, import and hiding lists" $
          linear EveryModule 1600 $ \k ->
            let sub = "T (" <> Text.intercalate ", " (cons k) <> ")"
             in [ modSrc
                    "A"
                    (Just [sub])
                    []
                    ["data T = " <> Text.intercalate " | " (cons k)],
                  modSrc "B" (Just [sub]) ["A (" <> sub <> ")"] [],
                  modSrc
                    "U"
                    Nothing
                    ["B (" <> sub <> ")", "A hiding (" <> sub <> ")"]
                    [use i ("C" <> tshow i) | i <- [1 .. k]]
                ],
        testCase "T(C1, …, Cn) in lists: comparisons per sub" $ filterCompares 4000,
        testCase "many lone children" $
          linear EveryModule 1000 $ \k ->
            [ modSrc
                "A"
                (Just ["T (..)"])
                []
                [ "data T = T {"
                    <> Text.intercalate ", " [f i <> " :: ()" | i <- [1 .. k]]
                    <> "}"
                ],
              modSrc "L" (Just [f i | i <- [1 .. k]]) ["A"] [],
              modSrc "L2" (Just ["T (..)"]) ["A (T)", "L"] [],
              modSrc "U" Nothing ["L2"] [use i (f i) | i <- [1 .. k]]
            ],
        testCase "many bundled pattern synonyms, imported and hidden one by one" $
          linear EveryModule 600 $ \k ->
            map
              withPatterns
              [ modSrc
                  "A"
                  (Just ["T (.., " <> Text.intercalate ", " (pats k) <> ")"])
                  []
                  ( "data T = T"
                      : concat [[p <> " :: T", p <> " = T"] | p <- patDecls k]
                  ),
                modSrc
                  "B"
                  (Just (patDecls k))
                  ["A (pattern " <> p <> ")" | p <- pats k]
                  [],
                modSrc
                  "H"
                  (Just ["T (..)"])
                  ["A hiding (pattern " <> p <> ")" | p <- pats k]
                  [],
                modSrc "C" (Just ["module B", "T"]) ["B", "A (T)"] [],
                modSrc
                  "U"
                  (Just ["T (..)"])
                  ["A (T)", "B", "qualified C", "qualified H"]
                  [ use i (p <> " C." <> p <> " H." <> p)
                  | (i, p) <- zip [1 :: Int ..] (pats k)
                  ]
              ],
        testCase "a SOURCE cycle of n" $
          linear OnlyU 150 $ \k ->
            let next i = m ((i + 1) `mod` k)
                source i = if i == k - 1 then "{-# SOURCE #-} " else ""
                cyc i =
                  modSrc
                    (m i)
                    (Just ["module " <> next i, x i])
                    [source i <> next i]
                    [x i <> " = ()"]
             in map cyc [0 .. k - 1]
                  <> [modSrc "U" Nothing [m i | i <- [0 .. k - 1]] ["u = x0"]],
        testCase "n imports of one module, each listing one name" $
          linear EveryModule 300 $ \k ->
            [ modSrc "A" Nothing [] [x i <> " = ()" | i <- [1 .. k]],
              modSrc "U" Nothing ["A (" <> x i <> ")" | i <- [1 .. k]] (uses k)
            ],
        testCase "an export list of n names from n imports" $
          linear EveryModule 300 $ \k ->
            [modSrc (kk i) (Just [x i]) [] [x i <> " = ()"] | i <- [1 .. k]]
              <> [ modSrc "R" (Just [x i | i <- [1 .. k]]) [kk i | i <- [1 .. k]] [],
                   modSrc "U" Nothing ["R"] ["u = x1"]
                 ],
        testCase "a name exported by n modules" $
          linear EveryModule 300 $ \k ->
            [modSrc (kk i) (Just ["x"]) [] ["x = ()"] | i <- [1 .. k]]
              <> [modSrc "U" Nothing [kk i | i <- [1 .. k]] ["u = x"]],
        testCase "a shared alias over n imports, n names" $
          linear EveryModule 300 $ \k ->
            [modSrc (kk i) (Just [x i]) [] [x i <> " = ()"] | i <- [1 .. k]]
              <> [ modSrc
                     "U"
                     Nothing
                     ["qualified " <> kk i <> " as Q" | i <- [1 .. k]]
                     [use i ("Q." <> x i <> " Q.missing" <> tshow i) | i <- [1 .. k]]
                 ],
        testCase "many modules, 20 imports each: every module" $
          linear EveryModule 400 $ \k ->
            [ modSrc
                (m i)
                (Just (x i : take 1 [x j | j <- deps i]))
                [m j | j <- deps i]
                [x i <> " = ()" <> Text.concat [" " <> x j | j <- deps i]]
            | i <- [0 .. k - 1]
            ]
              <> [modSrc "U" Nothing [m (k - 1)] ["u = " <> x (k - 2)]]
      ]
  where
    deps i = [j | j <- [i - 20 .. i - 1], j >= 0]
    m i = "M" <> tshow i
    kk i = "K" <> tshow i
    x i = "x" <> tshow i
    f i = "f" <> tshow i
    use i e = "u" <> tshow i <> " = " <> e
    -- Each x, and a y nothing has.
    uses k = [use i (x i <> " y" <> tshow i) | i <- [1 .. k]]
    cons k = ["C" <> tshow i | i <- [1 .. k]]
    pats k = ["P" <> tshow i | i <- [1 .. k]]
    patDecls k = ["pattern " <> p | p <- pats k]
    withPatterns (n, t) = (n, "{-# LANGUAGE PatternSynonyms #-}\n" <> t)

-- | A module: name, export list, imports (after @import@), body lines.
modSrc :: Text -> Maybe [Text] -> [Text] -> [Text] -> (Text, Text)
modSrc n es is body =
  ( n,
    Text.unlines $
      ( "module "
          <> n
          <> maybe "" (\xs -> " (" <> Text.intercalate ", " xs <> ")") es
          <> " where"
      )
        : map ("import " <>) is
          <> body
  )

-- | Which modules a run resolves.
data Resolving
  = EveryModule
  | -- | Only U. A cycle is never final, so each request walks it:
    -- resolving all n modules of a cycle is n * n.
    OnlyU

-- | Every export set, then every module (or U), at n / 4 and n: the
-- allocation must grow about linearly. Parsing is done and forced
-- first (D-125).
linear :: Resolving -> Int -> (Int -> [(Text, Text)]) -> Assertion
linear resolving big make = do
  small <- allocated resolving (make (big `div` 4))
  large <- allocated resolving (make big)
  let ratio = fromIntegral large / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, large) <> ", ratio " <> show ratio) (ratio < 6)

-- | 'withKeys', the filter for @T(C1, …, Cn)@ in lists, with n keys
-- over n children: key comparisons at n / 4 and n, at most 6 times
-- (n log n is about 5, a list scanned per child 16). A slow filter
-- barely allocates, so this counts comparisons instead of bytes.
filterCompares :: Int -> Assertion
filterCompares big = do
  small <- compares (big `div` 4)
  large <- compares big
  let ratio = fromIntegral large / fromIntegral small :: Double
  assertBool
    ("comparisons " <> show (small, large) <> ", ratio " <> show ratio)
    (ratio < 6)
  where
    compares n = do
      let keys = [Counted i | i <- [1 .. n]]
          xs = Set.fromList [(Counted i, ()) | i <- [1 .. n]]
      _ <- evaluate (Set.size xs)
      writeIORef comparisons 0
      found <- evaluate (Set.size (withKeys fst keys xs))
      found @?= n
      readIORef comparisons

-- | A key that counts its comparisons in 'comparisons'.
newtype Counted = Counted Int

instance Eq Counted where
  a == b = compare a b == EQ

instance Ord Counted where
  compare (Counted a) (Counted b) = countedCompare a b

countedCompare :: Int -> Int -> Ordering
countedCompare a b = unsafePerformIO $ do
  atomicModifyIORef' comparisons (\c -> (c + 1, ()))
  pure (compare a b)
{-# NOINLINE countedCompare #-}

comparisons :: IORef Int
comparisons = unsafePerformIO (newIORef 0)
{-# NOINLINE comparisons #-}

-- | Bytes allocated by one run.
allocated :: Resolving -> [(Text, Text)] -> IO Int
allocated resolving mods = do
  input <- forced mods
  a0 <- getAllocationCounter
  count <- run resolving input mods
  a1 <- getAllocationCounter
  assertBool "no results" (count > 0)
  pure (fromIntegral (a0 - a1))

-- | The input, every module parsed and forced (D-125).
forced :: [(Text, Text)] -> IO ResolveInput
forced mods = do
  let input = memInput Set.empty mods
  _ <-
    evaluate
      ( sum
          [ case input.state (modId n) of
              ModuleParsed p ->
                forceModuleParse (Parsed p.parsed) `seq`
                  Text.length p.text
              _ -> 0
          | (n, _) <- mods
          ]
      )
  pure input

-- | Every export set, then the modules, with the final sets as memo.
-- How many results.
run :: Resolving -> ResolveInput -> [(Text, Text)] -> IO Int
run resolving input mods = do
  let ids = map (modId . fst) mods
      sets = exportSets input (case resolving of EveryModule -> ids; OnlyU -> [])
  _ <- evaluate (foldr (\s r -> forceExportSet s `seq` r) () sets.sets)
  _ <- evaluate (foldr (\s r -> forceExportSet s `seq` r) () sets.newFinal)
  evaluate $
    sum
      [ forceModuleResolution r `seq` IntMap.size r.names
      | i <- case resolving of EveryModule -> ids; OnlyU -> [modId "U"],
        Just r <- [resolveModule input {memo = sets.newFinal} i]
      ]

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
