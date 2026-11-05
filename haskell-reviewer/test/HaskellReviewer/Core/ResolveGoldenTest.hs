-- | Golden tests for resolution: each dir in fixtures/resolve is a small
-- project, read from disk, every module parsed and resolved. And the
-- GHC oracle (Note [Oracle files] in "ResolveDump").
module HaskellReviewer.Core.ResolveGoldenTest (tests) where

import Control.Monad (forM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Code (nameRef)
import HaskellReviewer.Core.Entities
  ( Avail (..),
    Entity (..),
    ExportSet (..),
    Origin (..),
    everyEntity,
  )
import HaskellReviewer.Core.Exports (LocalTable (..), localTable)
import HaskellReviewer.Core.ImportScope (subNames, wildcard)
import HaskellReviewer.Core.ModuleGraph (ImportTarget (..), mapImport)
import HaskellReviewer.Core.ModuleModel
  ( Export (..),
    Extensions (..),
    Import (..),
    ImportList (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    ModuleExport (..),
    ModuleParse (..),
    Qualified (..),
    SubList (..),
    Subs (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (ParsedModule (..))
import HaskellReviewer.Core.Modules (ModuleId, renderModuleId)
import HaskellReviewer.Core.Names (ExternalModule (..))
import HaskellReviewer.Core.Occurrences
  ( FreeContext (..),
    Occurrence (..),
    Resolution (..),
    Role (..),
    TokenIx (..),
    occurrenceAt,
  )
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings, parseSource)
import HaskellReviewer.Core.Project (Extension (..), ModuleName (..))
import HaskellReviewer.Core.Resolve
  ( ExportSets (..),
    ExternalGuess (..),
    GuessKind (..),
    ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    Reason (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    exportSets,
    resolveModule,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
import HaskellReviewer.Core.RichText (NameRef (..))
import HaskellReviewer.Core.RichText qualified as Rich (RefNamespace (..))
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines)
import HaskellReviewer.Core.Tokens (tokenSpanAt)
import ResolveDump
  ( Fixture (..),
    OracleFile (..),
    cases,
    dumpFixture,
    dumpInput,
    fixtureDir,
    loadFixture,
    oracleCases,
    readOracleFile,
    renderResolved,
    resolveInput,
    sourceHash,
  )
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Resolve fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : goldenVsString "warm-up" (fixtureDir <> "warm-up.golden") warmUp
      : [goldenVsString c (fixtureDir <> c <> ".golden") (dump c) | c <- cases]
        <> [testCase ("GHC agrees: " <> c) (oracle c) | c <- oracleCases]

dump :: FilePath -> IO LBS.ByteString
dump c = do
  fx <- loadFixture (fixtureDir <> c)
  pure (LBS.fromStrict (Text.Encoding.encodeUtf8 (dumpFixture fx)))

-- | The warm-up project ("HaskellReviewer.Core.ResolveWarmUp").
warmUp :: IO LBS.ByteString
warmUp =
  pure . LBS.fromStrict . Text.Encoding.encodeUtf8 $
    dumpInput resolveWarmUpInput resolveWarmUpModules

-- | Fail if a fixture or golden file has no test.
noStale :: IO ()
noStale = do
  files <- Dir.listDirectory fixtureDir
  sort [dropExtension f | f <- files, takeExtension f == ".golden"]
    @?= sort ("warm-up" : cases)
  sort [f | f <- files, takeExtension f `notElem` [".golden", ".imports"]] @?= sort cases

-- * GHC oracle

-- | Both ways, per free use token (see Note [Oracle files] in
-- "ResolveDump"):
--
-- * an exact or listed-external result is backed by one of GHC's
--   minimal imports (qualified the same way) that names it, from a
--   module whose export set has our target;
-- * a name GHC takes from one of our modules, which lists all it
--   exports, is not unresolved for us, nor external unless that module
--   re-exports it from an external one.
--
-- An import names its items, their explicit subs, and for @T(..)@
-- every child T has where it's defined. GHC leaves the implicit Prelude
-- out: we take it as naming everything.
oracle :: FilePath -> Assertion
oracle c = do
  let dir = fixtureDir <> c
  file <- BS.readFile (fixtureDir <> c <> ".imports")
  hash <- sourceHash dir
  of' <-
    maybe
      (assertFailure "bad .imports file")
      pure
      (readOracleFile (Text.Encoding.decodeUtf8 file))
  assertBool ("stale " <> c <> ".imports: run make resolve-check") (of'.hash == hash)
  fx <- loadFixture dir
  let input = resolveInput fx
      sets = (exportSets input fx.modules).sets
      ghcImports = Map.fromList [(m, parseImports t) | (m, t) <- of'.modules]
      env = OracleEnv {input = input, sets = sets, kids = kidsByName input fx.modules}
  counts <- forM fx.modules $ \m -> case (input.state m, resolveModule input m) of
    (ModuleParsed p, Just r) -> case Map.lookup (renderModuleId m) ghcImports of
      Nothing ->
        assertFailure ("no entry in " <> c <> ".imports for " <> show (renderModuleId m))
      Just imps -> do
        let uses = oracleUses env m p r (imps <> [prelude | implicitPrelude p])
        fs <- mapM (forwardCheck env m) uses
        rs <- mapM (reverseCheck env m) uses
        pure (length (filter id fs), length (filter id rs))
    _ -> pure (0, 0)
  assertBool ("no forward check in " <> c) (sum (map fst counts) > 0)
  assertBool ("no reverse check in " <> c) (sum (map snd counts) > 0)
  where
    implicitPrelude p =
      "ImplicitPrelude" `elem` p.parsed.extensions.enabled
        && all ((/= preludeName) . (.name.value)) p.parsed.imports

-- | A plain use, with what GHC imports for it.
data OracleUse = OracleUse
  { name :: NameRef,
    result :: Resolved,
    -- | For messages.
    what :: String,
    -- | GHC's imports that may bring it (a whole Prelude too).
    mentioning :: [Import],
    -- | Those whose list names it.
    named :: [Import]
  }

oracleUses ::
  OracleEnv -> ModuleId -> ParsedInput -> ModuleResolution -> [Import] -> [OracleUse]
oracleUses env m p r imps =
  [ OracleUse
      { name = n,
        result = res,
        what = show (renderModuleId m, n.written, renderResolved res),
        mentioning = [i | i <- imps, qualifierOk n i, wholePrelude i || names i],
        named = [i | i <- imps, qualifierOk n i, names i]
      }
  | (k, res) <- IntMap.toList r.names,
    plainUse k,
    let n = nameRef Rich.AnyNs (sliceSpan ls (tokenSpanAt p.parsed.tokens k)),
    let names i = Set.member n.occ (mentions env i)
  ]
  where
    ls = sourceLines p.text
    wholePrelude i = isNothing i.list && i.name.value == preludeName
    plainUse k = case occurrenceAt p.parsed.occurrences (TokenIx k) of
      Just o -> o.role == Use && o.resolution == FreeRef Plain
      Nothing -> False

-- | Every Exact and External-listed use is backed by a GHC import;
-- nothing is ambiguous (GHC compiled it); a guess on one of our modules
-- is where GHC takes it from (GHC's lists name all it uses). True when
-- it checked.
forwardCheck :: OracleEnv -> ModuleId -> OracleUse -> IO Bool
forwardCheck env m u = case u.result of
  Exact _ -> backed
  External g | g.how == Listed -> backed
  Heuristic rs _ | any isAmbiguous rs -> do
    assertFailure ("GHC finds no ambiguity: " <> u.what)
  Heuristic _ (ModuleTarget tm) -> do
    assertBool
      ("GHC doesn't take it from there: " <> u.what)
      (any (homeOf tm) u.named)
    pure True
  _ -> pure False
  where
    target i = mapImport env.input.graph m i.package i.name.value
    homeOf tm i = case target i of
      HomeModule im _ -> im == tm
      _ -> False
    backed = do
      assertBool
        ("GHC doesn't back " <> u.what)
        (any (backs env m u.name u.result) u.mentioning)
      pure True
    isAmbiguous = \case
      Ambiguous _ -> True
      _ -> False

-- | GHC takes the name from one of our closed modules (an import that
-- names it), so we must find it there too. True when it checked.
reverseCheck :: OracleEnv -> ModuleId -> OracleUse -> IO Bool
reverseCheck env m u
  | any (closedModule env m) u.named = do
      when misses $ assertFailure ("GHC finds it in one of our modules: " <> u.what)
      pure True
  | otherwise = pure False
  where
    misses = case u.result of
      External _ -> not (any (backs env m u.name u.result) u.named)
      Unresolved _ -> True
      Heuristic _ (ModuleTarget _) -> True
      _ -> False

-- | The import is one of our modules that exports only what we can see,
-- from the fixture alone: parsed, no top-level splices, no @module X@
-- of an external import.
closedModule :: OracleEnv -> ModuleId -> Import -> Bool
closedModule env m imp = case mapImport env.input.graph m imp.package imp.name.value of
  HomeModule im _ -> closed env im
  _ -> False

-- | See 'closedModule'.
closed :: OracleEnv -> ModuleId -> Bool
closed env im = case env.input.state im of
  ModuleParsed q ->
    not (localTable im q.parsed).topSplices
      && not (any (externalAlias q) (concat q.parsed.exports))
  _ -> False
  where
    externalAlias q = \case
      ExportModule e ->
        or
          [ True
          | i <- q.parsed.imports,
            maybe i.name.value (.value) i.as == e.name.value,
            ExternalImport _ <- [mapImport env.input.graph im i.package i.name.value]
          ]
      _ -> False

-- | What the oracle reads.
data OracleEnv = OracleEnv
  { input :: ResolveInput,
    sets :: Map.Map ModuleId ExportSet,
    -- | See 'kidsByName'.
    kids :: Map.Map Text [Text]
  }

preludeName :: ModuleName
preludeName = ModuleName "Prelude"

-- | @import Prelude@.
prelude :: Import
prelude = case parseImports "import Prelude\n" of
  [i] -> i
  _ -> error "can't parse import Prelude"

-- | GHC's imports of a module, parsed with our parser.
parseImports :: Text -> [Import]
parseImports t = case parseSource settings ("module X where\n" <> t) of
  Parsed m -> m.imports
  TextOnly _ -> []
  where
    settings =
      defaultParseSettings
        { extensions =
            [ Extension e True
            | e <- ["PackageImports", "PatternSynonyms", "ExplicitNamespaces"]
            ]
        }

-- | The name is qualified as the import is.
qualifierOk :: NameRef -> Import -> Bool
qualifierOk n imp = case n.qualifier of
  Nothing -> imp.qualified == NotQualified
  Just q -> q == maybe imp.name.value (.value) imp.as

-- | The names a GHC import's list names. No list, or a hiding list:
-- the empty set. The caller treats a whole Prelude as naming
-- everything.
mentions :: OracleEnv -> Import -> Set Text
mentions env imp = case imp.list of
  Just l | l.kind == Only -> Set.fromList (concatMap item l.items)
  _ -> Set.empty
  where
    item i =
      i.name.occ
        : map (.occ) (subNames i)
          <> (if wildcard i then Map.findWithDefault [] i.name.occ env.kids else [])

-- | For each type name: its children where it is defined, plus names
-- bundled with it in any export list of the fixture. Loose (by name, over
-- every module), but it doesn't look at our results.
kidsByName :: ResolveInput -> [ModuleId] -> Map.Map Text [Text]
kidsByName input ms =
  Map.fromListWith
    (<>)
    ( [ (a.entity.occ, map (.occ) (Set.toList a.children))
      | m <- ms,
        ModuleParsed p <- [input.state m],
        a <- Map.elems (localTable m p.parsed).availOf
      ]
        <> [ (i.name.occ, map (.occ) l.names)
           | m <- ms,
             ModuleParsed p <- [input.state m],
             ExportItem i <- concat p.parsed.exports,
             SomeSubs l <- [i.subs]
           ]
    )

-- | This GHC import names the use, and our result agrees with it.
backs :: OracleEnv -> ModuleId -> NameRef -> Resolved -> Import -> Bool
backs env m n res imp = case (res, target) of
  (Exact (DeclTarget tm d), HomeModule im _) ->
    maybe
      False
      (any (\e -> e.origin == HomeDecl tm d) . everyEntity . (.names))
      (Map.lookup im env.sets)
  (External g, ExternalImport x) -> g.module_.name == x.name
  (External g, HomeModule im _) ->
    let theirs e = e.occ == n.occ && e.origin == ExternalName g.module_
     in maybe False (any theirs . everyEntity . (.names)) (Map.lookup im env.sets)
  _ -> False
  where
    target = mapImport env.input.graph m imp.package imp.name.value
