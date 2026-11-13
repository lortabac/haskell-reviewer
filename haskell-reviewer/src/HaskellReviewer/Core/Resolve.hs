{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

-- | Cross-module resolution at the syntactic tier (SPEC §3, §11.4.5):
-- each free name of a module, through its imports, to a declaration of
-- one of our modules, a guess, an external module, or nothing. Pure: it
-- reads a view of the modules' states and a memo of final export sets,
-- and gives back the new final sets. See Note [Resolution] and Note
-- [Export set walk].
module HaskellReviewer.Core.Resolve
  ( -- * Input
    ResolveInput (..),
    ModuleState (..),
    ParsedInput (..),

    -- * Results
    Resolved (..),
    Target (..),
    ExternalGuess (..),
    GuessKind (..),
    Reason (..),
    UnresolvedWhy (..),
    ModuleResolution (..),
    isPartial,
    ExportSets (..),
    forceResolved,
    forceModuleResolution,
    forceDocNames,

    -- * Running
    resolveModule,
    resolveModuleOnly,
    exportSets,
    homeImports,

    -- * Names
    quoter,
    occKey,
  )
where

import Control.Monad (forM)
import Control.Monad.ST (ST, runST)
import Data.Char (isAlpha, isSpace, isUpper)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Code (nameRef)
import HaskellReviewer.Core.Decls (Extent (..), Part (..), declExtent)
import HaskellReviewer.Core.DocLinks (DocRef (..), docNames, docRef)
import HaskellReviewer.Core.Entities
  ( Avail (..),
    Entity (..),
    EntityKind (..),
    ExportSet (..),
    OpenKind (..),
    Origin (..),
    Space (..),
    forceExportSets,
    itemSpace,
    minimumOn,
    namesLookup,
    openSet,
    withoutOpen,
  )
import HaskellReviewer.Core.Exports
  ( ExportScope (..),
    LocalTable (..),
    exportSet,
    localTable,
    needsScope,
    ownQualifier,
  )
import HaskellReviewer.Core.ImportScope
  ( Candidates (..),
    Hit (..),
    Lookup (..),
    Misses (..),
    OpenHit (..),
    OpenWhy (..),
    Scope,
    ScopeImport (..),
    ScopeTarget (..),
    lookupChildren,
    lookupImportItem,
    lookupScope,
    mkScope,
    noneMissing,
    scopeImport,
  )
import HaskellReviewer.Core.LineRegions (LineKind (..), LineRegion (..), lineRegionList)
import HaskellReviewer.Core.ModuleGraph
  ( ImportTarget (..),
    MappingGuess,
    ModuleGraph,
    mapImport,
  )
import HaskellReviewer.Core.ModuleModel
  ( Export (..),
    Extensions (..),
    ImportList (..),
    ImportSource (..),
    Item (..),
    ItemName (..),
    Located (..),
    ParsedModule (..),
    Qualified (..),
    SubList (..),
    Subs (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (Import (..), ImportLevel (..))
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Names
  ( DeclId,
    ExternalModule (..),
    forceDeclId,
    forceExternalModule,
  )
import HaskellReviewer.Core.Occurrences
  ( FreeContext (..),
    Namespace (..),
    Occurrence (..),
    Resolution (..),
    Role (..),
    TokenIx (..),
    occurrenceAt,
    occurrenceList,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText (NameRef (..))
import HaskellReviewer.Core.RichText qualified as Rich (RefNamespace (..))
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenCount, tokenSpanAt)

{- Note [Resolution]

We assume the code compiles. For each free name of a module (Note
[Occurrence table] in "HaskellReviewer.Core.Occurrences"), the imports
give the candidates (Note [Import scope] in
"HaskellReviewer.Core.ImportScope"). Then, in order:

- Exactly one distinct declaration of ours, through imports whose
  module mapping is sure: 'Exact'. An open external import doesn't
  change that.
- Two or more (or one of ours and a listed external one): 'Heuristic'
  ('Ambiguous' k) on the first, by import order.
- One external name from a list: 'External' ('Listed').
- Nothing found, in this order, whatever the order of the imports:
  1. an import of ours that may have it (not ready, not parsed, cut by
     a cycle, or its list names it): 'Heuristic' on that module, with
     why;
  2. open external imports: 'External' with the first (one of ours
     whose list names it first), and how many;
  3. an import of ours open only by its top-level splices: 'Heuristic'
     'MadeBySplice' on that module (splices make few names; most names
     next to them are the Prelude's);
  4. this module has top-level splices: 'Heuristic' 'MadeBySpliceHere';
  5. an import line skipped by CPP: 'Unresolved' 'ExcludedByCpp';
  6. else 'Unresolved'.

Found only through imports inside a CPP @#if@: 'CppImport'. A guessed
module mapping (Note [Import mapping] in
"HaskellReviewer.Core.ModuleGraph") adds its reason.

Contexts:

- 'MethodOfClass': the class first; then the method among all the
  class's methods, in the module that defines it. Same certainty.
- 'FieldOfCon': the constructor first; then its field.
- 'MaybeWildcardOf': the constructor; if it's ours and has the field,
  'Heuristic' 'Wildcard' on the field. Else the plain lookup, with
  'Wildcard' added when we can't see the constructor's fields.
- 'QuasiQuoter': the quoter cut out of @[q|@, as a variable.
- 'FieldOfDot': a field lookup; any hit is 'Heuristic' 'RecordDot'
  (HasField doesn't need it in scope). No hit: the fields of that name
  here (the walk links one, not several), else 'Unresolved'.
- NoFieldSelectors in a field's module: the field is no variable.
- Role Import: in that import's module only, the whole export set;
  subs as children of the item's parent.
- Role Export: local first, then the scope; subs are T's children in
  scope.
- In a splice: as usual; a name only this module has is 'Heuristic'
  'ViaSplice' on it (the stage restriction made it free).
- DataKinds: a type-level name with no type in scope but a constructor
  in scope is that constructor, 'Heuristic' 'DataKinds'.
- A definition the walk left free (a @type data@ constructor): this
  module's own.
- Type variables, implicit parameters, module words: 'NotAGlobal'.

Each distinct (written name, space, in splice) is looked up once per
module. See Note [Lookup cost] in "HaskellReviewer.Core.ImportScope".

Names in doc comments (Note [Names in docs] in
"HaskellReviewer.Core.DocLinks") go through the same lookup, not in a
splice, once per distinct name:

- @M.x@: through the imports with alias M, then through those of
  module M (@'Data.Map.insert'@ with @import qualified Data.Map as
  Map@).
- @t'T'@: types. @v'x'@: constructors if it starts with an upper case
  letter or @:@, else values.
- plain @'x'@: values. Plain @'T'@ or @':+'@: types, then
  constructors. Plain @'+'@: values, then types.
- Of these lookups (each space, alias then module name), the first
  declaration of ours wins; else the first other hit (an external name
  from a list, say); else the first guess made from a miss (an open
  import, a module not ready, splices here); else nothing. So a miss
  in the types doesn't hide a constructor found by a list.
- DataKinds: when a plain name tries constructors anyway, the types
  lookup has no DataKinds fallback, so a constructor of ours is exact.

Only the page uses doc names: the results are lazy, so the source view
doesn't pay for them. Their lookups get a scope of their own, indexed
for as many lookups as there are doc names (Note [Lookup cost] in
"HaskellReviewer.Core.ImportScope"), so the main scope's index choice
doesn't need them.
-}

{- Note [Export set walk]

Export sets are computed on demand, from the module asked for, through
the imports it needs, with a memo of final sets:

- A set is final when every module it read is done (parsed, or never
  will be) and no cycle was cut while computing it. Only final sets go
  in the memo; partial ones are computed again on each request.
- An export list with only local names needs no scope: its imports are
  not walked, and it is never cut.
- A module already on the walk's stack (an import cycle, through a
  SOURCE import) gives its local part, plus 'OpenCycle' for what its
  list takes from imports. Whatever read it is not final, so the memo
  never depends on the order of requests. A cut at the module itself
  is dropped from its own set.
- Resolving a module doesn't put it on the stack (its scope is not its
  set), and a cut at it is dropped from what its imports bring: those
  names are its own.
- Within one run each set is computed once (partial ones too).

Imports: the implicit @import Prelude@ when ImplicitPrelude is on and no
import names Prelude; SOURCE imports use the real module; @import
splice@ and @import quote@ are normal imports.
-}

-- * Input

-- | What resolution reads. The caller takes the states in one go, so
-- they are consistent.
data ResolveInput = ResolveInput
  { graph :: ModuleGraph,
    -- | Every module's state. Modules with no slot (generated, no
    -- source) are 'ModuleNotParsed'.
    state :: ModuleId -> ModuleState,
    -- | Final export sets from earlier runs.
    memo :: Map ModuleId ExportSet
  }

-- | A module, as resolution sees it.
data ModuleState
  = ModuleParsed ParsedInput
  | -- | Done, but no model: text only, failed, generated, no source.
    ModuleNotParsed
  | ModuleNotReady

-- | A parsed module and its file's text (for the names of free tokens).
data ParsedInput = ParsedInput
  { text :: Text,
    parsed :: ParsedModule
  }

-- * Results

-- | What a free name refers to. See Note [Resolution].
data Resolved
  = Exact Target
  | Heuristic (NonEmpty Reason) Target
  | External ExternalGuess
  | Unresolved UnresolvedWhy
  deriving (Eq, Show)

-- | A place in our modules.
data Target
  = DeclTarget ModuleId DeclId
  | -- | The declaration is unknown: made by a splice, or the module isn't
    -- parsed or ready.
    ModuleTarget ModuleId
  deriving (Eq, Ord, Show)

-- | An external module we think has it.
data ExternalGuess = ExternalGuess
  { module_ :: ExternalModule,
    how :: GuessKind
  }
  deriving (Eq, Show)

-- | How sure an external guess is.
data GuessKind
  = -- | An import list names it.
    Listed
  | -- | The only open external import.
    OnlyOpenImport
  | -- | The first of this many open external imports.
    OneOf Int
  deriving (Eq, Show)

-- | Why a result is a guess.
data Reason
  = -- | This many candidates: the first.
    Ambiguous Int
  | NotReady
  | NotParsed
  | -- | The module exports it, and has top-level splices.
    MadeBySplice
  | -- | Nothing has it, and this module has top-level splices.
    MadeBySpliceHere
  | -- | In a splice; only this module has it (stage restriction).
    ViaSplice
  | -- | Maybe bound by a @C {..}@.
    Wildcard
  | RecordDot
  | -- | A constructor in a type, with DataKinds.
    DataKinds
  | -- | Only through imports inside a CPP conditional.
    CppImport
  | ImportCycle
  | -- | The import's module is a guess.
    ModuleGuess MappingGuess
  | -- | The import list names it, but its module doesn't export it.
    ListedNotFound
  deriving (Eq, Ord, Show)

-- | Why nothing.
data UnresolvedWhy
  = NoCandidate
  | -- | A type variable, implicit parameter or module word.
    NotAGlobal
  | -- | No candidate, and CPP skipped some import lines.
    ExcludedByCpp
  deriving (Eq, Show)

-- | The results for one module.
data ModuleResolution = ModuleResolution
  { -- | Every free token, by index.
    names :: IntMap Resolved,
    -- | The field of each pun whose field is free (@C {f}@).
    punFields :: IntMap Resolved,
    -- | The names in doc comments. See Note [Names in docs] in
    -- "HaskellReviewer.Core.DocLinks". Lazy: only the page reads them,
    -- and forces them ('forceDocNames').
    docNames :: ~(Map DocRef Resolved),
    -- | Modules read that are not ready yet: results may change.
    notReady :: [ModuleId],
    -- | Final export sets computed here, not in the memo yet.
    newFinal :: Map ModuleId ExportSet
  }

-- | Some module read is not ready yet.
isPartial :: ModuleResolution -> Bool
isPartial r = not (null r.notReady)

-- | Export sets, final or not, and the new final ones.
data ExportSets = ExportSets
  { sets :: Map ModuleId ExportSet,
    newFinal :: Map ModuleId ExportSet
  }

-- | Force a result fully.
forceResolved :: Resolved -> ()
forceResolved = \case
  Exact t -> target t
  Heuristic rs t -> foldr seq () rs `seq` target t
  External g -> forceExternalModule g.module_ `seq` g.how `seq` ()
  Unresolved w -> w `seq` ()
  where
    target = \case
      DeclTarget m d -> m `seq` forceDeclId d
      ModuleTarget m -> m `seq` ()

-- | Force every result and set of a module's resolution, except
-- @docNames@, which stays lazy and is forced only by @pageLinks@, on
-- the request thread ('forceDocNames').
forceModuleResolution :: ModuleResolution -> ()
forceModuleResolution r =
  foldr (\x acc -> forceResolved x `seq` acc) () r.names `seq`
    foldr (\x acc -> forceResolved x `seq` acc) () r.punFields `seq`
      foldr seq () r.notReady `seq`
        forceExportSets r.newFinal

-- | Force the doc name results. Not in 'forceModuleResolution': the
-- source view doesn't read them.
forceDocNames :: ModuleResolution -> ()
forceDocNames r = foldr (\x acc -> forceResolved x `seq` acc) () r.docNames

-- * Running

-- | Resolve a module's free names. 'Nothing' if it isn't parsed.
resolveModule :: ResolveInput -> ModuleId -> Maybe ModuleResolution
resolveModule input = resolveWith input AllNames

-- | Like 'resolveModule', but only the free tokens and pun fields whose
-- 'occKey' passes, and no doc names. The tokens it keeps get the same
-- results as with 'resolveModule'. For find references.
resolveModuleOnly :: ResolveInput -> ModuleId -> (Text -> Bool) -> Maybe ModuleResolution
resolveModuleOnly input self keep = resolveWith input (OnlyNames keep) self

-- | Which names to resolve.
data Which
  = -- | All, doc names too.
    AllNames
  | -- | The free tokens whose key passes; no doc names.
    OnlyNames (Text -> Bool)

resolveWith :: ResolveInput -> Which -> ModuleId -> Maybe ModuleResolution
resolveWith input which self = case input.state self of
  ModuleParsed p -> Just $ runST $ do
    env <- newEnv input
    t <- tableOf env self p.parsed
    let ls = sourceLines p.text
    imports <- scopeFor env Set.empty self p.parsed (cppBlocks ls p.parsed)
    let occs = occurrenceList p.parsed.occurrences
        textAt i = sliceSpan ls (tokenSpanAt p.parsed.tokens i)
        wanted c txt = case which of
          AllNames -> True
          OnlyNames keep -> keep (occKey c txt)
        frees =
          [ freeToken i txt o o.namespace c
          | (TokenIx i, o) <- occs,
            FreeRef c <- [o.resolution],
            let txt = textAt i,
            wanted c txt
          ]
        -- A pun's field is a field.
        puns =
          [ freeToken i txt o FieldNs c
          | (TokenIx i, o) <- occs,
            Just (FreeRef c) <- [o.punField],
            let txt = textAt i,
            wanted c txt
          ]
        -- Distinct names to look up, for the scope's index choice.
        lookups = Set.size (Set.fromList (map (.text) frees))
        -- Lazy: only the page reads doc names.
        refs = case which of
          AllNames -> Set.fromList (map docRef (docNames p.parsed))
          OnlyNames _ -> Set.empty
        -- A cut at this module, in what an import brings: its own names.
        own si = case si.target of
          TargetHome m s -> si {target = TargetHome m (withoutOpen OpenCycle self s)}
          TargetExternal _ -> si
        scopeImports = map (own . (.import_)) imports
        scope' = mkScope lookups scopeImports
        ctx =
          Ctx
            { env = env,
              self = self,
              table = t,
              scope = scope',
              docScope = mkScope (Set.size refs) scopeImports,
              fullNames = fullNameScope (Set.size refs) p.parsed scopeImports,
              byIndex = IntMap.fromList [(si.index, si) | si <- scopeImports],
              lines = ls,
              parsed = p.parsed,
              items = itemIndex p.parsed,
              exportSubs = exportSubsOf scope' t p.parsed,
              cppExcluded = excludedImports p.parsed,
              dataKinds = "DataKinds" `elem` p.parsed.extensions.enabled
            }
    keys <- newSTRef Map.empty
    names <- forM frees $ \f -> (f.index,) <$> resolveToken ctx keys f
    punResults <- forM puns $ \f -> (f.index,) <$> resolveToken ctx keys f
    cache <- readSTRef env.cache
    pure
      ModuleResolution
        { names = IntMap.fromList names,
          punFields = IntMap.fromList punResults,
          docNames = Map.fromSet (docLookup ctx) refs,
          notReady =
            Set.toList $
              Set.unions
                [ Map.findWithDefault Set.empty OpenNotReady s.openHome
                | si <- scopeImports,
                  TargetHome _ s <- [si.target]
                ],
          newFinal = newFinalSets input cache
        }
  _ -> Nothing

-- | Export sets of these modules (final or not), and the new final ones.
exportSets :: ResolveInput -> [ModuleId] -> ExportSets
exportSets input ms = runST $ do
  env <- newEnv input
  sets <- forM ms $ \m -> (m,) . (.set) <$> setOf env Set.empty m
  cache <- readSTRef env.cache
  pure ExportSets {sets = Map.fromList sets, newFinal = newFinalSets input cache}

newFinalSets :: ResolveInput -> Map ModuleId Computed -> Map ModuleId ExportSet
newFinalSets input cache =
  Map.map
    (.set)
    (Map.filterWithKey (\k c -> c.final && not (Map.member k input.memo)) cache)

-- * The walk

-- | A set, and whether it's final. See Note [Export set walk].
data Computed = Computed
  { set :: ExportSet,
    final :: Bool
  }

data Env s = Env
  { input :: ResolveInput,
    cache :: STRef s (Map ModuleId Computed),
    tables :: STRef s (Map ModuleId LocalTable)
  }

newEnv :: ResolveInput -> ST s (Env s)
newEnv input = Env input <$> newSTRef Map.empty <*> newSTRef Map.empty

-- | A parsed module's local table, built once per run.
tableOf :: Env s -> ModuleId -> ParsedModule -> ST s LocalTable
tableOf env m p = do
  ts <- readSTRef env.tables
  case Map.lookup m ts of
    Just t -> pure t
    Nothing -> do
      let t = localTable m p
      modifySTRef' env.tables (Map.insert m t)
      pure t

-- | The local table of a module, if parsed.
tableOfId :: Env s -> ModuleId -> ST s (Maybe LocalTable)
tableOfId env m = case env.input.state m of
  ModuleParsed p -> Just <$> tableOf env m p.parsed
  _ -> pure Nothing

-- | A module's export set. The stack is the modules being computed.
setOf :: Env s -> Set ModuleId -> ModuleId -> ST s Computed
setOf env stack m
  | Just s <- Map.lookup m env.input.memo = pure (Computed s True)
  | otherwise = do
      cache <- readSTRef env.cache
      case (Map.lookup m cache, env.input.state m) of
        (Just c, _) -> pure c
        (Nothing, ModuleNotReady) -> store (Computed (openSet OpenNotReady m) False)
        (Nothing, ModuleNotParsed) -> store (Computed (openSet OpenNotParsed m) True)
        (Nothing, ModuleParsed p) -> do
          t <- tableOf env m p.parsed
          if
            | not (needsScope t p.parsed) ->
                store (Computed (exportSet t p.parsed NoScopeNeeded) True)
            | Set.member m stack ->
                -- A cut: not stored, so the module itself still gets its
                -- full set.
                pure (Computed (exportSet t p.parsed CycleCutHere) False)
            | otherwise -> do
                imports <- scopeFor env (Set.insert m stack) m p.parsed []
                let scope =
                      mkScope (maybe 0 length p.parsed.exports) (map (.import_) imports)
                    -- A cut at m itself: this is m's whole set.
                    set = withoutOpen OpenCycle m (exportSet t p.parsed (Scoped scope))
                store (Computed set (all (.final) imports))
  where
    store c = c <$ modifySTRef' env.cache (Map.insert m c)

-- | An import with its set, and whether that set is final.
data ScopeEntry = ScopeEntry
  { final :: Bool,
    import_ :: ScopeImport
  }

-- | The module's imports, mapped, with their sets. With CPP blocks,
-- imports inside one are marked.
scopeFor :: Env s -> Set ModuleId -> ModuleId -> ParsedModule -> [CppBlock] -> ST s [ScopeEntry]
scopeFor env stack m p blocks = do
  -- The implicit Prelude is first, as in GHC: index -1.
  let imports = [(-1, i) | i <- implicitPrelude p] <> zip [0 ..] p.imports
  forM imports $ \(k, i) ->
    let si = scopeImport k i (inCppIf blocks i)
     in case mapImport env.input.graph m i.package i.name.value of
          HomeModule target guesses -> do
            c <- setOf env stack target
            pure (ScopeEntry c.final (si guesses (TargetHome target c.set)))
          ExternalImport x -> pure (ScopeEntry True (si [] (TargetExternal x)))

-- | Our modules a module imports directly, the implicit Prelude too, in
-- import order, no repeats. What a request waits for (D-252).
homeImports :: ModuleGraph -> ModuleId -> ParsedModule -> [ModuleId]
homeImports g m p =
  nubOrd
    [ t
    | i <- implicitPrelude p <> p.imports,
      HomeModule t _ <- [mapImport g m i.package i.name.value]
    ]

-- | @import Prelude@, when it's implicit.
implicitPrelude :: ParsedModule -> [Model.Import]
implicitPrelude p
  | "ImplicitPrelude" `elem` p.extensions.enabled,
    all (\i -> i.name.value /= prelude) p.imports =
      [ Model.Import
          { span = noSpan,
            name = Located noSpan prelude,
            package = Nothing,
            source = NormalImport,
            safe = False,
            qualified = NotQualified,
            level = Model.NormalLevel,
            as = Nothing,
            list = Nothing
          }
      ]
  | otherwise = []
  where
    prelude = ModuleName "Prelude"
    noSpan = Span (Line 0) (Col 0) (Line 0) (Col 0)

-- * CPP

-- | The lines of a CPP @#if@ … @#endif@ block, both ends included.
data CppBlock = CppBlock
  { first :: Line,
    last :: Line
  }

-- | Is the import inside one of these blocks?
inCppIf :: [CppBlock] -> Model.Import -> Bool
inCppIf blocks i = any (\b -> b.first < l && l < b.last) blocks
  where
    l = i.span.startLine

-- | The CPP @#if@ blocks of a module. Nesting comes from the directive
-- lines, read from the text. Built once per module.
cppBlocks :: SourceLines -> ParsedModule -> [CppBlock]
cppBlocks ls p = go [] directiveLines
  where
    directiveLines =
      [ Line l
      | r <- lineRegionList p.regions,
        r.kind == CppDirectives,
        let Line a = r.firstLine
            Line b = r.lastLine,
        l <- [a .. b]
      ]
    go open = \case
      [] -> []
      l : rest -> case directive l of
        d
          | "if" `Text.isPrefixOf` d -> go (l : open) rest
          | "endif" `Text.isPrefixOf` d, o : open' <- open -> CppBlock o l : go open' rest
          | otherwise -> go open rest
    -- The start of the line is enough to tell @#if@ and @#endif@ from
    -- the other directives: 64 columns, spaces after the @#@ included.
    directive l =
      let start = sliceSpan ls (Span l (Col 1) l (Col 64))
       in Text.stripStart (Text.drop 1 (Text.stripStart start))

-- | CPP skipped lines before the first declaration: maybe imports.
excludedImports :: ParsedModule -> Bool
excludedImports p =
  any
    (\r -> r.kind == CppExcluded && maybe True (r.firstLine <) firstDecl)
    (lineRegionList p.regions)
  where
    firstDecl =
      listToMaybe
        [(NonEmpty.head (declExtent d).parts).span.startLine | d <- p.declarations]

-- * Resolving tokens

-- | What resolving a module's tokens needs.
data Ctx s = Ctx
  { env :: Env s,
    self :: ModuleId,
    table :: LocalTable,
    scope :: Scope,
    -- | The same imports, indexed for the doc names (Note [Lookup
    -- cost] in "HaskellReviewer.Core.ImportScope"). Built only when
    -- needed.
    docScope :: ~Scope,
    -- | The imports by module name, not alias, for doc names. Built
    -- only when needed.
    fullNames :: ~Scope,
    byIndex :: IntMap ScopeImport,
    lines :: SourceLines,
    parsed :: ParsedModule,
    items :: Map (Line, Col) ItemRef,
    -- | Subs of export items whose parent isn't ours, by parent (as
    -- written) and name.
    exportSubs :: Map (Maybe ModuleName, Text, Text) Lookup,
    cppExcluded :: Bool,
    dataKinds :: Bool
  }

-- | A name in an import or export list.
data ItemRef = ItemRef
  { -- | The import's index; 'Nothing' for the export list.
    import_ :: Maybe Int,
    parent :: Maybe ItemName,
    name :: ItemName
  }

-- | Items of the import and export lists, by where their name starts.
itemIndex :: ParsedModule -> Map (Line, Col) ItemRef
itemIndex p =
  Map.fromList . concat $
    [ entry (Just k) i
    | (k, imp) <- zip [0 ..] p.imports,
      Just l <- [imp.list],
      i <- l.items
    ]
      <> [entry Nothing i | Just es <- [p.exports], ExportItem i <- es]
  where
    entry k i =
      ((i.name.span.startLine, i.name.span.startCol), ItemRef k Nothing i.name)
        : [ ((s.span.startLine, s.span.startCol), ItemRef k (Just i.name) s)
          | SomeSubs l <- [i.subs],
            s <- l.names
          ]

-- | The item a token is in, if any.
itemAt :: Ctx s -> Span -> Maybe ItemRef
itemAt ctx s = case Map.lookupLE (s.startLine, s.startCol) ctx.items of
  Just (_, r)
    | let n = r.name.span,
      (s.startLine, s.startCol) < (n.endLine, n.endCol) ->
        Just r
  _ -> Nothing

-- | A free token (or a pun's field), from its occurrence.
data FreeToken = FreeToken
  { index :: Int,
    text :: Text,
    namespace :: Namespace,
    role :: Role,
    inSplice :: Bool,
    context :: FreeContext
  }

freeToken :: Int -> Text -> Occurrence -> Namespace -> FreeContext -> FreeToken
freeToken i t o ns c =
  FreeToken
    { index = i,
      text = t,
      namespace = ns,
      role = o.role,
      inSplice = o.inSplice,
      context = c
    }

-- | What one lookup depends on.
data Key = Key
  { -- | The name as written: it gives the qualifier and the name.
    written :: Text,
    space :: Space,
    inSplice :: Bool
  }
  deriving (Eq, Ord)

-- | Plain lookups done so far.
type Keys s = STRef s (Map Key Resolved)

-- | One free token (or pun field).
resolveToken :: Ctx s -> Keys s -> FreeToken -> ST s Resolved
resolveToken ctx keys f
  | f.namespace `elem` [TyVarNs, IPNs, ModuleNs, OtherNs] = pure (Unresolved NotAGlobal)
  | f.role == Import, Just r <- item, Just k <- r.import_ = pure (importItem ctx k r)
  | f.role == Export, Just r <- item, Nothing <- r.import_ = pure (exportItem ctx r)
  | f.role `elem` [Def, Sig],
    e : _ <- namesLookup (spaceOf f.namespace) (tokenName f).occ ctx.table.all,
    HomeDecl m d <- e.origin =
      pure (Exact (DeclTarget m d))
  | otherwise = case f.context of
      Plain -> plain ctx keys f
      QuasiQuoter -> keyed ctx keys (Key (quoter f.text).written ValueSpace f.inSplice)
      FieldOfDot -> pure (dotField ctx f)
      MethodOfClass t -> do
        cls <- anchor ctx keys t TypeSpace
        childOf ctx f cls $ \tbl d ->
          [ c
          | Just a <- [Map.lookup d tbl.availOf],
            c <- Set.toList a.children,
            c.kind == MethodKind
          ]
      FieldOfCon t -> do
        con <- anchor ctx keys t ConSpace
        childOf ctx f con conFieldsOf
      MaybeWildcardOf t -> wildcardField ctx keys f t
  where
    item = itemAt ctx (tokenSpanAt ctx.parsed.tokens f.index)

-- | The token's text as a name in its namespace.
tokenName :: FreeToken -> NameRef
tokenName f = nameRef (if f.namespace == TypeNs then Rich.TypeNs else Rich.ValueNs) f.text

-- | A plain lookup of the token.
plain :: Ctx s -> Keys s -> FreeToken -> ST s Resolved
plain ctx keys f = keyed ctx keys (Key f.text (spaceOf f.namespace) f.inSplice)

-- | A record dot field. See Note [Resolution].
dotField :: Ctx s -> FreeToken -> Resolved
dotField ctx f = case lookupScope ctx.scope n.qualifier n.occ FieldSpace of
  l@(Found _) -> addReason RecordDot (decide ctx l)
  Missing _ -> case namesLookup FieldSpace n.occ ctx.table.all of
    -- Several fields here of that name (the walk links only one).
    e : es
      | HomeDecl m d <- e.origin ->
          Heuristic
            (RecordDot :| [Ambiguous (1 + length es) | not (null es)])
            (DeclTarget m d)
    _ -> Unresolved NoCandidate
  where
    n = nameRef Rich.ValueNs f.text

-- | A field maybe bound by @C {..}@. See Note [Resolution].
wildcardField :: Ctx s -> Keys s -> FreeToken -> TokenIx -> ST s Resolved
wildcardField ctx keys f t = do
  con <- anchor ctx keys t ConSpace
  viaCon <- childOf ctx f con conFieldsOf
  case viaCon of
    Exact _ -> pure (addReason Wildcard viaCon)
    Heuristic _ (DeclTarget _ _) -> pure (addReason Wildcard viaCon)
    _ -> do
      r <- plain ctx keys f
      pure (if knownCon con then r else addReason Wildcard r)
  where
    knownCon = \case
      Exact (DeclTarget _ _) -> True
      Heuristic _ (DeclTarget _ _) -> True
      _ -> False

-- | The token an instance method or record field hangs on.
anchor :: Ctx s -> Keys s -> TokenIx -> Space -> ST s Resolved
anchor ctx keys (TokenIx a) sp
  | a >= 0,
    a < tokenCount ctx.parsed.tokens =
      -- The anchor may be ours: the walk links it to the top level.
      case occurrenceAt ctx.parsed.occurrences (TokenIx a) of
        Just o | TopLevelRef d _ <- o.resolution -> pure (Exact (DeclTarget ctx.self d))
        _ ->
          keyed
            ctx
            keys
            (Key (sliceSpan ctx.lines (tokenSpanAt ctx.parsed.tokens a)) sp False)
  | otherwise = pure (Unresolved NoCandidate)

-- | A child of the anchor's declaration, in its module.
childOf ::
  Ctx s ->
  FreeToken ->
  Resolved ->
  (LocalTable -> DeclId -> [Entity]) ->
  ST s Resolved
childOf ctx f parentR pick = case parentR of
  Exact (DeclTarget m d) -> found m d (Exact . DeclTarget m)
  Heuristic rs (DeclTarget m d) -> found m d (Heuristic rs . DeclTarget m)
  other -> pure other
  where
    found m d mk = do
      mt <- tableOfId ctx.env m
      pure $ case mt of
        Just tbl
          | c : _ <- [c | c <- pick tbl d, c.occ == (tokenName f).occ],
            HomeDecl _ cd <- c.origin ->
              mk cd
        _ -> Unresolved NoCandidate

conFieldsOf :: LocalTable -> DeclId -> [Entity]
conFieldsOf tbl d = Map.findWithDefault [] d tbl.conFields

-- | The space of a token's namespace.
spaceOf :: Namespace -> Space
spaceOf = \case
  ConNs -> ConSpace
  TypeNs -> TypeSpace
  FieldNs -> FieldSpace
  _ -> ValueSpace

-- | A plain lookup, once per key.
keyed :: Ctx s -> Keys s -> Key -> ST s Resolved
keyed ctx keys k = do
  done <- readSTRef keys
  case Map.lookup k done of
    Just r -> pure r
    Nothing -> do
      let r = plainLookup ctx k
      modifySTRef' keys (Map.insert k r)
      pure r

-- | A plain lookup, with the DataKinds and splice fallbacks.
plainLookup :: Ctx s -> Key -> Resolved
plainLookup ctx k = case lookupScope ctx.scope n.qualifier n.occ k.space of
  l@(Found _) -> decide ctx l
  l@(Missing _)
    | k.space == TypeSpace,
      ctx.dataKinds,
      cons@(Found _) <- lookupScope ctx.scope n.qualifier n.occ ConSpace ->
        addReason DataKinds (decide ctx cons)
    | k.inSplice,
      Nothing <- n.qualifier,
      e : _ <- namesLookup k.space n.occ ctx.table.all,
      HomeDecl m d <- e.origin ->
        Heuristic (ViaSplice :| []) (DeclTarget m d)
    | otherwise -> decide ctx l
  where
    n = nameRef (if k.space == TypeSpace then Rich.TypeNs else Rich.ValueNs) k.written

-- * Names in docs

-- | How good a doc name result is, best first. See Note [Resolution].
data DocRank = HomeHit | OtherHit | MissGuess
  deriving (Eq, Ord)

-- | A name in a doc comment. See Note [Resolution].
docLookup :: Ctx s -> DocRef -> Resolved
docLookup ctx r = case sortOn fst results of
  (_, x) : _ -> x
  [] -> Unresolved NoCandidate
  where
    spaces = docSpaces r
    -- Stable: the first of each rank wins.
    results =
      [ x
      | sp <- spaces,
        scope <- scopes,
        Just x <- [look scope sp]
      ]
    scopes = case r.qualifier of
      Nothing -> [ctx.docScope]
      Just _ -> [ctx.docScope, ctx.fullNames]
    -- No DataKinds fallback when constructors come next anyway.
    dataKinds = ctx.dataKinds && ConSpace `notElem` spaces
    look scope sp = case lookupScope scope r.qualifier r.occ sp of
      l@(Found _) -> hit (decide ctx l)
      Missing m
        | sp == TypeSpace,
          dataKinds,
          cons@(Found _) <- lookupScope scope r.qualifier r.occ ConSpace ->
            hit (addReason DataKinds (decide ctx cons))
        | otherwise -> case missing ctx m of
            Unresolved _ -> Nothing
            x -> Just (MissGuess, x)
    hit x = case x of
      Unresolved _ -> Nothing
      _ -> Just (if isHomeDecl x then HomeHit else OtherHit, x)
    isHomeDecl = \case
      Exact (DeclTarget _ _) -> True
      Heuristic _ (DeclTarget _ _) -> True
      _ -> False

-- | The spaces a doc name is looked up in, in order. See Note
-- [Resolution].
docSpaces :: DocRef -> [Space]
docSpaces r = case r.namespace of
  Rich.TypeNs -> [TypeSpace]
  Rich.ValueNs -> if conLike then [ConSpace] else [ValueSpace]
  Rich.AnyNs
    | conLike -> [TypeSpace, ConSpace]
    | varLike -> [ValueSpace]
    | otherwise -> [ValueSpace, TypeSpace]
  where
    first = fst <$> Text.uncons r.occ
    conLike = maybe False (\c -> isUpper c || c == ':') first
    varLike = maybe False (\c -> c == '_' || (isAlpha c && not (isUpper c))) first

-- | The imports with each one's module name as its alias, for doc
-- names written with the full name, indexed for this many lookups. The
-- implicit Prelude (index -1) isn't there: @docScope@ has it under its
-- name already.
fullNameScope :: Int -> ParsedModule -> [ScopeImport] -> Scope
fullNameScope lookups p is =
  mkScope lookups [si {alias = n} | si <- is, Just n <- [IntMap.lookup si.index names]]
  where
    names = IntMap.fromList (zip [0 ..] [i.name.value | i <- p.imports])

-- | A token of an import list: in that import's module only.
importItem :: Ctx s -> Int -> ItemRef -> Resolved
importItem ctx k r = case IntMap.lookup k ctx.byIndex of
  Nothing -> Unresolved NoCandidate
  Just si -> case si.target of
    TargetExternal x -> External ExternalGuess {module_ = x, how = Listed}
    TargetHome _ _ -> decide ctx (lookupImportItem si r.parent r.name)

-- | A free token of the export list: local first, then the scope.
exportItem :: Ctx s -> ItemRef -> Resolved
exportItem ctx r = case (local, r.parent) of
  -- The walk left it free, but it's ours (a field with no selector,
  -- say).
  (e : es, _)
    | HomeDecl m d <- e.origin ->
        if null es
          then Exact (DeclTarget m d)
          else Heuristic (Ambiguous (1 + length es) :| []) (DeclTarget m d)
  (_, Nothing) ->
    decide ctx (lookupScope ctx.scope r.name.qualifier r.name.occ (itemSpace r.name))
  (_, Just p) ->
    decide ctx $
      Map.findWithDefault
        (Missing noneMissing)
        (p.qualifier, p.occ, r.name.occ)
        ctx.exportSubs
  where
    ours = ownQualifier ctx.table
    local = case r.parent of
      Nothing
        | ours r.name -> namesLookup (itemSpace r.name) r.name.occ ctx.table.all
      Just p
        | ours p -> Map.findWithDefault [] (p.occ, r.name.occ) ctx.table.childByName
      _ -> []

-- | The subs of the export items whose parent isn't ours, each item in
-- one pass.
exportSubsOf ::
  Scope ->
  LocalTable ->
  ParsedModule ->
  Map (Maybe ModuleName, Text, Text) Lookup
exportSubsOf scope t p =
  Map.fromList
    [ ((i.name.qualifier, i.name.occ, o), l)
    | Just es <- [p.exports],
      ExportItem i <- es,
      SomeSubs sl <- [i.subs],
      not (ours i.name),
      (o, l) <-
        Map.toList
          ( lookupChildren
              scope
              i.name.qualifier
              i.name.occ
              (map (.occ) sl.names)
          )
    ]
  where
    ours n = ownQualifier t n && not (null (namesLookup TypeSpace n.occ t.all))

-- | A candidate of ours: where it goes, and what makes it a guess.
data HomeCandidate = HomeCandidate
  { place :: Target,
    reasons :: [Reason]
  }

-- | The rules of Note [Resolution].
decide :: Ctx s -> Lookup -> Resolved
decide ctx = \case
  Found hits -> found (NonEmpty.toList hits)
  Missing m -> missing ctx m
  where
    found hits =
      let entities = nubOrdOn (.entity) hits
          homes = [(h, c) | h <- entities, Right c <- [candidate h.entity.origin]]
          externals = [x | h <- entities, Left x <- [candidate h.entity.origin]]
       in case (homes, externals) of
            ([(h, c)], []) -> one hits h c
            ((h, c) : _, _) ->
              let amb = Ambiguous (length homes + length externals)
               in case one hits h c of
                    Exact t -> Heuristic (amb :| []) t
                    Heuristic rs t -> Heuristic (amb NonEmpty.<| rs) t
                    other -> other
            ([], x : _) -> External ExternalGuess {module_ = x, how = Listed}
            ([], []) -> Unresolved NoCandidate
    candidate = \case
      HomeDecl m d -> Right (HomeCandidate (DeclTarget m d) [])
      SpliceMade m -> Right (HomeCandidate (ModuleTarget m) [MadeBySplice])
      ExternalName x -> Left x
    -- One entity: its reasons are those of the surest import it came
    -- through.
    one hits h c =
      let fs = [f | f <- hits, f.entity == h.entity]
          guesses =
            maybe
              []
              (map ModuleGuess)
              (minimumOn length [f.via.guesses | f <- fs])
          cpp = [CppImport | all (.conditional) fs]
       in case c.reasons <> guesses <> cpp of
            [] -> Exact c.place
            r : rs -> Heuristic (r :| rs) c.place

-- | Nothing found. See Note [Resolution] (1 to 6).
missing :: Ctx s -> Misses -> Resolved
missing ctx m
  | Just o <- m.open = openResult o
  | Just c <- m.external =
      External
        ExternalGuess
          { module_ = c.first,
            how = if c.count == 1 then OnlyOpenImport else OneOf c.count
          }
  | Just o <- m.spliceOpen = openResult o
  | ctx.table.topSplices = Heuristic (MadeBySpliceHere :| []) (ModuleTarget ctx.self)
  | ctx.cppExcluded = Unresolved ExcludedByCpp
  | otherwise = Unresolved NoCandidate
  where
    openResult o =
      let r :| rs = case o.why of
            OpenParts ks -> NonEmpty.map openReason ks
            OpenAndListed ks -> NonEmpty.map openReason ks <> (ListedNotFound :| [])
            ListedButMissing -> ListedNotFound :| []
       in Heuristic (r :| rs <> map ModuleGuess o.via.guesses) (ModuleTarget o.module_)

-- | Add a reason to a home result.
addReason :: Reason -> Resolved -> Resolved
addReason r = \case
  Exact t -> Heuristic (r :| []) t
  Heuristic rs t
    | r `elem` rs -> Heuristic rs t
    | otherwise -> Heuristic (r NonEmpty.<| rs) t
  other -> other

openReason :: OpenKind -> Reason
openReason = \case
  OpenNotReady -> NotReady
  OpenNotParsed -> NotParsed
  OpenSplices -> MadeBySplice
  OpenCycle -> ImportCycle

-- | The quoter of @[q|...|]@ or @[M.q|...|]@.
quoter :: Text -> NameRef
quoter t =
  nameRef
    Rich.ValueNs
    ( Text.takeWhile
        (\c -> c /= '|' && not (isSpace c))
        (Text.drop 1 t)
    )

-- | The name a free token is looked up by, with no qualifier: the
-- quoter for a quasi-quote. A token can only resolve to a declaration
-- with this name. The key of the find references index (Note [Find
-- references] in "HaskellReviewer.Core.References").
occKey :: FreeContext -> Text -> Text
occKey c t = case c of
  QuasiQuoter -> (quoter t).occ
  _ -> (nameRef Rich.ValueNs t).occ
