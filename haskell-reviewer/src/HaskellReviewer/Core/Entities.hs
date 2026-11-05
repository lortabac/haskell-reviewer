{-# LANGUAGE StrictData #-}

-- StrictData, and 'forceExportSet': final export sets are shared by
-- request threads (Q-RTS-BLACKHOLE).

-- | What a module exports, as names: entities, avails, name sets and
-- export sets. Shared by "HaskellReviewer.Core.Exports" (what a module
-- exports) and "HaskellReviewer.Core.ImportScope" (what its imports
-- bring). See Note [Export sets] in "HaskellReviewer.Core.Exports".
module HaskellReviewer.Core.Entities
  ( -- * Entities
    Entity (..),
    EntityKind (..),
    Origin (..),
    Avail (..),
    samePlace,

    -- * Spaces
    Space (..),
    inSpace,
    kindSpace,
    itemSpace,
    guessKind,

    -- * Name sets
    Names (..),
    noNames,
    mkNames,
    availWith,
    valuesWith,
    bundlesOf,
    belongsTo,
    subKids,
    withKeys,
    unionNames,
    namesLookup,
    namesKeys,
    everyEntity,

    -- * Export sets
    ExportSet (..),
    OpenKind (..),
    OpenPart (..),
    openParts,
    openSet,
    withOpen,
    withoutOpen,
    setNames,
    forceExportSet,
    forceExportSets,

    -- * Helpers
    groupOn,
    unlessEmpty,
    minimumOn,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import HaskellReviewer.Core.Exported (typeLike)
import HaskellReviewer.Core.ModuleModel
  ( ItemName (..),
    NameKeyword (..),
    SubList (..),
    Subs (..),
  )
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Names
  ( DeclId,
    ExternalModule (..),
    forceDeclId,
    forceExternalModule,
  )

-- * Entities

-- | A name a module exports.
data Entity = Entity
  { occ :: Text,
    kind :: EntityKind,
    origin :: Origin,
    -- | The parent's name, for constructors, fields, methods and
    -- associated types. Not part of its identity: an associated type
    -- is the same with or without it.
    parent :: Maybe Text
  }
  deriving (Show)

instance Eq Entity where
  a == b = identity a == identity b

instance Ord Entity where
  compare a b = compare (identity a) (identity b)

-- | What tells entities apart.
identity :: Entity -> (Origin, Text, EntityKind)
identity e = (e.origin, e.occ, e.kind)

-- | What kind of name.
data EntityKind
  = ValueKind
  | MethodKind
  | FieldKind
  | -- | A field whose module has NoFieldSelectors: no function.
    BareFieldKind
  | ConKind
  | PatSynKind
  | TypeKind
  | ClassKind
  | -- | A constructor of a @type data@.
    TypeConKind
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Where a name is defined.
data Origin
  = -- | A declaration of one of our modules: where it's defined, after
    -- following re-exports.
    HomeDecl ModuleId DeclId
  | -- | Named in an import list of an external module.
    ExternalName ExternalModule
  | -- | Exported by this module, which has top-level splices, but not
    -- declared: a splice makes it, we guess.
    SpliceMade ModuleId
  deriving (Eq, Ord, Show)

-- | A type-level name and the children that go with it.
data Avail = Avail
  { entity :: Entity,
    children :: Set Entity
  }
  deriving (Eq, Show)

-- | Both from the same place: a parent and its child.
samePlace :: Entity -> Entity -> Bool
samePlace a c = case (a.origin, c.origin) of
  (HomeDecl m _, HomeDecl m' _) -> m == m'
  (ExternalName x, ExternalName x') -> x == x'
  _ -> False

-- * Spaces

-- | Which names a token or an item can mean.
data Space
  = -- | Variables: values, methods, fields with a selector.
    ValueSpace
  | -- | A variable in a list: values, methods, any field.
    VarItemSpace
  | -- | Constructors and pattern synonyms.
    ConSpace
  | -- | Types, classes, families, type-data constructors.
    TypeSpace
  | FieldSpace
  | -- | @data x@ in a list: any value-level name.
    AnyValueSpace
  deriving (Eq, Ord, Show)

-- | The kind is in the space.
inSpace :: Space -> EntityKind -> Bool
inSpace s k = case s of
  ValueSpace -> k `elem` [ValueKind, MethodKind, FieldKind]
  VarItemSpace -> k `elem` [ValueKind, MethodKind, FieldKind, BareFieldKind]
  ConSpace -> k `elem` [ConKind, PatSynKind]
  TypeSpace -> k `elem` [TypeKind, ClassKind, TypeConKind]
  FieldSpace -> k `elem` [FieldKind, BareFieldKind]
  AnyValueSpace -> not (inSpace TypeSpace k)

-- | The space a name of this kind is looked up in.
kindSpace :: EntityKind -> Space
kindSpace k
  | inSpace TypeSpace k = TypeSpace
  | inSpace FieldSpace k = FieldSpace
  | inSpace ConSpace k = ConSpace
  | otherwise = ValueSpace

-- | What an export or import item names, by its keyword and case.
itemSpace :: ItemName -> Space
itemSpace n = case n.keyword of
  PatternKeyword -> ConSpace
  TypeKeyword -> TypeSpace
  DataKeyword -> AnyValueSpace
  _
    | typeLike n.occ -> TypeSpace
    | otherwise -> VarItemSpace

-- | The kind of a name we know only from a list.
guessKind :: ItemName -> EntityKind
guessKind n = case itemSpace n of
  TypeSpace -> TypeKind
  ConSpace -> PatSynKind
  _ -> ValueKind

-- * Name sets

-- | Names by their text.
data Names = Names
  { values :: Map Text [Entity],
    types :: Map Text [Avail],
    -- | Children here without their parent's avail, by the parent's
    -- name (@module M (f)@ for a field f of T). A union puts them under
    -- the parent when it comes, as GHC merges avails.
    lone :: Map Text (Set Entity),
    -- | Each pattern synonym here, with the types it's bundled with.
    -- Kept even when the type isn't here, so a later union or @T(..)@
    -- puts it back under the type.
    bundles :: Map Entity (Set Entity)
  }
  deriving (Eq, Show)

-- | No names.
noNames :: Names
noNames =
  Names {values = Map.empty, types = Map.empty, lone = Map.empty, bundles = Map.empty}

-- | A type with these children; child types get an avail of their own.
availWith :: Entity -> Set Entity -> Names
availWith e kids =
  mkNames
    [c | c <- Set.toList kids, not (inSpace TypeSpace c.kind)]
    (Avail e kids : [Avail c Set.empty | c <- Set.toList kids, inSpace TypeSpace c.kind])

-- | Names for these values. A pattern synonym also keeps the types
-- it's bundled with (the function gives them), lone until a union
-- brings the type.
valuesWith :: (Entity -> Set Entity) -> [Entity] -> Names
valuesWith bundledWith es =
  Names
    { values = base.values,
      types = base.types,
      lone = Map.unionWith Set.union base.lone (Map.fromListWith Set.union under),
      bundles = Map.fromListWith Set.union ps
    }
  where
    base = mkNames es []
    under = [(t.occ, Set.singleton e) | (e, ts) <- ps, t <- Set.toList ts]
    ps =
      [ (e, ts)
      | e <- es,
        e.kind == PatSynKind,
        let ts = bundledWith e,
        not (Set.null ts)
      ]

-- | The types a pattern synonym is bundled with here (none for other
-- names).
bundlesOf :: Names -> Entity -> Set Entity
bundlesOf ns e = Map.findWithDefault Set.empty e ns.bundles

-- | The child goes under this parent: same place, or a pattern synonym
-- bundled with it. Takes the map, not 'Names': a union has only the
-- merged map at hand.
belongsTo :: Map Entity (Set Entity) -> Entity -> Entity -> Bool
belongsTo bs parent c
  | c.kind == PatSynKind = Set.member parent (Map.findWithDefault Set.empty c bs)
  | otherwise = samePlace parent c

-- | The children these subs name.
subKids :: Subs -> Set Entity -> Set Entity
subKids subs kids = case subs of
  NoSubs -> Set.empty
  AllSubs -> kids
  SomeSubs l
    | Just _ <- l.wildcardAt -> kids
    | otherwise -> withKeys (.occ) (map (.occ) l.names) kids

-- | The elements with one of these keys. A set of the keys, not a list
-- scanned per element (the tests count comparisons).
withKeys :: (Ord k) => (a -> k) -> [k] -> Set a -> Set a
withKeys key ks xs =
  let wanted = Set.fromList ks
   in Set.filter (\x -> Set.member (key x) wanted) xs

-- | Names of these values and avails; children not under their
-- parent's avail are lone. Avails of one entity are merged (an
-- associated type is its own avail and a child of its class's).
mkNames :: [Entity] -> [Avail] -> Names
mkNames vs as =
  Names
    { values = Map.map nubOrd (groupOn [(e.occ, e) | e <- vs]),
      types = types,
      lone =
        Map.fromListWith
          Set.union
          [ (p, Set.singleton c)
          | c <- vs <> map (.entity) as,
            Just p <- [c.parent],
            not (underParent types p c)
          ],
      bundles =
        Map.fromListWith
          Set.union
          [ (c, Set.singleton a.entity)
          | a <- as,
            c <- Set.toList a.children,
            c.kind == PatSynKind
          ]
    }
  where
    types =
      Map.map
        (foldl' (\acc a -> mergeAvails acc [a]) [])
        (groupOn [(a.entity.occ, a) | a <- as])

-- | The child is under its parent's avail.
underParent :: Map Text [Avail] -> Text -> Entity -> Bool
underParent types p c = any (Set.member c . (.children)) (Map.findWithDefault [] p types)

-- | Both. Lone children of one side go under the other side's avail of
-- their parent: looked up from the smaller side, so chains stay cheap.
unionNames :: Names -> Names -> Names
unionNames a b =
  Names
    { values = Map.unionWith mergeEntities a.values b.values,
      types = types',
      lone = foldl' unplace (Map.unionWith Set.union a.lone b.lone) meet,
      bundles = bundles
    }
  where
    bundles = Map.unionWith Set.union a.bundles b.bundles
    types = Map.unionWith mergeAvails a.types b.types
    types' = foldl' place types meet
    size n = Map.size n.types + Map.size n.lone
    (small, big) = if size a <= size b then (a, b) else (b, a)
    meet =
      [(p, cs) | (p, cs) <- Map.toList small.lone, Map.member p big.types]
        <> [(p, cs) | p <- Map.keys small.types, Just cs <- [Map.lookup p big.lone]]
    place ts (p, cs) =
      Map.adjust
        ( map
            ( \av ->
                Avail
                  av.entity
                  (Set.union av.children (Set.filter (belongsTo bundles av.entity) cs))
            )
        )
        p
        ts
    unplace l (p, _) =
      Map.update (unlessEmpty . Set.filter (not . underParent types' p)) p l

-- | Short lists (one name, a few kinds): appended, each once.
mergeEntities :: [Entity] -> [Entity] -> [Entity]
mergeEntities a b = case b of
  [] -> a
  _ -> nubOrd (a <> b)

-- | Lists of avails are short (one type of a name, maybe a class).
mergeAvails :: [Avail] -> [Avail] -> [Avail]
mergeAvails = foldl' add
  where
    add as b = case break (\a -> a.entity == b.entity) as of
      (before, a : after) ->
        before <> (Avail a.entity (Set.union a.children b.children) : after)
      _ -> as <> [b]

-- | The entities of that name in the space.
namesLookup :: Space -> Text -> Names -> [Entity]
namesLookup s occ ns = case s of
  TypeSpace -> map (.entity) (Map.findWithDefault [] occ ns.types)
  _ -> filter (inSpace s . (.kind)) (Map.findWithDefault [] occ ns.values)

-- | Every name, values and types.
namesKeys :: Names -> [Text]
namesKeys ns = Map.keys ns.values <> Map.keys ns.types

-- | Every entity, children too.
everyEntity :: Names -> [Entity]
everyEntity ns =
  concat (Map.elems ns.values)
    <> concat [a.entity : Set.toList a.children | as <- Map.elems ns.types, a <- as]

-- * Export sets

-- | What a module exports. See Note [Export sets] in
-- "HaskellReviewer.Core.Exports".
data ExportSet = ExportSet
  { names :: Names,
    -- | Our modules we can't list, by why.
    openHome :: Map OpenKind (Set ModuleId),
    -- | External modules re-exported whole.
    openExternal :: Set ExternalModule
  }
  deriving (Eq, Show)

-- | Why part of an export set is unknown.
data OpenKind
  = OpenNotReady
  | -- | Text only, failed, generated, preprocessed or no source.
    OpenNotParsed
  | -- | Top-level splices, and no export list.
    OpenSplices
  | -- | An import cycle was cut there.
    OpenCycle
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One open part, for dumps and tests.
data OpenPart
  = ReexportsExternal ExternalModule
  | UnparsedModule ModuleId
  | PendingModule ModuleId
  | SpliceModule ModuleId
  | CycleCut ModuleId
  deriving (Eq, Ord, Show)

-- | Every open part.
openParts :: ExportSet -> [OpenPart]
openParts s =
  map ReexportsExternal (Set.toList s.openExternal)
    <> [part k m | (k, ms) <- Map.toList s.openHome, m <- Set.toList ms]
  where
    part = \case
      OpenNotReady -> PendingModule
      OpenNotParsed -> UnparsedModule
      OpenSplices -> SpliceModule
      OpenCycle -> CycleCut

-- | Nothing known but why.
openSet :: OpenKind -> ModuleId -> ExportSet
openSet k m = withOpen k m (ExportSet noNames Map.empty Set.empty)

-- | Add an open part.
withOpen :: OpenKind -> ModuleId -> ExportSet -> ExportSet
withOpen k m s =
  ExportSet
    { names = s.names,
      openHome = Map.insertWith Set.union k (Set.singleton m) s.openHome,
      openExternal = s.openExternal
    }

-- | Without this open part.
withoutOpen :: OpenKind -> ModuleId -> ExportSet -> ExportSet
withoutOpen k m s =
  ExportSet
    { names = s.names,
      openHome = Map.update (unlessEmpty . Set.delete m) k s.openHome,
      openExternal = s.openExternal
    }

-- | The same set, with these names.
setNames :: Names -> ExportSet -> ExportSet
setNames ns s =
  ExportSet {names = ns, openHome = s.openHome, openExternal = s.openExternal}

-- | Force every set of a container. See 'forceExportSet'.
forceExportSets :: (Foldable f) => f ExportSet -> ()
forceExportSets = foldr (\s r -> forceExportSet s `seq` r) ()

-- | Walk every map, set, list and entity, so a set holds no thunk.
forceExportSet :: ExportSet -> ()
forceExportSet s =
  Map.foldr (\es r -> foldr entity' () es `seq` r) () s.names.values
    `seq` Map.foldr (\as r -> foldr avail' () as `seq` r) () s.names.types
    `seq` Map.foldr (\es r -> Set.foldr entity' () es `seq` r) () s.names.lone
    `seq` Map.foldrWithKey
      (\e ts r -> entity e `seq` Set.foldr entity' () ts `seq` r)
      ()
      s.names.bundles
    `seq` Map.foldr (\ms r -> Set.foldr seq () ms `seq` r) () s.openHome
    `seq` Set.foldr (\x r -> forceExternalModule x `seq` r) () s.openExternal
  where
    entity' e r = entity e `seq` r
    avail' a r = avail a `seq` r
    avail a = entity a.entity `seq` Set.foldr entity' () a.children
    entity e = origin e.origin `seq` maybe () (`seq` ()) e.parent
    origin = \case
      HomeDecl m d -> m `seq` forceDeclId d
      ExternalName x -> forceExternalModule x
      SpliceMade m -> m `seq` ()

-- * Helpers

-- | Group by key, keeping the order. 'Map.fromListWith' with an append
-- would be quadratic for long groups.
groupOn :: (Ord k) => [(k, a)] -> Map k [a]
groupOn = Map.map reverse . Map.fromListWith (<>) . map (\(k, a) -> (k, [a]))

-- | 'Nothing' when empty, for 'Map.update'.
unlessEmpty :: (Foldable f) => f a -> Maybe (f a)
unlessEmpty xs = if null xs then Nothing else Just xs

-- | The first smallest, if any.
minimumOn :: (Ord b) => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
  [] -> Nothing
  x : xs -> Just (foldl' (\a b -> if f b < f a then b else a) x xs)
