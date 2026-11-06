{-# LANGUAGE StrictData #-}

-- | A module's imports: what each brings, and the lookup of a name
-- through them. Pure; imports come already mapped to modules
-- ("HaskellReviewer.Core.ModuleGraph"). See Note [Import scope] and
-- Note [Lookup cost].
module HaskellReviewer.Core.ImportScope
  ( -- * Imports
    ScopeImport (..),
    ScopeTarget (..),
    scopeImport,
    subNames,
    wildcard,
    listKind,

    -- * Scope
    Scope,
    mkScope,
    importsWithAlias,
    unqualifiedPart,

    -- * Lookups
    Lookup (..),
    Hit (..),
    Misses (..),
    OpenHit (..),
    OpenWhy (..),
    Candidates (..),
    noneMissing,
    lookupScope,
    lookupChildren,
    lookupImportItem,
    childrenInScope,
  )
where

import Control.Monad (join)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import HaskellReviewer.Core.Entities
  ( Avail (..),
    Entity (..),
    EntityKind (..),
    ExportSet (..),
    Names (..),
    OpenKind (..),
    Origin (..),
    Space (..),
    availWith,
    belongsTo,
    bundlesOf,
    groupOn,
    guessKind,
    inSpace,
    itemSpace,
    kindSpace,
    minimumOn,
    mkNames,
    namesKeys,
    namesLookup,
    noNames,
    subKids,
    unionNames,
    unlessEmpty,
    valuesWith,
    withKeys,
  )
import HaskellReviewer.Core.Exported (typeLike)
import HaskellReviewer.Core.ModuleGraph (MappingGuess)
import HaskellReviewer.Core.ModuleModel
  ( Import (..),
    ImportList (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    NameKeyword (..),
    Qualified (..),
    SubList (..),
    Subs (..),
  )
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Names (ExternalModule (..))
import HaskellReviewer.Core.Project (ModuleName (..))

{- Note [Import scope]

A module's scope is its imports, each with what it brings ('visible'):

- no list: the whole export set;
- @import M (x, T, T(..), T(A, f), pattern P, type (+))@: @x@ the
  values, fields and methods named x; @T@ the type or class only;
  @T(..)@ and its children; @T(A, f)@ and those; @pattern P@ pattern
  synonyms and constructors; @type@ the type level only; @data x@ any
  value. A bundled pattern synonym remembers its type even when the
  type isn't imported (GHC's trimAvail);
- @hiding (...)@: the same names, minus, each item against the whole
  set. @hiding (C)@ hides type C and constructor C too (Haskell Report
  5.3.4); a hidden child leaves its parent's avail; children of a
  hidden type that stay are lone.

A lookup goes import by import, as GHC's pickGREs does (@x@ in imports
that are not qualified-only, @Q.x@ in imports with alias Q). An
external import with a list brings its listed names as external
entities; any other external import is open.

A type's children in scope (for @T(..)@ in an export list) come from
every import, whatever its qualifier, as GHC's findChildren.
-}

{- Note [Lookup cost]

Imports of one module are a source: a name is looked up once in the
module's export set, then the source's imports say which of them bring
it (the first with no list; a hiding one before it unless it hides
the name, and the hiders that do are few; the lists, indexed by the
names they bring). So many imports of one module cost about one.

Going source by source, @d@ distinct names and @s@ sources cost
@d * s@ lookups. With many of both that is quadratic, so a group also
gets an index of sources by name, which costs the size of what they
bring. The group builds it only when that is smaller (the caller says
how many distinct names to expect). Only names that many imports bring
at once stay quadratic, and then so is the answer.

A name nothing has needs the imports that may have it anyway (open
parts). Those that may have any name (no list, a hiding list, a list
with @T(..)@) are summed up once per group: the first open one, the
first one open only by its splices, the external modules. Only lists
that name it are looked at per name. So a miss costs about the lists
naming it, not the imports. A hiding list doesn't stop an import from
being a candidate for a name it hides: it's a guess anyway.
-}

-- * Imports

-- | One import, mapped, with what it brings. See Note [Import scope].
data ScopeImport = ScopeImport
  { -- | Its place among the written imports, from 0. The implicit
    -- Prelude is -1: first, as in GHC.
    index :: Int,
    -- | @as@, else the module name.
    alias :: ModuleName,
    qualifiedOnly :: Bool,
    target :: ScopeTarget,
    list :: Maybe ImportList,
    -- | The names its list writes.
    listed :: Set Listed,
    -- | What it brings, after the list.
    visible :: Names,
    -- | The children its list names, by parent and name, from its
    -- module's whole export set. For list tokens.
    subIndex :: ~(Map (Text, Text) [Entity]),
    -- | How sure the import → module mapping is.
    guesses :: [MappingGuess],
    -- | Inside a CPP conditional.
    conditional :: Bool
  }

-- | The module an import names.
data ScopeTarget
  = -- | One of ours, with its export set (maybe open).
    TargetHome ModuleId ExportSet
  | TargetExternal ExternalModule

-- | A name a list writes, and at which level.
data Listed = Listed Text ListedLevel
  deriving (Eq, Ord)

data ListedLevel = ListedType | ListedValue
  deriving (Eq, Ord)

-- | An import with what it brings. The index is its place (-1 for the
-- implicit Prelude).
scopeImport :: Int -> Import -> Bool -> [MappingGuess] -> ScopeTarget -> ScopeImport
scopeImport ix i conditional guesses target =
  ScopeImport
    { index = ix,
      alias = maybe i.name.value (.value) i.as,
      qualifiedOnly = i.qualified /= NotQualified,
      target = target,
      list = i.list,
      listed = maybe Set.empty listedNames i.list,
      visible = case target of
        TargetHome _ s -> case i.list of
          Nothing -> s.names
          Just l | l.kind == Only -> only s.names l.items
          Just l -> hiding s.names l.items
        TargetExternal e -> case i.list of
          Just l | l.kind == Only -> externalNames e l.items
          _ -> noNames,
      subIndex = case (target, i.list) of
        (TargetHome _ s, Just l) ->
          groupOn
            [ ((it.name.occ, c.occ), c)
            | it <- l.items,
              let wanted = map (.occ) (subNames it),
              not (null wanted),
              a <- Map.findWithDefault [] it.name.occ s.names.types,
              c <- Set.toList (withKeys (.occ) wanted a.children)
            ]
        _ -> Map.empty,
      guesses = guesses,
      conditional = conditional
    }

-- | The names in @T(A, b)@.
subNames :: Item -> [ItemName]
subNames i = case i.subs of
  SomeSubs l -> l.names
  _ -> []

-- | Every name a list writes, subs too.
listedNames :: ImportList -> Set Listed
listedNames l =
  Set.fromList
    ( concat
        [ Listed
            i.name.occ
            (if itemSpace i.name == TypeSpace then ListedType else ListedValue)
            : [Listed s.occ ListedValue | s <- subNames i]
        | i <- l.items
        ]
    )

-- | @T(..)@ or @T(.., A)@.
wildcard :: Item -> Bool
wildcard i = case i.subs of
  AllSubs -> True
  SomeSubs l -> isJust l.wildcardAt
  NoSubs -> False

-- | A type with the children these subs name.
availNames :: Subs -> Avail -> Names
availNames subs a = availWith a.entity (subKids subs a.children)

-- | @import M (...)@: just these.
only :: Names -> [Item] -> Names
only ns = foldl' (\acc i -> unionNames acc (one i)) noNames
  where
    one i = case itemSpace i.name of
      TypeSpace ->
        foldl'
          unionNames
          noNames
          ( map
              (availNames i.subs)
              (Map.findWithDefault [] i.name.occ ns.types)
          )
      sp -> valuesWith (bundlesOf ns) (namesLookup sp i.name.occ ns)

-- | @import M hiding (...)@.
hiding :: Names -> [Item] -> Names
hiding ns items = withOrphans (foldl' remove ns (concatMap gone items))
  where
    dropped = concatMap droppedType items
    -- Children of a hidden type that stay are lone now.
    withOrphans r =
      Names
        { values = r.values,
          types = r.types,
          lone = foldl' (orphans r) r.lone dropped,
          bundles = r.bundles
        }
    orphans r l p =
      let kids =
            Set.filter
              ( \c ->
                  if inSpace TypeSpace c.kind
                    then any ((== c) . (.entity)) (Map.findWithDefault [] c.occ r.types)
                    else c `elem` Map.findWithDefault [] c.occ r.values
              )
              (Set.unions (map (.children) (Map.findWithDefault [] p ns.types)))
       in if Set.null kids then l else Map.insertWith Set.union p kids l
    -- Each item against the whole set, not what the others left.
    droppedType i = case i.name.keyword of
      TypeKeyword -> [i.name.occ]
      NoKeyword | typeLike i.name.occ -> [i.name.occ]
      _ -> []
    gone i =
      let n = i.name.occ
          avails = Map.findWithDefault [] n ns.types
          kids = concatMap (Set.toList . subKids i.subs . (.children)) avails
          dropValues = case i.name.keyword of
            TypeKeyword -> []
            PatternKeyword -> [ConSpace]
            DataKeyword -> [AnyValueSpace]
            _
              | typeLike n -> [ConSpace]
              | otherwise -> [VarItemSpace]
          dropTypes = [e | n `elem` droppedType i, e <- namesLookup TypeSpace n ns]
       in [e | sp <- dropValues, e <- namesLookup sp n ns] <> dropTypes <> kids
    remove acc e =
      let -- Where e sits as a child: its parent, or for a pattern
          -- synonym the types it is bundled with.
          keys = maybe (map (.occ) (Set.toList (bundlesOf acc e))) pure e.parent
          dropKid = map (\a -> Avail a.entity (Set.delete e a.children))
          dropLone = unlessEmpty . Set.delete e
          ts
            | inSpace TypeSpace e.kind =
                Map.update (unlessEmpty . filter ((/= e) . (.entity))) e.occ acc.types
            | otherwise = acc.types
       in Names
            { values = Map.update (unlessEmpty . filter (/= e)) e.occ acc.values,
              types = foldl' (flip (Map.adjust dropKid)) ts keys,
              lone = foldl' (flip (Map.update dropLone)) acc.lone keys,
              bundles = Map.delete e acc.bundles
            }

-- | The names an external import lists.
externalNames :: ExternalModule -> [Item] -> Names
externalNames x = foldl' (\acc i -> unionNames acc (one i)) noNames
  where
    ent parent n k = Entity {occ = n, kind = k, origin = ExternalName x, parent = parent}
    one i =
      let n = i.name.occ
          -- @T(..)@: we don't know the children, but a constructor
          -- named like its type is common enough to guess.
          sameName = [ent (Just n) n ConKind | wildcard i, typeLike n]
          kids =
            sameName
              <> [ ent (Just n) s.occ (if typeLike s.occ then ConKind else ValueKind)
                 | s <- subNames i,
                   s.occ /= n || null sameName
                 ]
          e = ent Nothing n (guessKind i.name)
       in if inSpace TypeSpace e.kind
            then mkNames kids [Avail e (Set.fromList kids)]
            else mkNames [e] []

-- * Scope

-- | A module's imports, for lookups. See Note [Lookup cost].
data Scope = Scope
  { unqualified :: Group,
    byAlias :: Map ModuleName Group,
    -- | Every import, for children in scope (any qualifier).
    everything :: Group,
    aliases :: Map ModuleName [ScopeImport]
  }

-- | Imports a name can go through: the unqualified ones, or those with
-- one alias.
data Group = Group
  { -- | The imports by module, in order: a lookup goes module by
    -- module.
    sources :: [Source],
    -- | Sources by the names they bring. Built only when needed.
    byName :: ~(Map Text [Source]),
    useIndex :: Bool,
    -- | The first import that may bring any name, open for other reasons
    -- than splices.
    firstOpen :: Maybe ScopeImport,
    -- | The first one open only by its top-level splices.
    firstSpliceOpen :: Maybe ScopeImport,
    -- | External modules that may bring any name, each with the first
    -- import that brings it, in that order.
    externals :: [(ExternalModule, Int)],
    externalSet :: Set ExternalModule,
    -- | @import M (x)@ lists, by the names they list.
    listedBy :: Map Listed [ScopeImport],
    -- | Imports with lone children, by parent name.
    loneBy :: ~(Map Text [ScopeImport])
  }

-- | The imports of one module in a group.
data Source = Source
  { -- | Index of its first import.
    first :: Int,
    -- | Its export set's names (none for an external module).
    setNames :: Names,
    -- | Imports with no list, in order.
    whole :: [ScopeImport],
    hiders :: [ScopeImport],
    -- | Imports with a list.
    lists :: [ScopeImport],
    -- | Those by the names they bring.
    listIndex :: ~(Map Text [ScopeImport]),
    -- | What the hiding lists hide.
    hidden :: ~Hidden
  }

-- | The hiding lists of a source, by the indexes of the imports.
data Hidden = Hidden
  { -- | Names a list writes.
    byName :: Map Text [Int],
    -- | @T(A, f)@: by parent and name.
    subs :: Map (Text, Text) [Int],
    -- | @T(..)@: all of T's children.
    allOf :: Map Text [Int]
  }

-- | From the imports, in order, and about how many distinct names will
-- be looked up.
mkScope :: Int -> [ScopeImport] -> Scope
mkScope lookups is =
  Scope
    { unqualified = mkGroup lookups [i | i <- is, not i.qualifiedOnly],
      byAlias = Map.map (mkGroup lookups) aliases,
      everything = mkGroup lookups is,
      aliases = aliases
    }
  where
    aliases = groupOn [(i.alias, i) | i <- is]

-- | The imports with this alias (or module name).
importsWithAlias :: Scope -> ModuleName -> [ScopeImport]
importsWithAlias scope n = Map.findWithDefault [] n scope.aliases

mkGroup :: Int -> [ScopeImport] -> Group
mkGroup lookups gs =
  Group
    { sources = srcs,
      byName =
        Map.map
          (nubOrdOn (.first))
          (groupOn [(k, src) | src <- srcs, k <- keysOf src]),
      useIndex = lookups * length srcs > sum (map sizeBound srcs),
      firstOpen = listToMaybe [si | (si, ks) <- openGeneric, ks /= [OpenSplices]],
      firstSpliceOpen = listToMaybe [si | (si, [OpenSplices]) <- openGeneric],
      externals = exts,
      externalSet = Set.fromList (map fst exts),
      listedBy =
        groupOn
          [ (n, si)
          | si <- gs,
            listKind si == Just Only,
            n <- Set.toList si.listed
          ],
      loneBy = groupOn [(p, si) | si <- gs, p <- Map.keys si.visible.lone]
    }
  where
    generic = filter admitsAll gs
    openGeneric =
      [ (si, Map.keys set.openHome)
      | si <- generic,
        TargetHome _ set <- [si.target],
        not (Map.null set.openHome)
      ]
    exts =
      nubOrdOn
        fst
        [ (x, si.index)
        | si <- generic,
          x <- case si.target of
            TargetHome _ set -> Set.toList set.openExternal
            TargetExternal x -> [x]
        ]
    srcs = sourcesOf gs
    keysOf src =
      (if null src.whole && null src.hiders then [] else namesKeys src.setNames)
        <> concatMap (namesKeys . (.visible)) src.lists
    -- The index's size, without building it.
    sizeBound src =
      (if null src.whole && null src.hiders then 0 else namesSize src.setNames)
        + sum (map (namesSize . (.visible)) src.lists)
    namesSize ns = Map.size ns.values + Map.size ns.types

-- | It may bring any name: no list, a hiding list, or @T(..)@ in its
-- list.
admitsAll :: ScopeImport -> Bool
admitsAll si = case si.list of
  Nothing -> True
  Just l -> l.kind == Hiding || any wildcard l.items

-- | Only or hiding; nothing with no list.
listKind :: ScopeImport -> Maybe ListKind
listKind si = (.kind) <$> si.list

-- | Group imports by module, in order of their first import.
sourcesOf :: [ScopeImport] -> [Source]
sourcesOf gs = [source (Map.findWithDefault [] k byKey) | k <- nubOrdOn id (map keyOf gs)]
  where
    keyOf si = case si.target of
      TargetHome m _ -> Left m
      TargetExternal x -> Right x
    byKey = groupOn [(keyOf si, si) | si <- gs]
    source sis =
      let lists = [si | si <- sis, listKind si == Just Only]
          hiders = [si | si <- sis, listKind si == Just Hiding]
       in Source
            { first = minimum (map (.index) sis),
              setNames = case map (.target) sis of
                TargetHome _ set : _ -> set.names
                _ -> noNames,
              whole = [si | si <- sis, isNothing si.list],
              hiders = hiders,
              lists = lists,
              listIndex = groupOn [(k, si) | si <- lists, k <- namesKeys si.visible],
              hidden = hiddenBy hiders
            }

hiddenBy :: [ScopeImport] -> Hidden
hiddenBy hiders =
  Hidden
    { byName = groupOn [(i.name.occ, si.index) | (si, i) <- items],
      subs =
        groupOn
          [ ((i.name.occ, s.occ), si.index)
          | (si, i) <- items,
            s <- subNames i
          ],
      allOf = groupOn [(i.name.occ, si.index) | (si, i) <- items, wildcard i]
    }
  where
    items = [(si, i) | si <- hiders, Just l <- [si.list], i <- l.items]

-- * Lookups

-- | A name found through an import.
data Hit = Hit
  { entity :: Entity,
    -- | The avail, for a type-level name.
    avail :: Maybe Avail,
    -- | For a pattern synonym: the types it is bundled with, in what
    -- the import brings. Empty from 'lookupChildren' and
    -- 'lookupImportItem': those only give token results, not sets.
    bundledWith :: Set Entity,
    -- | The first import of its module that brings it.
    via :: ScopeImport,
    -- | Every import of that module that brings it is inside a CPP
    -- conditional.
    conditional :: Bool
  }

-- | What the imports have for a name.
data Lookup
  = -- | In import order. One entity can come through several modules.
    Found (NonEmpty Hit)
  | Missing Misses

-- | Nothing found: the imports that may have it anyway.
data Misses = Misses
  { -- | The first of our imports that may have it: not ready, not
    -- parsed, cut by a cycle, or its list names it.
    open :: Maybe OpenHit,
    -- | The first of our imports open only by its top-level splices.
    spliceOpen :: Maybe OpenHit,
    external :: Maybe Candidates
  }

-- | One of our imports that may have the name.
data OpenHit = OpenHit
  { module_ :: ModuleId,
    via :: ScopeImport,
    why :: OpenWhy,
    -- | The module's open parts.
    parts :: Map OpenKind (Set ModuleId)
  }

-- | Why we can't tell.
data OpenWhy
  = OpenParts (NonEmpty OpenKind)
  | -- | Open, and its list names it.
    OpenAndListed (NonEmpty OpenKind)
  | -- | Its list names it, but its module doesn't export it.
    ListedButMissing

-- | External modules that may have it: the first, and how many.
data Candidates = Candidates
  { first :: ExternalModule,
    count :: Int
  }

-- | Nothing may have it.
noneMissing :: Misses
noneMissing = Misses Nothing Nothing Nothing

-- | A name through the imports: @x@ in the unqualified ones, @Q.x@ in
-- those with alias Q.
lookupScope :: Scope -> Maybe ModuleName -> Text -> Space -> Lookup
lookupScope scope q occ sp =
  collect occ (Listed occ level) (groupOf scope q) $ \ns -> case sp of
    TypeSpace -> [(a.entity, Just a) | a <- Map.findWithDefault [] occ ns.types]
    _ -> [(e, Nothing) | e <- namesLookup sp occ ns]
  where
    level = if sp == TypeSpace then ListedType else ListedValue

-- | The children of a type-level name, as in @T(A, b)@ in an export
-- list: the type through the imports with its qualifier, the children
-- in scope through any import, by name. A name that is no child may be
-- a pattern synonym bundled there. One pass for the whole item.
lookupChildren :: Scope -> Maybe ModuleName -> Text -> [Text] -> Map Text Lookup
lookupChildren scope q parent occs = case lookupScope scope q parent TypeSpace of
  Missing ms -> Map.fromList [(o, Missing ms) | o <- occs]
  Found hs ->
    let byName =
          Map.fromListWith
            (flip (<>))
            [ ( c.occ,
                [ Hit
                    { entity = c,
                      avail = Nothing,
                      bundledWith = Set.empty,
                      via = h.via,
                      conditional = h.conditional
                    }
                ]
              )
            | h <- NonEmpty.toList hs,
              c <- Set.toList (childrenInScope scope h.entity)
            ]
        one o = case Map.findWithDefault [] o byName of
          x : xs -> Found (x :| xs)
          [] -> case lookupScope scope Nothing o ConSpace of
            Found ps
              | x : xs <- [p | p <- NonEmpty.toList ps, p.entity.kind == PatSynKind] ->
                  Found (x :| xs)
            _ -> Missing noneMissing
     in Map.fromList [(o, one o) | o <- occs]

-- | A type's children in scope, through any import. See Note [Import
-- scope].
childrenInScope :: Scope -> Entity -> Set Entity
childrenInScope scope e = Set.unions (fromSources <> fromLists <> fromLone)
  where
    g = scope.everything
    srcs = Map.findWithDefault [] e.occ g.byName
    mine ns = [a | a <- Map.findWithDefault [] e.occ ns.types, a.entity == e]
    fromSources =
      [ if null src.whole then Set.filter (byHider src) a.children else a.children
      | src <- srcs,
        not (null src.whole && null src.hiders),
        a <- mine src.setNames
      ]
    fromLists = [a.children | src <- srcs, si <- src.lists, a <- mine si.visible]
    fromLone =
      [ Set.filter (belongsTo si.visible.bundles e) cs
      | si <- Map.findWithDefault [] e.occ g.loneBy,
        cs <- maybeToList (Map.lookup e.occ si.visible.lone)
      ]
    -- Some hiding list of the module doesn't hide the child.
    byHider src c =
      let hiders =
            Set.fromList $
              Map.findWithDefault [] c.occ src.hidden.byName
                <> Map.findWithDefault [] e.occ src.hidden.allOf
                <> Map.findWithDefault [] (e.occ, c.occ) src.hidden.subs
       in Set.size hiders < length src.hiders

-- | The part of a qualified-only import that is also in scope
-- unqualified (for @module X@ in an export list). All of it when an
-- unqualified import of the same module has no list.
unqualifiedPart :: Scope -> ScopeImport -> Names
unqualifiedPart scope si
  | TargetHome m _ <- si.target,
    any (sameWhole m) (concatMap (.whole) scope.unqualified.sources) =
      si.visible
  | otherwise =
      Names
        { values = Map.mapMaybe (unlessEmpty . filter inScope) si.visible.values,
          types =
            Map.mapMaybe
              ( \as ->
                  unlessEmpty
                    [ Avail a.entity (Set.filter inScope a.children)
                    | a <- as,
                      inScope a.entity
                    ]
              )
              si.visible.types,
          lone =
            Map.unionWith
              Set.union
              (Map.mapMaybe (unlessEmpty . Set.filter inScope) si.visible.lone)
              orphans,
          bundles = Map.filterWithKey (\e _ -> inScope e) si.visible.bundles
        }
  where
    -- Children in scope whose parent isn't: lone now.
    orphans =
      Map.fromListWith
        Set.union
        [ (a.entity.occ, kids)
        | as <- Map.elems si.visible.types,
          a <- as,
          not (inScope a.entity),
          let kids = Set.filter inScope a.children,
          not (Set.null kids)
        ]
    sameWhole m w = case w.target of
      TargetHome m' _ -> m' == m
      TargetExternal _ -> False
    inScope e = case lookupScope scope Nothing e.occ (kindSpace e.kind) of
      Found hs -> any (\h -> h.entity == e) hs
      Missing _ -> False

-- | An item (or, with a parent, a sub) of this import's own list,
-- looked up in its module's whole export set.
lookupImportItem :: ScopeImport -> Maybe ItemName -> ItemName -> Lookup
lookupImportItem si parent n = case si.target of
  TargetExternal _ -> Missing noneMissing
  TargetHome m s ->
    let hit e av =
          Hit
            { entity = e,
              avail = av,
              bundledWith = Set.empty,
              via = si,
              conditional = si.conditional
            }
        found = case parent of
          Nothing -> case itemSpace n of
            TypeSpace
              | as@(_ : _) <- Map.findWithDefault [] n.occ s.names.types ->
                  [hit a.entity (Just a) | a <- as]
              -- @hiding (C)@ may name only a constructor.
              | hidingList ->
                  [hit e Nothing | e <- namesLookup ConSpace n.occ s.names]
              | otherwise -> []
            sp -> [hit e Nothing | e <- namesLookup sp n.occ s.names]
          Just p ->
            [ hit c Nothing
            | c <- Map.findWithDefault [] (p.occ, n.occ) si.subIndex
            ]
     in case found of
          x : xs -> Found (x :| xs)
          []
            | Map.null s.openHome,
              x : rest <- Set.toList s.openExternal ->
                Missing noneMissing {external = Just (Candidates x (1 + length rest))}
            | otherwise ->
                Missing noneMissing {open = openHit m si s ListNamesIt}
  where
    hidingList = listKind si == Just Hiding

-- | The imports a name with this qualifier looks at.
groupOf :: Scope -> Maybe ModuleName -> Maybe Group
groupOf scope = \case
  Nothing -> Just scope.unqualified
  Just q -> Map.lookup q scope.byAlias

-- | Look in each source that may have the key; with no hit anywhere,
-- the imports that may have the name anyway. See Note [Lookup cost].
collect :: Text -> Listed -> Maybe Group -> (Names -> [(Entity, Maybe Avail)]) -> Lookup
collect _ _ Nothing _ = Missing noneMissing
collect key name (Just g) pick =
  case concatMap (lookSource key pick) candidates of
    x : xs -> Found (x :| xs)
    [] -> Missing (missingIn g name)
  where
    candidates
      | g.useIndex = Map.findWithDefault [] key g.byName
      | otherwise = g.sources

-- | One module's imports: each entity once, through the first import
-- that brings it.
lookSource :: Text -> (Names -> [(Entity, Maybe Avail)]) -> Source -> [Hit]
lookSource key pick src =
  sortOn (.via.index) . Map.elems $
    Map.fromListWith merge [(h.entity, h) | h <- fromSet <> fromLists]
  where
    merge a b =
      let f = if a.via.index <= b.via.index then a else b
       in Hit
            { entity = f.entity,
              avail = unionAvail a.avail b.avail,
              bundledWith = Set.union a.bundledWith b.bundledWith,
              via = f.via,
              conditional = a.conditional && b.conditional
            }
    unionAvail a b = case (a, b) of
      (Just x, Just y) -> Just (Avail x.entity (Set.union x.children y.children))
      (Nothing, y) -> y
      (x, Nothing) -> x
    fromSet
      | null src.whole && null src.hiders = []
      | otherwise = [h | (e, av) <- pick src.setNames, Just h <- [admit pick src e av]]
    fromLists =
      [ Hit
          { entity = e,
            avail = av,
            bundledWith = bundlesOf si.visible e,
            via = si,
            conditional = si.conditional
          }
      | si <- Map.findWithDefault [] key src.listIndex,
        (e, av) <- pick si.visible
      ]

-- | The first import of the source that brings the entity: the first
-- with no list, or a hiding one before it that doesn't hide it. Hiders
-- that hide it are skipped: few per name.
admit ::
  (Names -> [(Entity, Maybe Avail)]) ->
  Source ->
  Entity ->
  Maybe Avail ->
  Maybe Hit
admit pick src e av = case firstHider of
  Just h -> Just (hit h (join (lookup e (pick h.visible))) (bundlesOf h.visible e))
  Nothing -> (\w -> hit w av (bundlesOf src.setNames e)) <$> firstWhole
  where
    firstWhole = listToMaybe src.whole
    before si = maybe True (\w -> si.index < w.index) firstWhole
    firstHider = listToMaybe [si | si <- takeWhile before src.hiders, has si]
    has si = any ((== e) . fst) (pick si.visible)
    unconditional =
      any (not . (.conditional)) src.whole
        || any (\si -> not si.conditional && has si) src.hiders
    hit v a bs =
      Hit
        { entity = e,
          avail = a,
          bundledWith = bs,
          via = v,
          conditional = not unconditional
        }

-- | What may have a name nothing has. See Note [Lookup cost].
missingIn :: Group -> Listed -> Misses
missingIn g name =
  Misses
    { open =
        minimumOn
          (.via.index)
          ( [ h
            | si <- maybeToList g.firstOpen,
              TargetHome m set <- [si.target],
              let named = if Set.member name si.listed then ListNamesIt else NotListed,
              Just h <- [openHit m si set named]
            ]
              <> take
                1
                [ h
                | si <- specials,
                  TargetHome m set <- [si.target],
                  Just h <- [openHit m si set ListNamesIt]
                ]
          ),
      spliceOpen = do
        si <- g.firstSpliceOpen
        TargetHome m set <- Just si.target
        openHit m si set NotListed,
      -- One of ours whose list names it comes first.
      external = case take 1 (sortOn snd specialExternals) <> take 1 g.externals of
        [] -> Nothing
        (x, _) : _ -> Just (Candidates x (length g.externals + length extra))
    }
  where
    specials = Map.findWithDefault [] name g.listedBy
    specialExternals =
      nubOrdOn
        fst
        [ (x, si.index)
        | si <- specials,
          not (admitsAll si),
          TargetHome _ set <- [si.target],
          x <- Set.toList set.openExternal
        ]
    extra = [p | p@(x, _) <- specialExternals, not (Set.member x g.externalSet)]

-- | One of our imports, as an open hit: open parts, or its list names
-- the name. A list naming it with only external open parts isn't a hit:
-- the external guess covers it.
openHit :: ModuleId -> ScopeImport -> ExportSet -> InList -> Maybe OpenHit
openHit m si set listed = (\w -> OpenHit m si w set.openHome) <$> why
  where
    why = case (nonEmpty (Map.keys set.openHome), listed) of
      (Just ks, NotListed) -> Just (OpenParts ks)
      (Just ks, ListNamesIt) -> Just (OpenAndListed ks)
      (Nothing, ListNamesIt) | Set.null set.openExternal -> Just ListedButMissing
      (Nothing, _) -> Nothing

-- | Does the import's list name the name?
data InList = ListNamesIt | NotListed
