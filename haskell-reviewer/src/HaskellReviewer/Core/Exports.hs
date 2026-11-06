{-# LANGUAGE StrictData #-}

-- | What a module exports: its own names, and its export set from its
-- export list and its imports. Pure; the walk over modules is in
-- "HaskellReviewer.Core.Resolve". See Note [Export sets].
module HaskellReviewer.Core.Exports
  ( -- * Local names
    LocalTable (..),
    localTable,

    -- * Export sets
    ExportScope (..),
    exportSet,
    needsScope,
    ownQualifier,
  )
where

import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import HaskellReviewer.Core.Code (nameRef)
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConArgs (..),
    ConDecl (..),
    Decl (..),
    DeclInfo (..),
    FamInstDecl (..),
    FieldDecl (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
    MethodDecl (..),
    PatSynDecl (..),
    TypeDecl (..),
    TypeForm (..),
    ValueDecl (..),
    famInstConstructors,
    formConstructors,
  )
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
    groupOn,
    guessKind,
    inSpace,
    itemSpace,
    mkNames,
    namesLookup,
    setNames,
    subKids,
    unionNames,
    valuesWith,
    withOpen,
  )
import HaskellReviewer.Core.Exported (nameOf)
import HaskellReviewer.Core.ImportScope
  ( Candidates (..),
    Hit (..),
    Lookup (..),
    Misses (..),
    OpenHit (..),
    Scope,
    ScopeImport (..),
    ScopeTarget (..),
    childrenInScope,
    importsWithAlias,
    listKind,
    lookupScope,
    subNames,
    unqualifiedPart,
  )
import HaskellReviewer.Core.ModuleModel
  ( Export (..),
    Extensions (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    ModuleExport (..),
    ModuleHeader (..),
    ParsedModule (..),
  )
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Names (DeclId)
import HaskellReviewer.Core.Occurrences (hasTopSplices)
import HaskellReviewer.Core.Project (ModuleName)
import HaskellReviewer.Core.RichText (NameRef (..), RefNamespace (..))

{- Note [Export sets]

An export set is what a module exports, as GHC's avails: values by
name, and types (types, classes, families, type-data constructors) by
name, each with the children exported with it (constructors, fields,
methods, associated types, bundled pattern synonyms). Children are in
the values too (associated types in the types).

- The local part comes from the exported flags 'markExported' set at
  parse time (Note [Exported] in "HaskellReviewer.Core.Exported"). A
  type's children are its exported children, however they were
  exported (GHC merges @T@ and a lone @f@ into one avail too).
- An item that names no local name is looked up in the module's own
  scope: a hit is exported with its origin. @T(..)@ brings T's children
  in scope through any import, @T(A, f)@ the named ones, and a named
  pattern synonym goes under T (bundling). A bundled pattern synonym
  remembers its type even when the type isn't exported (GHC's
  availFromGRE). With no hit, in the order of Note [Resolution] in
  "HaskellReviewer.Core.Resolve": a name made by a splice
  ('SpliceMade') if the module has top-level splices; the open parts of
  an import of ours that may have it; that name from the first open
  external import; the open parts of an import open only by its
  splices; else nothing.
- @module N@ (N not this module): what the imports with alias N bring,
  kept only if also in scope unqualified (GHC's pickGREsModExp), with
  their open parts. An external import adds itself as an open part, or
  its listed names.
- No export list: everything local, plus 'OpenSplices' if the module
  has top-level splices.
- A cycle cut: the local part; whatever the list takes from imports is
  'OpenCycle'.

Open parts say what we can't list: a module not parsed or not ready,
one with top-level splices and no export list, a cycle cut, an external
module re-exported whole. They are kept as sets so long re-export
chains share them.

Sets are 'Map's: a re-export of a whole import is a union, which shares
most of the bigger map, so a chain of n modules stays about n log n.

A child can come without its parent's avail (@import M (f)@, or
@module M (f)@). It is kept "lone", by its parent's name; when a union
brings the parent, it goes under it, as GHC merges avails. The union
looks this up from its smaller side only. A bundled pattern synonym has
no parent of its own: the set's 'bundles' says which types it goes
under (@import M (pattern P)@ keeps them, as GHC does).
-}

-- * Local names

-- | A module's own top-level names, from its declarations.
data LocalTable = LocalTable
  { self :: ModuleId,
    name :: Maybe ModuleName,
    -- | Every name, with every child.
    all :: Names,
    -- | The exported ones, with their exported children.
    exported :: Names,
    -- | Each constructor's (or record pattern synonym's) fields.
    conFields :: Map DeclId [Entity],
    -- | Each type's or class's avail, with every child.
    availOf :: Map DeclId Avail,
    -- | Every child, by parent name and name.
    childByName :: Map (Text, Text) [Entity],
    topSplices :: Bool
  }

-- | A name, its children, and whether each is exported.
data Local = Local
  { entity :: Entity,
    exported :: Bool,
    children :: [(Entity, Bool)]
  }

-- | The module's names. Built from the declarations directly, not
-- with 'HaskellReviewer.Core.Decls.namedDecls': we need the parents.
localTable :: ModuleId -> ParsedModule -> LocalTable
localTable self m =
  LocalTable
    { self = self,
      name = (.value) <$> m.header.name,
      all = everything,
      exported = names id,
      conFields =
        Map.fromList
          ( [ (c.info.id, [field (Just parent) f.info | f <- recordFields c.args])
            | (parent, c) <- allCons
            ]
              <> [ (p.info.id, [field Nothing f | f <- p.fields])
                 | PatSynD p <- m.declarations
                 ]
          ),
      availOf =
        Map.fromList
          [ (d, a)
          | as <- Map.elems everything.types,
            a <- as,
            HomeDecl _ d <- [a.entity.origin]
          ],
      childByName =
        groupOn
          [ ((a.entity.occ, c.occ), c)
          | as <- Map.elems everything.types,
            a <- as,
            c <- Set.toList a.children
          ],
      topSplices = hasTopSplices m.occurrences
    }
  where
    everything = names (const True)
    ent k parent i =
      Entity {occ = nameOf i, kind = k, origin = HomeDecl self i.id, parent = parent}
    field
      | "FieldSelectors" `elem` m.extensions.enabled = ent FieldKind
      | otherwise = ent BareFieldKind
    -- Data instance constructors go under their family when it's here.
    famChildren = Map.map concat (groupOn (map famInst famInstDecls))
    locals = map withFamily (concatMap decl m.declarations)
    withFamily l
      | l.entity.kind == TypeKind =
          Local
            { entity = l.entity,
              exported = l.exported,
              children =
                nubOrdOn
                  fst
                  (l.children <> Map.findWithDefault [] l.entity.occ famChildren)
            }
      | otherwise = l
    -- Constructors of a data instance of a family from elsewhere.
    stray =
      [ c
      | (fam, cs) <- Map.toList famChildren,
        not (any (\l -> l.entity.occ == fam && l.entity.kind == TypeKind) locals),
        c <- cs
      ]
    names keep =
      let kept = [l | l <- locals, keep l.exported]
          kids l = [e | (e, ex) <- l.children, keep ex]
          -- A child can be exported without its parent (@module M (f)@).
          valueNames =
            [l.entity | l <- kept, not (inSpace TypeSpace l.entity.kind)]
              <> [e | l <- locals, e <- kids l, not (inSpace TypeSpace e.kind)]
              <> [e | (e, ex) <- stray, keep ex]
          typeAvails =
            [ Avail l.entity (Set.fromList (kids l))
            | l <- kept,
              inSpace TypeSpace l.entity.kind
            ]
              <> [Avail e Set.empty | l <- locals, e <- kids l, inSpace TypeSpace e.kind]
       in mkNames (nubOrd valueNames) typeAvails
    decl = \case
      TypeD t -> [typeLocal Nothing t]
      ClassD c ->
        let n = nameOf c.info
         in Local
              { entity = ent ClassKind Nothing c.info,
                exported = c.info.exported,
                children =
                  [(ent TypeKind (Just n) a.info, a.info.exported) | a <- c.associated]
                    <> [ (ent MethodKind (Just n) md.info, md.info.exported)
                       | md <- c.methods
                       ]
              }
              : map (typeLocal (Just n)) c.associated
      ValueD v -> [Local (ent ValueKind Nothing v.info) v.info.exported []]
      PatSynD p ->
        Local (ent PatSynKind Nothing p.info) p.info.exported []
          : [Local (field Nothing f) f.exported [] | f <- p.fields]
      _ -> []
    -- An associated type has its class as parent.
    typeLocal parent t =
      let conKind = case t.form of
            TypeData _ -> TypeConKind
            _ -> ConKind
       in Local
            { entity = ent TypeKind parent t.info,
              exported = t.info.exported,
              children = conChildren conKind (nameOf t.info) (formConstructors t.form)
            }
    conChildren k n cs =
      nubOrdOn
        fst
        ( concat
            [ (ent k (Just n) c.info, c.info.exported)
                : [(field (Just n) f.info, f.info.exported) | f <- recordFields c.args]
            | c <- cs
            ]
        )
    famName f = (nameRef TypeNs f.family).occ
    famInst f = (famName f, conChildren ConKind (famName f) (famInstConstructors f.form))
    allCons =
      [ (nameOf t.info, c)
      | t <- concatMap types m.declarations,
        c <- formConstructors t.form
      ]
        <> [(famName f, c) | f <- famInstDecls, c <- famInstConstructors f.form]
    types = \case
      TypeD t -> [t]
      ClassD c -> c.associated
      _ -> []
    famInstDecls = concatMap famInsts m.declarations
    famInsts = \case
      FamInstD f -> [f]
      InstanceD i | ExplicitInstance b <- i.form -> b.associated
      _ -> []

-- | Fields of a record constructor.
recordFields :: ConArgs -> [FieldDecl]
recordFields = \case
  RecordArgs fs -> fs
  GadtRecordArgs fs -> fs
  _ -> []

-- * Export sets

-- | The scope an export list is read with.
data ExportScope
  = -- | The list names only local things ('needsScope' says no).
    NoScopeNeeded
  | -- | A cycle was cut here: what the list takes from imports is unknown.
    CycleCutHere
  | Scoped Scope

-- | The export list needs the module's scope: it names something that
-- isn't local, or another module.
needsScope :: LocalTable -> ParsedModule -> Bool
needsScope t m = case (t.name, m.exports) of
  (Just self, Just es) -> any (export self) es
  _ -> False
  where
    export self = \case
      ExportModule e -> e.name.value /= self
      ExportItem i ->
        not (isLocal t i.name)
          || any (not . localChild t i.name) (subNames i)
      _ -> False

-- | An item that names something of this module.
isLocal :: LocalTable -> ItemName -> Bool
isLocal t n = ownQualifier t n && not (null (namesLookup (itemSpace n) n.occ t.all))

-- | No qualifier, or this module's name.
ownQualifier :: LocalTable -> ItemName -> Bool
ownQualifier t n = maybe True (\q -> Just q == t.name) n.qualifier

-- | A sub of a local item that is one of its children here.
localChild :: LocalTable -> ItemName -> ItemName -> Bool
localChild t parent sub = Map.member (parent.occ, sub.occ) t.childByName

-- | A module's export set. See Note [Export sets].
exportSet :: LocalTable -> ParsedModule -> ExportScope -> ExportSet
exportSet t m scope = case (t.name, m.exports) of
  (Just self, Just es) -> foldl' (export self) local es
  (Just _, Nothing)
    | t.topSplices -> withOpen OpenSplices t.self local
  _ -> local
  where
    local = ExportSet {names = t.exported, openHome = Map.empty, openExternal = Set.empty}
    export self s = \case
      ExportModule e
        | e.name.value /= self -> withScope s (\sc -> reexport sc e.name.value s)
      ExportItem i
        | isLocal t i.name -> foldl' (bundleLocal i) s (subNames i)
        | otherwise -> withScope s (\sc -> item sc i s)
      _ -> s
    withScope s k = case scope of
      Scoped sc -> k sc
      CycleCutHere -> withOpen OpenCycle t.self s
      NoScopeNeeded -> s
    -- @T(.., P)@ for a local T: P goes under T, from here or an import.
    bundleLocal i s sub
      | localChild t i.name sub = s
      | otherwise = case filter patSyn (namesLookup ConSpace sub.occ t.all) of
          es@(_ : _) -> setNames (addKids i.name.occ es s.names) s
          [] -> withScope s $ \sc -> case importedPatSyns sc sub of
            [] -> s
            es -> setNames (addKids i.name.occ es s.names) s
    item sc i s = case lookupScope sc i.name.qualifier i.name.occ (itemSpace i.name) of
      Found hs ->
        setNames
          ( foldl'
              unionNames
              s.names
              (map (hitNames sc i) (NonEmpty.toList hs))
          )
          s
      Missing ms
        | t.topSplices ->
            setNames (unionNames s.names (listedOnly i (SpliceMade t.self))) s
        | Just h <- ms.open -> addOpen h s
        | Just c <- ms.external ->
            setNames
              ( unionNames
                  s.names
                  (listedOnly i (ExternalName c.first))
              )
              s
        | Just h <- ms.spliceOpen -> addOpen h s
        | otherwise -> s
    -- A found name; a type with its children in scope, and the pattern
    -- synonyms its subs bundle.
    hitNames sc i h = case h.avail of
      Nothing -> valuesWith (const h.bundledWith) [h.entity]
      Just _ ->
        let kids = subKids i.subs (childrenInScope sc h.entity)
            known = Set.map (.occ) kids
            bundled =
              [ e
              | sub <- subNames i,
                not (Set.member sub.occ known),
                e <- bundleable sc sub
              ]
         in availWith h.entity (Set.union kids (Set.fromList bundled))
    -- A pattern synonym of that name: one here wins over those in scope.
    bundleable sc sub = case filter patSyn (namesLookup ConSpace sub.occ t.all) of
      [] -> importedPatSyns sc sub
      here -> here
    importedPatSyns sc sub = case lookupScope sc sub.qualifier sub.occ ConSpace of
      Found hs -> filter patSyn (map (.entity) (NonEmpty.toList hs))
      Missing _ -> []
    patSyn e = e.kind == PatSynKind
    -- Under our own type of that name.
    addKids parent es ns =
      unionNames ns $
        mkNames
          es
          [ Avail a.entity (Set.fromList es)
          | a <- Map.findWithDefault [] parent ns.types,
            HomeDecl home _ <- [a.entity.origin],
            home == t.self
          ]
    -- A name we know only from the list.
    listedOnly i o =
      let e =
            Entity
              { occ = i.name.occ,
                kind = guessKind i.name,
                origin = o,
                parent = Nothing
              }
       in if inSpace TypeSpace e.kind
            then mkNames [] [Avail e Set.empty]
            else mkNames [e] []
    addOpen h s =
      ExportSet
        { names = s.names,
          openHome = Map.unionWith Set.union s.openHome h.parts,
          openExternal = s.openExternal
        }
    reexport sc n s = foldl' (moduleExport sc) s (importsWithAlias sc n)
    moduleExport sc s si =
      let names =
            unionNames
              s.names
              (if si.qualifiedOnly then unqualifiedPart sc si else si.visible)
       in case si.target of
            TargetHome _ set ->
              ExportSet
                { names = names,
                  openHome = Map.unionWith Set.union s.openHome set.openHome,
                  openExternal = Set.union s.openExternal set.openExternal
                }
            TargetExternal e
              | listKind si == Just Only -> setNames names s
              | otherwise ->
                  ExportSet
                    { names = names,
                      openHome = s.openHome,
                      openExternal = Set.insert e s.openExternal
                    }
