{-# LANGUAGE StrictData #-}

-- | Which declarations the export list exports (SPEC §4.2, D-10).
module HaskellReviewer.Core.Exported
  ( markExported,
  )
where

import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    FieldDecl (..),
    MethodDecl (..),
    NewtypeBody (..),
    PatSynDecl (..),
    TypeDecl (..),
    TypeForm (..),
    ValueDecl (..),
  )
import HaskellReviewer.Core.ModuleModel
  ( Export (..),
    Item (..),
    ItemName (..),
    ModuleExport (..),
    NameKeyword (..),
    SubList (..),
    Subs (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model (Located (..))
import HaskellReviewer.Core.Names (DeclId (..))
import HaskellReviewer.Core.Project (ModuleName)

{- Note [Exported]

- No @module@ line: it's @module Main (main) where@ (Haskell Report).
- A header with no export list: everything.
- @module M@ for this module: everything.
- @x@: values, class methods and fields named x.
- @T@ (or @type T@): the type or class only.
- @T(..)@: and its constructors, fields, methods, associated types.
- @T(A, b)@, @T(.., A)@: and the ones listed. A listed name can be a
  pattern synonym (bundled).
- @pattern P@: a pattern synonym or a constructor. @data x@: any
  value-level name.
- @M.x@ counts only when M is this module.

Instances are always exported. Fixities follow their target.
-}

-- | What the export list names.
data Wanted = Wanted
  { everything :: Bool,
    values :: Set Text,
    types :: Set Text,
    patterns :: Set Text,
    -- | @T(..)@.
    allOf :: Set Text,
    -- | @T(A, b)@.
    someOf :: Map Text (Set Text),
    -- | Every name in some @T(A, b)@.
    listedSubs :: Set Text
  }

-- | Set the exported flags. See Note [Exported].
markExported :: Maybe ModuleName -> Maybe [Export] -> [Decl] -> [Decl]
markExported self exports = map (decl wanted)
  where
    wanted = case (self, exports) of
      (Nothing, _) -> nothing {values = Set.singleton "main"}
      (Just _, Nothing) -> nothing {everything = True}
      (Just m, Just es) -> foldr (export m) nothing es
    nothing =
      Wanted
        { everything = False,
          values = Set.empty,
          types = Set.empty,
          patterns = Set.empty,
          allOf = Set.empty,
          someOf = Map.empty,
          listedSubs = Set.empty
        }

export :: ModuleName -> Export -> Wanted -> Wanted
export self e w = case e of
  ExportModule m | m.name.value == self -> w {everything = True}
  ExportItem i | ours i.name -> item i
  _ -> w
  where
    ours n = maybe True (== self) n.qualifier
    item i =
      let n = i.name.occ
       in case i.subs of
            NoSubs -> case i.name.keyword of
              NoKeyword
                | typeLike n -> w {types = Set.insert n w.types}
                | otherwise -> w {values = Set.insert n w.values}
              TypeKeyword -> w {types = Set.insert n w.types}
              PatternKeyword -> w {patterns = Set.insert n w.patterns}
              DataKeyword ->
                w {values = Set.insert n w.values, patterns = Set.insert n w.patterns}
              DefaultKeyword -> w
            AllSubs -> w {types = Set.insert n w.types, allOf = Set.insert n w.allOf}
            SomeSubs l ->
              let listed = Set.fromList (map (.occ) l.names)
               in w
                    { types = Set.insert n w.types,
                      allOf = if null l.wildcardAt then w.allOf else Set.insert n w.allOf,
                      someOf = Map.insertWith Set.union n listed w.someOf,
                      listedSubs = Set.union listed w.listedSubs
                    }
    -- @T@, @:+:@: a type or class. @x@, @+@: a value.
    typeLike n = case Text.uncons n of
      Just (c, _) -> isUpper c || c == ':'
      Nothing -> False

-- | Mark a declaration and the ones inside it. The records are built
-- again by hand: their field names are shared, so a record update would
-- be ambiguous.
decl :: Wanted -> Decl -> Decl
decl w = \case
  TypeD t -> TypeD (typeDecl w (has w.types (nameOf t.info)) t)
  ClassD c ->
    let parent = nameOf c.info
        child i = has w.values (nameOf i) || sub w parent (nameOf i)
        method (m :: MethodDecl) =
          MethodDecl
            { info = mark w (child m.info) m.info,
              signature = m.signature,
              defaultSig = m.defaultSig,
              hasDefault = m.hasDefault,
              fixity = m.fixity
            }
        assoc (t :: TypeDecl) =
          typeDecl w (has w.types (nameOf t.info) || sub w parent (nameOf t.info)) t
     in ClassD
          ClassDecl
            { info = mark w (has w.types parent) c.info,
              context = c.context,
              fundeps = c.fundeps,
              kindSig = c.kindSig,
              fixity = c.fixity,
              methods = map method c.methods,
              associated = map assoc c.associated,
              minimal = c.minimal
            }
  ValueD v ->
    ValueD
      ValueDecl
        { info = mark w (has w.values (nameOf v.info)) v.info,
          signature = v.signature,
          definition = v.definition,
          fixity = v.fixity
        }
  PatSynD p ->
    let n = nameOf p.info
        patField f = has w.values f || bundled w f
     in PatSynD
          PatSynDecl
            { info = mark w (has w.patterns n || bundled w n) p.info,
              signature = p.signature,
              direction = p.direction,
              fields =
                [mark w (patField (nameOf f)) f | f <- p.fields],
              fixity = p.fixity
            }

-- | A type (exported or not), its constructors and fields.
typeDecl :: Wanted -> Bool -> TypeDecl -> TypeDecl
typeDecl w exported t =
  TypeDecl
    { info = mark w exported t.info,
      kindSig = t.kindSig,
      form = case t.form of
        DataType b -> DataType (dataBody b)
        TypeData b -> TypeData (dataBody b)
        Newtype b ->
          Newtype NewtypeBody {constructor = con b.constructor, derivings = b.derivings}
        other -> other,
      fixity = t.fixity
    }
  where
    dataBody b = DataBody {constructors = map con b.constructors, derivings = b.derivings}
    parent = nameOf t.info
    child i = has w.values (nameOf i) || sub w parent (nameOf i)
    con (c :: ConDecl) =
      let n = nameOf c.info
       in ConDecl
            { info = mark w (has w.patterns n || sub w parent n) c.info,
              args = case c.args of
                RecordArgs fs -> RecordArgs (map field fs)
                GadtRecordArgs fs -> GadtRecordArgs (map field fs)
                other -> other,
              fixity = c.fixity
            }
    field (f :: FieldDecl) =
      FieldDecl {info = mark w (child f.info) f.info, type_ = f.type_}

-- | Named.
has :: Set Text -> Text -> Bool
has s n = Set.member n s

-- | A child of the parent, by @T(..)@ or @T(.., n)@.
sub :: Wanted -> Text -> Text -> Bool
sub w parent n =
  Set.member parent w.allOf || maybe False (Set.member n) (Map.lookup parent w.someOf)

-- | Listed under some type, as a bundled pattern synonym can be.
bundled :: Wanted -> Text -> Bool
bundled w n = Set.member n w.listedSubs

-- | Set the flag, or set it anyway when everything is exported.
mark :: Wanted -> Bool -> DeclInfo -> DeclInfo
mark w b i = i {exported = w.everything || b}

nameOf :: DeclInfo -> Text
nameOf i = case i.id of
  Named _ n -> n
  Field _ n -> n
