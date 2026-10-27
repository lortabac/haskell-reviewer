{-# LANGUAGE StrictData #-}

-- | Identifiers in doc comments (@'foo'@) to declarations of the same
-- module (SPEC §11.4.9). Other names stay plain names for now: they
-- need the imports, which come with resolution (SPEC §11.4.5).
--
-- Names are resolved when the page is encoded, not stored in the model.
module HaskellReviewer.Core.DocLinks
  ( LocalNames,
    localNames,
    resolveLocal,
  )
where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
import HaskellReviewer.Core.Names (DeclId (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText (NameRef (..), RefNamespace (..))

{- Note [Local doc links]

Two tables, by occ name, as GHC's two namespaces:

- types: types, classes, synonyms, families, associated types, and the
  constructors of a @type data@ (they live at the type level);
- values: values, methods, foreign imports, data constructors (also of
  data instances), pattern synonyms, record fields.

Instances have no name, so nothing links to them. If a name is declared
twice, the first one wins.

@v'x'@ looks only at values, @t'T'@ only at types, plain @'T'@ at types
first, as Haddock prefers the type when both exist. A qualified name
counts only with this module's name (@Main@ when there is no @module@
line), as for the orphan check.
-}

-- | The names declared in a module. See Note [Local doc links].
data LocalNames = LocalNames
  { self :: ModuleName,
    types :: Map Text DeclId,
    values :: Map Text DeclId
  }

-- | The tables for these declarations. The module name is the
-- header's, if there is one.
localNames :: Maybe ModuleName -> [Decl] -> LocalNames
localNames name decls =
  LocalNames
    { self = fromMaybe (ModuleName "Main") name,
      types = table [i | (Types, i) <- entries],
      values = table [i | (Values, i) <- entries]
    }
  where
    entries = concatMap decl decls
    -- Keep the first.
    table is = Map.fromListWith (\_ old -> old) [(k, i) | i <- is, Just k <- [key i]]
    key = \case
      Named _ n -> Just n
      Field _ n -> Just n
      Inst _ -> Nothing

-- | Which table a name goes in.
data Table = Types | Values

-- | The names one declaration brings, nested ones too, in source order.
decl :: Decl -> [(Table, DeclId)]
decl = \case
  TypeD t -> typeDecl t
  ClassD c ->
    (Types, c.info.id)
      : concatMap typeDecl c.associated
        <> [(Values, m.info.id) | m <- c.methods]
  ValueD v -> [(Values, v.info.id)]
  PatSynD p -> (Values, p.info.id) : [(Values, f.id) | f <- p.fields]
  InstanceD i -> case i.form of
    ExplicitInstance b -> concatMap famInst b.associated
    _ -> []
  FamInstD f -> famInst f
  SpliceD _ -> []
  where
    typeDecl t =
      (Types, t.info.id) : concatMap (con (conTable t.form)) (formConstructors t.form)
    conTable = \case
      TypeData _ -> Types
      _ -> Values
    famInst f = concatMap (con Values) (famInstConstructors f.form)
    con tbl c = (tbl, c.info.id) : [(Values, i) | i <- fields c.args]
    fields = \case
      RecordArgs fs -> map fieldId fs
      GadtRecordArgs fs -> map fieldId fs
      _ -> []
    fieldId (f :: FieldDecl) = f.info.id

-- | The local declaration a name in a comment means, if any.
resolveLocal :: LocalNames -> NameRef -> Maybe DeclId
resolveLocal names ref
  | maybe False (/= names.self) ref.qualifier = Nothing
  | otherwise = case ref.namespace of
      ValueNs -> value
      TypeNs -> type_
      AnyNs -> type_ <|> value
  where
    value = Map.lookup ref.occ names.values
    type_ = Map.lookup ref.occ names.types
