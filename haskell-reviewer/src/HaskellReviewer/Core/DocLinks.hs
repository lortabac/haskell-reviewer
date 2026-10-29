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
  ( Decl,
    DeclInfo (..),
    NameLevel (..),
    NamedDecl (..),
    namedDecls,
  )
import HaskellReviewer.Core.Names (DeclId, declName)
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
      types = table [n.info.id | n <- entries, n.level == TypeLevel],
      values = table [n.info.id | n <- entries, n.level == ValueLevel]
    }
  where
    entries = concatMap namedDecls decls
    -- Keep the first.
    table is = Map.fromListWith (\_ old -> old) [(k, i) | i <- is, Just k <- [declName i]]

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
