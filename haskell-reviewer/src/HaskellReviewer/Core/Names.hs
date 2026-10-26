{-# LANGUAGE StrictData #-}

-- | Name keys: what a link points at. Same parts as the HIE index key
-- (D-87), with the unit already mapped to a module. Also declaration
-- ids, the @<decl>@ part of a URL (SPEC §4.1).
module HaskellReviewer.Core.Names
  ( -- * Types
    Namespace (..),
    NameKey (..),
    ModuleRef (..),

    -- * Declaration ids
    DeclNs (..),
    DeclId (..),
    renderDeclId,
  )
where

import Data.Text (Text)
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Project (ModuleName, PackageName)

-- | GHC's namespaces, as in the index key. Not the SPEC §4.1 namespace
-- (that's the kind of declaration).
data Namespace = NsTypeOrClass | NsCon | NsField | NsVal
  deriving (Eq, Ord, Show)

-- | A top-level name.
data NameKey = NameKey
  { home :: ModuleRef,
    namespace :: Namespace,
    occ :: Text,
    -- | GHC's field parent: the type's first constructor, or the pattern
    -- synonym. 'Nothing' for anything but fields (D-88).
    fieldParent :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- | The module a name lives in: one of ours, or one of a dependency.
data ModuleRef
  = Home ModuleId
  | External PackageName ModuleName
  deriving (Eq, Ord, Show)

-- * Declaration ids

-- | The SPEC §4.1 namespace of a declaration: its kind, not GHC's
-- namespace. Fields and instances have their own 'DeclId' shapes.
data DeclNs = DType | DClass | DCon | DVal | DPat
  deriving (Eq, Ord, Show)

-- | Names a declaration within its module (SPEC §4.1, D-88). Operators
-- have no parens. Percent-encoding is for the URL code.
data DeclId
  = Named DeclNs Text
  | -- | @field:<Parent>.<name>@: the type (or pattern synonym) first.
    Field Text Text
  deriving (Eq, Ord, Show)

-- | Like @type:Maybe@, @con::|@ or @field:Config.port@.
renderDeclId :: DeclId -> Text
renderDeclId = \case
  Named ns n -> nsText ns <> ":" <> n
  Field parent n -> "field:" <> parent <> "." <> n
  where
    nsText = \case
      DType -> "type"
      DClass -> "class"
      DCon -> "con"
      DVal -> "val"
      DPat -> "pat"
