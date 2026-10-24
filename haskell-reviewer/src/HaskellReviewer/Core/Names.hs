{-# LANGUAGE StrictData #-}

-- | Name keys: what a link points at. Same parts as the HIE index key
-- (D-87), with the unit already mapped to a module.
module HaskellReviewer.Core.Names
  ( -- * Types
    Namespace (..),
    NameKey (..),
    ModuleRef (..),
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
