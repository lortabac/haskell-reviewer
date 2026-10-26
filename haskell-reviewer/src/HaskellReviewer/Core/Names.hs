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
    InstanceKey (..),
    KeyHead,
    keyHead,
    keyHeadChunks,
    headText,
    renderDeclId,
    forceDeclId,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
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
  | -- | @inst:<Class>:<Head>@.
    Inst InstanceKey
  deriving (Eq, Ord, Show)

-- | What tells an instance apart (D-20): the class and the head, as
-- printed by "HaskellReviewer.Core.Parser.InstanceKey". For a type or
-- data instance, the family and its arguments.
data InstanceKey = InstanceKey
  { -- | The class, or the family.
    name :: Text,
    head :: KeyHead
  }
  deriving (Eq, Ord, Show)

-- | An instance head's text, in chunks that keys can share: the type's
-- part of a derived instance's head is the same text for many classes
-- (Note [Instance keys] in "HaskellReviewer.Core.Parser.InstanceKey").
-- Compared as the whole text, whatever the chunks.
newtype KeyHead = KeyHead [Text]

instance Eq KeyHead where
  a == b = lazyText a == lazyText b

instance Ord KeyHead where
  compare a b = compare (lazyText a) (lazyText b)

instance Show KeyHead where
  show = show . headText

-- | A head of one chunk.
keyHead :: Text -> KeyHead
keyHead t = KeyHead [t]

-- | A head of several chunks, joined with nothing between them.
keyHeadChunks :: [Text] -> KeyHead
keyHeadChunks = KeyHead

-- | The whole text. A new copy: don't keep it.
headText :: KeyHead -> Text
headText (KeyHead ts) = Text.concat ts

-- | Force the chunks too, so a key holds no thunk.
forceDeclId :: DeclId -> ()
forceDeclId = \case
  Inst k | KeyHead ts <- k.head -> foldr seq () ts
  _ -> ()

-- | No copy: lazy text shares the chunks.
lazyText :: KeyHead -> Lazy.Text
lazyText (KeyHead ts) = Lazy.fromChunks ts

-- | Like @type:Maybe@, @con::|@, @field:Config.port@ or
-- @inst:Show:Maybe a@.
renderDeclId :: DeclId -> Text
renderDeclId = \case
  Named ns n -> nsText ns <> ":" <> n
  Field parent n -> "field:" <> parent <> "." <> n
  Inst k -> "inst:" <> k.name <> ":" <> headText k.head
  where
    nsText = \case
      DType -> "type"
      DClass -> "class"
      DCon -> "con"
      DVal -> "val"
      DPat -> "pat"
