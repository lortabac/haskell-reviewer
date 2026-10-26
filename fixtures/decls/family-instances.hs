{-# LANGUAGE TypeFamilies, GADTs, TypeApplications, DerivingStrategies #-}
module FamilyInstances (D (..), F, K (..)) where

import qualified Ext
import Data.Kind (Type)

type family F a
type instance F [a] = a
-- | Same key shape as above.
type instance F (Maybe b) = b

data family D a
-- | A data instance. Fields have the first constructor as parent.
data instance D Int = DInt { dx :: Int } | DInt2 { dx :: Int }
  deriving (Show)
newtype instance D Bool = DBool Bool deriving newtype Eq
data instance D [a] where
  DList :: { dl :: a } -> D [a]

type family G a b
type instance G a (f a) = Int

-- An external family.
type instance Ext.Fam Int = Bool

class K a where
  type KT a
  data KD a

instance K Int where
  type KT Int = Bool
  data KD Int = KDInt { kd :: Int } deriving Eq

-- Kind applications are dropped.
type family H (a :: k)
type instance H @Type Int = ()
