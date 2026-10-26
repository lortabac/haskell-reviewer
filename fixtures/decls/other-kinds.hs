{-# LANGUAGE TemplateHaskell, RoleAnnotations, StandaloneDeriving, TypeFamilies #-}
module OtherKinds where

data T a = T a

-- | Instance doc: not on the next one.
instance Show (T a) where
  show _ = "T"

-- | Deriving.
deriving instance Eq a => Eq (T a)

type role T representational

type family F a
-- | Type instance doc.
type instance F Int = Bool

data family D a
-- | Data instance doc.
data instance D Int = DInt

instance Semigroup (T a) where
  _ <> b = b
-- ^ After an instance.

foreign import ccall "sin" c_sin :: Double -> Double

-- | Foreign export doc: dropped, not on the next one.
foreign export ccall exported :: Int
afterExport :: Int
afterExport = 2

exported :: Int
exported = 3

-- | Default doc: dropped, not on the next one.
default (Int)
afterDefault :: Int
afterDefault = 4

{-# RULES "id" forall x. id x = x #-}
{-# COMPLETE T #-}
{-# DEPRECATED kept "old" #-}
{-# DEPRECATED afterExport "one"; afterDefault "two" #-}
{-# ANN kept "note" #-}

-- | Kept.
kept :: Int
kept = 1

-- Last: a splice splits GHC's declaration groups.
$(pure [])
