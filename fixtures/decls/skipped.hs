{-# LANGUAGE TemplateHaskell, RoleAnnotations, StandaloneDeriving, TypeFamilies #-}
module Skipped where

data T a = T a

-- | Instance doc: not on the next one.
instance Show (T a) where
  show _ = "T"

-- | Deriving.
deriving instance Eq a => Eq (T a)

type role T representational

type family F a
type instance F Int = Bool

data family D a
data instance D Int = DInt

foreign import ccall "sin" c_sin :: Double -> Double

{-# RULES "id" forall x. id x = x #-}
{-# COMPLETE T #-}
{-# DEPRECATED kept "old" #-}
{-# ANN kept "note" #-}
default (Int)

$(pure [])

-- | Kept.
kept :: Int
kept = 1
