{-# LANGUAGE RoleAnnotations, PatternSynonyms, TypeFamilies, DefaultSignatures #-}
module HaddockPairing where

import Data.Kind (Type)

f, g, h, k, m, n :: Int
f = 1

-- | doc inline
{-# INLINE f #-}
g = 2

-- | doc rules
{-# RULES "r" f = f #-}
h = 3

-- | doc role
type role R nominal
k = 4

-- | doc warning
{-# WARNING m "x" #-}
data R a = R

-- | doc fixity
infixr 5 :+
data L = Int :+ L | Nil

m = 5
-- | doc ann
{-# ANN m "x" #-}
data S = S

-- | kind doc
type K :: Type
-- | data K doc
data K = K1

-- | instance doc
instance Show S where
  show _ = "S"
n = 6

x :: Int
-- ^ doc after sig
x = 1

-- | sig doc
w :: Int
-- | eq doc
w = 4

-- | pair
(a, b) = (1, 2)

class C c where
  -- | method doc
  meth :: c -> c
  -- | default eq doc
  meth = id
  -- | fixity doc
  infixl 4 `meth`
  -- | inline doc
  {-# INLINE meth #-}
  -- | assoc doc
  type Fam c
  -- | assoc default doc
  type Fam c = Int
  -- | minimal doc
  {-# MINIMAL other #-}
  other :: c
  -- ^ other doc

class D d where
  dm :: Int -> d
  -- | default sig doc
  default dm :: Num d => Int -> d
  -- ^ after default sig
  dm = fromIntegral
