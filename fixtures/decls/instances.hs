{-# LANGUAGE TypeFamilies, InstanceSigs, DataKinds, PolyKinds #-}
module Instances where

import qualified Data.Map as M
import Control.Monad.State (MonadState (..), StateT)
import Data.Kind (Type)
import Data.Proxy (Proxy)

class C a b where
  type AT a b
  data AD a
  cm :: a -> b -> Int

data Box a = Box a

-- | With a context.
instance Show a => Show (Box a) where
  showsPrec d (Box x) = showParen (d > 10) (showString "Box " . showsPrec 11 x)
  show = \_ -> "box"

-- | Other variable names, same key shape.
instance Eq b => Eq (Box b) where
  Box x == Box y = x == y

-- | Multi-parameter, associated instances.
instance C [x] (Maybe y) where
  -- | GHC drops this.
  type AT [x] (Maybe y) = x
  data AD [x] = ADList x -- ^ and this
  cm _ _ = 0

-- | OVERLAPPING, an instance signature, method pragmas.
instance {-# OVERLAPPING #-} C Int Int where
  cm :: Int -> Int -> Int
  {-# INLINE cm #-}
  cm 0 b = b
  cm a b = a + b
  {-# SPECIALISE instance C Int Int #-}

instance forall s m. Monad m => MonadState s (StateT s m) where
  get = undefined
  put _ = undefined

data a :+: b = L a | R b

instance (Semigroup x, Semigroup y) => Semigroup (x :+: y) where
  a <> _ = a

-- Prefix: printed infix.
instance Monoid ((:+:) p q) where
  mempty = undefined

-- Lists, tuples, promoted, qualifiers, functions.
instance C '[] '(1, "s")
instance C (Proxy '[Int, Bool]) ([a], (a, b), ())
instance C (M.Map k v) (k -> v)
instance C (Maybe (Maybe c)) (Either (Maybe c) d)

-- Kinds: applications dropped, signatures kept.
instance Show (Proxy @Type a)
instance Show (Proxy (a :: k))
