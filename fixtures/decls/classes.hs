{-# LANGUAGE TypeFamilies, DefaultSignatures, FunctionalDependencies #-}
module Classes where

-- | Containers.
class (Eq a, Show a) => Container f a | f -> a where
  -- | The element type.
  type Elem f
  type Elem f = a
  data Key f
  -- | Empty.
  empty, none :: f
  -- | Insert.
  insert :: a -> f -> f
  insert _ f = f
  {-# INLINE insert #-}
  default size :: (Foldable g, f ~ g a) => f -> Int
  size :: f -> Int
  size = length
  -- ^ The size.
  (<+>) :: f -> f -> f
  infixr 5 <+>
  {-# MINIMAL empty, (<+>) #-}

class a ~~ b

infixl 4 `insert`
