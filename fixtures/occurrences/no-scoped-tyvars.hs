{-# LANGUAGE NoScopedTypeVariables #-}
module NoScopedTyVars where

-- Without ScopedTypeVariables, a forall covers its signature only.
f :: forall a. a -> a
f x = const x (Nothing :: Maybe a)

-- Class and instance methods don't see the head.
class C a where
  m :: a -> a
  m x = const x (Nothing :: Maybe a)

instance C [e] where
  m xs = const xs (Nothing :: Maybe e)
