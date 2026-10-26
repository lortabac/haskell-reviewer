{-# LANGUAGE PatternSynonyms, TemplateHaskell, NamedDefaults, RoleAnnotations #-}
module Pragmas where

{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"id" id = \x -> x
  #-}

pattern P :: Int
pattern P = 1
{-# COMPLETE P #-}
{-# COMPLETE P :: Int #-}

data T = A | B { fb :: Int }

class Cl a where
  cm :: a

f :: Int
f = 1

{-# DEPRECATED f "use g" #-}
{-# WARNING in "x-partial" A, cm ["one", "two"] #-}
{-# DEPRECATED type T "type only" #-}
{-# DEPRECATED data B "data only" #-}
{-# DEPRECATED fb, P "field and pattern" #-}
{-# DEPRECATED nope "no target: dropped" #-}

{-# ANN f "value" #-}
{-# ANN type T "type" #-}
{-# ANN module "module" #-}

default (Integer, Double)
default Cl (Int)

$(pure [])
makeLenses ''T

foreign import ccall unsafe "math.h sin" c_sin :: Double -> Double
foreign export ccall f :: Int
-- No local value: dropped.
foreign export ccall nope :: Int

type role R nominal
data R a = R
