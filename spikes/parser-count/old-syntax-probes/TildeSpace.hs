{-# LANGUAGE TypeFamilies #-}
module TildeSpace where
f :: (a ~ b) => a -> b
f = id
g :: (a ~b) => a -> b
g = id
