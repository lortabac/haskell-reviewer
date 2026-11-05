{-# LANGUAGE NoImplicitPrelude, PackageImports #-}
module Prelude (module P, myId) where

import "base" Prelude as P

myId :: a -> a
myId x = x
