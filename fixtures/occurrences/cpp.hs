{-# LANGUAGE CPP #-}
module Cpp where

#define NAME(x) x

NAME(macroDef) = 1

used = macroDef + NAME(macroDef)

local = let NAME(v) = 1 in v

#if 0
excluded = 2
#endif
