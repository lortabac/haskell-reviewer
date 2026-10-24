{-# LANGUAGE CPP, MagicHash #-}
module CppPlain (x#) where

-- x# is not a directive, nor is this: #if
x# :: Int
x# = 1
