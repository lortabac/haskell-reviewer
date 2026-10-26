{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Newtype where

-- | Wraps.
newtype Age = Age Int
  deriving newtype (Eq, Num)

newtype Rec a = Rec {unRec :: a} -- ^ Record newtype.
