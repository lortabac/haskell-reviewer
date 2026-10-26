{-# LANGUAGE DerivingVia, DeriveAnyClass, DerivingStrategies, DatatypeContexts #-}
module Data where

-- | A shape.
data Shape a
  = -- | A circle.
    Circle !Double
  | Rect {-# UNPACK #-} !Int ~Int
  | a :+ a
  -- ^ An infix one.
  deriving (Eq, Show)
  deriving stock (Ord)
  deriving anyclass (C)

-- | Config.
data Config = Config
  { -- | The port.
    port :: {-# NOUNPACK #-} !Int,
    host, user :: String -- ^ Host and user.
  }
  | Other { port :: Int }
  deriving newtype Show
  deriving (Semigroup) via (Max Int)

data Ord a => Ctx a = Ctx a

data Exists = forall a. Show a => Exists a

data Empty

infixr 5 :+
