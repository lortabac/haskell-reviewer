{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Shapes seen in real packages (optparse-applicative, pandoc, text).
module RealWorld
  ( Parser,
#if !MIN_VERSION_base(4,8,0)
    (<$>),
#endif
    run,
  )
where

#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
#define HAS_DERIVING_STRATEGIES 1
#endif

#ifdef HAS_DERIVING_STRATEGIES
newtype Parser a = Parser a deriving stock (Show)
#else
newtype Parser a = Parser a deriving (Show)
#endif

#if defined(mingw32_HOST_OS)
# define WINDOWS
#endif

#if __GLASGOW_HASKELL__ >= 704
#define INLINE_ALWAYS [1]
#else
#define INLINE_ALWAYS
#endif

{-# INLINE INLINE_ALWAYS run #-}
run :: Parser a -> a
run (Parser a) = a

-- | Haddock with a #-sign: not a directive.
--   #notADirective
x # y = x
