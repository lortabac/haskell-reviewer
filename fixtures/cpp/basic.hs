{-# LANGUAGE CPP #-}
-- A typical module: version checks, platform, a fallback definition.
module Basic
  ( foo,
#if MIN_VERSION_base(4,18,0)
    bar,
#endif
  )
where

#if !defined(MIN_VERSION_containers)
#define MIN_VERSION_containers(a,b,c) 0
#endif

#if __GLASGOW_HASKELL__ >= 910 && MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
import Data.List (foldl')
#else
import Data.Foldable (foldl')
#endif

#if defined(mingw32_HOST_OS)
sep = '\\'
#elif defined(linux_HOST_OS) || defined(darwin_HOST_OS)
sep = '/'
#else
#error unknown platform
#endif

#ifdef x86_64_HOST_ARCH
wordSize = 64
#endif

#if MIN_VERSION_containers(0,7,0)
new = True
#endif

foo = 1
bar = 2
