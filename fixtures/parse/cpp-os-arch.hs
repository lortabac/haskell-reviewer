{-# LANGUAGE CPP #-}
module CppOsArch
  ( common
#if defined(linux_HOST_OS)
  , linux
#elif defined(darwin_HOST_OS)
  , darwin
#else
  , otherOs
#endif
#if x86_64_HOST_ARCH
  , x86
#elif aarch64_HOST_ARCH
  , arm
#endif
#ifdef __SSE2__
  , sse2
#endif
  ) where
