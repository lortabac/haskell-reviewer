-- cpp-flags: -DDEBUG -DLEVEL=3 -UFOO -D__GLASGOW_HASKELL__=810 -DF(x)=[x] -Ulinux_HOST_OS
#if DEBUG && LEVEL > 2
debug = F(LEVEL)
#endif
#if __GLASGOW_HASKELL__ < 900
old = True
#endif
#ifdef linux_HOST_OS
linux = True
#else
other = True
#endif
