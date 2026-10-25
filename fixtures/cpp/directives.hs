#bogus FOO
#define FOO 1
#bogus FOO
#!FOO
#pragma once
#ident "x"
#
#line 40 "f"
# 50 "g"
#include "a.h"
#include <b.h>
#error boom
#warning careful
#assert machine(x86)
  #define INDENTED 1
  # if FOO
#if 0
#error not seen
#bogus not seen
#define NOT 1
#if 1
nested
#endif
#elif NOT
#else
else = FOO
#else
#endif
#endif
#elif 1
#if 1
