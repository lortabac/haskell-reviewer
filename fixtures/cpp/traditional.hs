-- How gcc -traditional reads lines, comments and quotes.
#define FOO 42
#define S(x) "x" 'x' x
#define P(a,b) a/**/b
long = foo\
bar FOO
gap = "a\
\b"
c1 = 1 /* gone */ + FOO
c2 = 2 /* over
lines */ + FOO
-- a /* b */ c FOO
{- FOO -}
x' = FOO
-- it's FOO
z = 'a' FOO
s = "FOO" FOO
n = 0x1FOO 1e10 FOO_ _FOO
q = S(hi)
p = P(foo,bar)
#if 1 /* a comment
#endif */
kept = True
#endif
a /*
#if 0
*/ b
