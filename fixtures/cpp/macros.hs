#define F(x) [x]
#define G(x,y) (x|y)
#define H F
#define T(x) x
#define AB a,b
#define X X+1
#define A B
#define B A
#define R(x) R(x)
one = F(1) F (2) F
two = G(a
  ,
  b) z
three = F
  (3)
four = H(4) T(T)(5)
five = F(F(F(6)))
six = G(AB) F(AB)
rec = X A B R(7)
wrong = F(1,2) G(1)
line = __LINE__
open = F(oops
#if 1
#endif
