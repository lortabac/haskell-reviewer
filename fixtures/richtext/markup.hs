{- |
Module      : Markup
Description : Every kind of markup.
Copyright   : (c) Someone

The fields above stay text for now.

= A heading

Text with /emphasis/, __bold__, @code with /markup/@, &#65;&#x42; and a\/b.
Math: \(x^2\) and \[\sum_i x_i\]. An #anchor# shows nothing.

@
code block
  with 'f' in it
@

> bird track
> two lines

== A smaller heading

* an item

    * a nested item

* another item

1. first
2. second

(10) tenth

[term]: definition
[other term] another one

>>> 1 + 1
2

>>> putStr "a\n\nb"
a
<BLANKLINE>
b

prop> \x -> x == (x :: Int)

+------+-----+
| head | er  |
+======+=====+
| one cell   |
+------+-----+
| a    | b   |
+------+-----+

@since 1.2.3
-}
module Markup (f, T (..)) where

-- | A function.
--
-- @since 0.1
f :: Int -> Int
f = id

-- | A type.
data T
  = -- | A constructor.
    A
      Int
      -- ^ Its /first/ argument.
  | B {field :: Int -- ^ A field.
      }
