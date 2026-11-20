# Heading *one*

Setext heading
--------------

Text with `code`, **bold**, ~~gone~~, a [link](https://example.org)
and an ![image](pic.png) and <https://auto.example> and www.example.com.
Raw <span>html</span> inline.

[ref]: https://example.org/ref

- item
- [x] done
  - nested `code`

1. one
2) two

> quote with `code`
> > nested
> - list in quote

```haskell
module M where
-- | Doc.
f :: Int -> Int
f x = x + 1 -- comment
```

~~~ bash
echo hi
~~~

    indented code
    more

> ```hs
> g = "s"
> ```

| a | b |
|---|:-:|
| `x` | [y](#heading-one) |

<!-- a comment -->

<div>
block html
</div>

***

```haskell
unclosed = 1
