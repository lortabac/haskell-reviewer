# Example cookbook page

This mirrors a servant doc page (D-218): markdown prose, with a
`# Overview`-style heading that would break GHC's unlit if read as
plain `.lhs`.

## Setup

```haskell
module MarkdownLoose (greet) where

greet :: String
greet = "hello"
```

Here is a shell session, unrelated to Haskell, whose prompt happens to
start with `>`. It must not be read as a Bird line:

```
> GET /users
200 OK
```

## More

```haskell
answer :: Int
answer = 42
```
