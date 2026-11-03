# CPP

```haskell
{-# LANGUAGE CPP #-}
module F where
```

Some prose.

```haskell
#if 0
gone :: Int
gone = 0
#endif
foo :: Int
foo = 1
```

```haskell
bar :: Int
bar = foo
```
