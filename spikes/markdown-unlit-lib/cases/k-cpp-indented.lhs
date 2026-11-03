# CPP in an indented fence (extra case)

1. Header:

   ```haskell
   {-# LANGUAGE CPP #-}
   module K where
   ```

2. Code:

   ```haskell
   #if 0
   gone :: Int
   gone = 0
   #endif
   foo :: Int
   foo = 1
   ```
