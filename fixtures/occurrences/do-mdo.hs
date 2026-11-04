{-# LANGUAGE RecursiveDo #-}
module DoMdo where

import Control.Monad.Fix (mfix)

main :: IO ()
main = do
  a <- pure 1
  let b = a + c
      c = 2
  a <- pure (a + b)
  print a

knot = mdo
  xs <- pure (1 : ys)
  ys <- pure (2 : xs)
  pure (take 3 xs)

recBlock = do
  rec p <- pure (1 : q)
      q <- pure (0 : p)
  pure p

nestedMdo = mdo { a <- f b; rec { b <- g a }; pure b }

nestedRec = do { rec { x <- f y; rec { y <- g x }; h y }; pure y }
