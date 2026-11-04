{-# LANGUAGE ViewPatterns, BangPatterns, NPlusKPatterns #-}
module ViewPatterns where

f k (lookup k -> Just v) = v
f _ _ = 0

g all@(x : ~(y : _)) !z = (all, x, y, z)

fact 0 = 1
fact (n + 1) = (n + 1) * fact n
