module WhereGuards where

classify n
  | n < lo = "low"
  | Just m <- big n, m > hi = "high " <> show m
  | let k = n * 2, k > lo = show k
  | otherwise = "mid"
  where
    lo = 10
    hi = lo * 10
    big v = if v > hi then Just v else Nothing

go acc [] = acc
go acc (y : ys) = go (acc + y) ys
