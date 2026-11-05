module Q (q) where

import "base" Data.List (sort)
import "this" Q.Missing (missing)
import P.Api (api)
import P.Internal (internal)

q :: [Int]
q = sort [api, internal, missing]
