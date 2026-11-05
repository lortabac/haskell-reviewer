module Top (module Mid, Base (..), top) where

import Mid hiding (mid)

top :: Int
top = 3
