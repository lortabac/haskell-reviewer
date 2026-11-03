# top block

The code first:

```haskell
foo :: Map.Map Int Int
foo = Map.empty
```

The imports last, moved to the top:

```haskell top
module C where

import qualified Data.Map as Map
```
