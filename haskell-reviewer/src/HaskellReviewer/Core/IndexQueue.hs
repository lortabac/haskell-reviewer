{-# LANGUAGE StrictData #-}

-- | What the parse workers do next: the modules to index, and the queue
-- they come out of (SPEC §11.5). Pure; the STM side is in
-- "HaskellReviewer.Session.Index".
module HaskellReviewer.Core.IndexQueue
  ( -- * Jobs
    Job (..),
    indexJobs,

    -- * Queue
    Priority (..),
    Ticket,
    Queue,
    fromList,
    push,
    pop,
    entries,
    remove,
    putBack,
    isEmpty,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ComponentSettings,
    FoundModule (..),
    HaskellFile,
    LooseFile (..),
    ModuleEntry (..),
    ModuleId (..),
    ModuleIndex (..),
    ModuleSource (..),
    OtherFile (..),
    PackageModules (..),
    looseModule,
  )

-- * Jobs

-- | A module to parse.
data Job = Job
  { id :: ModuleId,
    file :: HaskellFile,
    settings :: ComponentSettings
  }

-- | The modules that need a parse: Haskell sources of components, then
-- loose modules. Generated, preprocessed and no-source modules need
-- none: the index answers for them.
--
-- In module tree order: packages in project order, components in
-- .cabal order, modules by name, then loose modules by path. That's
-- the background order.
indexJobs :: ModuleIndex -> [Job]
indexJobs idx = owned <> loose
  where
    owned =
      [ Job {id = ComponentModule p.name c.name m.name, file = f, settings = c.settings}
      | p <- idx.packages,
        c <- p.components,
        m <- c.modules,
        HaskellSource f <- [m.source]
      ]
    loose =
      [ Job {id = LooseModule l.path, file = f, settings = found.settings}
      | OtherModule l <- idx.other,
        let found = looseModule l.path,
        HaskellSource f <- [found.source]
      ]

-- * Queue

-- | Highest first.
data Priority
  = -- | A request waits for it.
    Requested
  | -- | Changed in diff mode (not used yet).
    Changed
  | Background
  deriving (Eq, Ord, Show)

-- | Where a module is in the queue: by priority, then first in first
-- out.
data Ticket = Ticket
  { priority :: Priority,
    seqNo :: Word64
  }
  deriving (Eq, Ord, Show)

-- | Modules by priority, first in first out within one. A module can be
-- in it more than once (see "HaskellReviewer.Session.Index").
--
-- Fully strict (a strict map, strict fields): threads share it in STM,
-- so it must hold no thunk (Q-RTS-BLACKHOLE).
data Queue = Queue
  { items :: Map Ticket ModuleId,
    next :: Word64
  }

-- | All at 'Background', in this order.
fromList :: [ModuleId] -> Queue
fromList ms =
  Queue
    { items = Map.fromList [(Ticket Background n, m) | (n, m) <- zip [0 ..] ms],
      next = fromIntegral (length ms)
    }

-- | Add one at the end of its priority.
push :: Priority -> ModuleId -> Queue -> Queue
push p m q =
  Queue {items = Map.insert (Ticket p q.next) m q.items, next = q.next + 1}

-- | The first of the highest priority, and the rest.
pop :: Queue -> Maybe (ModuleId, Queue)
pop q = do
  ((_, m), rest) <- Map.minViewWithKey q.items
  Just (m, q {items = rest})

-- | Every entry, first one first.
entries :: Queue -> [(Ticket, ModuleId)]
entries q = Map.toAscList q.items

-- | Drop one entry.
remove :: Ticket -> Queue -> Queue
remove t q = q {items = Map.delete t q.items}

-- | Put an entry back where it was.
putBack :: Ticket -> ModuleId -> Queue -> Queue
putBack t m q = q {items = Map.insert t m q.items}

-- | Nothing in it.
isEmpty :: Queue -> Bool
isEmpty q = Map.null q.items
