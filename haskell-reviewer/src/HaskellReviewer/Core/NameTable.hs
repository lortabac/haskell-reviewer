{-# LANGUAGE StrictData #-}

-- | The names a module declares, for name search (SPEC §4.7, §11.4.10). The index
-- keeps one list per module and adds to it as modules get parsed.
module HaskellReviewer.Core.NameTable
  ( -- * Types
    NameEntry (..),

    -- * Building
    moduleNames,
    forceNames,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import HaskellReviewer.Core.Decls (Decl, DeclInfo (..), NamedDecl (..), namedDecls)
import HaskellReviewer.Core.Names (DeclId, declName, forceDeclId)

-- | One name a module declares.
data NameEntry = NameEntry
  { -- | As written, operators without parens.
    name :: Text,
    decl :: DeclId,
    exported :: Bool
  }
  deriving (Eq, Show)

-- | Every name the declarations bring, nested ones too, in source
-- order: types, classes, constructors, fields, methods, associated
-- types, values, pattern synonyms. Not instances: they have no name.
-- Each id once, the first: a field in several constructors (or a
-- declaration CPP makes twice) would show twice in search.
moduleNames :: [Decl] -> [NameEntry]
moduleNames decls =
  firstOfEach
    Set.empty
    [ NameEntry {name = n, decl = d.info.id, exported = d.info.exported}
    | d <- concatMap namedDecls decls,
      Just n <- [declName d.info.id]
    ]
  where
    firstOfEach _ [] = []
    firstOfEach seen (e : es)
      | Set.member e.decl seen = firstOfEach seen es
      | otherwise = e : firstOfEach (Set.insert e.decl seen) es

-- | Force the whole list, so it holds no thunk.
forceNames :: [NameEntry] -> ()
forceNames = foldr (\e r -> e `seq` forceDeclId e.decl `seq` r) ()
