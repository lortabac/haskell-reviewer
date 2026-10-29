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
moduleNames :: [Decl] -> [NameEntry]
moduleNames decls =
  [ NameEntry {name = n, decl = d.info.id, exported = d.info.exported}
  | d <- concatMap namedDecls decls,
    Just n <- [declName d.info.id]
  ]

-- | Force the whole list, so it holds no thunk.
forceNames :: [NameEntry] -> ()
forceNames = foldr (\e r -> e `seq` forceDeclId e.decl `seq` r) ()
