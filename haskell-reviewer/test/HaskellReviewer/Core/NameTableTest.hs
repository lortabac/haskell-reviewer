-- | The names a module declares, for search.
module HaskellReviewer.Core.NameTableTest (tests) where

import Data.Text (Text)
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.NameTable (NameEntry (..), moduleNames)
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Name table"
    [ testCase "nested names, in source order; no instances, no splices" $
        names
          "{-# LANGUAGE TypeFamilies, PatternSynonyms, TemplateHaskell #-}\n\
          \module M where\n\
          \data T = A | B {field :: Int}\n\
          \class C a where { type AT a; m :: a }\n\
          \instance C Int where { type AT Int = Int; m = 1 }\n\
          \data family D a\n\
          \data instance D Int = DI {di :: Int}\n\
          \pattern P {pf} = Just pf\n\
          \(+++) = (+)\n\
          \type data K = K1\n\
          \$(pure [])\n"
          [ ("T", "type:T"),
            ("A", "con:A"),
            ("B", "con:B"),
            ("field", "field:T.field"),
            ("C", "class:C"),
            ("AT", "type:AT"),
            ("m", "val:m"),
            ("D", "type:D"),
            ("DI", "con:DI"),
            ("di", "field:DI.di"),
            ("P", "pat:P"),
            ("pf", "field:P.pf"),
            ("+++", "val:+++"),
            ("K", "type:K"),
            ("K1", "con:K1")
          ],
      testCase "the same id once: a field in two constructors" $
        names
          "module M where\ndata F = A {ff :: Int} | B {ff, fh :: Int}\n"
          [ ("F", "type:F"),
            ("A", "con:A"),
            ("ff", "field:F.ff"),
            ("B", "con:B"),
            ("fh", "field:F.fh")
          ],
      testCase "the exported flag" $ do
        m <- parsed "module M (T (A), f) where\ndata T = A | B\nf = 1\ng = 2\n"
        [(e.name, e.exported) | e <- moduleNames m.declarations]
          @?= [("T", True), ("A", True), ("B", False), ("f", True), ("g", False)]
    ]

-- | Name and rendered id of each entry.
names :: Text -> [(Text, Text)] -> IO ()
names src expected = do
  m <- parsed src
  [(e.name, renderDeclId e.decl) | e <- moduleNames m.declarations] @?= expected

parsed :: Text -> IO ParsedModule
parsed src = case parseSource defaultParseSettings src of
  Parsed m -> pure m
  TextOnly t -> assertFailure ("not parsed: " <> show t)
