-- | Names in doc comments to local declarations.
module HaskellReviewer.Core.DocLinksTest (tests) where

import Data.Text (Text)
import HaskellReviewer.Core.DocLinks (localNames, resolveLocal)
import HaskellReviewer.Core.ModuleModel
  ( Located (..),
    ModuleHeader (..),
    ModuleParse (..),
    ParsedModule (..),
  )
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText (NameRef (..), RefNamespace (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Doc links"
    [ testCase "types first, then values; v' and t' pick one" $
        resolves
          "module M where\ndata T = T {field :: Int}\nf = 1\n"
          [ (any_ "T", Just "type:T"),
            (value "T", Just "con:T"),
            (type_ "T", Just "type:T"),
            (any_ "field", Just "field:T.field"),
            (type_ "f", Nothing),
            (any_ "f", Just "val:f"),
            (any_ "missing", Nothing)
          ],
      testCase "nested names: methods, associated types, pattern synonym fields" $
        resolves
          "{-# LANGUAGE TypeFamilies, PatternSynonyms #-}\nmodule M where\n\
          \class C a where { type A a; m :: a }\npattern P {pf} = Just pf\n\
          \foreign import ccall \"sin\" c_sin :: Double -> Double\ntype S = Int\n\
          \type family F a\ndata family D a\n(+++) = (+)\n"
          [ (any_ "C", Just "class:C"),
            (any_ "A", Just "type:A"),
            (any_ "m", Just "val:m"),
            (any_ "P", Just "pat:P"),
            (any_ "pf", Just "field:P.pf"),
            (any_ "c_sin", Just "val:c_sin"),
            (any_ "S", Just "type:S"),
            (any_ "F", Just "type:F"),
            (any_ "D", Just "type:D"),
            (any_ "+++", Just "val:+++")
          ],
      testCase "data instance constructors and fields, top-level and associated" $
        resolves
          "{-# LANGUAGE TypeFamilies #-}\nmodule M where\ndata family D a\n\
          \data instance D Int = DI {di :: Int}\nclass C a where { data E a }\n\
          \instance C Int where { data E Int = EI }\n"
          [ (any_ "DI", Just "con:DI"),
            (any_ "di", Just "field:DI.di"),
            (any_ "EI", Just "con:EI")
          ],
      testCase "type data constructors are types" $
        resolves
          "{-# LANGUAGE TypeData #-}\nmodule M where\ntype data K = K1\n"
          [ (any_ "K1", Just "con:K1"),
            (value "K1", Nothing),
            (type_ "K1", Just "con:K1")
          ],
      testCase "instances have no name" $
        resolves "module M where\ninstance Show T\n" [(any_ "Show", Nothing)],
      testCase "the first of two wins" $
        resolves "module M where\ndata T = A\ndata T = B\n" [(any_ "T", Just "type:T")],
      testCase "qualified: only with this module's name" $ do
        resolves
          "module M.N where\nf = 1\n"
          [ (qualified "M.N" "f", Just "val:f"),
            (qualified "M" "f", Nothing),
            (qualified "Main" "f", Nothing)
          ]
        resolves "f = 1\n" [(qualified "Main" "f", Just "val:f")]
    ]
  where
    ref ns o = NameRef {namespace = ns, qualifier = Nothing, occ = o, written = o}
    any_ = ref AnyNs
    value = ref ValueNs
    type_ = ref TypeNs
    qualified q o = (any_ o) {qualifier = Just (ModuleName q)}

-- | Each name resolves to the declaration with this id, or to nothing.
resolves :: Text -> [(NameRef, Maybe Text)] -> Assertion
resolves src cases = case parseSource defaultParseSettings src of
  Parsed m -> do
    let names = localNames ((.value) <$> m.header.name) m.declarations
    [(r.occ, renderDeclId <$> resolveLocal names r) | (r, _) <- cases]
      @?= [(r.occ, expected) | (r, expected) <- cases]
  other -> assertFailure (show other)
