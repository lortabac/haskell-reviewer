-- | Declaration ids, extents and the exported flag.
module HaskellReviewer.Core.DeclsTest (tests, linearTests) where

import Control.Exception (evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    Extent (..),
    FieldDecl (..),
    MethodDecl (..),
    NewtypeBody (..),
    Part (..),
    PartRole (..),
    PatSynDecl (..),
    TypeDecl (..),
    TypeForm (..),
    declInfo,
    mkExtent,
  )
import HaskellReviewer.Core.ModuleModel
  ( Import (..),
    Located (..),
    ModuleHeader (..),
    ModuleParse (..),
    ParsedModule (..),
  )
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..), renderDeclId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..), renderSpan)
import System.Mem (getAllocationCounter)
import Test.Tasty
  ( DependencyType (..),
    TestTree,
    after,
    dependentTestGroup,
    testGroup,
  )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Declarations"
    [ testCase "renderDeclId" $ do
        map
          renderDeclId
          [ Named DType "Maybe",
            Named DType ":+:",
            Named DClass "Monad",
            Named DCon "Just",
            Named DCon ":|",
            Named DVal "map",
            Named DVal "+++",
            Named DPat "Cons",
            Field "Config" "port"
          ]
          @?= [ "type:Maybe",
                "type::+:",
                "class:Monad",
                "con:Just",
                "con::|",
                "val:map",
                "val:+++",
                "pat:Cons",
                "field:Config.port"
              ],
      testCase "mkExtent: sorted, inner parts dropped, overlaps cut" $
        let e =
              mkExtent
                ( part 5 1 6 1 BodyPart
                    :| [ part 1 1 3 1 HeadPart,
                         part 2 1 2 5 DocPart,
                         part 2 5 4 1 PragmaPart
                       ]
                )
         in e.parts
              @?= ( part 1 1 3 1 HeadPart
                      :| [part 3 1 4 1 PragmaPart, part 5 1 6 1 BodyPart]
                  ),
      testCase "exported: no module line is Main (main)" $
        exported "main = pure ()\nhelper = 1\n"
          @?= [("val:main", True), ("val:helper", False)],
      testCase "exported: a header with no list exports everything" $
        exported "module M where\ndata T = A\nx = 1\n"
          @?= [("type:T", True), ("con:A", True), ("val:x", True)],
      testCase "exported: T without subs is the type only" $
        exported "module M (T) where\ndata T = A {f :: Int}\n"
          @?= [("type:T", True), ("con:A", False), ("field:T.f", False)],
      testCase "exported: T(..) and a bare field" $ do
        exported "module M (T (..)) where\ndata T = A {f :: Int}\n"
          @?= [("type:T", True), ("con:A", True), ("field:T.f", True)]
        exported "module M (f) where\ndata T = A {f :: Int}\n"
          @?= [("type:T", False), ("con:A", False), ("field:T.f", True)],
      testCase "exported: class methods by name, by C(..), by C(m)" $ do
        let cls = "class C a where\n  m :: a\n  n :: a\n"
        exported ("module M (C (..)) where\n" <> cls)
          @?= [("class:C", True), ("val:m", True), ("val:n", True)]
        exported ("module M (C (m), n) where\n" <> cls)
          @?= [("class:C", True), ("val:m", True), ("val:n", True)]
        exported ("module M (m) where\n" <> cls)
          @?= [("class:C", False), ("val:m", True), ("val:n", False)],
      testCase "exported: pattern P exports a constructor too" $
        exported (patterns <> "module M (pattern A) where\ndata T = A\n")
          @?= [("type:T", False), ("con:A", True)],
      testCase "exported: module M for this module, M.x, Other.x" $ do
        exported "module M (module M) where\nx = 1\n" @?= [("val:x", True)]
        exported "module M (M.x, N.y) where\nx = 1\ny = 2\n"
          @?= [("val:x", True), ("val:y", False)],
      testCase "LINE pragmas: header and import spans stay on the file" $
        case parseSource defaultParseSettings lineSrc of
          Parsed m -> do
            fmap (renderSpan . (.span)) m.header.name @?= Just "3:8-3:9"
            map (renderSpan . (.span)) m.imports @?= ["4:1-4:17"]
          TextOnly _ -> assertFailure "not parsed",
      testCase "exported: a bundled pattern synonym" $
        exported
          (patterns <> "module M (T (.., P)) where\ndata T = A\npattern P = A\n")
          @?= [("type:T", True), ("con:A", True), ("pat:P", True)]
    ]
  where
    part l1 c1 l2 c2 = Part (Span (Line l1) (Col c1) (Line l2) (Col c2))
    lineSrc =
      "{-# LINE 1 \"Gen.y\" #-}\n{-# LINE 9 \"G.y\" #-}\n\
      \module M where\nimport Data.List\n"
    patterns = "{-# LANGUAGE PatternSynonyms #-}\n"

{- Note [Linear time tests]

Untrusted input can have thousands of equations, pragmas or exports of
one kind (SPEC §9.1). A test parses n and 4n of them and checks the
work grows about 4 times, not 16 (quadratic): the ratio limit is 6.

Work is the bytes the parse allocates (getAllocationCounter), not time:
it doesn't depend on the machine's load, and every quadratic bug we had
(list appends, a list built per lookup, a fold per line) allocated
quadratically. Wall time at 4n was 6 to 8.3 times the time at n even
when linear (GC, small sizes), too close to the quadratic cases.

The group still runs after every other test, one test at a time.
-}

-- | Linear-time tests for hostile input. See Note [Linear time tests].
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    dependentTestGroup
      "Linear time"
      AllFinish
      [ testCase "alternating equations" $
          linear (\k -> Text.unlines (concat [[eq "f" i, eq "g" i] | i <- [1 .. k]])),
        testCase "INLINE pragmas" $
          linear (\k -> "f = 1\n" <> Text.replicate k "{-# INLINE f #-}\n"),
        testCase "bundled exports" $
          linear $ \k ->
            "{-# LANGUAGE PatternSynonyms #-}\nmodule M ("
              <> Text.intercalate ", " ["T" <> n i <> "(P" <> n i <> ")" | i <- [1 .. k]]
              <> ") where\n"
              <> Text.unlines (concatMap typeAndPattern [1 .. k]),
        testCase "LINE pragmas" $
          linear $ \k ->
            Text.unlines
              (concat [["{-# LINE " <> n i <> " \"G.y\" #-}", eq "f" i] | i <- [1 .. k]])
      ]
  where
    n :: Int -> Text
    n = Text.pack . show
    eq f i = f <> " " <> n i <> " = " <> n i
    typeAndPattern i = ["data T" <> n i, "pattern P" <> n i <> " = 1"]

-- | Parse 2,500 and 10,000 of something: the allocation must grow
-- about linearly. See Note [Linear time tests].
linear :: (Int -> Text) -> Assertion
linear make = do
  small <- allocated 2500
  big <- allocated 10000
  let ratio = fromIntegral big / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, big) <> ", ratio " <> show ratio) (ratio < 6)
  where
    allocated k = do
      let src = make k
      _ <- evaluate (Text.length src)
      a0 <- getAllocationCounter
      count <- evaluate $ case parseSource defaultParseSettings src of
        Parsed m -> length m.declarations
        TextOnly _ -> 0
      a1 <- getAllocationCounter
      assertBool "no declarations" (count > 0)
      -- The counter goes down.
      pure (a0 - a1)

-- | Each declaration, nested ones too, with its flag.
exported :: Text -> [(Text, Bool)]
exported src = case parseSource defaultParseSettings src of
  Parsed m -> concatMap decl m.declarations
  TextOnly _ -> [("not parsed", False)]
  where
    one i = (renderDeclId i.id, i.exported)
    decl d = one (declInfo d) : inner d
    inner = \case
      TypeD t -> typeInner t
      ClassD c ->
        map (\m -> one m.info) c.methods
          <> concatMap (\t -> one t.info : typeInner t) c.associated
      ValueD _ -> []
      PatSynD p -> map one p.fields
    typeInner t = concatMap con $ case t.form of
      DataType b -> b.constructors
      TypeData b -> b.constructors
      Newtype b -> [b.constructor]
      _ -> []
    con c = one c.info : map (\f -> one f.info) (fields c.args)
    fields = \case
      RecordArgs fs -> fs
      GadtRecordArgs fs -> fs
      _ -> []
