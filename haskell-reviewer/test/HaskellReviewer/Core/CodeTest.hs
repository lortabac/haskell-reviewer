-- | Code tokens: heads, signatures and local links.
module HaskellReviewer.Core.CodeTest (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Code
  ( Code,
    CodeClass (..),
    CodeEnv,
    CodeToken (..),
    classHead,
    codeEnv,
    codeOf,
    famInstHead,
    instanceHead,
    typeHead,
  )
import HaskellReviewer.Core.Decls
  ( Decl (..),
    DeclInfo (..),
    Signature (..),
    ValueDecl (..),
    declInfo,
  )
import HaskellReviewer.Core.DocLinks (localNames)
import HaskellReviewer.Core.ModuleModel
  ( Located (..),
    ModuleHeader (..),
    ModuleParse (..),
    ParsedModule (..),
  )
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Parser
  ( ParseSettings (..),
    defaultParseSettings,
    parseSource,
  )
import HaskellReviewer.Core.Tokens (TokenClass (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Code"
    [ testCase "heads of each kind" $
        heads
          "{-# LANGUAGE GADTs, TypeFamilies, TypeData, StandaloneDeriving #-}\n\
          \{-# LANGUAGE DerivingStrategies #-}\n\
          \module M where\n\
          \data   T a   =   A a | B\n\
          \  deriving Show\n\
          \newtype N = N Int deriving newtype Eq\n\
          \data G a where\n\
          \  G1 :: Int -> G Int\n\
          \data E deriving Show\n\
          \data Z\n\
          \type data K = K1\n\
          \class Eq a => C a where\n\
          \  m :: a -> T a\n\
          \class Plain a\n\
          \instance C (T a) where\n\
          \  m = undefined\n\
          \instance C Int\n\
          \deriving stock instance Eq (T a)\n\
          \type S = T Int\n\
          \type family F a\n\
          \type family Q a where\n\
          \  Q Int = Bool\n\
          \data family D a\n\
          \data instance D Int = DI Int deriving Show\n\
          \newtype instance D Bool = DB Int\n\
          \type instance F Int = Bool\n"
          [ ("type:T", "data T a"),
            ("inst:Show:T a", "Show"),
            ("type:N", "newtype N"),
            ("inst:Eq:N", "Eq"),
            ("type:G", "data G a where"),
            ("type:E", "data E"),
            ("inst:Show:E", "Show"),
            ("type:Z", "data Z"),
            ("type:K", "type data K"),
            ("class:C", "class Eq a => C a"),
            ("class:Plain", "class Plain a"),
            ("inst:C:T a", "instance {C|class:C} ({T|type:T} a)"),
            ("inst:C:Int", "instance {C|class:C} Int"),
            ("inst:Eq:T a", "deriving stock instance Eq ({T|type:T} a)"),
            ("type:S", "type S = {T|type:T} Int"),
            ("type:F", "type family F a"),
            ("type:Q", "type family Q a where"),
            ("type:D", "data family D a"),
            ("inst:D:Int", "data instance {D|type:D} Int"),
            ("inst:Show:D Int", "Show"),
            ("inst:D:Bool", "newtype instance {D|type:D} Bool"),
            ("inst:F:Int", "type instance {F|type:F} Int = Bool")
          ],
      testCase "gaps are one space; comments and Haddock go" $
        heads
          "module M where\n\
          \data T {- c -} a -- end\n\
          \  b\n\
          \  = -- | Doc.\n\
          \    A\n"
          [("type:T", "data T a b")],
      testCase "links: types; values only after a tick; this module's qualifier only" $
        signatures
          "{-# LANGUAGE DataKinds, TypeOperators #-}\n\
          \module M where\n\
          \data T = A | Int :+ Int\n\
          \type a + b = Either a b\n\
          \(+++) :: Int -> Int -> Int\n\
          \(+++) = (+)\n\
          \x :: M.T -> Other.T -> T + Int -> Proxy 'A -> Proxy ' A -> Maybe a\n\
          \x = undefined\n\
          \y :: (:+) -> Proxy '(:+) -> Int\n\
          \y = undefined\n"
          [ ("val:+++", "(+++) :: Int -> Int -> Int"),
            ( "val:x",
              "x :: {M.T|type:T} -> Other.T -> {T|type:T} {+|type:+} Int \
              \-> Proxy '{A|con:A} -> Proxy ' A -> Maybe a"
            ),
            ("val:y", "y :: (:+) -> Proxy '({:+|con::+}) -> Int")
          ],
      testCase "an imported type named like a local constructor doesn't link" $
        signatures
          "module M where\n\
          \data Value = String String | Bool Bool\n\
          \isB :: Value -> Bool\n\
          \isB _ = True\n"
          [("val:isB", "isB :: {Value|type:Value} -> Bool")],
      testCase "reserved operators never link to local ones" $
        signatures
          "{-# LANGUAGE BangPatterns, TypeApplications, RankNTypes #-}\n\
          \module M where\n\
          \data T = T\n\
          \(!) :: Int -> Int -> Int\n\
          \(!) = (+)\n\
          \(.) :: Int -> Int -> Int\n\
          \(.) = (+)\n\
          \(~) :: Int -> Int -> Int\n\
          \(~) = (+)\n\
          \f :: forall a. T -> a\n\
          \f = undefined\n"
          [ ("val:!", "(!) :: Int -> Int -> Int"),
            ("val:f", "f :: forall a. {T|type:T} -> a")
          ],
      testCase "Main without a module line qualifies as Main" $
        signatures
          "data T = T\nx :: Main.T\nx = T\n"
          [("val:x", "x :: {Main.T|type:T}")],
      testCase "a CPP macro is one token, as written"
        $ withCode
          defaultParseSettings
          "{-# LANGUAGE CPP #-}\nmodule M where\n#define TY Int\nx :: TY\nx = 1\n"
        $ \env m ->
          [(t.kind, t.text) | t <- sigOf env m "val:x"]
            @?= [ (Tok VarId, "x"),
                  (Space, " "),
                  (Tok Special, "::"),
                  (Space, " "),
                  (Tok Macro, "TY")
                ],
      testCase "tabs and CRLF" $
        signatures "module M where\r\nx\t::\tMaybe\r\n  Int\r\nx = Nothing\r\n" $
          [("val:x", "x :: Maybe Int")],
      testCase ".lhs: the bird tracks are not code"
        $ withCode
          defaultParseSettings {literate = BirdOrLatex}
          "> module M where\n> data T\n>   = A\n> x :: T\n> x = A\n"
        $ \env m -> do
          shown (headOf env m "type:T") @?= "data T"
          shown (sigOf env m "val:x") @?= "x :: {T|type:T}"
    ]

-- | The head of each declaration, by id.
heads :: Text -> [(Text, Text)] -> Assertion
heads src expected =
  withCode defaultParseSettings src $ \env m ->
    [(i, shown (headOf env m i)) | (i, _) <- expected] @?= expected

-- | The signature of each value, by id.
signatures :: Text -> [(Text, Text)] -> Assertion
signatures src expected =
  withCode defaultParseSettings src $ \env m ->
    [(i, shown (sigOf env m i)) | (i, _) <- expected] @?= expected

withCode :: ParseSettings -> Text -> (CodeEnv -> ParsedModule -> Assertion) -> Assertion
withCode settings src k = case parseSource settings src of
  Parsed m ->
    k (codeEnv src m.tokens (localNames ((.value) <$> m.header.name) m.declarations)) m
  other -> assertFailure (take 300 (show other))

headOf :: CodeEnv -> ParsedModule -> Text -> Code
headOf env m i = case findDecl m i of
  Just (TypeD t) -> typeHead env t
  Just (ClassD c) -> classHead env c
  Just (InstanceD x) -> instanceHead env x
  Just (FamInstD f) -> famInstHead env f
  _ -> []

sigOf :: CodeEnv -> ParsedModule -> Text -> Code
sigOf env m i = case findDecl m i of
  Just (ValueD v) | Just s <- v.signature -> codeOf env [v.info] s.span
  _ -> []

-- | A top-level declaration.
findDecl :: ParsedModule -> Text -> Maybe Decl
findDecl m i =
  case [d | d <- m.declarations, Just info <- [declInfo d], renderDeclId info.id == i] of
    d : _ -> Just d
    [] -> Nothing

-- | The text, with a linked token as @{text|id}@.
shown :: Code -> Text
shown = Text.concat . map piece
  where
    piece t = maybe t.text (\i -> "{" <> t.text <> "|" <> renderDeclId i <> "}") t.link
