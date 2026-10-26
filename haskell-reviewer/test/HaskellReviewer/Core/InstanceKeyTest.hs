-- | Instance keys (D-20) and the orphan rule.
module HaskellReviewer.Core.InstanceKeyTest (tests) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( DataBody (..),
    Decl (..),
    DeclInfo (..),
    DerivedClass (..),
    DerivingClause (..),
    FamInstDecl (..),
    FamInstForm (..),
    InstanceDecl (..),
    NewtypeBody (..),
    TypeDecl (..),
    TypeForm (..),
  )
import HaskellReviewer.Core.ModuleModel (ModuleParse (..), ParsedModule (..))
import HaskellReviewer.Core.Names (renderDeclId)
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Parser.InstanceKey
  ( KName (..),
    KeyType (..),
    Locals (..),
    derivedArity,
    isOrphan,
    renderArgs,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Instance keys"
    [ testCase "variables renamed in order of first appearance" $ do
        keys "instance Show (Maybe b)" @?= ["inst:Show:Maybe a"]
        keys "instance C (T b a) (a, b)" @?= ["inst:C:(T a b) (b, a)"]
        keys "instance forall s m. Monad m => MonadState s (StateT s m)"
          @?= ["inst:MonadState:a (StateT a b)"],
      testCase "one argument as is, several in argument position" $ do
        keys "instance C (Maybe a)" @?= ["inst:C:Maybe a"]
        keys "instance C [x] (Maybe y)" @?= ["inst:C:[a] (Maybe b)"]
        keys "instance C Int" @?= ["inst:C:Int"]
        keys "instance C" @?= ["inst:C:"],
      testCase "source parens don't count" $
        keys "instance C ((Maybe) (a))" @?= ["inst:C:Maybe a"],
      testCase "operators: infix, prefix, nested" $ do
        keys "instance C (a :+: b)" @?= ["inst:C:a :+: b"]
        keys "instance C ((:+:) a b)" @?= ["inst:C:a :+: b"]
        keys "instance C ((:+:) a)" @?= ["inst:C:(:+:) a"]
        keys "instance C (a :+: b :*: c)" @?= ["inst:C:a :+: (b :*: c)"]
        keys "instance C ((a :+: b) :*: c)" @?= ["inst:C:(a :+: b) :*: c"]
        keys "instance C (Maybe a :+: b)" @?= ["inst:C:Maybe a :+: b"]
        keys "instance a ~~ b" @?= ["inst:~~:a b"],
      testCase "functions" $ do
        keys "instance C (a -> b)" @?= ["inst:C:a -> b"]
        keys "instance C ((a -> b) -> c)" @?= ["inst:C:(a -> b) -> c"]
        keys "instance D (a -> b) c" @?= ["inst:D:(a -> b) c"],
      testCase "built-ins written prefix" $ do
        keys "instance C ((->) a b)" @?= ["inst:C:a -> b"]
        keys "instance C (((->) a b) -> c)" @?= ["inst:C:(a -> b) -> c"]
        keys "instance C ((->) a)" @?= ["inst:C:(->) a"]
        keys "instance C (->)" @?= ["inst:C:(->)"]
        keys "instance C ((,) a b)" @?= ["inst:C:(a, b)"]
        keys "instance C ((,,) a)" @?= ["inst:C:(,,) a"]
        keys "instance C (,)" @?= ["inst:C:(,)"]
        keys "instance C ([] a)" @?= ["inst:C:[a]"]
        keys "instance C []" @?= ["inst:C:[]"]
        keys "instance C ((#,#) a b)" @?= ["inst:C:(# a, b #)"]
        -- A user's own List is a plain name.
        keys "instance C (List a)" @?= ["inst:C:List a"],
      testCase "lists, tuples, promoted, literals" $ do
        keys "instance C [Maybe a]" @?= ["inst:C:[Maybe a]"]
        keys "instance C ()" @?= ["inst:C:()"]
        keys "instance C (a, b)" @?= ["inst:C:(a, b)"]
        keys "instance C (# a, b #)" @?= ["inst:C:(# a, b #)"]
        keys "instance C '[] '[a, b]" @?= ["inst:C:'[] '[a, b]"]
        keys "instance C '(a, b) 'True" @?= ["inst:C:'(a, b) 'True"]
        keys "instance C 1 \"s\"" @?= ["inst:C:1 \"s\""]
        keys "instance C ('(:) a b)" @?= ["inst:C:a ': b"],
      testCase "qualifiers dropped, kind applications dropped" $ do
        keys "instance M.C (M.T a)" @?= ["inst:C:T a"]
        keys "instance C (Proxy @k a)" @?= ["inst:C:Proxy a"]
        keys "instance C (Proxy (a :: k))" @?= ["inst:C:Proxy (a :: b)"],
      testCase "family instances" $ do
        keys "type instance F [a] = a" @?= ["inst:F:[a]"]
        keys "type instance F a (f a) = a" @?= ["inst:F:a (b a)"]
        keys "data instance D (Maybe a) = D" @?= ["inst:D:Maybe a"],
      testCase "deriving clauses: arity table, clamped" $ do
        derived "data T a b = T deriving (Eq, Functor, Bifunctor)"
          @?= ["inst:Eq:T a b", "inst:Functor:T a", "inst:Bifunctor:T"]
        derived "data Z = Z deriving (Functor, Bifunctor)"
          @?= ["inst:Functor:Z", "inst:Bifunctor:Z"]
        derived "newtype App a = App a deriving newtype (MonadState S)"
          @?= ["inst:MonadState:S App"]
        derived "data T s = T deriving (C s)" @?= ["inst:C:a (T a)"]
        derived "data a :+: b = L deriving (Eq)" @?= ["inst:Eq:a :+: b"]
        derived "data instance D [a] b = D deriving (Show, Functor)"
          @?= ["inst:Show:D [a] b", "inst:Functor:D [a]"],
      testCase "deriving clause and standalone deriving agree" $ do
        derived "newtype App a = App a deriving newtype (MonadThrow, Eq1, Arrow)"
          @?= ["inst:MonadThrow:App", "inst:Eq1:App", "inst:Arrow:App"]
        keys "deriving newtype instance MonadThrow App" @?= ["inst:MonadThrow:App"],
      testCase "arity table" $
        map
          derivedArity
          ["Show", "Functor", "MonadState", "MonadUnliftIO", "Bifunctor", "Eq2"]
          @?= [0, 1, 1, 1, 2, 2],
      testCase "renaming past z" $
        Text.words (renderArgs [KVar ("v" <> Text.pack (show i)) | i <- [1 .. 30 :: Int]])
          @?= map Text.singleton ['a' .. 'z'] <> ["a1", "b1", "c1", "d1"],
      testCase "orphans: local, qualified by this module, by another" $ do
        let locals =
              Locals
                { self = ModuleName "M",
                  types = Set.fromList ["T"],
                  constructors = Set.fromList ["A"]
                }
            name q n p = KName {qualifier = ModuleName <$> q, occ = n, promoted = p}
            con q n = name q n False
            show_ = con Nothing "Show"
        isOrphan locals show_ [KCon (con Nothing "T")] @?= False
        isOrphan locals show_ [KCon (con (Just "M") "T")] @?= False
        isOrphan locals show_ [KCon (con (Just "N") "T")] @?= True
        isOrphan locals show_ [KApp (KCon (con Nothing "Maybe")) [KVar "a"]] @?= True
        isOrphan locals (con Nothing "T") [KVar "a"] @?= False
        -- A constructor counts only when ticked.
        isOrphan locals show_ [KCon (name Nothing "A" True)] @?= False
        isOrphan locals show_ [KCon (con Nothing "A")] @?= True,
      testCase "orphans: parsed" $
        orphans
          ( "module M where\nimport qualified N\ndata T = T\ntype S = Int\n\
            \instance Show T\ninstance Show M.T\ninstance Show N.T\n\
            \instance Show S\ninstance Show (Maybe Int)\n\
            \type instance F T = Int\ntype instance F Int = Int"
          )
          @?= [ ("inst:Show:T", False),
                ("inst:Show:T", False),
                ("inst:Show:T", True),
                ("inst:Show:S", False),
                ("inst:Show:Maybe Int", True),
                ("inst:F:T", False),
                ("inst:F:Int", True)
              ],
      testCase "orphans: a constructor named like an imported type" $
        orphans
          "module M where\nimport Data.Text (Text)\ndata V = Text Text\n\
          \instance Pretty Text\ninstance Pretty (P 'Text)"
          @?= [("inst:Pretty:Text", True), ("inst:Pretty:P 'Text", False)],
      testCase "orphans: type data constructors are types" $
        orphans
          "{-# LANGUAGE TypeData #-}\nmodule M where\n\
          \type data K = KA\ninstance C KA"
          @?= [("inst:C:KA", False)],
      testCase "deriving: closed class arguments, same key as printed whole" $
        derived "data T a b = T deriving (MonadState S, C (M.X Int) [Y], Functor)"
          @?= [ "inst:MonadState:S (T a)",
                "inst:C:(X Int) [Y] (T a b)",
                "inst:Functor:T a"
              ],
      testCase "orphans: no module line is Main" $
        orphans "data T = T\ninstance Show Main.T\ninstance Show N.T"
          @?= [("inst:Show:T", False), ("inst:Show:T", True)]
    ]

-- | The declarations of a module, with the extensions the tests need,
-- each one shown with 'one'. Text only gives the reason.
declsOf :: (Decl -> [a]) -> (Text -> a) -> Text -> [a]
declsOf one bad src =
  case parseSource defaultParseSettings (extensions <> src <> "\n") of
    Parsed m -> concatMap one m.declarations
    TextOnly t -> [bad (Text.pack (show t))]
  where
    extensions =
      "{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, MagicHash #-}\n\
      \{-# LANGUAGE UnboxedTuples, DerivingStrategies, TypeApplications #-}\n\
      \{-# LANGUAGE ExplicitForAll #-}\n"

-- | The ids of every instance and family instance in a module.
keys :: Text -> [Text]
keys = declsOf one id
  where
    one = \case
      InstanceD i -> [renderDeclId i.info.id]
      FamInstD f -> [renderDeclId f.info.id]
      _ -> []

-- | The ids in the deriving clauses of a module's types and data
-- instances.
derived :: Text -> [Text]
derived = declsOf one id
  where
    one = \case
      TypeD t -> case t.form of
        DataType b -> clauses b.derivings
        Newtype b -> clauses b.derivings
        _ -> []
      FamInstD f -> case f.form of
        DataInstance b -> clauses b.derivings
        NewtypeInstance b -> clauses b.derivings
        TypeInstance -> []
      _ -> []
    clauses ds = [renderDeclId c.id | d <- ds, c <- d.classes]

-- | Each instance's id and orphan flag.
orphans :: Text -> [(Text, Bool)]
orphans = declsOf one (\t -> (t, False))
  where
    one = \case
      InstanceD i -> [(renderDeclId i.info.id, i.orphan)]
      FamInstD f -> [(renderDeclId f.info.id, f.orphan)]
      _ -> []
