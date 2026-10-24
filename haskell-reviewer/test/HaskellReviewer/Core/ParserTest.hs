module HaskellReviewer.Core.ParserTest (tests) where

import Data.List (nub)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParseWarning (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.ModuleModel qualified as Model
  ( Extensions (..),
    ParseError (..),
    ParsedModule (..),
    TextOnlyModule (..),
  )
import HaskellReviewer.Core.Parser
  ( ParseSettings (..),
    canonicalExtensionNames,
    defaultParseSettings,
    parseSource,
  )
import HaskellReviewer.Core.Project (Extension (..))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ testCase "canonical extension names: one per extension, GHC's main one" $ do
        nub canonicalExtensionNames @?= canonicalExtensionNames
        ("GeneralizedNewtypeDeriving" `elem` canonicalExtensionNames) @?= True
        ("GeneralisedNewtypeDeriving" `elem` canonicalExtensionNames) @?= False
        ("Cpp" `elem` canonicalExtensionNames) @?= False
        ("CPP" `elem` canonicalExtensionNames) @?= True,
      testCase "alternate spellings give the main name"
        $ parsed
          defaultParseSettings {extensions = [on "GeneralisedNewtypeDeriving"]}
          "module M where"
        $ \m -> has "GeneralizedNewtypeDeriving" m @?= True,
      testCase "fromPragmas: a name given twice is listed once, first place"
        $ parsed
          defaultParseSettings
          "{-# LANGUAGE LambdaCase, CPP #-}\n{-# OPTIONS_GHC -XLambdaCase -cpp #-}\n\
          \{-# LANGUAGE MultiWayIf, LambdaCase #-}\nmodule M where"
        $ \m -> m.extensions.fromPragmas @?= ["LambdaCase", "CPP", "MultiWayIf"],
      testCase "implied extensions are on" $
        parsed defaultParseSettings "{-# LANGUAGE TypeFamilyDependencies #-}" $ \m ->
          has "TypeFamilies" m @?= True,
      testCase "negative implications: RebindableSyntax turns off ImplicitPrelude" $ do
        parsed defaultParseSettings "module M where" $ \m ->
          has "ImplicitPrelude" m @?= True
        parsed defaultParseSettings "{-# LANGUAGE RebindableSyntax #-}" $ \m ->
          has "ImplicitPrelude" m @?= False,
      testCase "Safe turns off TemplateHaskell, GND and DerivingVia" $ do
        let src = "{-# LANGUAGE Safe, TemplateHaskell, DerivingVia #-}\nmodule M where"
        parsed defaultParseSettings {extensions = [on "GeneralizedNewtypeDeriving"]} src $
          \m -> do
            map (`has` m) ["TemplateHaskell", "DerivingVia", "GeneralizedNewtypeDeriving"]
              @?= [False, False, False]
            m.warnings
              @?= [ SafeTurnsOff "GeneralizedNewtypeDeriving",
                    SafeTurnsOff "DerivingVia",
                    SafeTurnsOff "TemplateHaskell"
                  ]
        -- Not under Trustworthy.
        parsed defaultParseSettings "{-# LANGUAGE Trustworthy, TemplateHaskell #-}" $
          \m -> (has "TemplateHaskell" m, m.warnings) @?= (True, []),
      testCase "two Safe modes: a warning, the first one stays" $ do
        -- Safe from the component, Unsafe from the pragma.
        parsed
          defaultParseSettings {extensions = [on "Safe"]}
          "{-# LANGUAGE Unsafe, TemplateHaskell #-}"
          $ \m -> do
            has "TemplateHaskell" m @?= False
            -- GHC2021 has GeneralizedNewtypeDeriving.
            m.warnings
              @?= [ SafeModesClash "Safe" "Unsafe",
                    SafeTurnsOff "GeneralizedNewtypeDeriving",
                    SafeTurnsOff "TemplateHaskell"
                  ]
        parsed defaultParseSettings "{-# LANGUAGE Unsafe, Safe, TemplateHaskell #-}" $
          \m ->
            (has "TemplateHaskell" m, m.warnings)
              @?= (True, [SafeModesClash "Unsafe" "Safe"]),
      testCase "only applied names are in fromPragmas" $
        parsed defaultParseSettings "{-# OPTIONS_GHC -XNoSuchThing -XLambdaCase #-}" $
          \m -> do
            m.extensions.fromPragmas @?= ["LambdaCase"]
            m.warnings @?= [UnknownExtension "NoSuchThing"],
      testCase "a lexer error in a LANGUAGE pragma is a pragma error, like GHC" $
        case parseSource defaultParseSettings "{-# LANGUAGE LambdaCase, \"oops #-}\n" of
          TextOnly t | PragmaFailed e <- t.reason -> e.code @?= Just "GHC-68686"
          other -> assertFailure (show other),
      testCase "a long line with tabs parses fast" $ do
        -- 20,000 exports on one line, a tab before each. Walking the
        -- line for each span took seconds.
        let names = ["x" <> Text.pack (show i) | i <- [1 :: Int .. 20000]]
            src = "module M (\t" <> Text.intercalate ",\t" names <> ") where\n"
        r <- timeout 5_000_000 $ case parseSource defaultParseSettings src of
          Parsed m -> pure (fmap length m.exports)
          other -> assertFailure (show other) >> pure Nothing
        r @?= Just (Just 20000)
    ]
  where
    on n = Extension {name = n, enabled = True}
    has n m = n `elem` m.extensions.enabled

-- | Check a parsed module.
parsed :: ParseSettings -> Text.Text -> (Model.ParsedModule -> Assertion) -> Assertion
parsed settings src check = case parseSource settings src of
  Parsed m -> check m
  other -> assertFailure (show other)
