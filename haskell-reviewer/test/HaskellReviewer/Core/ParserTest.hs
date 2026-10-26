module HaskellReviewer.Core.ParserTest (tests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.List (nub)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Cpp (CppProblem (..), CppWarning (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParseWarning (..),
    TextOnlyReason (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.ModuleModel qualified as Model
  ( Export (..),
    Extensions (..),
    Item (..),
    ItemName (..),
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
import HaskellReviewer.Core.Span (renderSpan)
import HaskellReviewer.Effects.Parser
  ( cppParseWarmUp,
    cppSettings,
    parseGuarded,
    parseWithin,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

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
        r @?= Just (Just 20000),
      testCase "OPTIONS_GHC -D and -U go to CPP, not to the warnings"
        $ parsed
          defaultParseSettings
          "{-# LANGUAGE CPP #-}\n{-# OPTIONS_GHC -DX=x -optP-DY=y -UZ #-}\n\
          \module M (X, Y) where"
        $ \m -> do
          fmap (map exportName) m.exports @?= Just ["x", "y"]
          m.warnings @?= [],
      testCase "cpp-options: ignored words warn"
        $ parsed
          defaultParseSettings {ignoredCppOptions = ["-traditional"]}
          "module M where"
        $ \m -> m.warnings @?= [IgnoredCppOption "-traditional"],
      testCase "a pragma error after CPP: on the file's lines" $
        case parseSource defaultParseSettings pragmaAfterCpp of
          TextOnly t | PragmaFailed e <- t.reason -> do
            fmap renderSpan e.span @?= Just "4:14-4:25"
            t.warnings @?= [FromCpp (CppWarning 2 (WarningDirective "w"))]
          other -> assertFailure (show other),
      testCase "CPP warnings on the text-only path" $
        case parseSource defaultParseSettings "{-# LANGUAGE CPP #-}\n#error e\nx = = 1" of
          TextOnly t | ParseFailed e <- t.reason -> do
            fmap renderSpan e.span @?= Just "3:5-3:6"
            t.warnings @?= [FromCpp (CppWarning 2 (ErrorDirective "e"))]
          other -> assertFailure (show other),
      testCase "the CPP warm-up takes every path (Q-RTS-BLACKHOLE)" $
        case map (parseSource cppSettings) cppParseWarmUp of
          [Parsed m, TextOnly bad, TextOnly badPragma] -> do
            m.extensions.fromPragmas @?= ["CPP", "LambdaCase"]
            fmap (map exportName) m.exports @?= Just ["f", "y", "o", "p", "c"]
            [renderParseWarning w | w <- m.warnings]
              @?= [ "ignored cpp-options flag: -bogus",
                    "ignored OPTIONS_GHC flag: -Wall",
                    "line 11: #include \"x.h\" read as empty",
                    "line 12: #error e"
                  ]
            case bad.reason of
              ParseFailed e -> fmap renderSpan e.span @?= Just "15:5-15:6"
              other -> assertFailure (show other)
            case badPragma.reason of
              PragmaFailed _ -> pure ()
              other -> assertFailure (show other)
          other -> assertFailure (show other),
      testCase "a parse over the time limit stops soon" $
        mapM_ stopsWithin [20_000, 200_000],
      testCase "after a stopped parse, another thread parses" $ do
        _ <- parseWithin 1_000 defaultParseSettings (slowModule 20000)
        done <- newEmptyMVar
        _ <- forkIO (parseGuarded defaultParseSettings (slowModule 2) >>= putMVar done)
        r <- timeout 10_000_000 (takeMVar done)
        case r of
          Just (Parsed m) -> fmap length m.exports @?= Just 1024
          other -> assertFailure (take 200 (show other))
    ]
  where
    on n = Extension {name = n, enabled = True}
    has n m = n `elem` m.extensions.enabled

-- | A pragma, after a #warning, in #if 1.
pragmaAfterCpp :: Text.Text
pragmaAfterCpp =
  "{-# LANGUAGE CPP #-}\n#warning w\n#if 1\n{-# LANGUAGE NoSuchThing #-}\n#endif\n"

-- | 20000 macro calls make about 1 MB of exports out of 40 KB: GHC
-- takes seconds. With a limit of a few ms, it must stop soon after the
-- limit, and keep the warnings known before.
stopsWithin :: Int -> Assertion
stopsWithin limit = do
  t0 <- getMonotonicTime
  r <- parseWithin limit settings (slowModule 20000)
  t1 <- getMonotonicTime
  case r of
    TextOnly t | TooSlow <- t.reason -> t.warnings @?= [UnknownExtension "NoSuchThing"]
    other -> assertFailure (take 200 (show other))
  assertBool (show (limit, t1 - t0)) (t1 - t0 < fromIntegral limit / 1e6 + 1.5)
  where
    settings = defaultParseSettings {extensions = [on "NoSuchThing"]}
    on n = Extension {name = n, enabled = True}

-- | Each call of C gives 512 exports.
slowModule :: Int -> Text.Text
slowModule calls =
  Text.unlines
    [ "{-# LANGUAGE CPP #-}",
      "#define A a,a,a,a,a,a,a,a",
      "#define B A,A,A,A,A,A,A,A",
      "#define C B,B,B,B,B,B,B,B",
      "module M (" <> Text.intercalate "," (replicate calls "C") <> ") where"
    ]

exportName :: Model.Export -> Text.Text
exportName = \case
  Model.ExportItem i -> i.name.occ
  other -> Text.pack (show other)

-- | Check a parsed module.
parsed :: ParseSettings -> Text.Text -> (Model.ParsedModule -> Assertion) -> Assertion
parsed settings src check = case parseSource settings src of
  Parsed m -> check m
  other -> assertFailure (show other)
