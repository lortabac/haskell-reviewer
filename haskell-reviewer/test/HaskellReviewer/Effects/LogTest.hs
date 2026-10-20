module HaskellReviewer.Effects.LogTest (tests) where

import Control.Exception (ErrorCall (..), SomeException, throwIO, try)
import Data.Text qualified as Text
import HaskellReviewer.Effects.Log (exceptionMessage)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Log"
    [ testCase "exceptionMessage: thrown and caught, no backtrace" $ do
        -- Thrown for real, so GHC 9.10 attaches a backtrace.
        r <- try @SomeException (throwIO (userError "boom"))
        case r of
          Right () -> assertFailure "not thrown"
          Left e -> do
            let msg = exceptionMessage e
            msg @?= "user error (boom)"
            assertBool "backtrace" (not ("HasCallStack backtrace" `Text.isInfixOf` msg)),
      testCase "exceptionMessage: one line" $ do
        r <- try @SomeException (throwIO (ErrorCall "a\nb"))
        either (\e -> exceptionMessage e @?= "a b") (\() -> assertFailure "not thrown") r
    ]
