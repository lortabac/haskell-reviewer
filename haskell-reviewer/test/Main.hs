module Main (main) where

import ExeTest qualified
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import HaskellReviewer.App.CliTest qualified
import HaskellReviewer.App.RunTest qualified
import HaskellReviewer.Core.AddressTest qualified
import HaskellReviewer.Core.RootTest qualified
import HaskellReviewer.Effects.HelperTest qualified
import HaskellReviewer.Effects.TraceTest qualified
import HaskellReviewer.Server.RouterTest qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  -- The binary writes UTF-8 on stderr. Read it as UTF-8 even when the
  -- test runs under LC_ALL=C.
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "haskell-reviewer"
      [ HaskellReviewer.App.CliTest.tests,
        HaskellReviewer.App.RunTest.tests,
        HaskellReviewer.Core.AddressTest.tests,
        HaskellReviewer.Core.RootTest.tests,
        HaskellReviewer.Effects.HelperTest.tests,
        HaskellReviewer.Effects.TraceTest.tests,
        HaskellReviewer.Server.RouterTest.tests,
        ExeTest.tests
      ]
