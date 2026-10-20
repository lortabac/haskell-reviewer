module HaskellReviewer.Core.RootTest (tests) where

import HaskellReviewer.Core.Root (isWithin)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Root"
    [ testCase "isWithin" $ do
        "/a/b" `isWithin` "/a/b" @?= True
        "/a/b/" `isWithin` "/a/b" @?= True
        "/a/b/c" `isWithin` "/a/b" @?= True
        "/a/b/c" `isWithin` "/a/b/" @?= True
        "/a/bc" `isWithin` "/a/b" @?= False
        "/a" `isWithin` "/a/b" @?= False
        "/x/b" `isWithin` "/a/b" @?= False
        "/anything" `isWithin` "/" @?= True
    ]
