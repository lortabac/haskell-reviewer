module Bench.ShellTest (tests) where

import Bench.Shell (assetPaths)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Bench.Shell"
    [ testCase "script and stylesheet, not the icon" $
        assetPaths
          "<link rel=\"icon\" href=\"data:,\" />\n\
          \<script type=\"module\" crossorigin src=\"/tok/assets/index-D0.js\"></script>\n\
          \<link rel=\"stylesheet\" crossorigin href=\"/tok/assets/index-3u.css\">"
          @?= ["/tok/assets/index-D0.js", "/tok/assets/index-3u.css"],
      testCase "once each" $
        assetPaths "<script src=\"/t/assets/a.js\"></script><script src=\"/t/assets/a.js\">"
          @?= ["/t/assets/a.js"],
      testCase "no assets" $
        assetPaths "<html></html>" @?= []
    ]
