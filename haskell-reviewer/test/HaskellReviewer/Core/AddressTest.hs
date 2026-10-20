module HaskellReviewer.Core.AddressTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import HaskellReviewer.Core.Address
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Address"
    [ testCase "token: 32 base64url chars" $ do
        let t = tokenText (tokenFromBytes (BS.pack [0 .. 23]))
        Text.length t @?= 32
        assertBool (show t) (Text.all base64url t)
        -- 0xFB bytes give '-' and '_', the chars that differ from base64.
        let t2 = tokenText (tokenFromBytes (BS.replicate tokenByteCount 0xFB))
        assertBool (show t2) (Text.all base64url t2),
      testCase "tokenMatches" $ do
        let t = token
            text = tokenText t
        tokenMatches t text @?= True
        tokenMatches t (Text.map (\c -> if c == 'A' then 'B' else 'A') text) @?= False
        tokenMatches t (Text.take 31 text) @?= False
        tokenMatches t (text <> "A") @?= False
        tokenMatches t "" @?= False,
      testCase "sessionUrl" $
        sessionUrl (Port 8080) token
          @?= "http://127.0.0.1:8080/" <> tokenText token <> "/",
      testCase "hostAllowed: good" $
        mapM_
          (\h -> assertBool (show h) (hostAllowed (Port 8080) (Just h)))
          ["127.0.0.1:8080", "localhost:8080", "LocalHost:8080"],
      testCase "hostAllowed: bad" $ do
        mapM_
          (\h -> assertBool (show h) (not (hostAllowed (Port 8080) (Just h))))
          badHosts
        hostAllowed (Port 8080) Nothing @?= False
    ]

token :: Token
token = tokenFromBytes (BS.pack [1 .. 24])

badHosts :: [ByteString]
badHosts =
  [ "127.0.0.1:8081",
    "127.0.0.1",
    "localhost",
    "evil.com:8080",
    "[::1]:8080",
    "localhost.:8080",
    "127.0.0.1:08080",
    ""
  ]

base64url :: Char -> Bool
base64url c =
  (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c == '-'
    || c == '_'
