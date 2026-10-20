-- | The embedded bundle (the committed web/dist).
module HaskellReviewer.Server.AssetsTest (tests) where

import Data.ByteString qualified as BS
import Data.List (stripPrefix)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Address (Token, tokenFromBytes, tokenText)
import HaskellReviewer.Server.Assets
  ( Asset (..),
    contentTypeFor,
    embeddedFiles,
    lookupAsset,
    shellFor,
    shellTemplate,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Assets"
    [ testCase "index.html is there" $
        assertBool "no index.html" (isJust (lookup "index.html" embeddedFiles)),
      testCase "every file is index.html or assets/<name>, with a known type" $
        mapM_
          ( \(path, _) -> do
              let ok = case stripPrefix "assets/" path of
                    Just name -> not (null name) && '/' `notElem` name
                    Nothing -> path == "index.html"
              assertBool ("unexpected file: " <> path) ok
              assertBool ("unknown type: " <> path) (isJust (contentTypeFor path))
          )
          embeddedFiles,
      testCase "index.html refers to ./assets/" $
        assertBool "no ./assets/ ref" (refCount "\"./assets/" shellTemplate >= 1),
      testCase "shellFor rewrites every ref and nothing else" $ do
        let shell = Text.Encoding.decodeUtf8 (shellFor token)
            new = "\"/" <> tokenText token <> "/assets/"
        refCount "\"./assets/" shell @?= 0
        refCount new shell @?= refCount "\"./assets/" shellTemplate
        Text.replace new "\"./assets/" shell @?= shellTemplate,
      testCase "lookupAsset finds every asset" $
        mapM_
          ( \(path, bytes) -> case stripPrefix "assets/" path of
              Nothing -> pure ()
              Just name -> case lookupAsset (Text.pack name) of
                Nothing -> assertFailure ("not found: " <> name)
                Just asset -> do
                  asset.body @?= bytes
                  Just asset.contentType @?= contentTypeFor name
          )
          embeddedFiles,
      testCase "lookupAsset: exact names only" $ do
        let names =
              [Text.pack n | (p, _) <- embeddedFiles, Just n <- [stripPrefix "assets/" p]]
        assertBool "no assets" (not (null names))
        let bad =
              ["", ".", "..", "index.html", "../index.html", "assets", "nope.js"]
                <> concatMap
                  ( \n ->
                      ["./" <> n, "/" <> n, n <> "x", "x" <> n, Text.toUpper n, n <> "/"]
                  )
                  names
        mapM_
          (\n -> assertBool ("found: " <> Text.unpack n) (isNothing (lookupAsset n)))
          bad,
      testCase "files are not empty" $
        mapM_
          (\(path, bytes) -> assertBool ("empty: " <> path) (not (BS.null bytes)))
          embeddedFiles
    ]

token :: Token
token = tokenFromBytes (BS.pack [1 .. 24])

refCount :: Text -> Text -> Int
refCount needle haystack = length (Text.breakOnAll needle haystack)
