{-# LANGUAGE ImportQualifiedPost #-}

-- | Dev only: CBOR on stdin, JSON on stdout. Run with `make cbor-json`,
-- e.g. `curl -s <url>/api/file/tree/a.hs | make -s cbor-json`.
module Main (main) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LChar8
import Data.Text qualified as Text
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)
import System.Exit (die)

main :: IO ()
main = do
  bytes <- LBS.getContents
  either (die . Text.unpack . renderCborJsonError) LChar8.putStrLn (cborToJson bytes)
