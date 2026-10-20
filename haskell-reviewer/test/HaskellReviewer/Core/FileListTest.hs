-- | The file list and file content classification.
module HaskellReviewer.Core.FileListTest (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileSize (..),
    TooLargeInfo (..),
    contentFromBytes,
    textLimit,
  )
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
    mkFileList,
  )
import HaskellReviewer.Core.RelPath (relPathFromBytes, relPathText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "FileList"
    [ testCase "sorted, first duplicate wins" $ do
        let entries =
              mapMaybe
                (\(b, k) -> (,k) <$> relPathFromBytes b)
                [("b.hs", EntryFile), ("a.hs", EntrySymlink), ("b.hs", EntrySymlink)]
            fl = mkFileList FromGit 2 entries
        map (\(p, k) -> (relPathText p, k)) (Map.toList fl.entries)
          @?= [("a.hs", EntrySymlink), ("b.hs", EntryFile)]
        fl.skippedNames @?= 2
        fl.truncated @?= False,
      testCase "cut at the cap" $ do
        let entriesOf n =
              mapMaybe
                (\i -> (,EntryFile) <$> relPathFromBytes (Char8.pack ("f" <> show i)))
                [1 .. n :: Int]
            atCap = mkFileList FromDisk 0 (entriesOf fileListCap)
            over = mkFileList FromDisk 0 (entriesOf (fileListCap + 1))
        Map.size atCap.entries @?= fileListCap
        atCap.truncated @?= False
        Map.size over.entries @?= fileListCap
        over.truncated @?= True,
      testCase "text" $ do
        contentFromBytes (FileSize 6) (Text.Encoding.encodeUtf8 "héllo")
          @?= TextContent "héllo"
        contentFromBytes (FileSize 0) "" @?= TextContent "",
      testCase "NUL in the first 8000 bytes: binary" $ do
        contentFromBytes (FileSize 3) "a\0b" @?= Binary (FileSize 3)
        -- Later NULs are not looked at.
        let late = BS.replicate 8000 97 <> "\0"
        contentFromBytes (FileSize 8001) late
          @?= TextContent (Text.replicate 8000 "a" <> "\0"),
      testCase "invalid UTF-8: binary" $
        contentFromBytes (FileSize 3) "a\xffz" @?= Binary (FileSize 3),
      testCase "over the limit: too large; a NUL wins" $ do
        let FileSize lim = textLimit
            big = BS.replicate (lim + 1) 97
        contentFromBytes (FileSize 5_000_000) big
          @?= TooLarge TooLargeInfo {size = FileSize 5_000_000, limit = textLimit}
        contentFromBytes (FileSize 5_000_000) ("\0" <> big)
          @?= Binary (FileSize 5_000_000)
        contentFromBytes (FileSize lim) (BS.replicate lim 97)
          @?= TextContent (Text.replicate lim "a")
    ]
