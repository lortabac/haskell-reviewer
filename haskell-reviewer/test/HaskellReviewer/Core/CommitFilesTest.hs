-- | Building a commit's file list from ls-tree rows.
module HaskellReviewer.Core.CommitFilesTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.CommitFiles
  ( BlobRef (..),
    BlobSize (..),
    CommitFiles (..),
    Dropped (..),
    DroppedRow (..),
    mkCommitFiles,
  )
import HaskellReviewer.Core.FileContent (FileSize (..))
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
  )
import HaskellReviewer.Core.Git
  ( BlobItem (..),
    ObjectId (..),
    TreeEntry (..),
    TreeItem (..),
    TreeSize (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Commit files"
    [ testCase "prefix: libs/ is not libsx/" $ do
        let cf =
              mkCommitFiles
                "libs/"
                [ blob "libs/a.hs" (SizeKnown (FileSize 1)),
                  blob "libsx/b.hs" (SizeKnown (FileSize 2)),
                  blob "libs" (SizeKnown (FileSize 3)),
                  blob "top.txt" (SizeKnown (FileSize 4))
                ]
        Map.keys cf.files.entries @?= [rel "a.hs"]
        Map.keys cf.outside @?= ["libs", "libsx/b.hs", "top.txt"]
        cf.outsideTruncated @?= False
        cf.files.source @?= FromGit,
      testCase "no prefix: everything is inside" $ do
        let cf =
              mkCommitFiles "" [blob "a.hs" (known 1), blob "d/b.hs" SizeNotListed]
        Map.keys cf.files.entries @?= [rel "a.hs", rel "d/b.hs"]
        cf.outside @?= Map.empty,
      testCase "kinds and sizes" $ do
        let cf =
              mkCommitFiles
                "p/"
                [ blob "p/a.hs" (SizeKnown (FileSize 10)),
                  TreeEntry "p/run" (ItemBlob (BlobItem (oid 'b') (known 3))),
                  TreeEntry "p/link" (ItemSymlink (BlobItem (oid 'c') (known 4))),
                  TreeEntry "p/sub" (ItemGitlink (oid 'd')),
                  blob "p/gone" SizeMissing,
                  blob "p/later" SizeNotListed
                ]
        cf.files.entries
          @?= Map.fromList
            [ (rel "a.hs", EntryFile),
              (rel "gone", EntryFile),
              (rel "later", EntryFile),
              (rel "link", EntrySymlink),
              (rel "run", EntryFile),
              (rel "sub", EntrySubmodule (CommitId (hex 'd')))
            ]
        cf.blobs
          @?= Map.fromList
            [ (rel "a.hs", BlobRef (oid 'a') (BlobSize (FileSize 10))),
              (rel "gone", BlobRef (oid 'a') BlobMissing),
              (rel "later", BlobRef (oid 'a') BlobSizeUnknown),
              (rel "link", BlobRef (oid 'c') (BlobSize (FileSize 4))),
              (rel "run", BlobRef (oid 'b') (BlobSize (FileSize 3)))
            ],
      testCase "non-UTF-8 counted, .git dropped, odd mode dropped" $ do
        let cf =
              mkCommitFiles
                ""
                [ blob "caf\xe9.txt" SizeNotListed,
                  blob "x/.git/config" SizeNotListed,
                  TreeEntry "odd" (ItemOther "100664"),
                  blob "ok" SizeNotListed
                ]
        Map.keys cf.files.entries @?= [rel "ok"]
        cf.files.skippedNames @?= 1
        cf.dropped
          @?= [ DroppedPath "x/.git/config",
                DroppedMode DroppedRow {path = "odd", mode = "100664"}
              ],
      testCase "cap: files and outside rows, blobs follow the files" $ do
        let n = fileListCap + 5
            names = [Char8.pack ("f" <> show i) | i <- [1 .. n]]
            cf =
              mkCommitFiles
                "in/"
                ( [blob ("in/" <> p) SizeNotListed | p <- names]
                    <> [blob ("out/" <> p) SizeNotListed | p <- names]
                )
        Map.size cf.files.entries @?= fileListCap
        cf.files.truncated @?= True
        Map.keys cf.blobs @?= Map.keys cf.files.entries
        Map.size cf.outside @?= fileListCap
        cf.outsideTruncated @?= True
    ]

known :: Int -> TreeSize
known = SizeKnown . FileSize

blob :: ByteString -> TreeSize -> TreeEntry
blob p s = TreeEntry p (ItemBlob (BlobItem (oid 'a') s))

oid :: Char -> ObjectId
oid = ObjectId . hex

hex :: Char -> Text
hex = Text.replicate 40 . Text.singleton

rel :: ByteString -> RelPath
rel p = either (error . show) id (parseRelPath p)
