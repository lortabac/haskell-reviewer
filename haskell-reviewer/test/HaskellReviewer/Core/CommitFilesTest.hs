-- | Building a commit's file list from ls-tree rows.
module HaskellReviewer.Core.CommitFilesTest (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
    OutsideChanges (..),
    OutsideRows (..),
    commitLink,
    linkTargetCap,
    mkCommitFiles,
    outsideChanges,
    outsidePathCap,
  )
import HaskellReviewer.Core.FileContent (FileSize (..), LinkInfo (..))
import HaskellReviewer.Core.FileList
  ( CommitId (..),
    EntryKind (..),
    FileList (..),
    ListSource (..),
    fileListCap,
  )
import HaskellReviewer.Core.Git
  ( BlobItem (..),
    Exec (..),
    ObjectId (..),
    TreeEntry (..),
    TreeItem (..),
    TreeSize (..),
  )
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Renames (Blob (..), DiffEntry (..))
import HaskellReviewer.Core.Sides (Pair (..))
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
        Map.keys cf.outside.rows @?= ["libs", "libsx/b.hs", "top.txt"]
        cf.outside.truncated @?= False
        cf.files.source @?= FromGit,
      testCase "no prefix: everything is inside" $ do
        let cf =
              mkCommitFiles "" [blob "a.hs" (known 1), blob "d/b.hs" SizeNotListed]
        Map.keys cf.files.entries @?= [rel "a.hs", rel "d/b.hs"]
        cf.outside.rows @?= Map.empty,
      testCase "kinds and sizes" $ do
        let cf =
              mkCommitFiles
                "p/"
                [ blob "p/a.hs" (SizeKnown (FileSize 10)),
                  TreeEntry "p/run" (ItemBlob Executable (BlobItem (oid 'b') (known 3))),
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
      testCase "entries for pairing: exec bit, sizes, submodule" $ do
        let cf =
              mkCommitFiles
                ""
                [ blob "a.hs" (known 10),
                  TreeEntry "run" (ItemBlob Executable (BlobItem (oid 'b') (known 3))),
                  TreeEntry "link" (ItemSymlink (BlobItem (oid 'c') (known 4))),
                  TreeEntry "sub" (ItemGitlink (oid 'd')),
                  blob "gone" SizeMissing,
                  blob "later" SizeNotListed
                ]
            sized o n = Blob {oid = Just (oid o), size = n}
        cf.entries
          @?= Map.fromList
            [ (rel "a.hs", File NotExecutable (sized 'a' (Just 10))),
              (rel "gone", File NotExecutable (sized 'a' Nothing)),
              (rel "later", File NotExecutable (sized 'a' Nothing)),
              (rel "link", Symlink (sized 'c' (Just 4))),
              (rel "run", File Executable (sized 'b' (Just 3))),
              (rel "sub", Submodule (oid 'd'))
            ],
      testCase "outside ROOT: one side only, mode or blob changed" $ do
        let side items truncated =
              OutsideRows {rows = Map.fromList items, truncated = truncated}
            file x o = ItemBlob x (BlobItem (oid o) (known 1))
            changes =
              outsideChanges
                Pair
                  { head =
                      side
                        [ ("new", file NotExecutable 'a'),
                          ("same", file NotExecutable 'a'),
                          ("chmod", file Executable 'a'),
                          ("edit", file NotExecutable 'b')
                        ]
                        False,
                    base =
                      side
                        [ ("gone", file NotExecutable 'a'),
                          ("same", file NotExecutable 'a'),
                          ("chmod", file NotExecutable 'a'),
                          ("edit", file NotExecutable 'a')
                        ]
                        True
                  }
        changes
          @?= OutsideChanges
            { total = 4,
              paths = ["chmod", "edit", "gone", "new"],
              truncated = True
            }
        let many =
              side [(Char8.pack (show i), plain) | i <- [1 .. 150 :: Int]] False
            plain = file NotExecutable 'a'
            none = side [] False
            c = outsideChanges Pair {head = many, base = none}
        (c.total, length c.paths, c.truncated) @?= (150, outsidePathCap, False),
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
        Map.size cf.outside.rows @?= fileListCap
        cf.outside.truncated @?= True,
      testCase "commitLink: resolved, cut at the cap, not UTF-8" $ do
        let link = rel "docs/link"
            long = BS.replicate linkTargetCap 97
        commitLink link (FileSize 7) "../a.hs"
          @?= LinkInfo {target = "../a.hs", resolved = Just (rel "a.hs")}
        -- Cut: shown cut, points nowhere, even if the cut text is a path.
        commitLink link (FileSize (linkTargetCap + 1)) long
          @?= LinkInfo {target = Text.replicate linkTargetCap "a", resolved = Nothing}
        commitLink link (FileSize linkTargetCap) long
          @?= LinkInfo
            { target = Text.replicate linkTargetCap "a",
              resolved = Just (rel ("docs/" <> long))
            }
        commitLink link (FileSize 2) "\xff\xfe"
          @?= LinkInfo {target = "\xfffd\xfffd", resolved = Nothing}
    ]

known :: Int -> TreeSize
known = SizeKnown . FileSize

blob :: ByteString -> TreeSize -> TreeEntry
blob p s = TreeEntry p (ItemBlob NotExecutable (BlobItem (oid 'a') s))

oid :: Char -> ObjectId
oid = ObjectId . hex

hex :: Char -> Text
hex = Text.replicate 40 . Text.singleton

rel :: ByteString -> RelPath
rel p = either (error . show) id (parseRelPath p)
