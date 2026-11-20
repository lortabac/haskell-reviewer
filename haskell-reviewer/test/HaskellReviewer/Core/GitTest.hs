module HaskellReviewer.Core.GitTest (tests) where

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.FileContent (FileSize (..))
import HaskellReviewer.Core.Git
  ( BlobItem (..),
    CatFileHeader (..),
    Exec (..),
    FoundHeader (..),
    GitVersion (..),
    History (..),
    IndexMode (..),
    ObjectFormat (..),
    ObjectId (..),
    ObjectType (..),
    OtherEntry (..),
    RepoInfo (..),
    StageEntry (..),
    TreeEntry (..),
    TreeItem (..),
    TreeSize (..),
    minimumGitVersion,
    parseCatFileHeader,
    parseCommitIds,
    parseGitVersion,
    parseObjectId,
    parseOthers,
    parseRepoInfo,
    parseStageEntries,
    parseTreeEntries,
    renderGitVersion,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Git (pure)"
    [ testCase "version: plain, Apple, Windows" $ do
        parseGitVersion "git version 2.53.0\n" @?= Just (GitVersion 2 53 0)
        parseGitVersion "git version 2.39.3 (Apple Git-146)" @?= Just (GitVersion 2 39 3)
        parseGitVersion "git version 2.45.1.windows.1" @?= Just (GitVersion 2 45 1)
        parseGitVersion "git version 2.46.0-rc1" @?= Just (GitVersion 2 46 0)
        parseGitVersion "git version 3.0" @?= Just (GitVersion 3 0 0),
      testCase "version: garbage" $
        mapM_
          (\t -> (t, parseGitVersion t) @?= (t, Nothing))
          ["", "hello", "git version", "git version x.y.z", "git version 2", "2.44.0"],
      testCase "version: 2.44.0 is too old, 2.44.1 is fine" $ do
        assertBool "2.43.9" (GitVersion 2 43 9 < minimumGitVersion)
        assertBool "2.44.0" (GitVersion 2 44 0 < minimumGitVersion)
        assertBool "2.44.1" (GitVersion 2 44 1 >= minimumGitVersion)
        assertBool "3.0.0" (GitVersion 3 0 0 >= minimumGitVersion)
        renderGitVersion (GitVersion 2 43 0) @?= "2.43.0",
      testCase "ls-files --stage: modes" $
        parseStageEntries
          ( "100644 aaaa 0\ta.hs\0\
            \100755 bbbb 0\tbin/run\0\
            \120000 cccc 0\tlink\0\
            \160000 dddd 0\tvendor/dep\0"
          )
          @?= Right
            [ StageEntry "a.hs" ModeFile (ObjectId "aaaa"),
              StageEntry "bin/run" ModeFile (ObjectId "bbbb"),
              StageEntry "link" ModeSymlink (ObjectId "cccc"),
              StageEntry "vendor/dep" ModeGitlink (ObjectId "dddd")
            ],
      testCase "ls-files --stage: odd modes" $
        parseStageEntries "100664 aaaa 0\ta\0\&040000 bbbb 0\tdir\0"
          @?= Right
            [ StageEntry "a" ModeFile (ObjectId "aaaa"),
              StageEntry "dir" (ModeOther "040000") (ObjectId "bbbb")
            ],
      testCase "ls-files --stage: conflict stages collapse, stage 2 wins" $
        parseStageEntries
          ( "100644 base 1\tc.hs\0\
            \100644 ours 2\tc.hs\0\
            \100644 them 3\tc.hs\0\
            \100644 eeee 0\td.hs\0"
          )
          @?= Right
            [ StageEntry "c.hs" ModeFile (ObjectId "ours"),
              StageEntry "d.hs" ModeFile (ObjectId "eeee")
            ],
      testCase "ls-files --stage: tabs and non-UTF-8 bytes in names" $
        parseStageEntries "100644 aaaa 0\ta\tb\0\&100644 bbbb 0\tcaf\xe9\0"
          @?= Right
            [ StageEntry "a\tb" ModeFile (ObjectId "aaaa"),
              StageEntry "caf\xe9" ModeFile (ObjectId "bbbb")
            ],
      testCase "ls-files --stage: bad records" $
        mapM_
          (\r -> assertBool (show r) (isLeft (parseStageEntries r)))
          ["junk\0", "100644 aaaa 0 a.hs\0", "100644 aaaa x\ta\0", "100644 aaaa 0\t\0"],
      testCase "ls-files --stage: empty" $
        parseStageEntries "" @?= Right [],
      testCase "ls-files --others: files and nested repos" $
        parseOthers "new.hs\0nested/\0dir/f\0"
          @?= [OtherFile "new.hs", OtherRepo "nested", OtherFile "dir/f"],
      testCase "ls-tree --long: modes, padded sizes, gitlink, BAD" $
        parseTreeEntries
          ( "100644 blob "
              <> a
              <> "      42\ta.hs\0\
                 \100755 blob "
              <> b
              <> "       7\tbin/run\0\
                 \120000 blob "
              <> c
              <> "       4\tlink\0\
                 \160000 commit "
              <> d
              <> "       -\tsub\0\
                 \100644 blob "
              <> a
              <> "     BAD\tgone.txt\0"
          )
          @?= Right
            [ entry "a.hs" (file a (known 42)),
              entry "bin/run" (ItemBlob Executable (BlobItem (oid b) (known 7))),
              entry "link" (ItemSymlink (BlobItem (oid c) (known 4))),
              entry "sub" (ItemGitlink (oid d)),
              entry "gone.txt" (file a SizeMissing)
            ],
      testCase "ls-tree --long: big size, no padding" $
        parseTreeEntries ("100644 blob " <> a <> " 1200000\tbig.txt\0")
          @?= Right [entry "big.txt" (file a (known 1200000))],
      testCase "ls-tree: sha256 ids" $
        parseTreeEntries ("100644 blob " <> long <> "\tf\0")
          @?= Right [entry "f" (file long SizeNotListed)],
      testCase "ls-tree: TAB and newline in a path, non-UTF-8 bytes kept" $
        parseTreeEntries
          ( "100644 blob "
              <> a
              <> "       1\ta\tb\0\
                 \100644 blob "
              <> a
              <> "\tnew\nline\0\
                 \100644 blob "
              <> a
              <> "\tcaf\xe9\0"
          )
          @?= Right
            [ entry "a\tb" (file a (SizeKnown (FileSize 1))),
              entry "new\nline" (file a SizeNotListed),
              entry "caf\xe9" (file a SizeNotListed)
            ],
      testCase "ls-tree plain: no sizes, gitlink" $
        parseTreeEntries
          ("100644 blob " <> a <> "\ta.hs\0\&160000 commit " <> d <> "\tsub\0")
          @?= Right
            [ entry "a.hs" (file a SizeNotListed),
              entry "sub" (ItemGitlink (oid d))
            ],
      testCase "ls-tree: empty, unknown mode" $ do
        parseTreeEntries "" @?= Right []
        parseTreeEntries ("100664 blob " <> a <> "       1\tx\0")
          @?= Right [entry "x" (ItemOther "100664")],
      testCase "ls-tree: bad records" $
        mapM_
          (\r -> assertBool (show r) (isLeft (parseTreeEntries r)))
          [ "junk\0",
            "100644 blob " <> a <> " 1 a.hs\0",
            "100644 blob nothex\ta\0",
            "100644 blob " <> a <> "       -\ta\0",
            "100644 blob " <> a <> "      -1\ta\0",
            "160000 commit " <> d <> "       5\tsub\0",
            "100644 blob " <> a <> "\t\0"
          ],
      testCase "ls-tree: the bad record in the error" $
        parseTreeEntries ("100644 blob " <> a <> "\tok\0junk\0")
          @?= Left "bad ls-tree record: junk",
      testCase "cat-file header: found, missing" $ do
        parseCatFileHeader (a <> " blob 12")
          @?= Right (HeaderFound (FoundHeader (oid a) TypeBlob (FileSize 12)))
        parseCatFileHeader (b <> " tree 0")
          @?= Right (HeaderFound (FoundHeader (oid b) TypeTree (FileSize 0)))
        parseCatFileHeader (long <> " commit 200")
          @?= Right (HeaderFound (FoundHeader (oid long) TypeCommit (FileSize 200)))
        parseCatFileHeader (c <> " missing") @?= Right (HeaderMissing (oid c)),
      testCase "cat-file header: junk" $
        mapM_
          (\h -> assertBool (show h) (isLeft (parseCatFileHeader h)))
          [ a <> " ambiguous",
            "junk",
            "",
            a <> " blob -3",
            a <> " blob 3 extra",
            a <> " blob",
            a <> " blob  3",
            a <> " weird 3",
            "HEAD blob 3"
          ],
      testCase "object ids: only 40 or 64 lowercase hex" $ do
        parseObjectId a @?= Just (oid a)
        parseObjectId long @?= Just (oid long)
        mapM_
          (\i -> parseObjectId i @?= Nothing)
          [ "",
            "ABCDEF0123456789abcdef0123456789abcdef01",
            BS.take 39 a,
            a <> "0",
            BS.take 39 a <> " ",
            BS.take 39 a <> "\n",
            "g" <> BS.drop 1 a
          ],
      testCase "rev-parse info: top, prefix, sha256, shallow" $ do
        parseRepoInfo "\nsha1\nfalse\n"
          @?= Right (RepoInfo "" Sha1 FullHistory)
        parseRepoInfo "libs/a/\nsha256\ntrue\n"
          @?= Right (RepoInfo "libs/a/" Sha256 ShallowClone)
        parseRepoInfo "caf\xe9/\nsha1\nfalse\n"
          @?= Right (RepoInfo "caf\xe9/" Sha1 FullHistory),
      testCase "rev-parse info: a prefix with a newline, junk" $
        mapM_
          (\o -> assertBool (show o) (isLeft (parseRepoInfo o)))
          [ "new\nline/\nsha1\nfalse\n",
            "",
            "\nsha1\n",
            "\nmd5\nfalse\n",
            "\nsha1\nmaybe\n"
          ],
      testCase "commit ids: lines, empty, junk" $ do
        parseCommitIds (a <> "\n" <> b <> "\n") @?= Right [oid a, oid b]
        parseCommitIds "" @?= Right []
        assertBool "junk" (isLeft (parseCommitIds (a <> "\n^" <> b <> "\n")))
    ]
  where
    a = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    b = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    c = "cccccccccccccccccccccccccccccccccccccccc"
    d = "0123456789abcdef0123456789abcdef01234567"
    long = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    oid = ObjectId . Text.Encoding.decodeLatin1
    entry = TreeEntry
    file o s = ItemBlob NotExecutable (BlobItem (oid o) s)
    known = SizeKnown . FileSize
