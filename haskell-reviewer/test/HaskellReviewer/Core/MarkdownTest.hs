-- | Markdown files (D-327, D-328): the doc, links and their safety,
-- anchors, paths. The runs have goldens (HighlightGoldenTest).
module HaskellReviewer.Core.MarkdownTest (tests, linearTests) where

import Control.Exception (evaluate)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (getAllocationCounter)
import HaskellReviewer.Core.Highlight (Run (..))
import HaskellReviewer.Core.Markdown
  ( Markdown (..),
    MarkdownEnv (..),
    isMarkdownPath,
    parseMarkdown,
    resolvePath,
    slugOf,
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments)
import HaskellReviewer.Core.RichText
  ( AnchorRef (..),
    Block (..),
    CodeListing (..),
    Doc (..),
    FileRef (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    Piece (..),
    TaskItem (..),
    forceDoc,
    safeUrl,
  )
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (DisplayClass (..))
import HaskellReviewer.Effects.Parser (markdownGuarded, markdownWithin)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Markdown"
    [ testGroup "blocks" blockTests,
      testGroup "links" linkTests,
      testGroup "anchors" anchorTests,
      testGroup "paths" pathTests,
      testGroup "haskell blocks" haskellTests
    ]

-- | Paths in a test snapshot.
path :: [Text] -> RelPath
path segs = case relPathFromSegments segs of
  Just p -> p
  Nothing -> error "bad test path"

-- | docs/README.md, in a snapshot with src/A.hs and docs/Other.md.
env :: MarkdownEnv
env =
  MarkdownEnv
    { path = path ["docs", "README.md"],
      exists = (`elem` [path ["src", "A.hs"], path ["docs", "Other.md"]])
    }

blocksOf :: Text -> [Block]
blocksOf t = (parseMarkdown env t).doc.blocks

-- | The inlines of a one-paragraph file.
para :: Text -> IO [Inline]
para t = case blocksOf t of
  [Paragraph is] -> pure is
  bs -> assertFailure ("not one paragraph: " <> show bs) >> pure []

-- | A code block, one plain piece.
plain :: Maybe Text -> Text -> Block
plain l t = Listing CodeListing {lang = l, pieces = [Piece Nothing t]}

-- | A link to a file.
file :: [Text] -> Maybe Text -> Text -> Inline
file p f l = FileLink FileRef {path = path p, fragment = f, label = Just [Str l]}

-- | A web link.
link :: Text -> Maybe [Inline] -> Inline
link u l = case safeUrl u of
  Just s -> Link Hyperlink {url = s, label = l}
  Nothing -> error "bad test url"

blockTests :: [TestTree]
blockTests =
  [ testCase "heading, paragraph, emphasis, strike" $
      blocksOf "# Hi *there*\n\nSome **bold** ~~old~~ `code`.\n"
        @?= [ Heading
                Header
                  { level = 1,
                    title = [Str "Hi ", Emphasis [Str "there"]],
                    anchor = Just "hi-there"
                  },
              Paragraph
                [ Str "Some ",
                  Bold [Str "bold"],
                  Str " ",
                  Strike [Str "old"],
                  Str " ",
                  Code [Str "code"],
                  Str "."
                ]
            ],
    testCase "lists, ordered from its start" $
      blocksOf "- a\n- b\n\n3. c\n4. d\n"
        @?= [ UnorderedList [[Paragraph [Str "a"]], [Paragraph [Str "b"]]],
              OrderedList
                [ ListItem {number = Just 3, content = [Paragraph [Str "c"]]},
                  ListItem {number = Just 4, content = [Paragraph [Str "d"]]}
                ]
            ],
    testCase "task list" $
      blocksOf "- [x] done\n- [ ] todo\n"
        @?= [ TaskList
                [ TaskItem {done = True, content = [Paragraph [Str "done"]]},
                  TaskItem {done = False, content = [Paragraph [Str "todo"]]}
                ]
            ],
    testCase "quote, rule" $
      blocksOf "> a\n> b\n\n---\n"
        @?= [BlockQuote [Paragraph [Str "a b"]], ThematicBreak],
    testCase "code blocks: language, plain" $
      blocksOf "```bash\necho\n```\n\n    x\n"
        @?= [ Listing CodeListing {lang = Just "bash", pieces = [Piece Nothing "echo\n"]},
              Listing CodeListing {lang = Nothing, pieces = [Piece Nothing "x\n"]}
            ],
    testCase "table" $ do
      bs <- pure (blocksOf "| a | b |\n|---|---|\n| 1 | 2 |\n")
      case bs of
        [Table _] -> pure ()
        _ -> assertFailure ("no table: " <> show bs),
    testCase "entities" $ do
      is <- para "a &amp; b &copy;"
      is @?= [Str "a & b ©"],
    testCase "raw HTML is text; a comment is dropped" $ do
      blocksOf "<script>alert(1)</script>\n\n<!-- hidden -->\n"
        @?= [plain Nothing "<script>alert(1)</script>\n"]
      is <- para "a <b onclick=\"x\">b</b>"
      is @?= [Str "a <b onclick=\"x\">b</b>"],
    testCase "CRLF then LF: two line ends" $
      blocksOf "a\r\n\n# H\n"
        @?= [ Paragraph [Str "a"],
              Heading Header {level = 1, title = [Str "H"], anchor = Just "h"}
            ],
    testCase "anything parses" $ do
      -- Deep nesting, odd bytes.
      let m = parseMarkdown env (mconcat (replicate 2000 "> ") <> "x\0\r\n[")
      length m.doc.blocks @?= 1
  ]

linkTests :: [TestTree]
linkTests =
  [ testCase "http and https" $ do
      is <- para "[a](https://x.org/p?q=1) <http://y.org>"
      is
        @?= [ link "https://x.org/p?q=1" (Just [Str "a"]),
              Str " ",
              link "http://y.org" (Just [Str "http://y.org"])
            ],
    testCase "other schemes are text" $ do
      is <- para "[a](javascript:alert(1)) [b](data:text/html,x) [c](mailto:me@x.org)"
      is
        @?= [ Str "a <javascript:alert(1)> b <data:text/html,x> c <mailto:me@x.org>"
            ],
    testCase "protocol-relative is text" $ do
      is <- para "[a](//evil.org/x)"
      is @?= [Str "a <//evil.org/x>"],
    testCase "an image is a link, never an image" $ do
      is <- para "![alt](https://x.org/i.png) ![](https://x.org/j.png)"
      is
        @?= [ link "https://x.org/i.png" (Just [Str "alt"]),
              Str " ",
              link "https://x.org/j.png" Nothing
            ],
    testCase "a file of the snapshot, with a fragment" $ do
      is <- para "[a](../src/A.hs#L3-L5) [b](Other.md#top) [c](/src/A.hs?raw=1)"
      is
        @?= [ file ["src", "A.hs"] (Just "L3-L5") "a",
              Str " ",
              file ["docs", "Other.md"] (Just "top") "b",
              Str " ",
              file ["src", "A.hs"] Nothing "c"
            ],
    testCase "a missing file, or out of the root, is text" $ do
      is <- para "[a](Nope.md) [b](../../etc/passwd)"
      is @?= [Str "a <Nope.md> b <../../etc/passwd>"],
    testCase "an anchor" $ do
      is <- para "[up](#usage) [x](#)"
      is
        @?= [ AnchorLink AnchorRef {anchor = "usage", label = Just [Str "up"]},
              Str " x <#>"
            ],
    testCase "escapes decoded in anchors and fragments" $ do
      is <- para "[c](#caf%C3%A9) [d](Other.md#caf%C3%A9)"
      is
        @?= [ AnchorLink AnchorRef {anchor = "café", label = Just [Str "c"]},
              Str " ",
              file ["docs", "Other.md"] (Just "café") "d"
            ]
  ]

anchorTests :: [TestTree]
anchorTests =
  [ testCase "like GitHub" $ do
      slugOf "Hello, World!" @?= "hello-world"
      slugOf "  `code` and_more-stuff " @?= "code-and_more-stuff"
      slugOf "Ünïcode Ok" @?= "ünïcode-ok",
    testCase "repeats get a number, in page order, nested too" $
      [ a
      | Heading h <- concatMap flat (blocksOf "# A\n\n> # A\n\n- # A\n"),
        Just a <- [h.anchor]
      ]
        @?= ["a", "a-1", "a-2"],
    testCase "a number already taken is skipped" $
      [a | Heading h <- blocksOf "# Foo-1\n\n# Foo\n\n# Foo\n", Just a <- [h.anchor]]
        @?= ["foo-1", "foo", "foo-2"]
  ]
  where
    flat = \case
      BlockQuote bs -> concatMap flat bs
      UnorderedList items -> concatMap (concatMap flat) items
      b -> [b]

pathTests :: [TestTree]
pathTests =
  [ testCase "relative, rooted, dots, escapes" $ do
      let from = path ["a", "b", "c.md"]
      resolvePath from "d.md" @?= Just (path ["a", "b", "d.md"])
      resolvePath from "../x/./y.md" @?= Just (path ["a", "x", "y.md"])
      resolvePath from "/top.md" @?= Just (path ["top.md"])
      resolvePath from "My%20File.md" @?= Just (path ["a", "b", "My File.md"])
      resolvePath from "%C3%A9.md" @?= Just (path ["a", "b", "é.md"])
      -- Not UTF-8: as written.
      resolvePath from "%FF.md" @?= Just (path ["a", "b", "%FF.md"])
      resolvePath from "../../../x" @?= Nothing
      resolvePath from "" @?= Nothing
      resolvePath from "./" @?= Just (path ["a", "b"]),
    testCase "markdown files" $ do
      isMarkdownPath (path ["README.md"]) @?= True
      isMarkdownPath (path ["d", "x.MARKDOWN"]) @?= True
      isMarkdownPath (path ["x.mdx"]) @?= False
      isMarkdownPath (path ["md"]) @?= False
  ]

haskellTests :: [TestTree]
haskellTests =
  [ testCase "pieces cut by token" $
      blocksOf "```haskell\nf = 1 -- c\n```\n"
        @?= [ Listing
                CodeListing
                  { lang = Just "haskell",
                    pieces =
                      [ Piece (Just VariableClass) "f",
                        Piece Nothing " = ",
                        Piece (Just LiteralClass) "1",
                        Piece Nothing " ",
                        Piece (Just CommentClass) "-- c",
                        Piece Nothing "\n"
                      ]
                  }
            ],
    testCase "a block that doesn't lex: one plain piece, literal runs" $ do
      let t = "```hs\nx = \"open\n```\n"
          m = parseMarkdown env t
      m.doc.blocks
        @?= [plain (Just "hs") "x = \"open\n"]
      [r.display | r <- m.runs] @?= [OperatorClass, LiteralClass, OperatorClass],
    testCase "a comment over lines in a quote: the > stays a marker" $ do
      let m = parseMarkdown env "> ```hs\n> {- a\n> b -}\n> ```\n"
          line3 = [(r.span.startCol, r.span.endCol, r.display) | r <- m.runs, onLine3 r]
          onLine3 r = r.span.startLine == Line 3
      line3 @?= [(Col 1, Col 2, OperatorClass), (Col 3, Col 7, CommentClass)],
    testCase "time limit: a paragraph of many links gives up" $ do
      let links = Text.replicate 50_000 "[a](b) "
      slow <- markdownWithin 50_000 env links
      fmap (const ()) slow @?= Nothing
      ok <- markdownGuarded env "# Fine\n"
      fmap (\m -> length m.doc.blocks) ok @?= Just 1,
    testCase "tokens of an indented block at file columns" $ do
      let m = parseMarkdown env "- a\n\n  ```haskell\n  yes\n  ```\n"
      [r | r <- m.runs, r.display == VariableClass]
        @?= [Run {span = Span (Line 4) (Col 3) (Line 4) (Col 6), display = VariableClass}]
  ]

-- | Allocation grows with the input, not faster: 4 times the input,
-- under 6 times the bytes. One long line with many marks, many Haskell
-- blocks, many lines with tabs.
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    testGroup
      "Linear time: Markdown"
      [ testCase "one long line of code spans" $
          linear (\k -> Text.replicate k "`a` *b* ") 5_000,
        testCase "many Haskell blocks" $
          linear (\k -> Text.replicate k "```haskell\nf x = x\n```\n") 1_000,
        testCase "many lines with tabs" $
          linear (\k -> Text.replicate k "-\tx\t`y`\n") 2_000
      ]

linear :: (Int -> Text) -> Int -> Assertion
linear input small = do
  a <- allocated small
  b <- allocated (4 * small)
  let ratio = fromIntegral b / fromIntegral a :: Double
  assertBool ("bytes " <> show (a, b) <> ", ratio " <> show ratio) (ratio < 6)
  where
    allocated k = do
      t <- evaluate (input k)
      _ <- evaluate (Text.length t)
      a0 <- getAllocationCounter
      let m = parseMarkdown env t
      _ <- evaluate (forceDoc m.doc)
      _ <- evaluate (length m.runs)
      a1 <- getAllocationCounter
      pure (a0 - a1)
