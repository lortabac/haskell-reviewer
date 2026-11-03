-- | Our port of markdown-unlit 0.6.0 (Note [Unlit] in
-- "HaskellReviewer.Core.SourceText"), against the library itself. The
-- only module that imports it: it's a test-only oracle (D-224).
module HaskellReviewer.Core.MarkdownUnlitTest
  ( tests,
    linearTests,
    warmUpOracle,
  )
where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParseWarning (..),
    forceModuleParse,
    renderParseWarning,
  )
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser
  ( ParseSettings (..),
    defaultParseSettings,
    earlyWarnings,
    parseSource,
  )
import HaskellReviewer.Core.SourceText
  ( FencedBlock (..),
    LooseUnlit (..),
    MarkdownCode (..),
    MarkdownLayout (..),
    MarkdownPiece (..),
    fenceClasses,
    hasMarkdownBlock,
    looseReadsMarkdown,
    markdownPieces,
    reorderKey,
    unlitLoose,
    unlitMarkdown,
  )
import HaskellReviewer.Core.Span (Line (..))
import Hedgehog (Gen, Property, annotateShow, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem (getAllocationCounter)
import Test.Tasty (DependencyType (..), TestTree, after, dependentTestGroup, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Text.Markdown.Unlit (CodeBlock (..), Selector (..))
import Text.Markdown.Unlit qualified as Lib

-- | Build the library's shared values once, before the tests use them
-- in parallel (Q-RTS-BLACKHOLE).
warmUpOracle :: IO ()
warmUpOracle = do
  let t =
        "~~~ {.haskell}\nx\n~~~\n```haskell top:1\ny\n```\n"
          <> "```haskell top\n  z\r\n```\n  ```haskell ignore\n"
  _ <- evaluate (oracle t)
  _ <- evaluate (length (show (Lib.parse (Text.unpack t))))
  pure ()

tests :: TestTree
tests =
  testGroup
    "markdown-unlit"
    [ gapTests,
      specTests,
      testProperty "like the library, on random files" differential
    ]

-- * One test per D-221 gap

gapTests :: TestTree
gapTests =
  testGroup
    "D-221 gaps"
    [ testCase "~~~ fences" $ do
        let t =
              file
                [ "# t",
                  "~~~ haskell",
                  "x = 1",
                  "~~~",
                  "~~~ {.haskell}",
                  "y = 2",
                  "~~~"
                ]
        unlitMarkdown t @?= Just (InPlace "\n\nx = 1\n\n\ny = 2\n\n")
        agrees t,
      testCase "``` closed by ~~~" $ do
        let t = file ["```haskell", "x = 1", "~~~", "y = 2", "```"]
        unlitMarkdown t @?= Just (InPlace "\nx = 1\n\n\n\n")
        agrees t,
      testCase "classes are whole words" $ do
        let selected f = hasMarkdownBlock (file [f, "x = 1", "```"])
        selected "```haskellish" @?= False
        selected "```{.haskell .ignore}" @?= False
        selected "```.haskell.ignore" @?= False
        selected "```{.haskell} ignore" @?= True
        selected "```{.haskell" @?= True
        selected "```haskell,ignore" @?= False
        selected "~~~~haskell" @?= True
        selected "```haskell\r" @?= True
        selected "\xA0```haskell" @?= True
        mapM_
          (\f -> agrees (file [f, "x = 1", "```"]))
          [ "```haskellish",
            "```{.haskell .ignore}",
            "```.haskell.ignore",
            "```{.haskell} ignore",
            "```{.haskell",
            "```haskell,ignore",
            "~~~~haskell",
            "```haskell\r"
          ],
      testCase "any fence closes (a ~~~ inside a ```sh block)" $ do
        -- markdown-unlit closes the sh block at ~~~, so the pairing
        -- shifts and bar is lost, like in the real build.
        let t =
              file
                [ "```haskell",
                  "foo = 1",
                  "```",
                  "```sh",
                  "~~~",
                  "```",
                  "```haskell",
                  "bar = 2",
                  "```"
                ]
        unlitMarkdown t @?= Just (InPlace "\nfoo = 1\n\n\n\n\n\n\n\n")
        agrees t,
      testCase "a fence with an info string closes" $ do
        let t = file ["```haskell", "x = 1", "```haskell", "y = 2", "```"]
        unlitMarkdown t @?= Just (InPlace "\nx = 1\n\n\n\n")
        agrees t,
      testCase "an unclosed fence runs to the end" $ do
        let t = file ["# t", "```haskell", "x = 1", "y = 2"]
        unlitMarkdown t @?= Just (InPlace "\n\nx = 1\ny = 2\n")
        agrees t,
      testCase "unlitLoose: > after an odd ~~~ is hidden" $ do
        let t = file ["~~~", "> x", "```haskell", "y = 1", "```"]
        -- ~~~ opens, ``` closes: the haskell fence is a close, so no block.
        looseReadsMarkdown t @?= False
        let t2 = file ["~~~", "> x", "~~~", "```haskell", "y = 1", "```", "~~~", "> z"]
        looseReadsMarkdown t2 @?= True
        agreesLoose t
        agreesLoose t2,
      testCase "unlitLoose: > after a closed ~~~ block counts" $ do
        let t = file ["~~~", "a", "~~~", "> x", "```haskell", "y = 1", "```"]
        looseReadsMarkdown t @?= False
        unlitLoose t @?= UsedGhcUnlit "\n\n\n  x\n\n\n\n"
        agreesLoose t,
      testCase "top blocks out of order: Reorders" $ do
        let blocks cs = file (concat [["```haskell " <> c, "x = 1", "```"] | c <- cs])
            check cs r = do
              unlitMarkdown (blocks cs) @?= Just r
              agrees (blocks cs)
            inPlace = InPlace . Text.intercalate "\n" . lines'
            lines' :: [Text] -> [Text]
            lines' cs = concat [["", "x = 1", ""] | _ <- cs] <> [""]
        check ["", "top"] (NeedsLayout Reorders)
        check ["", "top:3"] (NeedsLayout Reorders)
        check ["top:1", "top"] (NeedsLayout Reorders)
        check ["top:2", "top:1"] (NeedsLayout Reorders)
        check ["top"] (inPlace ["top"])
        check ["top", "top", ""] (inPlace ["top", "top", ""])
        check ["top", "top:1", ""] (inPlace ["top", "top:1", ""])
        check ["", "top:x"] (inPlace ["", "top:x"])
        check ["", "top:x top"] (NeedsLayout Reorders)
        -- top:-1: minBound - 1 wraps to maxBound, so it goes last.
        check ["top:-1", ""] (NeedsLayout Reorders)
        check ["", "top:-1"] (inPlace ["", "top:-1"])
        -- Bigger than an Int: read wraps it, like in the library.
        check ["", "top:99999999999999999999"] (NeedsLayout Reorders)
        check ["top:99999999999999999999", ""] (inPlace ["top:99999999999999999999", ""]),
      testCase "an indented haskell fence: Dedents" $ do
        let t = file ["1. a", "   ```haskell", "   x = 1", "   ```"]
        unlitMarkdown t @?= Just (NeedsLayout Dedents)
        agrees t
        let tabbed = file ["\t```haskell", "\tx = 1", "\t```"]
        unlitMarkdown tabbed @?= Just (NeedsLayout Dedents)
        agrees tabbed
        -- Blank content: markdown-unlit still dedents, so the notice
        -- anyway (D-225).
        unlitMarkdown (file ["  ```haskell", "", "  ```"]) @?= Just (NeedsLayout Dedents)
        agrees (file ["  ```haskell", "", "  ```"])
        -- Reorders wins.
        unlitMarkdown (file ["```haskell", "x", "```", "  ```haskell top", "y", "```"])
          @?= Just (NeedsLayout Reorders),
      testCase "an indented fence that isn't code: in place" $ do
        let t = file ["```haskell", "x = 1", "```", "  ```json", "  {}", "  ```"]
        unlitMarkdown t @?= Just (InPlace "\nx = 1\n\n\n\n\n")
        agrees t,
      testCase "notice and loose files" $ do
        let t = file ["```haskell", "x = 1", "```", "```haskell top", "y", "```"]
        unlitLoose t @?= UsedMarkdownUnlit (NeedsLayout Reorders)
        agreesLoose t,
      testCase "earlyWarnings on a notice file: no NoMarkdownCode" $ do
        let t = file ["```haskell", "x = 1", "```", "```haskell top", "y", "```"]
            early l = earlyWarnings defaultParseSettings {literate = l} t
        early MarkdownUnlit @?= []
        early LooseLiterate @?= [ReadAsMarkdown]
        earlyWarnings defaultParseSettings {literate = MarkdownUnlit} "no fence"
          @?= [NoMarkdownCode]
    ]
  where
    file = Text.unlines

-- * The library's own tests

-- | UnlitSpec from the markdown-unlit 0.6.0 tarball, ported. The
-- selector tests run with the default selector: foo became haskell
-- and bar became ignore. Not ported: parseSelector and run's usage
-- message (we take no selector, D-226).
specTests :: TestTree
specTests =
  testGroup
    "UnlitSpec"
    [ testCase "unlits .haskell unless .ignore" $ do
        let t =
              file
                [ "```haskell",
                  "some code",
                  "```",
                  "```haskell ignore",
                  "some other code",
                  "```"
                ]
        oracle t @?= Just [(2, ["some code"])]
        unlitMarkdown t @?= Just (InPlace "\nsome code\n\n\n\n\n"),
      testCase "moves .top to the beginning" $ do
        let t =
              file
                [ "```haskell top",
                  "module Foo where",
                  "```",
                  "",
                  "```haskell",
                  "foo :: Int",
                  "foo = 23",
                  "```",
                  "",
                  "```haskell top",
                  "import Bar",
                  "```"
                ]
        oracle t
          @?= Just
            [ (2, ["module Foo where"]),
              (11, ["import Bar"]),
              (6, ["foo :: Int", "foo = 23"])
            ]
        unlitMarkdown t @?= Just (NeedsLayout Reorders),
      testCase "-optL ignored: the default selector always" $ do
        let t =
              file
                [ "```foo",
                  "some code",
                  "",
                  "```",
                  "``` {.bar}",
                  "some other code",
                  "```"
                ]
        unlitMarkdown t @?= Nothing
        agrees t,
      testCase "parseReorderingKey" $ do
        reorderKey [] @?= 0
        reorderKey ["top"] @?= minBound
        reorderKey ["top:20"] @?= minBound + 20,
      testCase "unlit: one class" $
        selected ["~~~ {.haskell}", "foo", "~~~", "~~~ {.ignore}", "bar", "~~~"]
          @?= Just [(2, ["foo"])],
      testCase "unlit: Not" $
        -- With the default selector, a block needs haskell too.
        selected ["~~~ {.haskell .ignore}", "1", "~~~", "~~~ {.haskell}", "2", "~~~"]
          @?= Just [(5, ["2"])],
      testCase "unlit: :&:" $
        selected
          [ "~~~ {.ignore}",
            "some code",
            "~~~",
            "~~~ {.haskell .x}",
            "some other code",
            "~~~"
          ]
          @?= Just [(5, ["some other code"])],
      testCase "unlit: :|:" $
        selected ["~~~ {.haskell}", "foo", "~~~", "~~~ {.haskell}", "bar", "~~~"]
          @?= Just [(2, ["foo"]), (5, ["bar"])],
      testCase "unlit: :&: and Not" $
        selected
          [ "~~~ {.haskell}",
            "1",
            "~~~",
            "~~~ {.haskell .ignore}",
            "2",
            "~~~",
            "~~~ {.x .ignore}",
            "3",
            "~~~"
          ]
          @?= Just [(2, ["1"])],
      testCase "parse: empty input" $ ourParse "" @?= [],
      testCase "parse: a code block" $
        map
          (.content)
          ( ourParse
              ( file
                  [ "some text",
                    "~~~",
                    "some",
                    "code",
                    "~~~",
                    "some other text"
                  ]
              )
          )
          @?= [["some", "code"]],
      testCase "parse: backticks as fence" $
        ourParse (file ["``` {.haskell .ignore}", "some", "code", "```"])
          @?= [Block ["haskell", "ignore"] ["some", "code"] 2],
      testCase "parse: an indented code block" $
        map
          (.content)
          ( ourParse
              ( file
                  [ "1. some text",
                    "    ~~~",
                    "    some",
                    "      code",
                    "    ~~~",
                    "2. x"
                  ]
              )
          )
          @?= [["some", "  code"]],
      testCase "parse: an empty code block" $
        map (.content) (ourParse (file ["some text", "~~~", "~~~", "some other text"]))
          @?= [[]],
      testCase "parse: classes" $
        map (.classes) (ourParse (file ["~~~ {.haskell .literate}", "some code", "~~~"]))
          @?= [["haskell", "literate"]],
      testCase "parse: classes of an indented block" $
        map
          (.classes)
          ( ourParse
              ( file
                  [ "1. x",
                    "    ~~~ {.haskell .literate}",
                    "    some code",
                    "    ~~~",
                    "2. y"
                  ]
              )
          )
          @?= [["haskell", "literate"]],
      testCase "parse: start lines" $
        map (.start) (ourParse (file ["text", "", "~~~", "some", "code", "~~~", "x"]))
          @?= [4],
      testCase "parseClasses" $ do
        fenceClasses "~~~ {.foo .bar}" @?= ["foo", "bar"]
        fenceClasses "~~~ {foo.bar. ..}" @?= ["foo", "bar"]
        fenceClasses "```haskell" @?= ["haskell"]
        fenceClasses "```.haskell .ignore" @?= ["haskell", "ignore"]
    ]
  where
    file = Text.unlines
    selected ls = do
      let t = file ls
      r <- oracle t
      -- In order, no indent: ours is the same lines in place.
      if Just (InPlace (inPlaceFrom t r)) == unlitMarkdown t then Just r else Nothing

-- | A block, the library's way: classes, content (dedented) and the
-- line of its first content line.
data Block = Block
  { classes :: [Text],
    content :: [Text],
    start :: Int
  }
  deriving (Eq, Show)

-- | Our blocks, in the library's shape. The library reads lines with
-- 'lines', so a last newline doesn't make one more line.
ourParse :: Text -> [Block]
ourParse t =
  [ Block
      { classes = fenceClasses b.fence,
        content = map (Text.drop indent) b.body,
        start = fromIntegral fenceLine + 1
      }
  | Fenced b <- markdownPieces (fromMaybe t (Text.stripSuffix "\n" t)),
    let Line fenceLine = b.line
        -- Copied on purpose, not SourceText's fenceIndent: the oracle
        -- side stays independent.
        indent = Text.length (Text.takeWhile isSpace b.fence)
  ]

-- * Oracle

-- | The library's output with the default selector, as (first line,
-- lines) per block in output order. 'Nothing' when it's empty.
-- Inputs must not have @#line@ lines of their own.
oracle :: Text -> Maybe [(Int, [Text])]
oracle t = case lines (Lib.unlit "input" selector (Text.unpack t)) of
  [] -> Nothing
  ls -> Just (chunks ls)
  where
    selector = "haskell" :&: Not "ignore"
    chunks = \case
      l : rest
        | Just n <- lineNumber l ->
            let (body, more) = break (isJust . lineNumber) rest
             in (n, map Text.pack body) : chunks more
      _ -> []
    lineNumber :: String -> Maybe Int
    lineNumber l = case words l of
      ["#line", n, "\"input\""] -> Just (read n)
      _ -> Nothing

-- | The library's blocks, from its parse.
libParse :: Text -> [Block]
libParse t =
  [ Block
      { classes = map Text.pack cb.codeBlockClasses,
        content = map Text.pack cb.codeBlockContent,
        start = cb.codeBlockStartLine
      }
  | cb <- Lib.parse (Text.unpack t)
  ]

-- | What the library's blocks give, in place: their lines at their
-- file lines, the rest empty.
inPlaceFrom :: Text -> [(Int, [Text])] -> Text
inPlaceFrom t r =
  Text.intercalate
    "\n"
    [IntMap.findWithDefault "" n code | n <- [1 .. length (Text.splitOn "\n" t)]]
  where
    code = IntMap.fromList [(n + i, l) | (n, ls) <- r, (i, l) <- zip [0 ..] ls]

-- | What our unlitMarkdown should say, from the library alone.
expected :: Text -> Maybe MarkdownCode
expected t = case oracle t of
  Nothing -> Nothing
  Just r
    | not (rising (map fst r)) -> Just (NeedsLayout Reorders)
    | any (indented . fst) r -> Just (NeedsLayout Dedents)
    | otherwise -> Just (InPlace (inPlaceFrom t r))
  where
    rising ns = and (zipWith (<) ns (drop 1 ns))
    ls = IntMap.fromList (zip [1 ..] (Text.splitOn "\n" t))
    -- The fence is the line before the first content line.
    indented n =
      maybe False (not . Text.null . Text.takeWhile isSpace) (IntMap.lookup (n - 1) ls)

-- | Our loose choice, from the library alone: markdown-unlit when no
-- Bird or LaTeX line is outside the library's fenced lines and it
-- finds a block.
expectedLoose :: Text -> Bool
expectedLoose t = not birdOrLatex && oracle t /= Nothing
  where
    ls = Text.splitOn "\n" t
    total = length ls
    -- From the open fence to the close one (or the end).
    hidden =
      IntSet.fromList
        (concat [[b.start - 1 .. closeLine b] | b <- libParse t])
    closeLine b = min total (b.start + length b.content)
    -- Copied on purpose, not SourceText's: the oracle stays independent.
    birdOrLatex =
      or
        [ "\\begin{code}" `Text.isPrefixOf` Text.stripStart l || ">" `Text.isPrefixOf` l
        | (n, l) <- zip [1 ..] ls,
          not (IntSet.member n hidden)
        ]

agrees :: Text -> Assertion
agrees t = do
  unlitMarkdown t @?= expected t
  hasMarkdownBlock t @?= (oracle t /= Nothing)

agreesLoose :: Text -> Assertion
agreesLoose t = do
  looseReadsMarkdown t @?= expectedLoose t
  isMarkdown (unlitLoose t) @?= expectedLoose t

isMarkdown :: LooseUnlit -> Bool
isMarkdown = \case
  UsedMarkdownUnlit _ -> True
  UsedGhcUnlit _ -> False

-- * Differential property

differential :: Property
differential = withTests 1000 . property $ do
  ls <- forAll (Gen.list (Range.linear 0 20) markdownLine)
  let t = Text.intercalate "\n" ls
  annotateShow (oracle t)
  hasMarkdownBlock t === (oracle t /= Nothing)
  unlitMarkdown t === expected t
  looseReadsMarkdown t === expectedLoose t
  isMarkdown (unlitLoose t) === expectedLoose t
  map (.classes) (ourParse t) === map (.classes) (libParse t)
  map (.start) (ourParse t) === map (.start) (libParse t)

-- | Fences of every kind, code, prose, Bird and LaTeX lines. No
-- @#line@ line (the oracle reads them), no BOM.
markdownLine :: Gen Text
markdownLine = do
  l <-
    Gen.frequency
      [ (3, Gen.element fences),
        (2, (<>) <$> Gen.element indents <*> Gen.element indented),
        (3, Gen.element ["x = 1", "  y = 2", "", "text", "> b", "\\begin{code}", "#if 1"])
      ]
  cr <- Gen.bool
  pure (if cr then l <> "\r" else l)
  where
    fences =
      [ "```",
        "~~~",
        "````",
        "```haskell",
        "``` haskell",
        "~~~ haskell",
        "```{.haskell .ignore}",
        "```haskell ignore",
        "```haskellish",
        "```haskell top",
        "```haskell top:1",
        "```haskell top:-1",
        "```haskell top:x",
        "~~~ {.haskell top:2}",
        "```{.haskell",
        "```sh",
        "`~`haskell",
        "~`~ haskell",
        "```.haskell.ignore",
        "```{haskell top:1} ignore",
        "```{.haskell top:2} x",
        "```haskell top:(2)",
        "```haskell top:0x3",
        "```haskell top:1e1",
        "```haskell top:99999999999999999999",
        "```haskell top:9223372036854775807"
      ]
    -- NBSP, an em space and \v are spaces to isSpace too.
    indents = ["  ", "\t", " \t", "\xA0", "\x2003", "\v", " \xA0"]
    indented = ["```haskell", "~~~ haskell", "```", "~~~", "```sh", "```haskell top"]

-- * Linear time

-- | Hostile markdown: the work must grow about linearly (Note [Linear
-- time tests] in "HaskellReviewer.Core.DeclsTest"). Runs after the
-- other tests, like the Declarations group.
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time/" $
    dependentTestGroup
      "Linear time: markdown-unlit"
      AllFinish
      ( concat
          [ [ testCase (name <> ", earlyWarnings") (linearBy make (early l)),
              testCase (name <> ", parse") (linearBy make (parse l))
            ]
          | (name, l, make) <- hostile
          ]
          <> [ testCase "earlyWarnings stops at the first haskell block" $ do
                 let make k = block "" <> block ("top:" <> digits k)
                 small <- allocated make (early MarkdownUnlit) 1000
                 big <- allocated make (early MarkdownUnlit) 200000
                 assertBool ("bytes " <> show (small, big)) (big < 2 * small + 10000)
             ]
      )
  where
    -- Each hostile fence line comes first in a block that isn't code,
    -- so earlyWarnings has to read it too, and again in a code block
    -- out of order, so the parse reads its key.
    hostile =
      [ ("a long top: number", MarkdownUnlit, twice . ("top:" <>) . digits),
        ("a top: key in parentheses", MarkdownUnlit, twice . ("top:" <>) . parens),
        ("many top:i blocks, decreasing", MarkdownUnlit, decreasing),
        ("a { with no }", MarkdownUnlit, \k -> "``` {.haskell" <> classes k <> code),
        ("many fences, no haskell block", MarkdownUnlit, fences),
        ("loose: many > lines in fences", LooseLiterate, loose)
      ]
    twice c = "```sh " <> c <> "\n" <> code <> block "" <> block c
    decreasing k =
      Text.concat [block ("top:" <> Text.pack (show i)) | i <- down (k `div` 20)]
    classes k = Text.replicate k " .a" <> "\n"
    loose k = Text.replicate (k `div` 18) shell <> twice "top:9"
    shell = "```\n> x\n```\ntext\n"
    fences k = Text.replicate (k `div` 12) sh
    block c = "```haskell " <> c <> "\n" <> code
    code = "x = 1\n```\n"
    down :: Int -> [Int]
    down n = [n, n - 1 .. 1]
    digits k = Text.replicate k "9"
    parens k = Text.replicate k "(" <> "1" <> Text.replicate k ")"
    sh = "```sh\nx\n```\n"
    settings l = defaultParseSettings {literate = l}
    early l t =
      sum (map (Text.length . renderParseWarning) (earlyWarnings (settings l) t))
    parse l t =
      let p = parseSource (settings l) t
       in forceModuleParse p `seq` case p of
            Parsed _ -> 1
            TextOnly _ -> 2

-- | Measure at 64 KiB and 256 KiB of input (about): 4 times the input,
-- under 6 times the bytes.
linearBy :: (Int -> Text) -> (Text -> Int) -> Assertion
linearBy make measure = do
  small <- allocated make measure 65536
  big <- allocated make measure 262144
  let ratio = fromIntegral big / fromIntegral small :: Double
  assertBool ("bytes " <> show (small, big) <> ", ratio " <> show ratio) (ratio < 6)

-- | Bytes allocated to measure the input for k.
allocated :: (Int -> Text) -> (Text -> Int) -> Int -> IO Int
allocated make measure k = do
  let t = make k
  _ <- evaluate (Text.length t)
  a0 <- getAllocationCounter
  _ <- evaluate (measure t)
  a1 <- getAllocationCounter
  -- The counter goes down.
  pure (fromIntegral (a0 - a1))
