{-# LANGUAGE CPP #-}

-- | Haddock markup to rich text: what each construct gives, that nothing
-- unsafe gets through, and that hostile comments stay cheap.
module HaskellReviewer.Core.HaddockTest (tests, linearTests) where

import Control.Exception (evaluate)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( Decl (..),
    DeclInfo (..),
    DocComment (..),
    ValueDecl (..),
  )
import HaskellReviewer.Core.ModuleModel
  ( DocItem (..),
    Export (..),
    ModuleHeader (..),
    ModuleParse (..),
    ParsedModule (..),
    Section (..),
  )
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Parser.Haddock
  ( hasBigNumber,
    maxCommentLength,
    parseDoc,
    parseHeading,
    plainOnly,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText
  ( Block (..),
    Cell (..),
    DefItem (..),
    Doc (..),
    Example (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    MathStyle (..),
    ModLink (..),
    NameRef (..),
    RefNamespace (..),
    Since (..),
    TableBody (..),
    forceDoc,
    safeUrl,
    urlText,
  )
import HaskellReviewer.Effects.Parser (haddockWarmUp)
import Hedgehog (forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem (getAllocationCounter)
import Test.Tasty
  ( DependencyType (..),
    TestTree,
    after,
    dependentTestGroup,
    testGroup,
  )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Haddock"
    [ testGroup "safety" safety,
      testGroup "markup" markup,
      testGroup "identifiers" identifiers,
      testGroup "plain only" plain,
      testCase "the warm-up has every construct (Q-RTS-BLACKHOLE)" warmUp,
      testProperty "any text: no crash, every link http(s)" . property $ do
        t <- forAll (Text.concat <$> Gen.list (Range.linear 0 100) piece)
        forceDoc (parseDoc t) === ()
        Hedgehog.assert (all safe (links (parseDoc t)))
    ]
  where
    tricky = ["&#", "&#x", "99999999", "1114112", ";", "+---+\n", "|a|\n", "\r", "\r\n"]
    chars = "<>[]()@/_'`\"#\\*-=+|:!&;. \nahHtpxX0123456789" :: String
    safe u = any (`Text.isPrefixOf` Text.toLower u) ["http://", "https://"]
    -- Single characters, and pieces that reach the cases that crash
    -- haddock-library (Note [Haddock cost]).
    piece =
      Gen.choice
        [ Text.singleton <$> Gen.element chars,
          Gen.element tricky
        ]

safety :: [TestTree]
safety =
  [ testCase "numbers past Int: no item numbers, no @since (Note [Big numbers])" $ do
      map
        hasBigNumber
        [ "9223372036854775807.",
          "0009223372036854775807",
          "9223372036854775808.",
          "@since 18446744073709551615",
          "99999999999999999999"
        ]
        @?= [False, False, True, True, True]
      let numbers t = [i.number | OrderedList is <- (parseDoc t).blocks, i <- is]
      numbers "7. a" @?= [Just 7]
      numbers "9223372036854775808. a" @?= [Nothing]
      numbers "99999999999999999999. a" @?= [Nothing]
      (parseDoc "a\n\n@since 18446744073709551615").since @?= Nothing
      (parseDoc "a\n\n@since 1.2").since
        @?= Just Since {package = Nothing, version = 1 :| [2]},
    testCase "raw HTML is text" $ do
      let d = parseDoc "<b>bold</b> <script>alert(1)</script> <img src=x onerror=y>"
      links d @?= []
      allText d @?= "bbold/b scriptalert(1)/script src=x onerror=y <img>",
    testCase "<<image>> is a link, never an image" $ do
      "<<http://evil.example/x.png>>" `gives` [link "http://evil.example/x.png" Nothing]
      "<<https://e.example/x.png a title>>"
        `gives` [link "https://e.example/x.png" (Just [Str "a title"])]
      "![alt text](http://e.example/x.png)"
        `gives` [link "http://e.example/x.png" (Just [Str "alt text"])]
      "![ ](http://e.example/x.png)" `gives` [link "http://e.example/x.png" Nothing],
    testCase "an image with an unsafe URL is text" $ do
      "<<x.png>>" `gives` [Str "x.png"]
      "<<data:image/png;base64,AAAA t>>" `gives` [Str "t <data:image/png;base64,AAAA>"],
    testCase "external links keep the full URL" $ do
      "<https://example.com/a/b?c=d&e=f#g label>"
        `gives` [link "https://example.com/a/b?c=d&e=f#g" (Just [Str "label"])]
      "[the /label/](http://example.com/x_(y))"
        `gives` [ link "http://example.com/x_(y" (Just theLabel),
                  Str ")"
                ]
      "see https://example.com/a?b=1."
        `gives` [Str "see ", link "https://example.com/a?b=1" Nothing, Str "."]
      "<HTTP://EXAMPLE.COM>" `gives` [link "HTTP://EXAMPLE.COM" Nothing],
    testCase "only http and https links" $ do
      "<javascript:alert(1) click>" `gives` [Str "click <javascript:alert(1)>"]
      "[x](javascript:alert(1))" `gives` [Str "x <javascript:alert(1>)"]
      "<mailto:a@b.c>" `gives` [Str "mailto:a@b.c"]
      "<Other.html>" `gives` [Str "Other.html"]
      "ftp://example.com/x" `gives` [Str "ftp://example.com/x"]
      "<//evil.example/x>" `gives` [Str "//evil.example/x"],
    testCase "safeUrl" $ do
      map url ["http://a", "https://a/b", "Https://a", "https://exämple.com"]
        @?= map Just ["http://a", "https://a/b", "Https://a", "https://exämple.com"]
      map url ["http://", "http:/a", "http://a b", "http://a\nb", "http://a\DEL"]
        @?= replicate 5 Nothing,
    testCase "safeUrl: no invisible characters (SPEC §6.4)" $
      -- Right-to-left override, zero width space, C1 controls, other
      -- spaces, line and paragraph separators.
      map
        url
        [ "http://a\x202E/moc.b",
          "http://a\x200B\&b",
          "http://a\x85",
          "http://a\x9F",
          "http://a\xA0\&b",
          "http://a\x3000\&b",
          "http://a\x2028\&b",
          "http://a\x2029\&b"
        ]
        @?= replicate 8 Nothing
  ]
  where
    url = fmap urlText . safeUrl
    theLabel = [Str "the ", Emphasis [Str "label"]]

markup :: [TestTree]
markup =
  [ testCase "inline markup" $
      "/em/ __bold__ @code /em/@ &#65;&#x42; a\\/b"
        `gives` [ Emphasis [Str "em"],
                  Str " ",
                  Bold [Str "bold"],
                  Str " ",
                  Code [Str "code ", Emphasis [Str "em"]],
                  Str " AB a/b"
                ],
    testCase "math" $
      "\\(x^2\\) and \\[y\\]"
        `gives` [Math InlineMath "x^2", Str " and ", Math DisplayMath "y"],
    testCase "anchors show nothing" $ do
      "a #anchor# b" `gives` [Str "a  b"]
      (parseDoc "#anchor#").blocks @?= []
      (parseDoc "#a# ").blocks @?= [],
    testCase "module links" $ do
      "\"Data.Map\"" `gives` [modLink "Data.Map" Nothing Nothing]
      "\"Data.Map#v:insert\"" `gives` [modLink "Data.Map" (Just "v:insert") Nothing]
      "\"Data.Map\\#t:Map\"" `gives` [modLink "Data.Map" (Just "t:Map") Nothing]
      "\"M#\" \"M\\#\""
        `gives` [modLink "M" Nothing Nothing, Str " ", modLink "M" Nothing Nothing]
      "[maps](\"Data.Map\")" `gives` [modLink "Data.Map" Nothing (Just [Str "maps"])],
    testCase "paragraphs, headings, code blocks" $
      (parseDoc "= Title\n\nText.\n\n@\ncode /x/\n@\n\n> bird\n> two").blocks
        @?= [ Heading Header {level = 1, title = [Str "Title"]},
              Paragraph [Str "Text."],
              CodeBlock [Str "code ", Emphasis [Str "x"], Str "\n"],
              CodeBlock [Str "bird\ntwo"]
            ],
    testCase "lists" $ do
      -- The nested list needs a blank line after it, as in Haddock.
      (parseDoc "* a\n\n    * b\n\n* c").blocks
        @?= [ UnorderedList
                [ [Paragraph [Str "a"], UnorderedList [[Paragraph [Str "b"]]]],
                  [Paragraph [Str "c"]]
                ]
            ]
      (parseDoc "1. a\n\n(5) b").blocks
        @?= [ OrderedList
                [ ListItem {number = Just 1, content = [Paragraph [Str "a"]]},
                  ListItem {number = Just 5, content = [Paragraph [Str "b"]]}
                ]
            ]
      (parseDoc "[term]: def").blocks
        @?= [DefinitionList [DefItem [Str "term"] [Paragraph [Str "def"]]]],
    testCase "examples and properties" $
      (parseDoc ">>> 1 + 1\n2\n<BLANKLINE>\n\nprop> x == x").blocks
        @?= [ Examples [Example {expression = "1 + 1", results = ["2", ""]}],
              Property "x == x"
            ],
    testCase "tables" $
      (parseDoc "+---+---+\n| a | b |\n+===+===+\n| c     |\n+---+---+").blocks
        @?= [ Table
                TableBody
                  { header = [[cell 1 [Str "a"], cell 1 [Str "b"]]],
                    body = [[cell 2 [Str "c"]]]
                  }
            ],
    testCase "@since" $ do
      let d = parseDoc "Text.\n\n@since 1.2.3"
      d.since @?= Just Since {package = Nothing, version = 1 :| [2, 3]}
      d.blocks @?= [Paragraph [Str "Text."]],
    testCase "@since with a package: only GHC 9.12 and later" $ do
      let d = parseDoc "@since base-4.5"
      (d.since, d.blocks) @?= sinceWithPackage,
    testCase "headings are one line of inline text" $ do
      parseHeading "Section 'x' /em/"
        @?= [Str "Section ", name AnyNs Nothing "x" "x", Str " ", Emphasis [Str "em"]]
      parseHeading "* not a list" @?= [Str "* not a list"]
  ]
  where
    cell n is = Cell {colspan = n, rowspan = 1, content = is}

-- | What @\@since base-4.5@ gives. See Note [Two haddock-libraries] in
-- "HaskellReviewer.Core.Parser.Haddock".
sinceWithPackage :: (Maybe Since, [Block])
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
sinceWithPackage = (Just Since {package = Just "base", version = 4 :| [5]}, [])
#else
sinceWithPackage = (Nothing, [Paragraph [Str "@since base-4.5"]])
#endif

identifiers :: [TestTree]
identifiers =
  [ testCase "namespaces" $
      "'x' v'y' t'T'"
        `gives` [ name AnyNs Nothing "x" "x",
                  Str " ",
                  name ValueNs Nothing "y" "y",
                  Str " ",
                  name TypeNs Nothing "T" "T"
                ],
    testCase "operators, backticks, qualifiers" $
      "'(+)' '`elem`' 'Data.Map.insert' '(M.<>)' 'M..' '(,)'"
        `gives` [ name AnyNs Nothing "+" "(+)",
                  Str " ",
                  name AnyNs Nothing "elem" "`elem`",
                  Str " ",
                  name AnyNs (Just "Data.Map") "insert" "Data.Map.insert",
                  Str " ",
                  name AnyNs (Just "M") "<>" "(M.<>)",
                  Str " ",
                  name AnyNs (Just "M") "." "M..",
                  Str " ",
                  name AnyNs Nothing "(,)" "(,)"
                ],
    testCase "not identifiers: keywords stay text, quotes and all" $
      let t = "'case' 'M.case' 'forall' '(::)' don't" in t `gives` [Str t],
    testCase "a lower case name with a dot is text, as in Haddock" $ do
      let t = "'x.y' 'Foo.bar.baz' 'x.Y' 'stack.yaml'" in t `gives` [Str t]
      "'M.x' 'M..' '_x'"
        `gives` [ name AnyNs (Just "M") "x" "M.x",
                  Str " ",
                  name AnyNs (Just "M") "." "M..",
                  Str " ",
                  name AnyNs Nothing "_x" "_x"
                ]
  ]

plain :: [TestTree]
plain =
  [ testCase "real comments are not plain only" $ do
      plainOnly "Use 'foo' or <http://x.y>, see \"M\".\n\n* a\n\n    * b" @?= False
      plainOnly (table 10 39) @?= False
      plainOnly (Text.replicate maxCommentLength "a") @?= False,
    testCase "too long: plain text, in paragraphs" $ do
      let t = Text.replicate (maxCommentLength - 5) "<" <> "\n\nmore"
      plainOnly t @?= True
      (parseDoc t).blocks
        @?= [ Paragraph [Str (Text.replicate (maxCommentLength - 5) "<")],
              Paragraph [Str "more"]
            ],
    testCase "a table line over 80 wide, or over 2,000 of table lines" $ do
      plainOnly (table 1 39) @?= False
      plainOnly (table 1 40) @?= True
      plainOnly (table 60 5) @?= False
      plainOnly (table 100 5) @?= True,
    testCase "out of range &#: haddock-library would crash" $ do
      map plainOnly ["Char &#99999999; here.", "&#x110000;", "&#X110000;", "&#-1;"]
        @?= [True, True, True, False]
      map plainOnly ["&#1114111;", "&#x10FFFF;", "&#99999999", "&#;"]
        @?= replicate 4 False
      -- The number wraps as in haddock-library: 2^64 + 65 is 'A'.
      "&#18446744073709551681;" `gives` [Str "A"]
      parsedOk "-- | Char &#99999999; here.\nx = 1\n"
      -- haddock-library drops \r before parsing: so do we, before the check.
      plainOnly "&#\r99999999;" @?= False
      (parseDoc "Char &#\r99999999; here.").blocks
        @?= [Paragraph [Str "Char &#99999999; here."]]
      parsedOk "-- | Char &#\r99999999; here.\nx = 1\n"
      parsedOk "{- | Char &#\r99999999; here. -}\nx = 1\n"
      parsedOk "module M (\n  -- * Char &#\r99999999; here.\n  x) where\nx = 1\n"
      parsedOk "module M where\n-- * Char &#\r99999999; here.\nx = 1\n",
    testCase "a table row shorter than the border: haddock-library would crash" $ do
      plainOnly "+---+\n|a|\n+---+" @?= True
      plainOnly "  +---+\n  |a|\n" @?= True
      plainOnly "+---+\n|a| \n" @?= False
      plainOnly "+---+\n|abc|\n+---+" @?= False
      plainOnly "+---+\nx |a|\n" @?= False
      parsedOk "-- | +---+\n-- |a|\n-- +---+\nx = 1\n"
      parsedOk "-- | +---+\r\n-- |  |\r\nx = 1\r\n"
  ]
  where
    parsedOk src = case parseSource defaultParseSettings src of
      Parsed _ -> pure ()
      other -> assertFailure (take 300 (show other))

warmUp :: Assertion
warmUp = case parseSource defaultParseSettings haddockWarmUp of
  Parsed m -> do
    m.warnings @?= []
    let docs =
          maybe [] pure m.header.haddock
            <> [d | DocChunk _ d <- m.docItems]
            <> declDocs m
        sections = [s | ExportSection s <- concat m.exports]
    sort (nub (concatMap (kinds . (.parsed)) docs <> concatMap headingKinds sections))
      @?= sort allKinds
    -- The last comment is plain text.
    [d.parsed.blocks | d <- take 1 (reverse (declDocs m))]
      @?= [[Paragraph [Str ("Plain text (Note [Haddock cost]):\n " <> border)]]]
  other -> assertFailure (take 300 (show other))
  where
    headingKinds s = kindsOfInlines (deep s.heading.parsed)
    border = "+" <> Text.replicate 90 "-" <> "+"

-- | The inline text of a one-paragraph doc.
gives :: Text -> [Inline] -> Assertion
gives t expected = case (parseDoc t).blocks of
  [Paragraph is] -> is @?= expected
  other -> assertFailure (show other)

link :: Text -> Maybe [Inline] -> Inline
link u l = case safeUrl u of
  Just url -> Link Hyperlink {url = url, label = l}
  Nothing -> error ("not safe: " <> Text.unpack u)

modLink :: Text -> Maybe Text -> Maybe [Inline] -> Inline
modLink m a l = ModuleLink ModLink {name = ModuleName m, anchor = a, label = l}

name :: RefNamespace -> Maybe Text -> Text -> Text -> Inline
name ns q o w = Name (NameRef ns (ModuleName <$> q) o w)

-- | The doc comments of the top-level values.
declDocs :: ParsedModule -> [DocComment Doc]
declDocs m = concat [v.info.haddock | ValueD v <- m.declarations]

-- | What constructs a doc uses, by name. See 'haddockWarmUp'.
kinds :: Doc -> [Text]
kinds d =
  ["since" | Just _ <- [d.since]]
    <> concatMap block d.blocks
    <> kindsOfInlines (inlines d)
  where
    block = \case
      Paragraph _ -> ["paragraph"]
      Heading _ -> ["heading"]
      CodeBlock _ -> ["code block"]
      UnorderedList items -> "list" : concatMap (concatMap block) items
      OrderedList items -> "ordered list" : concatMap (concatMap block . (.content)) items
      DefinitionList items -> "definitions" : concatMap (concatMap block . defs) items
      Examples _ -> ["examples"]
      Property _ -> ["property"]
      Table _ -> ["table"]
    defs (i :: DefItem) = i.definition

-- | What these inlines are, by name.
kindsOfInlines :: [Inline] -> [Text]
kindsOfInlines = concatMap $ \case
  Str _ -> ["str"]
  Emphasis _ -> ["emphasis"]
  Bold _ -> ["bold"]
  Code _ -> ["code"]
  Math InlineMath _ -> ["math"]
  Math DisplayMath _ -> ["display math"]
  Link l -> ["link", maybe "link without label" (const "link with label") l.label]
  Name r ->
    [ case r.namespace of
        ValueNs -> "value name"
        TypeNs -> "type name"
        AnyNs -> "name",
      maybe "unqualified" (const "qualified") r.qualifier
    ]
  ModuleLink l ->
    [ "module",
      maybe "no anchor" (const "anchor") l.anchor,
      maybe "no label" (const "label") l.label
    ]

-- | Every name 'kinds' gives.
allKinds :: [Text]
allKinds =
  [ "since",
    "paragraph",
    "heading",
    "code block",
    "list",
    "ordered list",
    "definitions",
    "examples",
    "property",
    "table",
    "str",
    "emphasis",
    "bold",
    "code",
    "math",
    "display math",
    "link",
    "link without label",
    "link with label",
    "value name",
    "type name",
    "name",
    "unqualified",
    "qualified",
    "module",
    "anchor",
    "no anchor",
    "label",
    "no label"
  ]

-- | A grid table of this many rows and columns: @2 * cols + 1@ wide.
table :: Int -> Int -> Text
table rows cols = Text.unlines (border : concat (replicate rows [row, border]))
  where
    border = "+" <> Text.replicate cols "-+"
    row = "|" <> Text.replicate cols "a|"

-- | Every inline, nested ones too.
inlines :: Doc -> [Inline]
inlines d = concatMap block d.blocks
  where
    block = \case
      Paragraph is -> deep is
      Heading h -> deep h.title
      CodeBlock is -> deep is
      UnorderedList items -> concatMap (concatMap block) items
      OrderedList items -> concatMap (concatMap block . (.content)) items
      DefinitionList items -> concatMap definition items
      Examples _ -> []
      Property _ -> []
      Table t -> [i | r <- t.header <> t.body, c <- r, i <- deep c.content]
    definition i = deep i.term <> concatMap block i.definition

-- | These inlines and all the ones inside them.
deep :: [Inline] -> [Inline]
deep = concatMap $ \i ->
  i : case i of
    Emphasis is -> deep is
    Bold is -> deep is
    Code is -> deep is
    Link l -> maybe [] deep l.label
    ModuleLink l -> maybe [] deep l.label
    _ -> []

-- | The URLs of all links.
links :: Doc -> [Text]
links d = [urlText l.url | Link l <- inlines d]

-- | All the plain text, joined.
allText :: Doc -> Text
allText d = Text.concat [t | Str t <- inlines d]

{- Note [Hostile comments]

Each case parses a module of size n and 4n and checks the work (bytes
allocated) grows about linearly, as Note [Linear time tests] in
"HaskellReviewer.Core.DeclsTest". Markup that haddock-library parses in
linear time is tested under the length cap. Markup it parses in
quadratic time is tested above the cap, and in many short comments: the
cap and the table rule (Note [Haddock cost]) are what keep those
linear. The group runs after every other test, the Declarations
one included.
-}

-- | See Note [Hostile comments].
linearTests :: TestTree
linearTests =
  after AllFinish "!/Linear time: Haddock/" $
    dependentTestGroup
      "Linear time: Haddock"
      AllFinish
      ( map aboveCap quadratic
          <> map manyUnderCap quadratic
          <> map underCap linearPieces
          <> [ -- Each level reads the text again, so one deep list is
               -- superlinear; the cap bounds it.
               testCase "nested lists, many comments" $
                 linear 10 40 (\k -> comments (replicate k nestedList)),
               testCase "a wide table" $ linear 1000 4000 (oneComment . table 2),
               testCase "a tall table" $ linear 1000 4000 (\k -> oneComment (table k 2)),
               testCase "a wide random grid" $ linear 1000 4000 (oneComment . grid 3),
               testCase "many tables just under the rule" $
                 linear 10 40 (\k -> comments (replicate k (table 30 10)))
             ]
      )
  where
    quadratic =
      [ ("unclosed <", "a < "),
        ("unclosed <<", "<<a "),
        ("unclosed [", "[a "),
        ("label, unclosed (", "[a](b "),
        ("unclosed \\(", "\\(a "),
        ("unclosed \\[", "\\[a "),
        ("escaped >", "< \\> "),
        ("escaped \\)", "\\( \\\\) ")
      ]
    linearPieces =
      [ ("@ and escaped @", "@\\@ "),
        ("/ and escaped /", "/\\/ "),
        ("__", "__a "),
        ("#", "#\\# "),
        ("quotes", "'a "),
        ("module names", "\"A "),
        ("&#", "&#1"),
        ("lines", "a\n"),
        ("paragraphs", "a\n\n"),
        ("list items", "* a\n"),
        ("headings", "= a\nb\n"),
        ("examples", ">>> a\n"),
        ("code blocks", "@\na\n@\n\n")
      ]
    -- One comment of n and 4n characters, both over the cap.
    aboveCap (title, p) =
      testCase (title <> ", above the cap") $
        linear over (4 * over) (oneComment . sized p)
    -- n and 4n comments of 500 characters.
    manyUnderCap (title, p) =
      testCase (title <> ", many comments") $
        linear 5 20 (\k -> comments (replicate k (sized p 500)))
    -- One comment of 2,500 and 10,000 characters.
    underCap (title, p) = testCase title (linear 2500 10000 (oneComment . sized p))
    sized p n = Text.take n (Text.replicate (n `div` Text.length p + 1) p)
    over = maxCommentLength + 1000
    nestedList = Text.concat [Text.replicate (4 * i) " " <> "* a\n\n" | i <- [0 .. 30]]

-- | A grid of @+@ and @-@, this many rows and columns, with every line
-- edged: haddock-library's worst table.
grid :: Int -> Int -> Text
grid rows cols = Text.unlines (Text.replicate cols "+" : replicate (rows - 1) row)
  where
    row = "+" <> Text.pack (take (cols - 2) (cycle "+--+-+---")) <> "+"

-- | A module with this one doc comment.
oneComment :: Text -> Text
oneComment c = comments [c]

-- | A module with these doc comments, each on its own value.
comments :: [Text] -> Text
comments cs = Text.concat (zipWith one [1 :: Int ..] cs)
  where
    one i c =
      "-- | "
        <> Text.intercalate "\n-- " (Text.lines c)
        <> "\nx"
        <> Text.pack (show i)
        <> " = 1\n"

-- | Parse a module made at the two sizes: the allocation must grow
-- about linearly. See Note [Hostile comments].
linear :: Int -> Int -> (Int -> Text) -> Assertion
linear small big make = do
  a <- allocated small
  b <- allocated big
  let ratio = fromIntegral b / fromIntegral a :: Double
  assertBool ("bytes " <> show (a, b) <> ", ratio " <> show ratio) (ratio < 6)
  where
    allocated k = do
      let src = make k
      _ <- evaluate (Text.length src)
      a0 <- getAllocationCounter
      n <- evaluate $ case parseSource defaultParseSettings src of
        Parsed m -> sum [length d.parsed.blocks | d <- declDocs m]
        TextOnly _ -> 0
      a1 <- getAllocationCounter
      assertBool "no doc" (n > 0)
      -- The counter goes down.
      pure (a0 - a1)
