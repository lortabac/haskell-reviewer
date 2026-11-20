{-# LANGUAGE StrictData #-}

-- | Markdown files (SPEC §4.2, §5.6): one commonmark parse gives the
-- rendered doc and the source view's colours. Pure. See Note [Markdown].
module HaskellReviewer.Core.Markdown
  ( -- * Files
    isMarkdownPath,

    -- * Parsing
    MarkdownEnv (..),
    Markdown (..),
    parseMarkdown,

    -- * Pieces, for tests
    slugOf,
    resolvePath,
  )
where

import Commonmark
  ( IsBlock (..),
    IsInline (..),
    ListType,
    Rangeable (..),
    SourcePos,
    SourceRange (..),
    SyntaxSpec,
    Tok (..),
    TokType (..),
    defaultSyntaxSpec,
    parseCommonmarkWith,
  )
import Commonmark qualified as CM (ListType (..))
import Commonmark.Entity (lookupEntity)
import Commonmark.Extensions
  ( HasPipeTable (..),
    HasStrikethrough (..),
    HasTaskList (..),
    autolinkSpec,
    pipeTableSpec,
    strikethroughSpec,
    taskListSpec,
  )
import Commonmark.Types (HasAttributes (..))
import Control.Monad (join)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isMark, isSpace, toLower)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (mapAccumL, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import HaskellReviewer.Core.Highlight (Run (..))
import HaskellReviewer.Core.Parser (lexSnippet)
import HaskellReviewer.Core.RelPath
  ( RelPath,
    relPathFromSegments,
    relPathName,
    relPathSegments,
  )
import HaskellReviewer.Core.RichText
  ( AnchorRef (..),
    Block (..),
    Cell (..),
    CodeListing (..),
    Doc (..),
    FileRef (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    ModLink (..),
    NameRef (..),
    Piece (..),
    TableBody (..),
    TaskItem (..),
    safeUrl,
  )
import HaskellReviewer.Core.SourceText (sliceSpan, sourceLines, stripBom)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (DisplayClass (..), Token (..), displayClass, tokenList)
import Text.Parsec.Pos
  ( initialPos,
    setSourceColumn,
    setSourceLine,
    sourceColumn,
    sourceLine,
  )

{- Note [Markdown]

commonmark (with the GFM bits we want: tables, strikethrough, autolinks,
task lists) builds our own small tree, which keeps the source range of
every node. From that tree come:

- the doc, as rich text (SPEC §11.4.9). No raw HTML and no images, as
  for Haddock (D-151): HTML is shown as text, an image is a link.
  Relative links that name a file of the snapshot link to it; links to
  @#anchor@ go to a heading of the page.
- the runs of the source view, with the SPEC §5.6 classes (D-327):
  headings keyword, code literal, markers operator, links typeCon, HTML
  comment. @haskell@ / @hs@ code blocks are lexed by GHC's lexer.

Positions. commonmark's columns count a tab up to the next multiple of
4, and its tokenizer normalises the text to NFC. We tokenize ourselves
(the same rules, no NFC, a lone @\\r@ is no line end, as for GHC) and
map each column back to ours per line (Note [Span convention] in
"HaskellReviewer.Core.Span"). Block ranges are whole lines: a list of
(start, start of next line), one per line, joined when they touch.
-}

-- * Files

-- | @.md@ or @.markdown@, any case.
isMarkdownPath :: RelPath -> Bool
isMarkdownPath p = any (`Text.isSuffixOf` name) [".md", ".markdown"]
  where
    name = Text.toLower (relPathName p)

-- * Parsing

-- | What links need.
data MarkdownEnv = MarkdownEnv
  { -- | The Markdown file.
    path :: RelPath,
    -- | Whether a file is in the snapshot.
    exists :: RelPath -> Bool
  }

-- | A parsed file.
data Markdown = Markdown
  { doc :: Doc,
    -- | Sorted, not overlapping.
    runs :: [Run]
  }

-- | Parse a file's text. Never fails: commonmark accepts anything.
parseMarkdown :: MarkdownEnv -> Text -> Markdown
parseMarkdown env raw = case runIdentity (parseCommonmarkWith syntax (tokenize text)) of
  Right (Bls bs) ->
    Markdown
      { doc = Doc {blocks = anchors (concatMap (block env) bs), since = Nothing},
        runs = cleanRuns ls (concatMap (blockMarks ls) bs)
      }
  -- Not expected.
  Left _ ->
    Markdown
      { doc = Doc {blocks = [Listing (plainListing text)], since = Nothing},
        runs = []
      }
  where
    text = stripBom raw
    ls = fileLines text

syntax :: SyntaxSpec Identity Ils Bls
-- Extensions first: they win over the default specs.
syntax =
  strikethroughSpec
    <> pipeTableSpec
    <> autolinkSpec
    <> taskListSpec
    <> defaultSyntaxSpec

-- * The tree

-- | Where a node is: commonmark's (start, end) pairs.
type Range = [(SourcePos, SourcePos)]

newtype Ils = Ils (Seq Il)
  deriving (Show)

data Il = Il
  { range :: Range,
    node :: IlNode
  }
  deriving (Show)

data IlNode
  = IStr Text
  | ISpace
  | IEmph Ils
  | IStrong Ils
  | IStrike Ils
  | ILink Text Ils
  | IImage Text Ils
  | ICode Text
  | IRaw Text
  | -- | A range around several nodes.
    IGroup Ils
  deriving (Show)

newtype Bls = Bls (Seq Bl)
  deriving (Show)

data Bl = Bl
  { range :: Range,
    node :: BlNode
  }
  deriving (Show)

data BlNode
  = BPara Ils
  | BBreak
  | BQuote Bls
  | -- | Info string, text.
    BCode Text Text
  | BHeading Int Ils
  | BRaw Text
  | BRefDef
  | BList ListType [Bls]
  | BTasks [(Bool, Bls)]
  | -- | Header cells, rows.
    BTable [Ils] [[Ils]]
  | BGroup Bls
  deriving (Show)

instance Semigroup Ils where
  Ils a <> Ils b = Ils (a <> b)

instance Monoid Ils where
  mempty = Ils Seq.empty

instance Semigroup Bls where
  Bls a <> Bls b = Bls (a <> b)

instance Monoid Bls where
  mempty = Bls Seq.empty

il :: IlNode -> Ils
il n = Ils (Seq.singleton (Il [] n))

bl :: BlNode -> Bls
bl n = Bls (Seq.singleton (Bl [] n))

instance Rangeable Ils where
  ranged (SourceRange r) (Ils xs) = case toList xs of
    [Il [] n] -> Ils (Seq.singleton (Il r n))
    _ -> Ils (Seq.singleton (Il r (IGroup (Ils xs))))

instance Rangeable Bls where
  ranged (SourceRange r) (Bls xs) = case toList xs of
    [Bl [] n] -> Bls (Seq.singleton (Bl r n))
    _ -> Bls (Seq.singleton (Bl r (BGroup (Bls xs))))

instance HasAttributes Ils where
  addAttributes _ x = x

instance HasAttributes Bls where
  addAttributes _ x = x

instance IsInline Ils where
  lineBreak = il ISpace
  softBreak = il ISpace
  str = il . IStr
  entity t = il (IStr (fromMaybe t (lookupEntity (Text.drop 1 t))))
  escapedChar c = il (IStr (Text.singleton c))
  emph = il . IEmph
  strong = il . IStrong
  link dest _ d = il (ILink dest d)
  image src _ d = il (IImage src d)
  code = il . ICode
  rawInline _ t = il (IRaw t)

instance IsBlock Ils Bls where
  paragraph = bl . BPara
  plain = bl . BPara
  thematicBreak = bl BBreak
  blockQuote = bl . BQuote
  codeBlock info t = bl (BCode info t)
  heading n = bl . BHeading n
  rawBlock _ t = bl (BRaw t)
  referenceLinkDefinition _ _ = bl BRefDef
  list t _ items = bl (BList t items)

instance HasStrikethrough Ils where
  strikethrough = il . IStrike

instance HasPipeTable Ils Bls where
  pipeTable _ header rows = bl (BTable header rows)

instance HasTaskList Ils Bls where
  taskList _ _ items = bl (BTasks items)

-- * Tokens

-- | commonmark's tokenizer, without NFC, and a lone @\\r@ is a symbol.
-- See Note [Markdown]. A @\\r@ is a group of its own (groupBy compares
-- with a group's first char, so @\\r\\n\\n@ would be one group): the
-- @\\n@ after it is joined here.
tokenize :: Text -> [Tok]
tokenize = go (initialPos "") . Text.groupBy together . Text.replace "\x0" "\xFFFD"
  where
    together ' ' ' ' = True
    together x y = wordChar x && wordChar y
    wordChar c = isAlphaNum c || isMark c
    go _ [] = []
    go pos (t : ts) = case maybe ' ' fst (Text.uncons t) of
      ' ' -> Tok Spaces pos t : go (col pos (Text.length t)) ts
      '\t' -> Tok Spaces pos t : go (col pos (4 - (sourceColumn pos - 1) `mod` 4)) ts
      '\n' -> Tok LineEnd pos t : go (nextLine pos) ts
      '\r' -> case ts of
        "\n" : rest -> Tok LineEnd pos "\r\n" : go (nextLine pos) rest
        _ -> Tok (Symbol '\r') pos t : go (col pos 1) ts
      c
        | wordChar c -> Tok WordChars pos t : go (col pos (Text.length t)) ts
        | isSpace c -> Tok UnicodeSpace pos t : go (col pos 1) ts
        | otherwise -> Tok (Symbol c) pos t : go (col pos 1) ts
    col pos n = setCol pos (sourceColumn pos + n)
    nextLine pos = setLine (setCol pos 1) (sourceLine pos + 1)

setCol :: SourcePos -> Int -> SourcePos
setCol = setSourceColumn

setLine :: SourcePos -> Int -> SourcePos
setLine = setSourceLine

-- * Lines and columns

-- | The file by line, without the @\\r@ of a CRLF. Lengths and tab
-- maps are worked out once a line: a file can be one huge line with
-- many marks on it.
data FileLines = FileLines
  { texts :: Vector.Vector Text,
    -- | In code points.
    lengths :: Unboxed.Vector Int,
    -- | Lines with tabs: our column for each commonmark column, from 1.
    -- Each built on first use (a boxed vector holds thunks).
    tabMaps :: Vector.Vector (Maybe (Unboxed.Vector Int))
  }

fileLines :: Text -> FileLines
fileLines t =
  FileLines
    { texts = ls,
      lengths = Unboxed.fromList (map Text.length (Vector.toList ls)),
      tabMaps = Vector.map tabMap ls
    }
  where
    ls = Vector.fromList (map noCr (Text.splitOn "\n" t))
    noCr l = fromMaybe l (Text.stripSuffix "\r" l)
    -- A tab spans up to the next multiple of 4; each of its columns is
    -- the tab's. The last entry is the line's end.
    tabMap l
      | Text.any (== '\t') l = Just (Unboxed.fromList (go 1 1 (Text.unpack l)))
      | otherwise = Nothing
    go vis cp = \case
      [] -> [cp]
      ch : rest ->
        let w = if ch == '\t' then 4 - (vis - 1) `mod` 4 else 1
         in replicate w cp <> go (vis + w) (cp + 1) rest

lineAt :: FileLines -> Int -> Text
lineAt ls n = fromMaybe "" (ls.texts Vector.!? (n - 1))

lineLength :: FileLines -> Int -> Int
lineLength ls n = fromMaybe 0 (ls.lengths Unboxed.!? (n - 1))

-- | A place in our columns.
data Pos = Pos Int Int
  deriving (Eq, Ord, Show)

-- | commonmark's column on a line, as ours. A column inside a tab is
-- the tab's.
ourCol :: FileLines -> Int -> Int -> Int
ourCol ls n c = case join (ls.tabMaps Vector.!? (n - 1)) of
  Just m
    | c >= 1, c <= Unboxed.length m -> m Unboxed.! (c - 1)
    -- Past the end.
    | c > Unboxed.length m -> Unboxed.last m + (c - Unboxed.length m)
  _ -> c

ourPos :: FileLines -> SourcePos -> Pos
ourPos ls p = Pos (sourceLine p) (ourCol ls (sourceLine p) (sourceColumn p))

-- | Each line of a block's range, with the column (ours) it starts at.
rangeLines :: FileLines -> Range -> [(Int, Int)]
rangeLines ls = concatMap seg
  where
    seg (a, b) =
      let first = sourceLine a
          lastL = if sourceColumn b == 1 then sourceLine b - 1 else sourceLine b
       in [ (n, if n == first then ourCol ls n (sourceColumn a) else 1)
          | n <- [first .. lastL]
          ]

-- | The rest of a line, from a column.
lineFrom :: FileLines -> Int -> Int -> Text
lineFrom ls n c = Text.drop (c - 1) (lineAt ls n)

-- | One mark, to the end of the line.
toEnd :: FileLines -> DisplayClass -> (Int, Int) -> Mark
toEnd ls d (n, c) = Mark (Pos n c) (Pos n (lineLength ls n + 1)) d

-- * Marks

-- | A run before cleaning.
data Mark = Mark Pos Pos DisplayClass

-- | Sorted, no overlap, no empty run, ends on a real line.
cleanRuns :: FileLines -> [Mark] -> [Run]
cleanRuns ls = go (Pos 0 0) . sortOn (\(Mark a _ _) -> a) . map clip
  where
    go _ [] = []
    go prev (Mark a b d : rest)
      | a' < b = run a' b d : go b rest
      | otherwise = go prev rest
      where
        a' = max prev a
    -- (n + 1, 1) is the end of line n.
    clip (Mark a (Pos n 1) d)
      | n > 1 = Mark a (Pos (n - 1) (lineLength ls (n - 1) + 1)) d
    clip m = m
    run (Pos l1 c1) (Pos l2 c2) d =
      Run
        { span = Span (Line (w l1)) (Col (w c1)) (Line (w l2)) (Col (w c2)),
          display = d
        }
    w = fromIntegral

blockMarks :: FileLines -> Bl -> [Mark]
blockMarks ls b = case b.node of
  BPara is -> inlineMarks ls is
  BBreak -> map (toEnd ls OperatorClass) lns
  BQuote (Bls bs) -> mapMaybe quoteMark lns <> concatMap (blockMarks ls) bs
  BCode info _ -> codeMarks ls info lns
  BHeading _ _ -> map (toEnd ls KeywordClass) lns
  BRaw _ -> map (toEnd ls CommentClass) lns
  BRefDef -> map (toEnd ls TypeConClass) lns
  BList _ items -> concatMap item items
  BTasks items -> concatMap (item . snd) items
  BTable header rows ->
    map (toEnd ls OperatorClass) (take 1 (filter delimiterRow lns))
      <> concatMap (inlineMarks ls) (header <> concat rows)
  BGroup (Bls bs) -> concatMap (blockMarks ls) bs
  where
    lns = rangeLines ls b.range
    -- @|---|:-:|@: only these chars, and a @-@.
    delimiterRow (n, c) =
      let l = Text.strip (lineFrom ls n c)
       in Text.any (== '-') l && Text.all (`elem` ("|-: \t" :: String)) l
    -- @>@ after up to 3 spaces.
    quoteMark (n, c) =
      let l = lineFrom ls n c
          sp = Text.length (Text.takeWhile (== ' ') l)
       in if sp <= 3 && Text.take 1 (Text.drop sp l) == ">"
            then Just (Mark (Pos n (c + sp)) (Pos n (c + sp + 1)) OperatorClass)
            else Nothing
    item (Bls xs) =
      concatMap (\x -> markerMarks ls x.range) (take 1 (toList xs))
        <> concatMap (blockMarks ls) xs

-- | A list item's range starts at its marker (@-@, @1.@, ...), maybe
-- followed by a task box.
markerMarks :: FileLines -> Range -> [Mark]
markerMarks ls r = case rangeLines ls r of
  (n, c) : _ ->
    let l = lineFrom ls n c
        bullet = Text.take 1 l `elem` ["-", "+", "*"]
        digits = Text.length (Text.takeWhile isDigit l)
        k
          | bullet = 1
          | digits > 0, Text.take 1 (Text.drop digits l) `elem` [".", ")"] = digits + 1
          | otherwise = 0
        afterK = Text.drop k l
        sp = Text.length (Text.takeWhile (== ' ') afterK)
        box = Text.take 3 (Text.drop sp afterK) `elem` ["[ ]", "[x]", "[X]"]
     in [Mark (Pos n c) (Pos n (c + k)) OperatorClass | k > 0]
          <> [ Mark (Pos n (c + k + sp)) (Pos n (c + k + sp + 3)) OperatorClass
             | k > 0,
               box
             ]
  [] -> []

codeMarks :: FileLines -> Text -> [(Int, Int)] -> [Mark]
codeMarks ls info lns = case lns of
  first : rest
    | Just fence <- fenceOf (lineFrom ls (fst first) (snd first)) ->
        let (body, close) = case reverse rest of
              l : before
                | isClose fence (lineFrom ls (fst l) (snd l)) -> (reverse before, [l])
              _ -> (rest, [])
         in toEnd ls OperatorClass first
              : map (toEnd ls OperatorClass) close
                <> bodyMarks body
  _ -> map (toEnd ls LiteralClass) lns
  where
    bodyMarks body
      | isHaskell info,
        Just ts <- lexSnippet (masked body) =
          [ m
          | t <- tokenList ts,
            Just d <- [displayClass t.kind],
            m <- perLine body d t
          ]
      | otherwise = map (toEnd ls LiteralClass) body
    -- The body lines as in the file, what's before the block's column
    -- blank, so the tokens' columns are the file's. Lines from 1: the
    -- tokens are shifted after.
    masked body = Text.intercalate "\n" [blankTo k (lineAt ls n) | (n, k) <- body]
    shift = case lns of
      (n0, _) : _ -> n0
      [] -> 0
    blankTo k t = Text.replicate (k - 1) " " <> Text.drop (k - 1) t
    -- A token over several lines (a block comment), one mark a line,
    -- each from the body's column: in a quote, the @>@ stays a marker.
    perLine body d t =
      let l1 = shift + lineOf t.span.startLine
          l2 = shift + lineOf t.span.endLine
          c1 = colOf t.span.startCol
          c2 = colOf t.span.endCol
          from n = if n == l1 then c1 else fromMaybe 1 (lookup n body)
          to n = if n == l2 then c2 else lineLength ls n + 1
       in [Mark (Pos n (from n)) (Pos n (to n)) d | n <- [l1 .. l2]]
    lineOf (Line n) = fromIntegral n
    colOf (Col n) = fromIntegral n

-- | The fence char and length, if the line opens a fence.
fenceOf :: Text -> Maybe (Char, Int)
fenceOf l0 = case Text.uncons l of
  Just (ch, _)
    | ch == '`' || ch == '~',
      n <- Text.length (Text.takeWhile (== ch) l),
      n >= 3 ->
        Just (ch, n)
  _ -> Nothing
  where
    l = Text.stripStart l0

isClose :: (Char, Int) -> Text -> Bool
isClose (ch, n) l0 =
  let l = Text.strip l0
   in Text.length l >= n && Text.all (== ch) l

isHaskell :: Text -> Bool
isHaskell info = case Text.words (Text.toLower info) of
  w : _ -> w `elem` ["haskell", "hs"]
  [] -> False

inlineMarks :: FileLines -> Ils -> [Mark]
inlineMarks ls (Ils xs) = concatMap one (toList xs)
  where
    one x = case x.node of
      IStr _ -> []
      ISpace -> []
      IEmph is -> inlineMarks ls is
      IStrong is -> inlineMarks ls is
      IStrike is -> inlineMarks ls is
      ILink _ _ -> marks TypeConClass x.range
      IImage _ _ -> marks TypeConClass x.range
      ICode _ -> marks LiteralClass x.range
      IRaw _ -> marks CommentClass x.range
      IGroup is -> inlineMarks ls is
    marks d = map (\(a, b) -> Mark (ourPos ls a) (ourPos ls b) d)

-- * The doc

block :: MarkdownEnv -> Bl -> [Block]
block env b = case b.node of
  BPara is -> case inlines env is of
    [] -> []
    xs -> [Paragraph xs]
  BBreak -> [ThematicBreak]
  BQuote bs -> [BlockQuote (blocks env bs)]
  BCode info t -> [Listing (listing info t)]
  BHeading n is -> [Heading Header {level = n, title = inlines env is, anchor = Nothing}]
  BRaw t
    | isComment t -> []
    | otherwise -> [Listing (plainListing t)]
  BRefDef -> []
  BList (CM.BulletList _) items -> [UnorderedList (map (blocks env) items)]
  BList (CM.OrderedList start _ _) items ->
    [ OrderedList
        [ ListItem {number = Just k, content = blocks env i}
        | (k, i) <- zip [start ..] items
        ]
    ]
  BTasks items -> [TaskList [TaskItem {done = d, content = blocks env i} | (d, i) <- items]]
  BTable header rows ->
    [ Table
        TableBody
          { header = [map cell header | not (null header)],
            body = map (map cell) rows
          }
    ]
  BGroup bs -> blocks env bs
  where
    cell is = Cell {colspan = 1, rowspan = 1, content = inlines env is}
    isComment t =
      let s = Text.strip t
       in "<!--" `Text.isPrefixOf` s && "-->" `Text.isSuffixOf` s

blocks :: MarkdownEnv -> Bls -> [Block]
blocks env (Bls bs) = concatMap (block env) bs

-- | Text as one plain piece.
plainListing :: Text -> CodeListing
plainListing t = CodeListing {lang = Nothing, pieces = [Piece Nothing t]}

-- | A code block: Haskell cut by token, the rest one piece.
listing :: Text -> Text -> CodeListing
listing info t =
  CodeListing
    { lang = case Text.words info of
        w : _ -> Just w
        [] -> Nothing,
      pieces =
        if isHaskell info
          then maybe plainPieces lexed (lexSnippet t)
          else plainPieces
    }
  where
    plainPieces = [Piece Nothing t]
    src = sourceLines t
    nLines = length (Text.splitOn "\n" t)
    slice a b = sliceSpan src (Span (fst a) (snd a) (fst b) (snd b))
    lexed ts =
      let coloured = [(tk, d) | tk <- tokenList ts, Just d <- [displayClass tk.kind]]
          go cur = \case
            [] -> [Piece Nothing (slice cur (Line (fromIntegral nLines), Col maxBound))]
            (tk, d) : rest ->
              let s = (tk.span.startLine, tk.span.startCol)
                  e = (tk.span.endLine, tk.span.endCol)
               in Piece Nothing (slice cur s) : Piece (Just d) (slice s e) : go e rest
       in merge (filter (\p -> not (Text.null p.text)) (go (Line 1, Col 1) coloured))
    merge = \case
      Piece Nothing a : Piece Nothing b : rest -> merge (Piece Nothing (a <> b) : rest)
      p : rest -> p : merge rest
      [] -> []

inlines :: MarkdownEnv -> Ils -> [Inline]
inlines env (Ils xs) = joinStrs (concatMap one (toList xs))
  where
    one x = case x.node of
      IStr t -> [Str t]
      ISpace -> [Str " "]
      IEmph is -> [Emphasis (inlines env is)]
      IStrong is -> [Bold (inlines env is)]
      IStrike is -> [Strike (inlines env is)]
      ILink dest d -> linkTo env dest (inlines env d)
      IImage src d -> linkTo env src (inlines env d)
      ICode t -> [Code [Str t]]
      IRaw t -> [Str t]
      IGroup is -> inlines env is

joinStrs :: [Inline] -> [Inline]
joinStrs = \case
  Str a : Str b : rest -> joinStrs (Str (a <> b) : rest)
  x : rest -> x : joinStrs rest
  [] -> []

-- | A link or an image: to the web (http, https), to a heading, or to a
-- file of the snapshot. Anything else is text, as for Haddock (D-151).
linkTo :: MarkdownEnv -> Text -> [Inline] -> [Inline]
linkTo env dest0 label0
  | Just a <- Text.stripPrefix "#" dest,
    not (Text.null a) =
      [AnchorLink AnchorRef {anchor = percentDecode a, label = label}]
  | hasScheme dest || "//" `Text.isPrefixOf` dest = case safeUrl dest of
      Just u -> [Link Hyperlink {url = u, label = label}]
      Nothing -> asText
  | Just p <- resolvePath env.path pathPart,
    env.exists p =
      [FileLink FileRef {path = p, fragment = fragment, label = label}]
  | otherwise = asText
  where
    dest = Text.strip dest0
    label = if null label0 then Nothing else Just label0
    (beforeHash, hashPart) = Text.breakOn "#" dest
    fragment = case Text.drop 1 hashPart of
      "" -> Nothing
      f -> Just (percentDecode f)
    pathPart = Text.takeWhile (/= '?') beforeHash
    asText = case label of
      Just is -> is <> [Str (" <" <> dest <> ">")]
      Nothing -> [Str dest]

-- | @scheme:@ at the start.
hasScheme :: Text -> Bool
hasScheme t = case Text.uncons t of
  Just (c, rest)
    | ascii c ->
        let schemeChar x = ascii x || isDigit x || x `elem` ("+.-" :: String)
            (s, after) = Text.span schemeChar rest
         in Text.take 1 after == ":" && Text.length s < 32
  _ -> False
  where
    ascii x = isAsciiLower x || isAsciiUpper x

-- | A relative link from a file, as a path from the root: from the
-- file's directory, or from the root with a leading @/@. Percent
-- escapes decoded. 'Nothing' if it leaves the root or is no file.
resolvePath :: RelPath -> Text -> Maybe RelPath
resolvePath from target
  | Text.null target = Nothing
  | otherwise = relPathFromSegments =<< walk start (Text.splitOn "/" (percentDecode target))
  where
    start
      | "/" `Text.isPrefixOf` target = []
      | otherwise = drop 1 (reverse (relPathSegments from))
    -- Reversed segments so far.
    walk acc = \case
      [] -> Just (reverse acc)
      s : rest
        | s == "" || s == "." -> walk acc rest
        | s == ".." -> case acc of
            _ : up -> walk up rest
            [] -> Nothing
        | otherwise -> walk (s : acc) rest

-- | @%20@ and the like, as UTF-8. If the result isn't UTF-8, the text
-- as it is.
percentDecode :: Text -> Text
percentDecode t
  | Text.any (== '%') t =
      either (const t) id (Text.Encoding.decodeUtf8' (BS.pack (go (BS.unpack bytes))))
  | otherwise = t
  where
    bytes = Text.Encoding.encodeUtf8 t
    go = \case
      37 : h : l : rest
        | Just x <- hex h,
          Just y <- hex l ->
            x * 16 + y : go rest
      b : rest -> b : go rest
      [] -> []
    hex b
      | b >= 48 && b <= 57 = Just (b - 48)
      | b >= 97 && b <= 102 = Just (b - 87)
      | b >= 65 && b <= 70 = Just (b - 55)
      | otherwise = Nothing

-- * Anchors

-- | Give each heading its anchor, in page order, like GitHub: a repeat
-- gets @-1@, @-2@, ..., skipping any already taken (@# Foo-1@ first).
anchors :: [Block] -> [Block]
anchors = snd . goBlocks Map.empty
  where
    goBlocks = mapAccumL goBlock
    goBlock seen = \case
      Heading h ->
        let base = slugOf (plainText h.title)
            (seen', a) = unique seen base
         in (seen', Heading Header {level = h.level, title = h.title, anchor = Just a})
      BlockQuote bs -> BlockQuote <$> goBlocks seen bs
      UnorderedList items -> UnorderedList <$> mapAccumL goBlocks seen items
      OrderedList items ->
        OrderedList
          <$> mapAccumL
            ( \s i ->
                (\c -> ListItem {number = i.number, content = c}) <$> goBlocks s i.content
            )
            seen
            items
      TaskList items ->
        TaskList
          <$> mapAccumL
            ( \s i ->
                (\c -> TaskItem {done = i.done, content = c}) <$> goBlocks s i.content
            )
            seen
            items
      other -> (seen, other)
    -- seen: each anchor taken, with the next number to try after it.
    unique seen base = case Map.lookup base seen of
      Nothing -> (Map.insert base (1 :: Int) seen, base)
      Just k ->
        let next = base <> "-" <> Text.pack (show k)
            seen' = Map.insert base (k + 1) seen
         in if Map.member next seen
              then unique seen' base
              else (Map.insert next 1 seen', next)

-- | GitHub's heading anchor: lower case, only letters, digits, @-@ and
-- @_@ kept, spaces to @-@.
slugOf :: Text -> Text
slugOf = Text.map dash . Text.filter keep . Text.map toLower . Text.strip
  where
    keep c = isAlphaNum c || isMark c || c == '-' || c == '_' || c == ' '
    dash c = if c == ' ' then '-' else c

plainText :: [Inline] -> Text
plainText = foldMap $ \case
  Str t -> t
  Emphasis is -> plainText is
  Bold is -> plainText is
  Code is -> plainText is
  Math _ t -> t
  Link l -> maybe "" plainText l.label
  Name r -> r.written
  ModuleLink l -> maybe "" plainText l.label
  Strike is -> plainText is
  FileLink l -> maybe "" plainText l.label
  AnchorLink l -> maybe "" plainText l.label
