{-# LANGUAGE StrictData #-}

-- | Highlighting for other languages, from rule files (SPEC §5.6).
-- Pure: the caller reads the files. See Note [Language rules].
module HaskellReviewer.Core.Lang
  ( -- * Rules
    Lang (..),
    BlockComment (..),
    StringRule (..),
    ParsedLang (..),
    parseLang,

    -- * All the languages
    Langs,
    noLangs,
    buildLangs,
    langFor,
    langCount,
    forceLangs,

    -- * Scanning
    scanLang,
  )
where

import Data.Char (isAlphaNum, isControl, isDigit, isSpace)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Unsafe qualified as Text.Unsafe
import Data.Word (Word32)
import HaskellReviewer.Core.Config (ConfigWarning, Warn, quoted, typeName, walkToml)
import HaskellReviewer.Core.Highlight (Run (..))
import HaskellReviewer.Core.RelPath (RelPath, relPathName)
import HaskellReviewer.Core.SourceText (stripBom)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (DisplayClass (..))
import Toml.Semantics (Table' (..), Value' (..), valueAnn)
import Toml.Syntax (Position (..))

{- Note [Language rules]

Files that are not Haskell or Markdown get their colours from rule
files: one TOML file per language, in the user config dir
(highlight/*.toml). Nothing about a language is in the code. `make
install` copies the files we ship (highlight/ in the repo).

  extensions = ["scm", "ss"]       # needed; any case, no dot
  word-chars = "!$%&*/:<=>?^_~+-.#" # in words, on top of letters and digits
  line-comments = [";"]
  numbers = true                   # a word that starts with a digit: literal

  [[block-comments]]
  open = "#|"
  close = "|#"
  nested = true                    # default false

  [[strings]]
  open = "\""
  close = "\""
  escape = "\\"                    # one character; optional

  [words]                          # class = words
  keyword = ["define", "lambda"]
  literal = ["#t", "#f"]

The classes are the SPEC §5.6 ones: keyword, typeCon, variable,
operator, literal, comment, pragma. Comments are comment, strings are
literal. A word in no list stays plain.

'scanLang' reads the text once, left to right. At each place it tries,
in this order: a block comment, a line comment, a string, a word. A
comment or string with no end goes to the end of the file. Runs can
span lines; the client cuts them.

Problems are warnings, as for config.toml (Note [Config file] in
"HaskellReviewer.Core.Config"): a bad key is ignored, and a file with no
usable extension is ignored. A delimiter can't be empty or have a
space in it.
-}

-- * Rules

-- | One language. See Note [Language rules].
data Lang = Lang
  { -- | Lower case, no dot.
    extensions :: [Text],
    -- | Word characters that aren't letters or digits.
    wordChars :: Set Char,
    lineComments :: [Text],
    blockComments :: [BlockComment],
    strings :: [StringRule],
    numbers :: Bool,
    wordClasses :: Map Text DisplayClass
  }
  deriving (Eq, Show)

data BlockComment = BlockComment
  { open :: Text,
    close :: Text,
    nested :: Bool
  }
  deriving (Eq, Show)

data StringRule = StringRule
  { open :: Text,
    close :: Text,
    -- | The next character doesn't close the string.
    escape :: Maybe Char
  }
  deriving (Eq, Show)

-- | What a rule file gives.
data ParsedLang = ParsedLang
  { -- | 'Nothing': file ignored.
    lang :: Maybe Lang,
    -- | In file order.
    warnings :: [ConfigWarning]
  }
  deriving (Eq, Show)

-- | The rules of a file, and its problems. See Note [Language rules].
parseLang :: Text -> ParsedLang
parseLang = uncurry ParsedLang . walkToml Nothing walk
  where
    walk (MkTable top) =
      let (l, ws) = Map.foldlWithKey' langKey (emptyLang, []) top
       in if null l.extensions
            then (Nothing, (Position 0 1 1, "no extensions; file ignored") : ws)
            else (Just l, ws)

emptyLang :: Lang
emptyLang =
  Lang
    { extensions = [],
      wordChars = Set.empty,
      lineComments = [],
      blockComments = [],
      strings = [],
      numbers = False,
      wordClasses = Map.empty
    }

-- * All the languages

-- | The languages, by extension.
newtype Langs = Langs (Map Text Lang)
  deriving (Eq, Show)

noLangs :: Langs
noLangs = Langs Map.empty

-- | The first language to claim an extension keeps it. Also gives the
-- claims that lost: whose, and the extension.
buildLangs :: [(a, Lang)] -> (Langs, [(a, Text)])
buildLangs = done . foldl' add (Map.empty, [])
  where
    done (m, lost) = (Langs m, reverse lost)
    add acc (name, l) = foldl' (claim name l) acc l.extensions
    claim name l (m, lost) ext
      | Map.member ext m = (m, (name, ext) : lost)
      | otherwise = (Map.insert ext l m, lost)

-- | The language of a file, from its extension (any case).
langFor :: Langs -> RelPath -> Maybe Lang
langFor (Langs m) path
  | Map.null m = Nothing
  | otherwise = case Text.breakOnEnd "." (relPathName path) of
      ("", _) -> Nothing
      (_, ext) -> Map.lookup (Text.toLower ext) m

-- | How many extensions have rules.
langCount :: Langs -> Int
langCount (Langs m) = Map.size m

-- | Build everything inside, so request threads find no thunk
-- (Q-RTS-BLACKHOLE).
forceLangs :: Langs -> ()
forceLangs (Langs m) = Map.foldl' (\() l -> one l) () m
  where
    one l =
      sum (map Text.length (l.extensions <> l.lineComments)) `seq`
        foldr seq () l.blockComments `seq`
          foldr seq () l.strings `seq`
            Set.size l.wordChars `seq`
              Map.foldl' (flip seq) () l.wordClasses

-- * Scanning

-- | Where we are in the file.
data Pos = Pos Word32 Word32

-- | The runs of a file in this language, sorted, not overlapping. Linear
-- in the text. See Note [Language rules].
scanLang :: Lang -> Text -> [Run]
scanLang l = go (Pos 1 1) . noLastCr . stripBom
  where
    -- An empty delimiter would match everywhere.
    full = not . Text.null
    blocks = [b | b <- l.blockComments, full b.open, full b.close]
    lines' = filter full l.lineComments
    strs = [s | s <- l.strings, full s.open, full s.close]
    -- Only these characters can start a comment or a string.
    starts =
      Set.fromList . map Text.Unsafe.unsafeHead $
        map (.open) blocks <> lines' <> map (.open) strs
    isWord c = isAlphaNum c || Set.member c l.wordChars

    go p t = case Text.uncons t of
      Nothing -> []
      Just (c, rest)
        | Set.member c starts, Just (cls, after) <- delimited t -> emit p t after cls
        | isWord c ->
            let (w, after) = Text.span isWord t
                p' = over w p
             in case wordClass w of
                  Just cls -> run p p' cls : go p' after
                  Nothing -> go p' after
        | otherwise -> go (step c p) rest

    -- A run from here to where the rest starts.
    emit p t after cls = run p p' cls : go p' after
      where
        p' = over (Text.Unsafe.takeWord8 used t) p
        used = Text.Unsafe.lengthWord8 t - Text.Unsafe.lengthWord8 after

    -- A comment or a string that starts here: its class and what
    -- follows it.
    delimited t
      | Just b <- find (\b -> b.open `Text.isPrefixOf` t) blocks =
          Just (CommentClass, blockEnd b 1 (dropLen b.open t))
      | Just d <- find (`Text.isPrefixOf` t) lines' =
          Just (CommentClass, lineEnd (dropLen d t))
      | Just s <- find (\s -> s.open `Text.isPrefixOf` t) strs =
          Just (LiteralClass, stringEnd s (dropLen s.open t))
      | otherwise = Nothing

    -- The line end stays out, and the \r of a CRLF.
    lineEnd t =
      let (body, after) = Text.break (== '\n') t
       in if "\r" `Text.isSuffixOf` body && full after
            then Text.Unsafe.dropWord8 (Text.Unsafe.lengthWord8 body - 1) t
            else after

    blockEnd :: BlockComment -> Int -> Text -> Text
    blockEnd b depth t0
      | Text.null t = t
      | Just after <- Text.stripPrefix b.close t =
          if depth == 1 then after else blockEnd b (depth - 1) after
      | b.nested, Just after <- Text.stripPrefix b.open t = blockEnd b (depth + 1) after
      | otherwise = blockEnd b depth (Text.drop 1 t)
      where
        c1 = Text.Unsafe.unsafeHead b.close
        c2 = Text.Unsafe.unsafeHead b.open
        t = Text.dropWhile (\c -> c /= c1 && c /= c2) t0

    stringEnd s t0
      | Text.null t = t
      | Just after <- Text.stripPrefix s.close t = after
      | Just (c, after) <- Text.uncons t,
        Just c == s.escape =
          stringEnd s (Text.drop 1 after)
      | otherwise = stringEnd s (Text.drop 1 t)
      where
        c1 = Text.Unsafe.unsafeHead s.close
        t = Text.dropWhile (\c -> c /= c1 && Just c /= s.escape) t0

    wordClass w = case Map.lookup w l.wordClasses of
      Just cls -> Just cls
      Nothing
        | l.numbers, isDigit (Text.Unsafe.unsafeHead w) -> Just LiteralClass
        | otherwise -> Nothing

    dropLen d = Text.Unsafe.dropWord8 (Text.Unsafe.lengthWord8 d)

-- | The last line's @\\r@ isn't part of the line (as 'sourceLines').
noLastCr :: Text -> Text
noLastCr t = fromMaybe t (Text.stripSuffix "\r" t)

run :: Pos -> Pos -> DisplayClass -> Run
run (Pos l1 c1) (Pos l2 c2) cls =
  Run {span = Span (Line l1) (Col c1) (Line l2) (Col c2), display = cls}

-- | After one character.
step :: Char -> Pos -> Pos
step '\n' (Pos l _) = Pos (l + 1) 1
step _ (Pos l c) = Pos l (c + 1)

-- | After some text.
over :: Text -> Pos -> Pos
over t p = Text.foldl' (flip step) p t

-- * Internal: the TOML walk

type Acc = (Lang, [Warn])

langKey :: Acc -> Text -> (Position, Value' Position) -> Acc
langKey (l, ws) key (keyPos, value) = case key of
  "extensions" ->
    let (xs, w) = listOf key extension value in (l {extensions = xs}, w <> ws)
  "word-chars" -> case value of
    Text' pos t
      | Text.any spaceLike t ->
          (l, (pos, "word-chars: a space or control character") : ws)
      | otherwise -> (l {wordChars = Set.fromList (Text.unpack t)}, ws)
    _ -> (l, wrongType key "a string" value : ws)
  "line-comments" ->
    let (xs, w) = listOf key delimiter value in (l {lineComments = xs}, w <> ws)
  "numbers" -> case value of
    Bool' _ b -> (l {numbers = b}, ws)
    _ -> (l, wrongType key "a boolean" value : ws)
  "block-comments" ->
    let (xs, w) = tablesOf key blockComment value in (l {blockComments = xs}, w <> ws)
  "strings" ->
    let (xs, w) = tablesOf key stringRule value in (l {strings = xs}, w <> ws)
  "words" -> case value of
    Table' _ (MkTable classes) ->
      let (m, w) = Map.foldlWithKey' wordsKey (Map.empty, []) classes
       in (l {wordClasses = m}, w <> ws)
    _ -> (l, wrongType key "a table" value : ws)
  _ -> case value of
    Table' _ _ -> (l, (keyPos, "unknown table [" <> key <> "]") : ws)
    _ -> (l, (keyPos, "unknown key " <> quoted key) : ws)

-- | One class of @[words]@. A word in two classes: the first class in
-- alphabetical order.
wordsKey ::
  (Map Text DisplayClass, [Warn]) ->
  Text ->
  (Position, Value' Position) ->
  (Map Text DisplayClass, [Warn])
wordsKey (m, ws) key (keyPos, value) = case classFromName key of
  Just cls ->
    let (xs, w) = listOf ("words." <> key) word value
     in (Map.union m (Map.fromList [(x, cls) | x <- xs]), w <> ws)
  Nothing ->
    ( m,
      (keyPos, "words." <> key <> ": unknown class, expected " <> classNames) : ws
    )
  where
    word t = if Text.null t then Left "an empty word" else Right t

classFromName :: Text -> Maybe DisplayClass
classFromName = \case
  "keyword" -> Just KeywordClass
  "typeCon" -> Just TypeConClass
  "variable" -> Just VariableClass
  "operator" -> Just OperatorClass
  "literal" -> Just LiteralClass
  "comment" -> Just CommentClass
  "pragma" -> Just PragmaClass
  _ -> Nothing

classNames :: Text
classNames = "keyword, typeCon, variable, operator, literal, comment or pragma"

blockComment :: Text -> Position -> Table' Position -> (Maybe BlockComment, [Warn])
blockComment name pos (MkTable t) =
  (BlockComment <$> o <*> c <*> n, concat [w1, w2, w3, unknownKeys name known t])
  where
    known = ["open", "close", "nested"]
    (o, w1) = delimiterField name pos t "open"
    (c, w2) = delimiterField name pos t "close"
    (n, w3) = case Map.lookup "nested" t of
      Nothing -> (Just False, [])
      Just (_, Bool' _ b) -> (Just b, [])
      Just (_, v) -> (Nothing, [wrongType (name <> ".nested") "a boolean" v])

stringRule :: Text -> Position -> Table' Position -> (Maybe StringRule, [Warn])
stringRule name pos (MkTable t) =
  (StringRule <$> o <*> c <*> e, concat [w1, w2, w3, unknownKeys name known t])
  where
    known = ["open", "close", "escape"]
    (o, w1) = delimiterField name pos t "open"
    (c, w2) = delimiterField name pos t "close"
    (e, w3) = case Map.lookup "escape" t of
      Nothing -> (Just Nothing, [])
      Just (_, Text' p s) -> case Text.unpack s of
        [ch] -> (Just (Just ch), [])
        _ -> (Nothing, [(p, name <> ".escape: expected one character")])
      Just (_, v) -> (Nothing, [wrongType (name <> ".escape") "a string" v])

-- | A delimiter that must be there.
delimiterField ::
  Text ->
  Position ->
  Map Text (Position, Value' Position) ->
  Text ->
  (Maybe Text, [Warn])
delimiterField name pos t key = case Map.lookup key t of
  Nothing -> (Nothing, [(pos, field <> ": missing")])
  Just (_, Text' p s) -> case delimiter s of
    Right d -> (Just d, [])
    Left why -> (Nothing, [(p, field <> ": " <> why)])
  Just (_, v) -> (Nothing, [wrongType field "a string" v])
  where
    field = name <> "." <> key

unknownKeys :: Text -> [Text] -> Map Text (Position, Value' Position) -> [Warn]
unknownKeys name known t =
  [ (pos, "unknown key " <> quoted (name <> "." <> k))
  | (k, (pos, _)) <- Map.toList t,
    k `notElem` known
  ]

-- | An array of strings. Bad items are left out.
listOf :: Text -> (Text -> Either Text a) -> Value' Position -> ([a], [Warn])
listOf name check = \case
  List' _ vs -> let (ws, xs) = partitionEithers (map item vs) in (xs, ws)
  v -> ([], [wrongType name "an array" v])
  where
    item = \case
      Text' pos t -> either (\why -> Left (pos, name <> ": " <> why)) Right (check t)
      v -> Left (wrongType name "a string" v)

-- | An array of tables. Bad ones are left out.
tablesOf ::
  Text ->
  (Text -> Position -> Table' Position -> (Maybe a, [Warn])) ->
  Value' Position ->
  ([a], [Warn])
tablesOf name entry = \case
  List' _ vs ->
    let rs = map item vs in ([x | (Just x, _) <- rs], concatMap snd rs)
  v -> ([], [wrongType name "an array of tables" v])
  where
    item = \case
      Table' pos t -> entry name pos t
      v -> (Nothing, [wrongType name "a table" v])

wrongType :: Text -> Text -> Value' Position -> Warn
wrongType name expected v =
  (valueAnn v, name <> ": expected " <> expected <> ", got " <> typeName v)

-- | Lower case, without the dot some people will write.
extension :: Text -> Either Text Text
extension t
  | Text.null ext || Text.any bad ext = Left ("bad extension " <> quoted t)
  | otherwise = Right ext
  where
    ext = Text.toLower (fromMaybe t (Text.stripPrefix "." t))
    bad c = c == '.' || c == '/' || spaceLike c

delimiter :: Text -> Either Text Text
delimiter t
  | Text.null t = Left "an empty delimiter"
  | Text.any spaceLike t = Left "a delimiter with a space or control character"
  | otherwise = Right t

spaceLike :: Char -> Bool
spaceLike c = isSpace c || isControl c
