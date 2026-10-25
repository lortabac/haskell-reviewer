{-# LANGUAGE StrictData #-}

-- | Lines and tokens, the way gcc's traditional mode reads them. See
-- Note [Traditional mode] in "HaskellReviewer.Core.Cpp".
module HaskellReviewer.Core.Cpp.Lines
  ( -- * Logical lines
    LLine (..),
    Lines (..),
    allLines,
    nextLine,
    lastLineOf,
    startsDirective,
    Directive (..),
    Ending (..),
    directiveText,
    dropChars,

    -- * Tokens
    Tok (..),
    tokText,
    lexText,
    isIdentChar,
    isIdentStart,
    isBlank,
    identWords,

    -- * Reading code
    Chunk (..),
    Cursor (..),
    SrcState (..),
    Step (..),
    startAt,
    readSrc,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp.Positions (Pos (..))

-- * Logical lines

-- | Physical lines joined by backslash-newline.
data LLine = LLine
  { -- | Without the backslashes, newlines and line-end @\\r@.
    text :: Text,
    firstLine :: Int,
    lineCount :: Int,
    -- | Offset in 'text' (code points) to file place, one per joined
    -- line. Key 0 is always there.
    starts :: IntMap Pos
  }
  deriving (Show)

-- | The lines still to read: the rest of the input (none left:
-- 'Nothing') and the number of its first line. Lines are made from it
-- when read, so keeping one, to read again from there, keeps only a
-- slice of the input (Note [Put back] in "HaskellReviewer.Core.Cpp.Expand").
data Lines = Lines
  { input :: Maybe Text,
    number :: Int
  }

-- | All the lines of a text.
allLines :: Text -> Lines
allLines t = Lines (Just t) 1

-- | The next logical line. A line ending in a backslash (maybe with
-- blanks after it, as gcc) joins the next one. A @\r@ before the
-- newline is dropped, as gcc does.
nextLine :: Lines -> Maybe (LLine, Lines)
nextLine ls = case ls.input of
  Nothing -> Nothing
  Just t -> Just (join ls.number [] 0 IntMap.empty 1 t)
  where
    join first acc len st count t =
      let (l0, rest) = Text.break (== '\n') t
          l = fromMaybe l0 (Text.stripSuffix "\r" l0)
          after = if Text.null rest then Nothing else Just (Text.drop 1 rest)
          st' = IntMap.insert len (Pos (first + count - 1) 1) st
       in case (spliced l, after) of
            (Just body, Just t') ->
              join first (body : acc) (len + Text.length body) st' (count + 1) t'
            _ ->
              ( LLine
                  { text = Text.concat (reverse (l : acc)),
                    firstLine = first,
                    lineCount = count,
                    starts = st'
                  },
                Lines after (first + count)
              )
    spliced l = case Text.unsnoc (Text.dropWhileEnd isBlank l) of
      Just (body, '\\') -> Just body
      _ -> Nothing

-- | The last file line of a logical line.
lastLineOf :: LLine -> Int
lastLineOf l = l.firstLine + l.lineCount - 1

-- | A directive needs its @#@ in column 1, as in gcc.
startsDirective :: LLine -> Bool
startsDirective l = "#" `Text.isPrefixOf` l.text

-- | A directive's text, with comments taken out.
data Directive = Directive
  { -- | Split where comments were: a comment leaves nothing, but in a
    -- macro body it still ends a name (Note [Traditional mode] in
    -- "HaskellReviewer.Core.Cpp").
    chunks :: [Text],
    -- | The lines after the directive.
    after :: Lines,
    ending :: Ending
  }

-- | How the text ended.
data Ending = Clean | InComment
  deriving (Eq, Show)

-- | Read a directive. A comment can go on past the end of the line:
-- the lines it covers are part of the directive.
directiveText :: LLine -> Lines -> Directive
directiveText l0 ls0 = go [] [] l0.text ls0
  where
    -- done: finished chunks; cur: the chunk so far. Both reversed.
    go done cur t ls = case Text.break (\c -> c == '"' || c == '\'' || c == '/') t of
      (a, rest) -> case Text.uncons rest of
        Nothing -> Directive (reverse (chunk (a : cur) : done)) ls Clean
        Just (c, r)
          | c == '/',
            Just r' <- Text.stripPrefix "*" r ->
              comment (chunk (a : cur) : done) r' ls
          | c == '/' -> go done ("/" : a : cur) r ls
          | otherwise -> let (q, r') = quoted c r in go done (q : a : cur) r' ls
    comment done t ls = case Text.breakOn "*/" t of
      (_, rest)
        | not (Text.null rest) -> go done [] (Text.drop 2 rest) ls
        | Just (l, more) <- nextLine ls -> comment done l.text more
        | otherwise -> Directive (reverse done) ls InComment
    chunk = Text.concat . reverse

-- | Drop this many chars from the start of the chunks.
dropChars :: Int -> [Text] -> [Text]
dropChars n = \case
  c : cs
    | n >= Text.length c -> dropChars (n - Text.length c) cs
    | otherwise -> Text.drop n c : cs
  [] -> []

-- * Tokens

-- | A token of traditional cpp. Identifiers are ASCII only.
data Tok
  = Ident Text
  | -- | A pp-number, like @0x1F@ or @1e10@: never expanded.
    Number Text
  | -- | A @"…"@ or @'…'@ literal, up to the matching quote or the end of
    -- the line.
    Quoted Text
  | Space Text
  | -- | Any other char.
    Punct Text
  deriving (Eq, Show)

-- | The token as text.
tokText :: Tok -> Text
tokText = \case
  Ident t -> t
  Number t -> t
  Quoted t -> t
  Space t -> t
  Punct t -> t

-- | Tokens of a line with no comments (macro bodies, #if lines).
lexText :: Text -> [Tok]
lexText t = case lexOne t of
  Nothing -> []
  Just (tok, rest) -> tok : lexText rest

lexOne :: Text -> Maybe (Tok, Text)
lexOne t = case Text.uncons t of
  Nothing -> Nothing
  Just (c, r)
    | isIdentStart c -> let (a, b) = Text.span isIdentChar t in Just (Ident a, b)
    | isDigit c -> Just (number t)
    | c == '.', Just (d, _) <- Text.uncons r, isDigit d -> Just (number t)
    | c == '"' || c == '\'' -> let (q, b) = quoted c r in Just (Quoted q, b)
    | isBlank c -> let (a, b) = Text.span isBlank t in Just (Space a, b)
    | otherwise -> Just (Punct (Text.singleton c), r)

-- | A pp-number: digits, letters, _, . and a sign after e, E, p or P.
number :: Text -> (Tok, Text)
number t = go 1 (Text.drop 1 t)
  where
    go n r = case Text.uncons r of
      Just (c, r')
        | c `elem` ("eEpP" :: String),
          Just (s, r'') <- Text.uncons r',
          s == '+' || s == '-' ->
            go (n + 2) r''
        | isIdentChar c || c == '.' -> go (n + 1) r'
      _ -> (Number (Text.take n t), r)

-- | The rest of a literal opened by quote @q@ (already taken off):
-- up to the matching quote or the end, with backslash escapes. The
-- result has both quotes.
quoted :: Char -> Text -> (Text, Text)
quoted q t0 = go [Text.singleton q] t0
  where
    go acc t = case Text.break (\c -> c == q || c == '\\') t of
      (a, rest) -> case Text.uncons rest of
        Nothing -> (Text.concat (reverse (a : acc)), "")
        Just (c, r)
          | c == q -> (Text.concat (reverse (Text.singleton q : a : acc)), r)
          | otherwise ->
              let (e, r') = Text.splitAt 1 r
               in go (e : "\\" : a : acc) r'

isIdentStart :: Char -> Bool
isIdentStart c = isAsciiLower c || isAsciiUpper c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isIdentStart c || isDigit c

-- | Blanks inside a line.
isBlank :: Char -> Bool
isBlank c = c == ' ' || c == '\t' || c == '\f' || c == '\v'

-- | Every word that could be a macro name. More than the real ones
-- (it ignores quotes and comments): only used to skip lines quickly.
identWords :: Text -> [Text]
identWords t =
  [ w
  | w <- Text.split (not . isIdentChar) t,
    Just (c, _) <- [Text.uncons w],
    isIdentStart c
  ]

-- * Reading code

-- | File text and where it starts.
data Chunk = Chunk
  { text :: Text,
    from :: Pos
  }
  deriving (Eq, Show)

-- | A place inside a line of code.
data Cursor = Cursor
  { current :: LLine,
    -- | In code points.
    offset :: Int,
    rest :: Text,
    next :: Lines
  }

-- | Where we are in the code lines.
data SrcState = InLine Cursor | AtStart Lines

-- | One step of reading code.
data Step
  = -- | A token, its pieces of file text (more than one when it spans
    -- joined lines), and the state after it.
    Token Tok [Chunk] SrcState
  | -- | The end of a line. These lines are next.
    LineEnd Lines
  | -- | The next line is a directive: code stops before it.
    AtDirective
  | EndOfInput Ending

-- | Start reading in this line, even if it starts with @#@.
startAt :: LLine -> Lines -> SrcState
startAt l ls = InLine Cursor {current = l, offset = 0, rest = l.text, next = ls}

-- | Read one token. Comments are skipped (they may span lines); a line
-- starting with @#@ is a directive, unless it's inside a comment. Also
-- gives how many chars of comments were skipped (they cost work, see
-- Note [CPP limits] in "HaskellReviewer.Core.Cpp").
readSrc :: SrcState -> (Int, Step)
readSrc = go 0
  where
    go n = \case
      AtStart ls0 -> case nextLine ls0 of
        Nothing -> (n, EndOfInput Clean)
        Just (l, ls)
          | startsDirective l -> (n, AtDirective)
          | otherwise -> go n (startAt l ls)
      InLine c -> case Text.stripPrefix "/*" c.rest of
        Just r -> comment (n + 2) c.current (c.offset + 2) r c.next
        Nothing -> case lexOne c.rest of
          Nothing -> (n, LineEnd c.next)
          Just (tok, r) ->
            let s = tokText tok
                c' = Cursor c.current (c.offset + Text.length s) r c.next
             in (n, Token tok (chunksOf c.current c.offset s) (InLine c'))
    comment n l o t ls = case Text.breakOn "*/" t of
      (a, rest)
        | not (Text.null rest) ->
            let k = Text.length a + 2
             in go (n + k) (InLine (Cursor l (o + k) (Text.drop 2 rest) ls))
        | Just (l', more) <- nextLine ls ->
            comment (n + Text.length t + 1) l' 0 l'.text more
        | otherwise -> (n + Text.length t, EndOfInput InComment)

-- | A token's text, split where joined lines meet, with file places.
chunksOf :: LLine -> Int -> Text -> [Chunk]
chunksOf l o t = case IntMap.lookupLE o l.starts of
  Nothing -> [Chunk t (Pos l.firstLine (o + 1))]
  Just (k, p) ->
    let here = p {col = p.col + o - k}
     in case IntMap.lookupGT o l.starts of
          Just (k2, _)
            | k2 < o + Text.length t ->
                let (a, b) = Text.splitAt (k2 - o) t in Chunk a here : chunksOf l k2 b
          _ -> [Chunk t here]
