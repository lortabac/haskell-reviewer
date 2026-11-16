{-# LANGUAGE StrictData #-}

-- | Text search: plain text, exact bytes, over many files. Pure. See
-- Note [Text search].
module HaskellReviewer.Core.TextSearch
  ( -- * Queries
    Needle,
    needle,
    needleText,
    maxTextQueryLength,

    -- * One file
    LineHit (..),
    Match (..),
    FileHits (..),
    searchFile,

    -- * Many files
    TextHit (..),
    TextAnswer (..),
    Search,
    startSearch,
    addFile,
    finishSearch,
    searchTexts,
    forceTextAnswer,
    textResultsLimit,
    spansPerLine,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Unsafe qualified as Text.Unsafe
import Data.Word (Word8)
import HaskellReviewer.Core.References (lineWindow)
import HaskellReviewer.Core.RelPath (RelPath)

{- Note [Text search]

D-306. Text search looks for one piece of text in every text file of
the snapshot (the text store, Note [Text store] in
"HaskellReviewer.Session.TextStore"). Plain text, case-sensitive: a
hit is the query's UTF-8 bytes, byte for byte. No regex.

- A query with @\\n@ or @\\r@, or an empty one, has no hits.
- Lines are the source view's (sourceLines in
  "HaskellReviewer.Core.SourceText"): split on @\\n@, a leading BOM is
  skipped and not counted in line 1's columns. A hit never has a
  @\\r@ (the query has none), so it never takes a CRLF's @\\r@.
- Hits don't overlap, left to right: @aa@ in @aaa@ is one hit.
- A valid UTF-8 query starts on a code point, so a hit does too.
- Columns are code points, from 1, end exclusive (Note [Span
  convention] in "HaskellReviewer.Core.Span").

How a hit is found: we pick the query's rarest ASCII byte ('byteRank',
a rough table for code and prose, like ripgrep's), look for it with
memchr (BS.elemIndex), and compare the whole query there. A query with
no ASCII byte (@λ@) looks for its last byte, a UTF-8 continuation byte:
those are rare in code (BS.breakSubstring was slower here). Any byte
works as the anchor: the query's first byte starts a code point, so a
hit does too.

The caller can leave files out with a path filter ('startSearch'). The
route searches every file; later the LLM tools will pass their glob
and deny-list.

One result per matching line, with at most 'spansPerLine' spans and
the line cut to 'lineCap' code points around the first hit
('lineWindow', shared with find references). Results go in the order
the files come (path order), then by line. The first
'textResultsLimit' (or fewer, if asked) are kept; @total@ counts every
matching line, @files@ every file with a hit. Once the results are
full, the rest is only counted: after a hit we jump to the next line.
'addFile' builds a file's result lines fully, so a deadline checked
between files covers that work too.

Worst case: a query like @aaa…ab@ over @aaa…@ compares the whole query
at each byte. The caller stops between files after a deadline and
marks the answer stopped (Note [Text search route] in
"HaskellReviewer.Server.TextSearch").
-}

-- * Queries

-- | A query that can have hits: not empty, no line break.
data Needle = Needle
  { text :: Text,
    bytes :: ByteString,
    -- | Its length in code points.
    codePoints :: Int,
    -- | The byte we look for first. See Note [Text search].
    anchor :: Anchor
  }

-- | A byte of the query, and where it is in it.
data Anchor = Anchor
  { offset :: Int,
    byte :: Word8
  }

-- | The needle of a query, if it can have hits. See Note [Text search].
needle :: Text -> Maybe Needle
needle t
  | Text.null t || Text.any (\c -> c == '\n' || c == '\r') t = Nothing
  | otherwise =
      Just
        Needle
          { text = t,
            bytes = bs,
            codePoints = Text.length t,
            anchor = rarest
          }
  where
    bs = Text.Encoding.encodeUtf8 t
    rarest = case [(byteRank b, i, b) | (i, b) <- zip [0 ..] (BS.unpack bs), b < 0x80] of
      -- No ASCII byte: the last one, a continuation byte. Lead bytes
      -- are shared by many letters.
      [] -> Anchor {offset = BS.length bs - 1, byte = BS.last bs}
      c : cs ->
        let (_, i, b) = foldl' (\x y -> if rank y < rank x then y else x) c cs
         in Anchor {offset = i, byte = b}
    rank (r, _, _) = r

-- | The query text.
needleText :: Needle -> Text
needleText n = n.text

-- | Queries are at most this many code points, as for search (D-207).
maxTextQueryLength :: Int
maxTextQueryLength = 128

-- | How common an ASCII byte is in code and prose, roughly: higher is
-- more common. A case, not a table, so no shared value to warm up.
byteRank :: Word8 -> Int
byteRank b = case chr (fromIntegral b) of
  ' ' -> 10
  c
    | c `elem` ['e', 't', 'a', 'o', 'i', 'n', 's', 'r'] -> 9
    | c `elem` ['l', 'c', 'd', 'u', 'm', 'p', 'h'] -> 8
    | c `elem` ['=', '(', ')', ',', '.', '-', ':', '_', '>'] -> 7
    | c >= 'a' && c <= 'z' -> 6
    | c `elem` ['"', '\'', '<', '$', '[', ']', '{', '}', '|', '/', '\\'] -> 5
    | c >= 'A' && c <= 'Z' -> 4
    | c >= '0' && c <= '9' -> 4
    | otherwise -> 2

-- | Where the next hit starts, at byte @i@ or after.
nextHit :: Needle -> ByteString -> Int -> Maybe Int
nextHit n hay i = anchored (i + n.anchor.offset)
  where
    a = n.anchor
    -- j: where to look for the anchor byte.
    anchored j = case BS.elemIndex a.byte (BS.drop j hay) of
      Nothing -> Nothing
      Just k ->
        let s = j + k - a.offset
         in if n.bytes `BS.isPrefixOf` BS.drop s hay
              then Just s
              else anchored (j + k + 1)

-- * One file

-- | One matching line.
data LineHit = LineHit
  { -- | From 1.
    line :: Int,
    -- | The column of the first code point of 'text'.
    lineCol :: Int,
    -- | The line, cut to 'lineCap' code points around the first hit.
    text :: Text,
    -- | The hits on the line. At most 'spansPerLine'.
    matches :: [Match]
  }
  deriving (Eq, Show)

-- | One hit on a line, as columns of the whole line (not of the cut
-- text): code points, from 1, end exclusive.
data Match = Match
  { start :: Int,
    end :: Int
  }
  deriving (Eq, Show)

-- | The hits of one file.
data FileHits = FileHits
  { -- | Every matching line.
    lineCount :: Int,
    -- | The first ones, as many as asked.
    lines :: [LineHit]
  }
  deriving (Eq, Show)

-- | At most this many spans per line.
spansPerLine :: Int
spansPerLine = 20

-- | The hits of a needle in a file's bytes (UTF-8): count every
-- matching line, give the first @want@ in full. See Note [Text search].
searchFile :: Needle -> Int -> ByteString -> FileHits
searchFile n want raw = go 0 1 start start []
  where
    len = BS.length raw
    start = if "\xEF\xBB\xBF" `BS.isPrefixOf` raw then 3 else 0
    slice a b = BS.take (b - a) (BS.drop a raw)
    -- count: lines so far. line: the number of the line at pos, and
    -- where it starts. Only kept up to date while lines are wanted.
    go !count !line !lineStart !pos acc = case nextHit n raw pos of
      Nothing -> FileHits {lineCount = count, lines = reverse acc}
      Just s
        | count >= want -> go (count + 1) line lineStart (lineEnd + 1) acc
        | otherwise ->
            let between = slice pos s
                breaks = BS.count 10 between
                hitStart = case BS.elemIndexEnd 10 between of
                  Just k -> pos + k + 1
                  Nothing -> lineStart
                hitLine = line + breaks
                hit = lineHit n hitLine (noCr (slice hitStart lineEnd)) (s - hitStart)
             in go (count + 1) (hitLine + 1) (lineEnd + 1) (lineEnd + 1) (hit : acc)
        where
          lineEnd = maybe len (+ s) (BS.elemIndex 10 (BS.drop s raw))
    noCr l
      | not (BS.null l) && BS.last l == 13 = BS.init l
      | otherwise = l

-- | One line's result: the line's bytes, and where its first hit is.
lineHit :: Needle -> Int -> ByteString -> Int -> LineHit
lineHit n line bytes first =
  -- A copy: the result doesn't keep the whole decoded line alive.
  LineHit {line = line, lineCol = col, text = Text.copy window, matches = spans}
  where
    -- The store has UTF-8 only, and lines are cut at ASCII bytes.
    t = Text.Encoding.decodeUtf8Lenient bytes
    m = BS.length n.bytes
    offsets = take spansPerLine (first : more (first + m))
    more i = case nextHit n bytes i of
      Just s -> s : more (s + m)
      Nothing -> []
    -- Byte offsets to columns, one walk over the line.
    spans = columns 0 1 offsets
    columns _ _ [] = []
    columns b c (o : os) =
      let c1 = c + codePointsIn b o
          c2 = c1 + n.codePoints
       in Match {start = c1, end = c2} : columns (o + m) c2 os
    codePointsIn a b =
      Text.length (Text.Unsafe.takeWord8 (b - a) (Text.Unsafe.dropWord8 a t))
    firstCol = case spans of
      x : _ -> x.start
      [] -> 1
    (col, window) = lineWindow cut firstCol n.codePoints
    cut a k = Text.take k (Text.drop (a - 1) t)

-- * Many files

-- | A matching line of a file.
data TextHit = TextHit
  { path :: RelPath,
    hit :: LineHit
  }
  deriving (Eq, Show)

-- | The answer. See Note [Text search].
data TextAnswer = TextAnswer
  { -- | Stopped at the deadline: some files weren't searched.
    stopped :: Bool,
    -- | Every matching line.
    total :: Int,
    -- | Files with a hit.
    files :: Int,
    results :: [TextHit]
  }
  deriving (Eq, Show)

-- | A search under way: add the files one by one.
data Search = Search
  { -- | Files to search; the others are left out.
    keep :: RelPath -> Bool,
    total :: Int,
    files :: Int,
    -- | How many more results we keep.
    room :: Int,
    -- | Kept results, last file first.
    found :: [[TextHit]]
  }

-- | At most this many results (the results page).
textResultsLimit :: Int
textResultsLimit = 1000

-- | A search over the files the filter keeps, with at most this many
-- results ('textResultsLimit' at most).
startSearch :: (RelPath -> Bool) -> Int -> Search
startSearch keep limit =
  Search
    { keep = keep,
      total = 0,
      files = 0,
      room = max 0 (min textResultsLimit limit),
      found = []
    }

-- | Search one more file, if the filter keeps it. In WHNF the file is
-- searched and its result lines are built: see Note [Text search].
addFile :: Needle -> Search -> RelPath -> ByteString -> Search
addFile n s p bytes
  | not (s.keep p) || h.lineCount == 0 = s
  | otherwise =
      forceHits hits `seq`
        s
          { total = s.total + h.lineCount,
            files = s.files + 1,
            room = s.room - length h.lines,
            found = hits : s.found
          }
  where
    h = searchFile n s.room bytes
    hits = map (TextHit p) h.lines

-- | The answer; stopped if not every file was added.
finishSearch :: Bool -> Search -> TextAnswer
finishSearch stopped s =
  TextAnswer
    { stopped = stopped,
      total = s.total,
      files = s.files,
      results = concat (reverse s.found)
    }

-- | Search the files the filter keeps, with no deadline.
searchTexts ::
  (RelPath -> Bool) -> Needle -> Int -> [(RelPath, ByteString)] -> TextAnswer
searchTexts keep n limit =
  finishSearch False . foldl' (\s (p, b) -> addFile n s p b) (startSearch keep limit)

-- | Force it fully. After 'addFile' only the spine of the joined list
-- is left to build.
forceTextAnswer :: TextAnswer -> ()
forceTextAnswer a = forceHits a.results

-- | Force result lines: each one, its text (strict fields) and every
-- match.
forceHits :: [TextHit] -> ()
forceHits = foldr (\r rest -> r.hit `seq` foldr seq () r.hit.matches `seq` rest) ()
