{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}

-- | Haddock markup to our rich text (SPEC §11.4.9, D-59), with
-- haddock-library, the parser Haddock itself uses.
module HaskellReviewer.Core.Parser.Haddock
  ( -- * Parsing
    parseDoc,
    parseHeading,

    -- * For tests
    plainOnly,
    maxCommentLength,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.Char (isAlpha, isDigit, isHexDigit, isSpace, isUpper, ord)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Documentation.Haddock.Parser qualified as H
import Documentation.Haddock.Types qualified as H
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
    safeUrl,
  )

-- * Parsing

-- | A doc comment's text, as GHC gives it (no @|@ or @^@).
parseDoc :: Text -> Doc
parseDoc raw
  | plainOnly t = plainDoc t
  | otherwise =
      let H.MetaDoc meta d = parseParas t
       in Doc {blocks = toBlocks (H.overIdentifier identifier d), since = sinceOf meta}
  where
    t = noCR raw

-- | haddock-library drops every @\\r@ before it parses, so we drop them
-- first too: 'plainOnly' must see the text it will parse.
noCR :: Text -> Text
noCR = Text.filter (/= '\r')

-- | haddock-library's paragraphs, with the @mod@ type fixed.
parseParas :: Text -> H.MetaDoc () H.Identifier
parseParas t = H.parseParas Nothing (Text.unpack t)

-- | A section heading (@-- * Title@): one line of inline text, as
-- Haddock reads it.
parseHeading :: Text -> [Inline]
parseHeading raw
  | plainOnly t = [Str t]
  | otherwise = toInlines (H.overIdentifier identifier (H.parseString (Text.unpack t)))
  where
    t = noCR raw

{- Note [Two haddock-libraries]

GHC 9.12 and 9.14 ship their own haddock-library, also called 1.11.0, and
cabal uses it (Hackage's 1.11.0 says base < 4.21). It is newer than
Hackage's, which GHC 9.10 builds. The difference we see: @\@since
base-4.5@ is a 'Since' with a package on 9.12 and later, but a plain
paragraph on 9.10 (only @\@since 1.2@ is a 'Since' there). The rest of
the API is the same, and so is the rest of the markup we test. Both say
1.11.0, so the GHC version tells them apart.
-}

-- | The @\@since@ line. See Note [Two haddock-libraries].
sinceOf :: H.Meta -> Maybe Since
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
sinceOf (H.Meta s) = do
  H.MetaSince p v <- s
  version <- NonEmpty.nonEmpty v
  pure Since {package = Text.pack <$> p, version = version}
#else
sinceOf (H.Meta v _) = do
  version <- NonEmpty.nonEmpty =<< v
  pure Since {package = Nothing, version = version}
#endif

-- | The text as paragraphs, split at blank lines, with no markup.
plainDoc :: Text -> Doc
plainDoc t = Doc {blocks = map (Paragraph . pure . Str) (paragraphs t), since = Nothing}
  where
    paragraphs =
      map (Text.strip . Text.unlines) . filter (not . null) . splitBlank . Text.lines
    splitBlank ls = case break (Text.all isSpace) ls of
      (p, []) -> [p]
      (p, _ : rest) -> p : splitBlank rest

-- * Plain only

{- Note [Haddock cost]

haddock-library backtracks. Some markup scans ahead for its end and,
when the end isn't there, gives up and goes on one character later. So
a comment full of @<@ with no @>@ takes time quadratic in its length:
16 KB of @<<a @ took 0.6 s and 8.6 GB of allocation. Same for @[@,
@](@, @\\(@, @\\[@. Grid tables are worse: they index rows and columns
from the start each time, so a 3 KB grid of @+@ and @-@ took 15 s.
And two inputs make it crash.

So some comments are shown as plain text, split in paragraphs:

- longer than 'maxCommentLength' (12 KiB). Under it, the worst hostile
  comment we found, 12 KiB of @<@, takes 1.0 s and 13.4 GB of
  allocation on GHC 9.14 (1.25 s and 26.7 GB on 9.10). So two such
  comments in one module hit the 2 s limit of D-136 and the module is
  text only, "too slow". No real comment is that long: none of 17,863
  comments in 120 Hackage packages (aeson included) and this project is
  cut;
- with a table line (one that starts and ends with @+@ or @|@) wider
  than 80, or with more than 2,000 characters of table lines. Under
  that, the worst grid of @+@ and @-@ takes about 0.1 s;
- with something that makes haddock-library crash: a character
  reference like @&#99999999;@ out of Unicode's range, or a table row
  shorter than the border above it.

The 2 s limit on a whole parse (D-136) is the safety net for a module
with many costly comments.
-}

-- | 12 KiB, in characters. See Note [Haddock cost].
maxCommentLength :: Int
maxCommentLength = 12 * 1024

-- | Show this comment as plain text. See Note [Haddock cost].
plainOnly :: Text -> Bool
plainOnly t =
  Text.length t > maxCommentLength
    || any (> 80) tableWidths
    || sum tableWidths > 2000
    || badTable ls
    || badCharRef t
  where
    ls = Text.lines t
    tableWidths = [Text.length l | l <- map Text.strip ls, edged l]

-- | Starts and ends with @+@ or @|@.
edged :: Text -> Bool
edged l = case (Text.uncons l, Text.unsnoc l) of
  (Just (a, rest), Just (_, z)) -> not (Text.null rest) && edge a && edge z
  _ -> False
  where
    edge c = c == '+' || c == '|'

-- | A border line, then a row haddock-library takes as part of the
-- table but that is shorter than the border: it indexes past its end.
badTable :: [Text] -> Bool
badTable = \case
  [] -> False
  l : rest
    | border (Text.strip l) -> rows (Text.length (Text.strip l)) rest
    | otherwise -> badTable rest
  where
    border b =
      Text.length b >= 2 && Text.take 1 b == "+" && Text.all (`elem` ("+-" :: String)) b
    -- haddock-library takes the border's width of each row, after the
    -- indentation.
    rows w = \case
      [] -> False
      r : more
        | Text.length row < w -> edged row || badTable (r : more)
        | edged (Text.take w row) && Text.all isSpace (Text.drop w row) -> rows w more
        | otherwise -> badTable (r : more)
        where
          row = Text.stripStart r

-- | @&#N;@ or @&#xN;@ out of Unicode's range: haddock-library's @chr@
-- fails. The number wraps as an 'Int', as there.
badCharRef :: Text -> Bool
badCharRef t = any bad (drop 1 (Text.splitOn "&#" t))
  where
    bad s = case Text.uncons s of
      Just (x, rest) | x == 'x' || x == 'X' -> outOfRange hex (Text.span isHexDigit rest)
      _ -> outOfRange decimal (Text.span isDigit s)
    outOfRange f (digits, after) =
      not (Text.null digits)
        && Text.take 1 after == ";"
        && let n = f (Text.unpack digits) in n < 0 || n > 0x10FFFF
    decimal = foldl' (\a c -> a * 10 + (ord c - 48)) (0 :: Int)
    hex = foldl' (\a c -> (a `shiftL` 4) .|. hexDigit (ord c)) (0 :: Int)
    hexDigit w
      | w >= 48 && w <= 57 = w - 48
      | w >= 97 = w - 87
      | otherwise = w - 55

-- * Identifiers

-- | Like Haddock's parseIdent, by hand: strip the parens or backticks,
-- split off the qualifier, and reject keywords. haddock-library has
-- already checked the characters. Haddock asks GHC's parser with the
-- module's extensions instead, so a few names that depend on them
-- (MagicHash's @foo#@) may differ. GHC also stops at a dot after a
-- lower case name, and Haddock then leaves the whole thing as text: so
-- do we for @'x.y'@, @'M.x.y'@, @'stack.yaml'@. Not a name: 'Nothing',
-- and haddock-library keeps the text, quotes and all.
identifier :: H.Namespace -> String -> Maybe NameRef
identifier ns s
  | Text.null occ || occ `elem` keywords || dotted = Nothing
  | otherwise =
      Just
        NameRef
          { namespace = case ns of
              H.Value -> ValueNs
              H.Type -> TypeNs
              H.None -> AnyNs,
            qualifier = ModuleName <$> qualifier,
            occ = occ,
            written = written
          }
  where
    written = Text.pack s
    -- Tuples, unit and list (@(,)@, @()@, @[]@) are names with their
    -- brackets, as in Haddock.
    inner = case Text.unpack written of
      '(' : c : _ | c /= ',', c /= ')' -> Text.drop 1 (Text.dropEnd 1 written)
      '`' : _ : _ -> Text.drop 1 (Text.dropEnd 1 written)
      _ -> written
    (qualifier, occ) = splitQualifier inner
    dotted = case Text.uncons occ of
      Just (c, _) -> (isAlpha c || c == '_') && Text.any (== '.') occ
      Nothing -> False

-- | @M.N.x@ to @M.N@ and @x@. Each module part starts upper case.
splitQualifier :: Text -> (Maybe Text, Text)
splitQualifier t = case go [] t of
  ([], o) -> (Nothing, o)
  (ms, o) -> (Just (Text.intercalate "." (reverse ms)), o)
  where
    go ms s =
      let (part, rest) = Text.span (\c -> c /= '.' && not (isSymbol c)) s
       in case Text.uncons rest of
            Just ('.', o)
              | Just (c, _) <- Text.uncons part,
                isUpper c,
                not (Text.null o) ->
                  go (part : ms) o
            _ -> (ms, s)
    isSymbol c = c `elem` ("!#$%&*+/<=>?@\\^|-~:" :: String)

-- | Reserved words and symbols: GHC's parser rejects them as names.
keywords :: [Text]
keywords =
  [ "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "forall",
    "foreign",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where",
    "_",
    "..",
    "::",
    "=",
    "\\",
    "|",
    "<-",
    "=>"
  ]

-- * Conversion

-- | The parser never makes 'H.DocIdentifierUnchecked' (Haddock's renamer
-- does), so its type is @()@.
type HDoc = H.DocH () NameRef

-- | The pieces of a 'H.DocAppend' tree, in order.
pieces :: HDoc -> [HDoc] -> [HDoc]
pieces d rest = case d of
  H.DocAppend a b -> pieces a (pieces b rest)
  H.DocEmpty -> rest
  _ -> d : rest

-- | Paragraph-level. Inline pieces next to each other make a paragraph.
toBlocks :: HDoc -> [Block]
toBlocks d = go (pieces d [])
  where
    go = \case
      [] -> []
      x : xs -> case block x of
        Just bs -> bs <> go xs
        Nothing ->
          let (inl, rest) = span (isNothing . block) xs
           in paragraph (mergeStr (concatMap inline (x : inl))) <> go rest

-- | A paragraph, unless it has no text (an anchor alone, say).
paragraph :: [Inline] -> [Block]
paragraph is = [Paragraph is | not (all blank is)]
  where
    blank = \case
      Str s -> Text.all isSpace s
      _ -> False

-- | A paragraph-level piece, or 'Nothing' for an inline one.
block :: HDoc -> Maybe [Block]
block = \case
  H.DocParagraph x -> Just (paragraph (toInlines x))
  H.DocHeader (H.Header l x) -> Just [Heading Header {level = l, title = toInlines x}]
  H.DocCodeBlock x -> Just [CodeBlock (toInlines x)]
  H.DocUnorderedList xs -> Just [UnorderedList (map toBlocks xs)]
  H.DocOrderedList xs -> Just [OrderedList (map item xs)]
  H.DocDefList xs -> Just [DefinitionList (map definition xs)]
  H.DocExamples es -> Just [Examples (map example es)]
  H.DocProperty p -> Just [Property (Text.pack p)]
  H.DocTable (H.Table h b) -> Just [Table (TableBody (map row h) (map row b))]
  H.DocWarning x -> Just (toBlocks x)
  _ -> Nothing
  where
    item (n, x) = ListItem {number = n, content = toBlocks x}
    definition (a, b) = DefItem {term = toInlines a, definition = toBlocks b}
    example (H.Example e rs) = Example (Text.pack e) (map Text.pack rs)
    row (H.TableRow cs) = map cell cs
    cell (H.TableCell cols rows x) = Cell cols rows (toInlines x)

-- | Inline text.
toInlines :: HDoc -> [Inline]
toInlines d = mergeStr (concatMap inline (pieces d []))

-- | One piece, inline. A block here can't come from haddock-library's
-- inline parsers; just in case, it's its paragraphs' text.
inline :: HDoc -> [Inline]
inline = \case
  H.DocString s -> [Str (Text.pack s)]
  H.DocIdentifier r -> [Name r]
  H.DocIdentifierUnchecked () -> []
  H.DocModule (H.ModLink m l) -> [ModuleLink (modLink m (toInlines <$> l))]
  H.DocWarning x -> toInlines x
  H.DocEmphasis x -> [Emphasis (toInlines x)]
  H.DocMonospaced x -> [Code (toInlines x)]
  H.DocBold x -> [Bold (toInlines x)]
  H.DocHyperlink (H.Hyperlink u l) -> link (Text.pack u) (toInlines <$> l)
  H.DocPic (H.Picture u l) -> link (Text.pack u) (title <$> l)
  H.DocMathInline s -> [Math InlineMath (Text.pack s)]
  H.DocMathDisplay s -> [Math DisplayMath (Text.pack s)]
  -- An anchor: nothing to show.
  H.DocAName _ -> []
  x@H.DocAppend {} -> toInlines x
  H.DocEmpty -> []
  x -> concat [is | Paragraph is <- toBlocks x]
  where
    title = pure . Str . Text.strip . Text.pack

-- | A link if the URL is safe, else its text: the label, then the URL.
-- Images too: never shown, only linked (SPEC §6.4).
link :: Text -> Maybe [Inline] -> [Inline]
link u l = case safeUrl u of
  Just url -> [Link Hyperlink {url = url, label = label l}]
  Nothing -> case label l of
    Just is -> is <> [Str (" <" <> u <> ">")]
    Nothing -> [Str u]

-- | A label with no text is no label.
label :: Maybe [Inline] -> Maybe [Inline]
label = \case
  Just is | all blank is -> Nothing
  l -> l
  where
    blank = \case
      Str s -> Text.all isSpace s
      _ -> False

-- | @M@, @M#anchor@ or @M\\#anchor@.
modLink :: String -> Maybe [Inline] -> ModLink
modLink s l =
  ModLink
    { name = ModuleName (Text.dropWhileEnd (== '\\') m),
      anchor = case Text.drop 1 a of
        "" -> Nothing
        x -> Just x,
      label = label l
    }
  where
    (m, a) = Text.breakOn "#" (Text.pack s)

-- | Join runs of 'Str'.
mergeStr :: [Inline] -> [Inline]
mergeStr = \case
  Str a : rest@(Str _ : _) ->
    let (strs, more) = span isStr rest
     in Str (Text.concat (a : [s | Str s <- strs])) : mergeStr more
  x : rest -> x : mergeStr rest
  [] -> []
  where
    isStr = \case
      Str _ -> True
      _ -> False
