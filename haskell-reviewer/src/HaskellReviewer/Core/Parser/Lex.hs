{-# LANGUAGE StrictData #-}

-- | The token stream of a module: GHC's lexer a second time, on the
-- same text with the same options as the parse.
module HaskellReviewer.Core.Parser.Lex
  ( Lexed (..),
    lexTokens,
  )
where

import Data.List (isPrefixOf, sortOn)
import GHC.Data.Strict qualified as Strict
import GHC.Data.StringBuffer (StringBuffer, atEnd, currentChar, stepOn)
import GHC.Parser.Lexer
  ( ExtBits (..),
    P (..),
    PState (..),
    ParseResult (..),
    ParserOpts (..),
    Token (..),
    initParserState,
    lexer,
    xset,
    xunset,
  )
import GHC.Types.SrcLoc
  ( GenLocated (..),
    Located,
    PsLoc (..),
    RealSrcLoc,
    RealSrcSpan,
    SrcSpan (..),
    advancePsLoc,
    combineRealSrcSpans,
    mkRealSrcSpan,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,
  )
import HaskellReviewer.Core.Cpp (inExpansion, mapSpan)
import HaskellReviewer.Core.Parser.Helpers (SpanMap (..))
import HaskellReviewer.Core.SourceText (GhcRange (..), ghcToSpan)
import HaskellReviewer.Core.Span (Span (..))
import HaskellReviewer.Core.Tokens (TokenClass (..), Tokens, tokensFromList)
import HaskellReviewer.Core.Tokens qualified as Tokens (Token (..))

{- Note [Second lex]

The parser doesn't keep its tokens, so we run GHC's lexer again on the
text after unlit and CPP, with the same options as the parse, plus
comments (like lexTokenStream, see lexAll). The parser sees the same
text, except for blanked LINE and COLUMN pragmas (Note [LINE pragmas]).
The lexer alone can't do the layout rule's "parse error" case, but that
only changes virtual braces, which we drop along with virtual
semicolons (they have no width).

Spans go back to the file like the AST's (SpanMap). Tokens that a CPP
call made all map to the call, so they become one Macro token with the
call's span.

If it fails where the parse doesn't, there are no tokens and a
CouldNotLex warning. The lexer and the parser differ only in comments
and position pragmas, and we know of no input that fails one and not
the other now (a LINE pragma with a bad end used to, before
positionPragma checked the whole pragma).
-}

{- Note [LINE pragmas]

@{-# LINE 100 "x" #-}@ and @{-# COLUMN 5 #-}@ make GHC's parser move its
positions, so the AST's spans would not be the file's. The lexer we run
for tokens doesn't do that (lexTokenStream turns it off), and the parser
can't take the pragmas as tokens. So we lex first, and if there are such
pragmas, the parser gets the text with them blanked out (same length,
so nothing else moves). Each one is a single Pragma token.

Only a whole pragma, as GHC's lexer takes it, is blanked (see
positionPragma): LINE, a number and a string, then the usual end or
the old one without @#@; COLUMN, a number, then the usual end. (The ends
can't be written here: they would close this comment.) Anything else
is a Pragma token
over the opening LINE or COLUMN alone, and the parser gets it as it is.

Known limits:
- Positions don't move after blanking, but layout can: a COLUMN pragma
  at the start of a line with code after it sets the column of that
  code for GHC, and blanked, the code is indented. Such a module can
  fail to parse where GHC parses it. A test pins this.
- @#line 7 "f"@ and @# 20 "f"@ lines in a file without CPP move
  positions in GHC's lexer and parser alike (a GHC feature for
  preprocessed files). We don't undo that: spans after such a line
  are GHC's. A test pins this too.
-}

-- | What the second lex found.
data Lexed = Lexed
  { tokens :: Tokens,
    -- | LINE and COLUMN pragmas, on the lexed text, in our columns. See
    -- Note [LINE pragmas].
    positionPragmas :: [Span]
  }

-- | The tokens, sorted. 'Nothing' if the lexer fails. See Note [Second
-- lex].
lexTokens :: ParserOpts -> SpanMap -> RealSrcLoc -> StringBuffer -> Maybe Lexed
lexTokens opts spans loc buf = case lexAll opts buf loc of
  Just ts ->
    let raw = rawTokens (filter (not . virtual) ts)
     in Just
          Lexed
            { tokens = tokensFromList (sorted (foldMacros (map token raw))),
              positionPragmas = [r.out | r <- raw, r.position]
            }
  Nothing -> Nothing
  where
    start t = (t.span.startLine, t.span.startCol)
    -- They almost always are already.
    sorted xs
      | and (zipWith (\a b -> start a <= start b) xs (drop 1 xs)) = xs
      | otherwise = sortOn start xs
    -- inExpansion needs the span on the CPP output, before mapSpan.
    token r =
      Tokens.Token
        { span = mapSpan spans.cpp r.out,
          kind = if inExpansion spans.cpp r.out then Macro else r.kind
        }
    rawTokens = \case
      L (RealSrcSpan r _) t : rest
        | Just (end, rest') <- positionPragma t rest ->
            let whole = outSpan (combineRealSrcSpans r end)
             in Raw {out = whole, kind = Pragma, position = True}
                  : rawTokens rest'
      L (RealSrcSpan r _) t : rest
        | Just c <- tokenClass t,
          not (empty r) ->
            Raw {out = outSpan r, kind = c, position = False} : rawTokens rest
      _ : rest -> rawTokens rest
      [] -> []
    outSpan = ghcToSpan spans.columns . ghcRange

-- | Like GHC's lexTokenStream (with comments, position pragmas as
-- tokens), but it steps over the brace of an old LINE pragma end
-- itself. The lexer would take that brace as closing a layout block, and
-- fail when there is none; GHC's parser reads it as part of the pragma.
lexAll :: ParserOpts -> StringBuffer -> RealSrcLoc -> Maybe [Located Token]
lexAll opts buf loc = go (initParserState opts' buf loc) []
  where
    opts' =
      opts {pExtsBitmap = xunset UsePosPragsBit (xset RawTokenStreamBit opts.pExtsBitmap)}
    -- Tokens in reverse.
    go st acc = case unP (lexer False pure) st of
      PFailed _ -> Nothing
      POk st' t -> case unLoc t of
        ITeof -> Just (reverse acc)
        _
          | oldEnd (t : acc),
            not (atEnd st'.buffer),
            currentChar st'.buffer == '}' ->
              let here = st'.loc
                  next = advancePsLoc here '}'
                  brace = mkRealSrcSpan (psRealLoc here) (psRealLoc next)
                  st'' = st' {buffer = stepOn st'.buffer, loc = next}
               in go st'' (L (RealSrcSpan brace Strict.Nothing) ITccurly : t : acc)
          | otherwise -> go st' (t : acc)
    -- A minus, after LINE, a number and a string.
    oldEnd ts = case take 4 (filter (not . virtual) ts) of
      L _ m : L _ (ITstring _ _) : L _ (ITinteger _) : L _ (ITline_prag _) : _ ->
        isMinus m
      _ -> False

isMinus :: Token -> Bool
isMinus = \case
  ITminus -> True
  ITprefixminus -> True
  ITvarsym "-" -> True
  _ -> False

-- | A whole LINE or COLUMN pragma, as GHC's lexer takes it: @LINE@, a
-- number and a string on one line, then @#-}@ or @-}@; @COLUMN@, a
-- number, then @#-}@. Its end and the tokens after it. Anything else is
-- not one (Note [LINE pragmas]).
positionPragma :: Token -> [Located Token] -> Maybe (RealSrcSpan, [Located Token])
positionPragma t rest = case (t, rest) of
  (ITline_prag _, L (RealSrcSpan n _) (ITinteger _) : L (RealSrcSpan s _) str : more)
    | ITstring _ _ <- str,
      srcSpanStartLine n == srcSpanEndLine s ->
        closer more
  (ITcolumn_prag _, L _ (ITinteger _) : L (RealSrcSpan e _) ITclose_prag : more) ->
    Just (e, more)
  _ -> Nothing
  where
    closer = \case
      L (RealSrcSpan e _) ITclose_prag : more -> Just (e, more)
      -- The old @-}@ end: a minus right before a brace.
      L (RealSrcSpan m _) minus : L (RealSrcSpan e _) ITccurly : more
        | isMinus minus,
          srcSpanEndLine m == srcSpanStartLine e,
          srcSpanEndCol m == srcSpanStartCol e ->
            Just (e, more)
      _ -> Nothing

-- | A token before mapping.
data Raw = Raw
  { -- | On the lexed text, in our columns.
    out :: Span,
    kind :: TokenClass,
    -- | A LINE or COLUMN pragma.
    position :: Bool
  }

ghcRange :: RealSrcSpan -> GhcRange
ghcRange r =
  GhcRange
    { startLine = srcSpanStartLine r,
      startCol = srcSpanStartCol r,
      endLine = srcSpanEndLine r,
      endCol = srcSpanEndCol r
    }

-- | A layout token: no width. The lexer can put one inside a pragma
-- (after @where@, a block opens at the pragma's number).
virtual :: Located Token -> Bool
virtual = \case
  L (RealSrcSpan r _) _ -> empty r
  _ -> True

-- | A zero-width token (layout).
empty :: RealSrcSpan -> Bool
empty r = srcSpanStartLine r == srcSpanEndLine r && srcSpanStartCol r == srcSpanEndCol r

-- | One Macro token per call: the tokens of one call start at the
-- call's start.
foldMacros :: [Tokens.Token] -> [Tokens.Token]
foldMacros = \case
  a : b : rest
    | a.kind == Macro,
      b.kind == Macro,
      sameStart a.span b.span ->
        let e = maxEnd a.span b.span
            s = Span a.span.startLine a.span.startCol e.endLine e.endCol
         in foldMacros (Tokens.Token {span = s, kind = Macro} : rest)
  t : rest -> t : foldMacros rest
  [] -> []
  where
    sameStart x y = (x.startLine, x.startCol) == (y.startLine, y.startCol)
    maxEnd x y = if (x.endLine, x.endCol) >= (y.endLine, y.endCol) then x else y

-- | The class of a GHC token. 'Nothing' for layout tokens.
tokenClass :: Token -> Maybe TokenClass
tokenClass = \case
  ITvocurly -> Nothing
  ITvccurly -> Nothing
  ITeof -> Nothing
  -- Identifiers.
  ITvarid _ -> Just VarId
  ITqvarid _ -> Just VarId
  ITdupipvarid _ -> Just VarId
  ITlabelvarid _ _ -> Just VarId
  ITconid _ -> Just ConId
  ITqconid _ -> Just ConId
  ITvarsym _ -> Just VarSym
  ITqvarsym _ -> Just VarSym
  ITconsym _ -> Just ConSym
  ITqconsym _ -> Just ConSym
  ITcolon -> Just ConSym
  -- Operators the lexer names on its own: they are syntax too (a
  -- strictness mark, forall's dot, a type application), so they never
  -- link to a local operator.
  ITminus -> Just ReservedOp
  ITprefixminus -> Just ReservedOp
  ITbang -> Just ReservedOp
  ITtilde -> Just ReservedOp
  ITat -> Just ReservedOp
  ITtypeApp -> Just ReservedOp
  ITpercent -> Just ReservedOp
  ITstar _ -> Just ReservedOp
  ITdot -> Just ReservedOp
  ITproj _ -> Just ReservedOp
  ITlolly -> Just ReservedOp
  -- Literals.
  ITinteger _ -> Just Literal
  ITrational _ -> Just Literal
  ITprimint _ _ -> Just Literal
  ITprimword _ _ -> Just Literal
  ITprimint8 _ _ -> Just Literal
  ITprimint16 _ _ -> Just Literal
  ITprimint32 _ _ -> Just Literal
  ITprimint64 _ _ -> Just Literal
  ITprimword8 _ _ -> Just Literal
  ITprimword16 _ _ -> Just Literal
  ITprimword32 _ _ -> Just Literal
  ITprimword64 _ _ -> Just Literal
  ITprimfloat _ -> Just Literal
  ITprimdouble _ -> Just Literal
  ITstring _ _ -> Just StringLit
  ITstringMulti _ _ -> Just StringLit
  ITprimstring _ _ -> Just StringLit
  ITquasiQuote _ -> Just StringLit
  ITqQuasiQuote _ -> Just StringLit
  ITchar _ _ -> Just CharLit
  ITprimchar _ _ -> Just CharLit
  -- Comments.
  ITlineComment _ _ -> Just Comment
  -- A pragma the lexer doesn't know (LANGUAGE, outside the header) is a
  -- block comment to it.
  ITblockComment c _
    | "{-#" `isPrefixOf` c -> Just Pragma
    | otherwise -> Just Comment
  ITcomment_line_prag -> Just Comment
  ITdocComment _ _ -> Just Haddock
  ITdocOptions _ _ -> Just Haddock
  -- Pragmas.
  ITinline_prag {} -> Just Pragma
  ITopaque_prag _ -> Just Pragma
  ITspec_prag _ -> Just Pragma
  ITspec_inline_prag _ _ -> Just Pragma
  ITsource_prag _ -> Just Pragma
  ITrules_prag _ -> Just Pragma
  ITwarning_prag _ -> Just Pragma
  ITdeprecated_prag _ -> Just Pragma
  ITline_prag _ -> Just Pragma
  ITcolumn_prag _ -> Just Pragma
  ITscc_prag _ -> Just Pragma
  ITunpack_prag _ -> Just Pragma
  ITnounpack_prag _ -> Just Pragma
  ITann_prag _ -> Just Pragma
  ITcomplete_prag _ -> Just Pragma
  ITclose_prag -> Just Pragma
  IToptions_prag _ -> Just Pragma
  ITinclude_prag _ -> Just Pragma
  ITlanguage_prag -> Just Pragma
  ITminimal_prag _ -> Just Pragma
  IToverlappable_prag _ -> Just Pragma
  IToverlapping_prag _ -> Just Pragma
  IToverlaps_prag _ -> Just Pragma
  ITincoherent_prag _ -> Just Pragma
  ITctype _ -> Just Pragma
  -- Keywords, also the ones only special in some places (as, family,
  -- stock...): the lexer can't tell those apart.
  ITas -> Just Keyword
  ITcase -> Just Keyword
  ITclass -> Just Keyword
  ITdata -> Just Keyword
  ITdefault -> Just Keyword
  ITderiving -> Just Keyword
  ITdo _ -> Just Keyword
  ITelse -> Just Keyword
  IThiding -> Just Keyword
  ITforeign -> Just Keyword
  ITif -> Just Keyword
  ITimport -> Just Keyword
  ITin -> Just Keyword
  ITinfix -> Just Keyword
  ITinfixl -> Just Keyword
  ITinfixr -> Just Keyword
  ITinstance -> Just Keyword
  ITlet -> Just Keyword
  ITmodule -> Just Keyword
  ITnewtype -> Just Keyword
  ITof -> Just Keyword
  ITqualified -> Just Keyword
  ITthen -> Just Keyword
  ITtype -> Just Keyword
  ITwhere -> Just Keyword
  ITforall _ -> Just Keyword
  ITexport -> Just Keyword
  ITlabel -> Just Keyword
  ITdynamic -> Just Keyword
  ITsafe -> Just Keyword
  ITinterruptible -> Just Keyword
  ITunsafe -> Just Keyword
  ITstdcallconv -> Just Keyword
  ITccallconv -> Just Keyword
  ITcapiconv -> Just Keyword
  ITprimcallconv -> Just Keyword
  ITjavascriptcallconv -> Just Keyword
  ITmdo _ -> Just Keyword
  ITfamily -> Just Keyword
  ITrole -> Just Keyword
  ITgroup -> Just Keyword
  ITby -> Just Keyword
  ITusing -> Just Keyword
  ITpattern -> Just Keyword
  ITstatic -> Just Keyword
  ITstock -> Just Keyword
  ITanyclass -> Just Keyword
  ITvia -> Just Keyword
  ITunit -> Just Keyword
  ITsignature -> Just Keyword
  ITdependency -> Just Keyword
  ITrequires -> Just Keyword
  ITproc -> Just Keyword
  ITrec -> Just Keyword
  ITlcase -> Just Keyword
  ITlcases -> Just Keyword
  -- Everything else: brackets, punctuation, reserved symbols, TH.
  _ -> Just Special
