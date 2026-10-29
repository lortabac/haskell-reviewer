{-# LANGUAGE StrictData #-}

-- | Code as tokens, for declaration heads and signatures on the docs
-- page (SPEC §4.2): each token with its class, its text from the file,
-- and a link if it names a declaration of this module.
module HaskellReviewer.Core.Code
  ( -- * Code
    Code,
    CodeToken (..),
    CodeClass (..),

    -- * Building
    CodeEnv (..),
    codeEnv,
    codeOf,
    codeOfSpans,
    plainCode,

    -- * Heads
    typeHead,
    classHead,
    instanceHead,
    famInstHead,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Decls
  ( ClassDecl (..),
    ConDecl (..),
    DataBody (..),
    DeclInfo (..),
    DerivingClause (..),
    Extent (..),
    FamInstDecl (..),
    FamInstForm (..),
    InstanceDecl (..),
    InstanceForm (..),
    NewtypeBody (..),
    Part (..),
    PartRole (..),
    TypeDecl (..),
    TypeForm (..),
  )
import HaskellReviewer.Core.DocLinks (LocalNames, resolveLocal)
import HaskellReviewer.Core.Names (DeclId)
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText (NameRef (..), RefNamespace (..))
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col, Line, Span (..))
import HaskellReviewer.Core.Tokens (Token (..), TokenClass (..), Tokens, tokensIn)

-- * Code

-- | Tokens in source order, with spaces where the source had a gap.
type Code = [CodeToken]

-- | One token, or a space.
data CodeToken = CodeToken
  { kind :: CodeClass,
    -- | As written in the file. A space is one @' '@.
    text :: Text,
    -- | The local declaration it names. See Note [Local links in code].
    link :: Maybe DeclId
  }
  deriving (Eq, Show)

-- | A token's class, or a space. Never 'Comment' or 'Haddock': code
-- leaves them out.
data CodeClass = Space | Tok TokenClass
  deriving (Eq, Show)

-- * Building

-- | What code needs from a module.
data CodeEnv = CodeEnv
  { source :: SourceLines,
    tokens :: Tokens,
    names :: LocalNames
  }

-- | From the file's text, its tokens and its names.
codeEnv :: Text -> Tokens -> LocalNames -> CodeEnv
codeEnv text ts names =
  CodeEnv {source = sourceLines text, tokens = ts, names = names}

{- Note [Code tokens]

Code is the tokens of a span, in order, without comments and Haddock.
Where the source has a gap between two tokens (spaces, a newline, a
comment) there is one space instead: newlines collapse, the page wraps
long heads itself. A CPP macro call is one token, as written.

The text of each token is its slice of the file, so tabs, CRLF and
unlit (.lhs) don't matter: tokens never cover the @>@ of a bird track.
-}

{- Note [Local links in code]

Code with links is type-level code: heads, signatures, field and
argument types, kind signatures. A token links to a declaration of
this module when it's a ConId, a ConSym or a VarSym (a real operator,
not a 'ReservedOp' like @!@ or forall's @.@: so a local type operator
named @.@ or @-@ doesn't link either), unqualified or qualified
with this module's name, and the name is in the module's types table
(Note [Local doc links] in "HaskellReviewer.Core.DocLinks"). Right
after a tick (@'A@, a promoted constructor) it looks in the values
table instead. So @Bool@ in a signature never links to a local
constructor @Bool@ of some other type.

A VarId never links: in a head it's a type variable, and in a
signature the name itself. A declaration's own name doesn't link (in
its head, its signature, its kind signature...), nor does anything that
would link to the declaration itself. Pragmas (RULES and the like)
have value-level code: no links at all ('plainCode').
-}

-- | The code of a span, in the head or signature of these declarations
-- (Note [Local links in code]). See Note [Code tokens].
codeOf :: CodeEnv -> [DeclInfo] -> Span -> Code
codeOf env self s = render env self (tokensBefore env (spanEnd s) s)

-- | Like 'codeOf', for several spans, joined with a space.
codeOfSpans :: CodeEnv -> [DeclInfo] -> [Span] -> Code
codeOfSpans env self ss =
  render env self (concatMap (\s -> tokensBefore env (spanEnd s) s) ss)

-- | The code of a span, with no links: for pragmas, whose code is at
-- the value level.
plainCode :: CodeEnv -> Span -> Code
plainCode env s = renderWith NoLinks env (tokensBefore env (spanEnd s) s)

{- Note [Declaration heads]

The head is what the page shows as the declaration's title line. It
comes from the declaration's main part: the head part with the name
in it (the others are kind signatures, roles, fixities, which have
their own fields).

- data, newtype, type data, data and newtype instances: up to the
  first constructor or deriving clause, without a last @=@. A GADT
  keeps its @where@.
- closed families: up to the first equation, @where@ kept.
- classes and explicit instances: up to @where@, without it.
- synonyms, open and data families, type instances, standalone
  deriving: the whole main part.
- an instance from a deriving clause: its class (the strategy and the
  via type are in its own fields).

Values and pattern synonyms have no head: their signature is the
title, or just the name without one.
-}

-- | The head of a type. See Note [Declaration heads].
typeHead :: CodeEnv -> TypeDecl -> Code
typeHead env t = case t.form of
  DataType b -> dataHead (dataStops b.constructors b.derivings)
  TypeData b -> dataHead (dataStops b.constructors b.derivings)
  Newtype b -> dataHead (dataStops [b.constructor] b.derivings)
  ClosedFamily (e : _) -> render env self (tokensBefore env (spanStart e) main)
  _ -> codeOf env self main
  where
    self = [t.info]
    main = mainSpan t.info
    dataHead stops = render env self (dataTokens env main stops)

-- | The head of a class. See Note [Declaration heads].
classHead :: CodeEnv -> ClassDecl -> Code
classHead env c = render env [c.info] (beforeWhere env (mainSpan c.info))

-- | The head of an instance. See Note [Declaration heads].
instanceHead :: CodeEnv -> InstanceDecl -> Code
instanceHead env i = case i.form of
  ExplicitInstance _ -> render env [] (beforeWhere env (mainSpan i.info))
  StandaloneDeriving _ -> codeOf env [] (mainSpan i.info)
  DerivedInstance _ -> codeOf env [] i.info.nameSpan

-- | The head of a type or data instance. See Note [Declaration heads].
famInstHead :: CodeEnv -> FamInstDecl -> Code
famInstHead env f = case f.form of
  TypeInstance -> codeOf env [] main
  DataInstance b -> dataHead (dataStops b.constructors b.derivings)
  NewtypeInstance b -> dataHead (dataStops [b.constructor] b.derivings)
  where
    main = mainSpan f.info
    dataHead stops = render env [] (dataTokens env main stops)

-- * Internal

-- | A data head: up to the first stop, without a last @=@.
dataTokens :: CodeEnv -> Span -> [Pos] -> [Token]
dataTokens env main stops = dropEquals env (tokensBefore env (firstOf main stops) main)

-- | Where constructors and deriving clauses start.
dataStops :: [ConDecl] -> [DerivingClause] -> [Pos]
dataStops cs ds =
  [spanStart (NonEmpty.head c.info.extent.parts).span | c <- cs]
    <> [spanStart d.span | d <- ds]

-- | The earliest stop inside the span, or its end.
firstOf :: Span -> [Pos] -> Pos
firstOf s stops = minimum (spanEnd s : [p | p <- stops, p > spanStart s])

-- | The head parts of a declaration.
headSpans :: DeclInfo -> [Span]
headSpans i = [p.span | p <- NonEmpty.toList i.extent.parts, p.role == HeadPart]

-- | The head part with the name in it, else the first head part, else
-- the name. See Note [Declaration heads].
mainSpan :: DeclInfo -> Span
mainSpan i =
  case [s | s <- heads, spanStart s <= at, at < spanEnd s] of
    s : _ -> s
    [] -> fromMaybe i.nameSpan (listToMaybe heads)
  where
    heads = headSpans i
    at = spanStart i.nameSpan

-- | The tokens of a span that start before the point, comments and
-- Haddock left out.
tokensBefore :: CodeEnv -> Pos -> Span -> [Token]
tokensBefore env end s =
  [ t
  | t <- tokensIn s env.tokens,
    spanStart t.span < end,
    t.kind `notElem` [Comment, Haddock]
  ]

-- | The tokens before the first @where@.
beforeWhere :: CodeEnv -> Span -> [Token]
beforeWhere env s = takeWhile (not . isWhere) (tokensBefore env (spanEnd s) s)
  where
    isWhere t = t.kind == Keyword && sliceSpan env.source t.span == "where"

-- | Without a last @=@.
dropEquals :: CodeEnv -> [Token] -> [Token]
dropEquals env ts = case reverse ts of
  t : rest | t.kind == Special, sliceSpan env.source t.span == "=" -> reverse rest
  _ -> ts

-- | Whether code links, and what it must not link to.
data Links
  = -- | Type-level code in the head or signature of these declarations.
    TypeLinks [DeclInfo]
  | NoLinks

-- | Type-level code to code. See Note [Local links in code].
render :: CodeEnv -> [DeclInfo] -> [Token] -> Code
render env self = renderWith (TypeLinks self) env

-- | Tokens to code: text, links, and a space for each gap.
renderWith :: Links -> CodeEnv -> [Token] -> Code
renderWith links env = go []
  where
    -- The tokens before, the last first, with their text.
    go _ [] = []
    go before (t : ts) =
      let text = sliceSpan env.source t.span
          tok = CodeToken {kind = Tok t.kind, text = text, link = linkOf before t text}
          rest = tok : go (take 2 ((t, text) : before)) ts
       in case before of
            (p, _) : _ | spanEnd p.span /= spanStart t.span -> space : rest
            _ -> rest
    space = CodeToken {kind = Space, text = " ", link = Nothing}
    linkOf before t text = case links of
      TypeLinks self
        | t.kind `elem` [ConId, ConSym, VarSym],
          t.span `notElem` map (.nameSpan) self ->
            case resolveLocal env.names (nameRef (namespace before t) text) of
              Just i | i `notElem` map (.id) self -> Just i
              _ -> Nothing
      _ -> Nothing
    -- Right after a tick, a promoted constructor: @'A@, or @'(:+)@.
    namespace before t = case before of
      (p, "'") : _ | next p t -> ValueNs
      (p, "(") : (q, "'") : _ | t.kind == ConSym, next p t, next q p -> ValueNs
      _ -> TypeNs
    next a b = spanEnd a.span == spanStart b.span

-- | A token's text as a name in this namespace: the qualifier split off.
nameRef :: RefNamespace -> Text -> NameRef
nameRef ns t =
  NameRef
    { namespace = ns,
      qualifier =
        if null quals then Nothing else Just (ModuleName (Text.intercalate "." quals)),
      occ = occ,
      written = t
    }
  where
    (quals, occ) = split [] t
    -- Take @M.@ parts while something is left after them.
    split acc s = case Text.span isConChar s of
      (c, rest)
        | Just (h, _) <- Text.uncons c,
          isUpper h,
          Just ('.', after) <- Text.uncons rest,
          not (Text.null after) ->
            split (acc <> [c]) after
      _ -> (acc, s)
    isConChar ch = isAlphaNum ch || ch == '_' || ch == '\''

-- | A place in the file.
type Pos = (Line, Col)

spanStart :: Span -> Pos
spanStart s = (s.startLine, s.startCol)

spanEnd :: Span -> Pos
spanEnd s = (s.endLine, s.endCol)
