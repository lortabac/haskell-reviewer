{-# LANGUAGE StrictData #-}

-- | Macro expansion, as gcc's traditional mode. See Note [Traditional
-- mode] and Note [CPP limits] in "HaskellReviewer.Core.Cpp".
module HaskellReviewer.Core.Cpp.Expand
  ( -- * State
    Shared (..),
    newShared,
    warn,
    limitHit,

    -- * Expanding
    CodeRun (..),
    expandCode,
    expandIf,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp.Lines
  ( Chunk (..),
    Ending (..),
    LLine,
    Lines (..),
    SrcState (..),
    Step (..),
    Tok (..),
    lexText,
    readSrc,
    startAt,
    tokText,
  )
import HaskellReviewer.Core.Cpp.Macros
  ( BodyPart (..),
    Macro (..),
    MacroTable,
    isDefined,
    lookupMacro,
  )
import HaskellReviewer.Core.Cpp.Positions (Piece (..), Pos (..), Run (..), follows)
import HaskellReviewer.Core.Cpp.Warning
  ( CppProblem (..),
    CppWarning (..),
    Limit (..),
    problemKind,
    shorten,
  )

-- * State

-- | State for the whole file.
data Shared = Shared
  { macros :: MacroTable,
    -- | Work left. See Note [CPP limits].
    budget :: Int,
    -- | Newest first.
    warnings :: [CppWarning],
    -- | How many, and which (kind, line) we have.
    warned :: Int,
    seen :: Set (Int, Int),
    hit :: Set Limit,
    -- | The line we're on, for @__LINE__@ and warnings.
    current :: Int
  }

-- | Start with these macros and this much work.
newShared :: MacroTable -> Int -> Shared
newShared m b =
  Shared
    { macros = m,
      budget = b,
      warnings = [],
      warned = 0,
      seen = Set.empty,
      hit = Set.empty,
      current = 1
    }

-- | Most warnings per file. See Note [CPP limits].
maxWarnings :: Int
maxWarnings = 1000

-- | Add a warning on the current line: one of a kind per line, at most
-- 'maxWarnings', texts cut short.
warn :: CppProblem -> Shared -> Shared
warn p sh = case p of
  LimitHit l -> limitHit l sh
  _ -> addWarning p sh

-- | Note a limit, warning the first time.
limitHit :: Limit -> Shared -> Shared
limitHit l sh
  | Set.member l sh.hit = sh
  | otherwise = addWarning (LimitHit l) sh {hit = Set.insert l sh.hit}

addWarning :: CppProblem -> Shared -> Shared
addWarning p sh
  | Set.member key sh.seen = sh
  | sh.warned > maxWarnings = sh
  | sh.warned == maxWarnings = add WarningsDropped
  | otherwise = add (shorten p)
  where
    key = (problemKind p, sh.current)
    add q =
      sh
        { warnings = CppWarning sh.current q : sh.warnings,
          warned = sh.warned + 1,
          seen = Set.insert key sh.seen
        }

-- * Expanding

-- | What a run of code gives.
data CodeRun = CodeRun
  { state :: Shared,
    -- | The output line, and where its parts come from.
    text :: Text,
    runs :: [Run],
    -- | The lines after the ones used.
    after :: Lines
  }

-- | Expand code starting at this line (not a directive). It may read
-- more lines: a comment or a macro call can go on.
expandCode :: Shared -> LLine -> Lines -> CodeRun
expandCode sh l ls = case scan emitCode ex0 (CodeOut [] Nothing) of
  Scanned ex (CodeOut done growing) rest ->
    let rs = reverse (maybe done (\o -> close o : done) growing)
     in CodeRun
          { state = ex.shared,
            text = Text.concat (map (.text) rs),
            runs = rs,
            after = rest
          }
  where
    ex0 = start (FromLines (startAt l ls)) sh

-- | Expand an @#if@ line, with @defined@.
expandIf :: Shared -> [Tok] -> (Shared, [Tok])
expandIf sh ts = case scan emitIf (start (FromToks ts) sh) [] of
  Scanned ex out _ -> (ex.shared, reverse out)

-- * Internal

-- | Expansion state.
data Ex = Ex
  { -- | Macro bodies being read, innermost first.
    ctxs :: [Ctx],
    -- | The names of the open bodies in 'ctxs', innermost first, and
    -- as a set. Kept here so nothing walks 'ctxs'; stamping a token
    -- (Note [Put back]) shares the set.
    names :: [Text],
    openSet :: Set Text,
    src :: Source,
    shared :: Shared
  }

start :: Source -> Shared -> Ex
start s sh =
  Ex
    { ctxs = [],
      names = [],
      openSet = Set.empty,
      src = s,
      shared = sh
    }

-- | Code lines, or the tokens of an @#if@ line (where @defined@ works).
-- 'Unclosed': the code ended inside a comment.
data Source = FromLines SrcState | FromToks [Tok] | Unclosed

inIf :: Ex -> Bool
inIf ex = case ex.src of
  FromToks _ -> True
  _ -> False

-- | Text being read again: a macro's expansion (with its name, which
-- can't expand inside it), or text put back after a failed call.
data Ctx = Ctx
  { name :: Maybe Text,
    items :: [Item]
  }

-- | A token, or a line end in code.
data Item = Item Lexeme | Break Lines

data Lexeme = Lexeme
  { tok :: Tok,
    at :: Where,
    -- | Object-like macros this token can't expand as: the bodies that
    -- were open when it was read, before a failed call put it back. See
    -- Note [Put back].
    hide :: Set Text
  }

data Where
  = -- | From the file, as is.
    Src [Chunk]
  | -- | Made by a macro call spanning from here to there.
    Gen Pos Pos

{- Note [Put back]

A call that fails (no paren, no closing paren, wrong count) leaves the
name as is, and what was read for it is read again:

- Text from the file (or the #if line) is read again from where the
  call started: we keep the reader's state from then, so nothing read
  from the file is kept in memory.
- Text from macro bodies is put back, on top of any text put back
  before (so put-back text never stacks up). It may come from a body
  that is closed by now. Read again, it could expand that macro again,
  and fail again, forever (until the budget is spent): #define Q P(Q
  then Q. So a token read from a body gets the names of the bodies open
  when it was read, and won't expand as any of them that is
  object-like. Function-like macros are not hidden: gcc lets them call
  themselves (20 levels, then recursion), and F(G F(1)) must give
  [G [1]]; the budget ends a loop of those. Tokens of a body keep the
  names of the token that called it.
-}

-- | Where a token was read from: a macro body, text put back, or the
-- file (or #if line).
data From = FromBody | FromPutBack | FromSource

-- | What a look ahead (for a paren and arguments) has read: the body
-- tokens, to put back (newest first), and the end of the furthest one,
-- for the call's span.
data Look = Look
  { kept :: [Item],
    end :: Maybe Pos
  }

-- | Output of code: finished runs (newest first), and the one growing.
data CodeOut = CodeOut [Run] (Maybe Open)

-- | A run still growing: its length, its piece and its text.
data Open = Open
  { len :: Int,
    piece :: Piece,
    acc :: Acc
  }

-- | Text built a bit at a time: blocks (newest first) and recent bits
-- (newest first). Every 'blockSize' bits become one block, so long
-- text doesn't keep one small text per token.
data Acc = Acc
  { blocks :: [Text],
    recent :: [Text],
    nRecent :: Int
  }

blockSize :: Int
blockSize = 256

emptyAcc :: Acc
emptyAcc = Acc [] [] 0

addText :: Text -> Acc -> Acc
addText t a
  | a.nRecent >= blockSize =
      let !b = Text.concat (reverse a.recent) in Acc (b : a.blocks) [t] 1
  | otherwise = a {recent = t : a.recent, nRecent = a.nRecent + 1}

accText :: Acc -> Text
accText a = Text.concat (reverse a.blocks <> reverse a.recent)

-- | How to add a token to the output.
type Emit s = Lexeme -> s -> s

data Scanned s = Scanned Ex s Lines

-- | gcc lets a function-like macro call itself this deep before it
-- calls it recursion (traditional.c, recursive_macro).
funRecursion :: Int
funRecursion = 20

-- | How many macro bodies can be open at once.
maxNesting :: Int
maxNesting = 64

maxArgs :: Int
maxArgs = 256

-- | Read one item, or stop (with the state at the end). A body that's
-- done is closed first ("reading past its end", as gcc): only then can
-- its macro expand again.
next :: Ex -> Either Ex (Item, From, Ex)
next ex = case ex.ctxs of
  c : cs -> case c.items of
    i : is ->
      let from = maybe FromPutBack (const FromBody) c.name
       in Right (i, from, ex {ctxs = c {items = is} : cs})
    [] -> next (closeTop c cs)
  [] -> case ex.src of
    FromToks (t : ts) ->
      let lx = Lexeme t (Gen origin origin) Set.empty
       in Right (Item lx, FromSource, ex {src = FromToks ts})
    FromToks [] -> Left ex
    Unclosed -> Left ex
    FromLines s -> case readSrc s of
      -- Skipped comments cost work: a failed call reads them again.
      (skipped, step) -> fromLines (if skipped > 0 then charge skipped ex else ex) step
  where
    origin = Pos ex.shared.current 1
    closeTop c cs = case c.name of
      Just n ->
        -- At most 'maxNesting' names: a walk is cheap.
        let rest = drop 1 ex.names
         in ex
              { ctxs = cs,
                names = rest,
                openSet = if n `elem` rest then ex.openSet else Set.delete n ex.openSet
              }
      Nothing -> ex {ctxs = cs}

-- | 'next' in the code lines.
fromLines :: Ex -> Step -> Either Ex (Item, From, Ex)
fromLines ex = \case
  Token t cs s' ->
    let sh = ex.shared
        here = case cs of
          c : _ -> c.from.line
          [] -> sh.current
        ex' = ex {src = FromLines s', shared = sh {current = here}}
     in Right (Item (Lexeme t (Src cs) Set.empty), FromSource, ex')
  LineEnd ls -> Right (Break ls, FromSource, ex {src = FromLines (AtStart ls)})
  AtDirective -> Left ex
  -- The warning comes from 'scan', once (a look ahead that gets
  -- here is undone).
  EndOfInput InComment -> Left ex {src = Unclosed}
  EndOfInput Clean -> Left ex

-- | Note what a look ahead read. A token read from a body gets the
-- names of the bodies open when it was read, in case it's put back:
-- it won't expand as those of them that are object-like (Note [Put
-- back]). That happens once per token: text put back is on top, so
-- the bodies under it, which it was stamped with, stay open while it
-- is read again.
see :: Item -> From -> Ex -> Look -> Look
see i from ex look =
  Look
    { kept = case (from, i) of
        (FromBody, Item lx) ->
          let !h = if Set.null lx.hide then ex.openSet else Set.union lx.hide ex.openSet
           in Item lx {hide = h} : look.kept
        (FromSource, _) -> look.kept
        _ -> i : look.kept,
      end = case i of
        Item lx ->
          let !e = maybe (endOf lx.at) (max (endOf lx.at)) look.end in Just e
        Break _ -> look.end
    }

-- | Back to where a failed look ahead started (Note [Put back]). The
-- work spent stays spent.
rewind :: Ex -> Ex -> Look -> Ex
rewind ex0 ex look =
  putBack
    (reverse look.kept)
    ex {src = ex0.src, shared = ex.shared {current = ex0.shared.current}}

-- | The main loop: emit items, expanding macros. In code, a line end
-- outside any call ends the run.
scan :: Emit s -> Ex -> s -> Scanned s
-- The output is forced at each step: a chain of lazy emits would keep
-- every token read.
scan e ex !out = case next ex of
  Left ex' -> case ex'.src of
    Unclosed -> Scanned ex' {shared = warn UnterminatedComment ex'.shared} out noLines
    _ -> Scanned ex' out (remaining ex')
  Right (Break ls, _, ex') -> Scanned ex' out ls
  Right (Item lx, _, ex') -> case lx.tok of
    Ident n | ex'.shared.budget > 0 -> ident e n lx ex' out
    _ -> scan e ex' (e lx out)
  where
    remaining x = case x.src of
      FromLines (AtStart ls) -> ls
      _ -> noLines
    noLines = Lines Nothing 0

emitIf :: Emit [Tok]
emitIf lx ts = lx.tok : ts

-- | Runs that go on from each other are joined now, so the output never
-- holds a list of every token.
emitCode :: Emit CodeOut
emitCode lx out = case lx.at of
  Src cs -> foldl' add out [Run c.text (Copied c.from) | c <- cs]
  Gen a b -> add out (Run (tokText lx.tok) (Expanded a b))
  where
    add (CodeOut done growing) (Run t p) = case growing of
      Just o
        | follows o.piece o.len p -> CodeOut done (Just (grow t o))
        | otherwise -> let !r = close o in CodeOut (r : done) (Just (fresh t p))
      Nothing -> CodeOut done (Just (fresh t p))

fresh :: Text -> Piece -> Open
fresh t p = Open {len = Text.length t, piece = p, acc = addText t emptyAcc}

grow :: Text -> Open -> Open
grow t o = o {len = o.len + Text.length t, acc = addText t o.acc}

close :: Open -> Run
close o = Run (accText o.acc) o.piece

-- | A name: expand it if it's a macro.
ident :: Emit s -> Text -> Lexeme -> Ex -> s -> Scanned s
ident e n lx ex out
  | inIf ex && n == "defined" = definedOp e lx ex out
  | otherwise = case lookupMacro n ex.shared.macros of
      Nothing -> asIsWith id
      Just LineMacro ->
        let t = Number (Text.pack (show ex.shared.current))
         in scan e ex (e (Lexeme t (gen lx.at) lx.hide) out)
      Just (ObjectMacro body)
        -- Only object-like macros are hidden: gcc lets function-like
        -- ones recurse (deepRecursion), and the budget ends a loop.
        | Set.member n lx.hide || Set.member n ex.openSet ->
            asIsWith (warn (RecursiveMacro n))
        | length ex.names >= maxNesting -> asIsWith (limitHit ExpansionDepth)
        | otherwise -> case push n [Lexeme b (gen lx.at) lx.hide | b <- body] ex of
            Just ex' -> scan e ex' out
            Nothing -> asIsWith (limitHit ExpansionWork)
      Just m -> call e n lx m ex out
  where
    asIsWith f = scan e ex {shared = f ex.shared} (e lx out)

-- | A function-like macro: look for the paren, read the arguments,
-- expand. If anything's wrong the name stays as is and what we read is
-- read again (Note [Put back]).
call :: Emit s -> Text -> Lexeme -> Macro -> Ex -> s -> Scanned s
call e n lx m ex0 out = case blanks ex0 (Look [] Nothing) of
  (ex1, look1) -> case next ex1 of
    Right (i@(Item (Lexeme (Punct "(") _ _)), from, ex2) ->
      case args ex2 (Collect 0 emptyAcc [] 0 (see i from ex2 look1)) of
        NoArgs failure ex3 look ->
          let problem = case failure of
                Unterminated -> UnterminatedCall n
                TooMuch l -> LimitHit l
           in fallBack (warn problem) ex3 look
        GotArgs as ex3 look -> expand as ex3 look
    Right (i, from, ex2) -> fallBack id ex2 (see i from ex2 look1)
    Left ex2 -> fallBack id ex2 look1
  where
    -- Warn after the rewind, so the warning is on the call's line.
    fallBack f ex look =
      let ex' = rewind ex0 ex look in scan e ex' {shared = f ex'.shared} (e lx out)
    expand as ex look
      | deepRecursion n ex = fallBack (warn (RecursiveMacro n)) ex look
      | length ex.names >= maxNesting = fallBack (limitHit ExpansionDepth) ex look
      | otherwise = case m of
          FunctionMacro arity body
            | Just as' <- matchArgs arity as ->
                let quoted = map (Text.replace "\"" "\\\"") as'
                    part = \case
                      Lit t -> t
                      Param k -> nth "" k as'
                      QuotedParam k -> nth "" k quoted
                    -- Each length once; worked out before the text is
                    -- made, so a huge one costs no memory.
                    lens = map Text.length as'
                    qlens = map Text.length quoted
                    size = sum (map (partSize lens qlens) body)
                 in if size > ex.shared.budget
                      then fallBack spent ex look
                      else pushOr (lexText (Text.concat (map part body))) ex look
            | otherwise -> fallBack (warn (WrongArgCount n)) ex look
          _ -> pushOr [Number "1"] ex look
    pushOr ts ex look =
      let g = callSpan lx.at look
       in case push n [Lexeme t g lx.hide | t <- ts] ex of
            Just ex' -> scan e ex' out
            Nothing -> fallBack spent ex look
    spent sh = (limitHit ExpansionWork sh) {budget = 0}
    partSize lens qlens = \case
      Lit t -> Text.length t
      Param k -> nth 0 k lens
      QuotedParam k -> nth 0 k qlens
    -- No parameters: only F() fits, as in gcc (F( ) has one argument).
    matchArgs arity as = case as of
      [a] | arity == 0, Text.null a -> Just []
      _ | length as == arity -> Just as
      _ -> Nothing

-- | The k-th, or the default. Arity was checked, so it's there.
nth :: a -> Int -> [a] -> a
nth d k xs = case drop k xs of
  x : _ -> x
  [] -> d

-- | Read blanks and (in code) line ends.
blanks :: Ex -> Look -> (Ex, Look)
blanks ex !look = case next ex of
  Right (i@(Item (Lexeme t@(Space _) _ _)), from, ex') ->
    blanks (charge (cost t) ex') (see i from ex' look)
  Right (i@(Break _), from, ex') -> blanks (charge 1 ex') (see i from ex' look)
  Right _ -> (ex, look)
  Left ex' -> (ex', look)

-- | Reading arguments: paren depth, the argument so far, the ones
-- done (reversed), how many, and what was read.
data Collect = Collect
  { depth :: Int,
    cur :: Acc,
    done :: [Text],
    count :: Int,
    look :: Look
  }

data ArgsResult = GotArgs [Text] Ex Look | NoArgs Failure Ex Look

data Failure = Unterminated | TooMuch Limit

-- | Read arguments after the paren, up to its match. Newlines in
-- arguments are spaces. Each token read costs its length: a call that
-- never ends, tried again and again, still runs out (Note [CPP limits]).
args :: Ex -> Collect -> ArgsResult
args ex !c
  | ex.shared.budget <= 0 = NoArgs (TooMuch ExpansionWork) ex c.look
  | c.count >= maxArgs = NoArgs (TooMuch ArgCount) ex c.look
  | otherwise = case next ex of
      Left ex' -> NoArgs Unterminated ex' c.look
      Right (i, from, ex') ->
        let c1 = c {look = see i from ex' c.look}
            took !k = c1 {cur = addText k c.cur}
         in case i of
              Break _ -> args (charge 1 ex') (took " ")
              Item (Lexeme t _ _) ->
                let ex'' = charge (cost t) ex'
                 in case t of
                      Punct "(" -> args ex'' (took "(") {depth = c.depth + 1}
                      Punct ")"
                        | c.depth == 0 -> GotArgs (reverse (arg : c.done)) ex'' c1.look
                        | otherwise -> args ex'' (took ")") {depth = c.depth - 1}
                      Punct ","
                        | c.depth == 0 ->
                            let c2 = c1 {cur = emptyAcc, done = arg : c.done}
                             in args ex'' c2 {count = c.count + 1}
                      _ -> args ex'' (took (tokText t))
  where
    arg = accText c.cur

-- | @defined X@ or @defined ( X )@ in @#if@: 1 or 0. The name isn't
-- expanded. Anything else stays as is, for the expression to reject.
definedOp :: Emit s -> Lexeme -> Ex -> s -> Scanned s
definedOp e lx ex out = case definedName ex of
  Just (x, ex') ->
    let v = if isDefined x ex.shared.macros then "1" else "0"
     in scan e ex' (e lx {tok = Number v} out)
  Nothing -> scan e ex (e lx out)

-- | The @X@ or @( X )@ after @defined@.
definedName :: Ex -> Maybe (Text, Ex)
definedName ex0 = case nonSpace ex0 of
  Just (Ident x, ex1) -> Just (x, ex1)
  Just (Punct "(", ex1) -> do
    (Ident x, ex2) <- nonSpace ex1
    (Punct ")", ex3) <- nonSpace ex2
    Just (x, ex3)
  _ -> Nothing
  where
    nonSpace ex = case next ex of
      Right (Item (Lexeme (Space _) _ _), _, ex') -> nonSpace ex'
      Right (Item lx, _, ex') -> Just (lx.tok, ex')
      _ -> Nothing

-- | gcc's rule for function-like macros: recursion only when one of
-- its bodies is more than 'funRecursion' levels down.
deepRecursion :: Text -> Ex -> Bool
deepRecursion n ex =
  or [d > funRecursion | (d, m) <- zip [1 :: Int ..] ex.names, m == n]

-- | Open a macro's body, if the budget has room. Costs its length.
push :: Text -> [Lexeme] -> Ex -> Maybe Ex
push n ls ex
  | k > ex.shared.budget = Nothing
  | otherwise =
      Just
        ( charge
            k
            ex
              { ctxs = Ctx (Just n) (map Item ls) : ex.ctxs,
                names = n : ex.names,
                openSet = Set.insert n ex.openSet
              }
        )
  where
    k = sum [Text.length (tokText l.tok) | l <- ls]

-- | Read these again, first. Onto text put back before, if that's on
-- top, so put-back text never stacks up.
putBack :: [Item] -> Ex -> Ex
putBack [] ex = ex
putBack is ex = case ex.ctxs of
  Ctx Nothing old : cs -> ex {ctxs = Ctx Nothing (is <> old) : cs}
  cs -> ex {ctxs = Ctx Nothing is : cs}

cost :: Tok -> Int
cost = max 1 . Text.length . tokText

-- | Spend work. Running out stops all expansion for the rest of the
-- file, with one warning.
charge :: Int -> Ex -> Ex
charge k ex =
  let sh = ex.shared {budget = ex.shared.budget - max 1 k}
   in if sh.budget > 0 then ex {shared = sh} else ex {shared = limitHit ExpansionWork sh}

-- | What a macro makes points at the whole call.
gen :: Where -> Where
gen = \case
  Src cs -> uncurry Gen (srcSpan cs)
  g -> g

-- | The span of a call: the name and everything read for it.
callSpan :: Where -> Look -> Where
callSpan w look = case look.end of
  Nothing -> gen w
  Just e -> Gen (startOf w) (max e (endOf w))

startOf :: Where -> Pos
startOf = \case
  Src cs -> fst (srcSpan cs)
  Gen a _ -> a

endOf :: Where -> Pos
endOf = \case
  Src cs -> snd (srcSpan cs)
  Gen _ b -> b

srcSpan :: [Chunk] -> (Pos, Pos)
srcSpan cs = case (cs, reverse cs) of
  (a : _, b : _) -> (a.from, b.from {col = b.from.col + Text.length b.text})
  -- A token always has text.
  _ -> (Pos 1 1, Pos 1 1)
