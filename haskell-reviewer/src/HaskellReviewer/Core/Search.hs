{-# LANGUAGE StrictData #-}

-- | Name search (SPEC §4.7, §11.4.10): an fzf-like scorer over module
-- and declaration names. Pure. See Note [Search].
module HaskellReviewer.Core.Search
  ( -- * Queries
    Query (..),
    Qualified (..),
    Pattern (..),
    MatchCase (..),
    parseQuery,
    maxQueryLength,

    -- * Candidates
    ModuleCandidate (..),
    moduleCandidates,
    forceCandidates,

    -- * Searching
    SearchResult (..),
    Hit (..),
    Indexing (..),
    search,
    searchLimit,

    -- * Internal, for tests
    makePattern,
    scoreText,
    scoreV1,
    scoreV2,
  )
where

import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Char
  ( isAlpha,
    isAlphaNum,
    isLower,
    isNumber,
    isSpace,
    isUpper,
    toLower,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Unsafe (Iter (..), iter, lengthWord8)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import HaskellReviewer.Core.Modules
  ( ModuleId (..),
    ModuleIndex (..),
    moduleNameFromPath,
    moduleRef,
    renderModuleId,
  )
import HaskellReviewer.Core.NameTable (NameEntry (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RelPath (relPathText)

{- Note [Search]

D-206. The query is matched against every module name and every name
the index has so far (Core.NameTable: types, classes, constructors,
fields, methods, values, pattern synonyms).

Query ('parseQuery'): all whitespace dropped; empty or over
'maxQueryLength' code points is no query. If it starts with one or more
@ident.@ (ident: a letter, then letters, digits, _ or ') and something
is left after them, it's also a qualified query: @Data.Map.insert@ is
module @Data.Map@ and name @insert@; same for @M.insert@, @Map.!@,
@Prelude..@. @.@, @<$>@ and @Map.@ are plain only.

Smart case, per part: a part with an upper case letter is matched
case-sensitively, else both sides go through 'toLower' (per char, no
Unicode normalisation). The module part of a qualified query is always
case-insensitive.

Scorer: a port of fzf's FuzzyMatchV2 (algorithm only). Match 16, gap
start -3, gap extension -1, consecutive bonus 4, first query char's
bonus x2. Bonus by the char before: start of text 10, after . or / 9,
after another symbol (like _) 8, lower to upper or letter to digit 7;
a matched symbol gets 8. ' counts as a letter. We only need the score,
so no backtrace:

- a one-char query: the best occurrence, 16 + 2 x its bonus. What V2
  gives, in one pass over the text, any length;
- else we check the query is a subsequence (one pass, no writes: most
  names fail here), then load the text's folded chars and bonuses into
  scratch arrays, and run the DP column by column, between the first
  match of the query's first char and the last match of its last char.
  One column of state (O(m));
- texts over 'maxV2Length' chars, or with n*m over 'maxV2Cells', use
  fzf's V1 instead (greedy forward, shrink backward, score that window;
  O(n)). A DP cell costs about 4 ns: the cap keeps any name under
  about 20 us (a 64-char query on a 64-char name), so many long names
  that match can't make a search slow. Both match exactly when the
  query is a subsequence.

One search makes the scratch arrays once, sized for the longest text
and the longest of the query's patterns (plain, module part, name
part), and works on the texts' UTF-8 directly: a name costs no
allocation unless it matches. Reads and writes are unchecked, so every
pattern scored must fit: 'search' sizes by all three, and 'scoreText'
makes its own scratch.

What is scored:
- a module: the whole query against its name (a loose module's name
  comes from its path; its path if it has none);
- a name: the whole query against the name, and, for a qualified
  query, the name part against the name plus the module part against
  the module's name (both must match; the module part is scored once
  per module). The better of the two counts.

Order, best first ('Key'): tier (name equal to the query, or to the
name part; equal ignoring case; the rest), score, exported first,
shorter name, kind (module, type, class, constructor, pattern synonym,
value, field), module ref, place in the module. A declaration is in
its module's list once ('moduleNames' drops repeats). A file in two
components is two modules: it shows twice, with different refs.

We keep the best 'searchLimit' in a map capped at that size (O(N log
K)), and count all matches. Once the map is full, a hit whose tier and
score are worse than the worst kept one is only counted.
-}

-- * Queries

-- | A parsed query. See Note [Search].
data Query = Query
  { -- | The whole query: for modules and names.
    plain :: Pattern,
    -- | @Data.Map.insert@: module part and name part.
    qualified :: Maybe Qualified
  }
  deriving (Eq, Show)

-- | A query like @Data.Map.insert@.
data Qualified = Qualified
  { -- | The module part, without the last dot. Always case-insensitive.
    qual :: Pattern,
    -- | The name part.
    base :: Pattern
  }
  deriving (Eq, Show)

-- | One part of a query.
data Pattern = Pattern
  { -- | As typed.
    text :: Text,
    matchCase :: MatchCase,
    -- | The chars to match, folded by 'matchCase'. Never empty in a
    -- parsed query.
    chars :: VU.Vector Char
  }
  deriving (Eq, Show)

-- | How a pattern treats case.
data MatchCase
  = CaseSensitive
  | -- | Both sides go through 'toLower'.
    IgnoreCase
  deriving (Eq, Show)

-- | The longest query, in code points.
maxQueryLength :: Int
maxQueryLength = 128

-- | 'Nothing' if blank or too long. See Note [Search].
parseQuery :: Text -> Maybe Query
parseQuery raw
  | Text.null t || Text.length t > maxQueryLength = Nothing
  | otherwise =
      Just
        Query
          { plain = smartCase t,
            qualified = case splitQualified t of
              Just (q, b) ->
                Just Qualified {qual = makePattern IgnoreCase q, base = smartCase b}
              Nothing -> Nothing
          }
  where
    t = Text.filter (not . isSpace) raw

-- | Case-sensitive if it has an upper case letter.
smartCase :: Text -> Pattern
smartCase t = makePattern (if Text.any isUpper t then CaseSensitive else IgnoreCase) t

-- | A pattern for this text. An empty one matches nothing.
makePattern :: MatchCase -> Text -> Pattern
makePattern mc t =
  Pattern
    { text = t,
      matchCase = mc,
      chars = VU.fromList (map (fold mc) (Text.unpack t))
    }

-- | The longest prefix of @ident.@ parts, and the rest, if both are
-- there.
splitQualified :: Text -> Maybe (Text, Text)
splitQualified t = case go 0 t of
  0 -> Nothing
  n
    | n >= Text.length t -> Nothing
    | otherwise -> Just (Text.take (n - 1) t, Text.drop n t)
  where
    -- How many chars the @ident.@ parts take.
    go n s = case Text.uncons s of
      Just (c, rest)
        | isAlpha c,
          (ident, after) <- Text.span isIdentChar rest,
          Just ('.', more) <- Text.uncons after ->
            go (n + 1 + Text.length ident + 1) more
      _ -> n
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- * Candidates

-- | A module, as search sees it.
data ModuleCandidate = ModuleCandidate
  { id :: ModuleId,
    -- | The module name. A loose module's comes from its path; if it
    -- has none, it's the path.
    name :: Text,
    -- | For URLs ('moduleRef').
    ref :: Text
  }
  deriving (Eq, Show)

-- | Every module of the index, by id.
moduleCandidates :: ModuleIndex -> Map ModuleId ModuleCandidate
moduleCandidates idx = Map.fromSet candidate idx.ids
  where
    candidate i = ModuleCandidate {id = i, name = nameOf i, ref = moduleRef idx i}
    nameOf = \case
      ComponentModule _ _ (ModuleName n) -> n
      LooseModule p -> case moduleNameFromPath p of
        Just (ModuleName n) -> n
        Nothing -> relPathText p

-- | Force them fully, ids too. See Note [Forced snapshot] in
-- "HaskellReviewer.Server.Api".
forceCandidates :: Map ModuleId ModuleCandidate -> ()
forceCandidates =
  Map.foldr (\c r -> Text.length (renderModuleId c.id) `seq` c `seq` r) ()

-- * Searching

-- | The best hits and how many matched in all.
data SearchResult = SearchResult
  { -- | All the hits, before the cap.
    matches :: Int,
    -- | Best first.
    hits :: [Hit]
  }
  deriving (Eq, Show)

-- | One result.
data Hit
  = ModuleHit ModuleCandidate
  | -- | A name and its module.
    DeclHit ModuleCandidate NameEntry
  deriving (Eq, Show)

-- | Whether the names searched are all there is.
data Indexing
  = -- | Indexing isn't done: names may be missing.
    Partial
  | Complete
  deriving (Eq, Show)

-- | How many hits we send.
searchLimit :: Int
searchLimit = 50

-- | Search modules and names, keep the best @limit@. See Note [Search].
search ::
  Int -> Query -> Map ModuleId ModuleCandidate -> Map ModuleId [NameEntry] -> SearchResult
search limit q modules names = runST $ do
  sc <- newScratch longestPattern longest
  afterModules <- foldM (moduleStep sc) (Found 0 Map.empty) modules
  Found count best <- foldM (namesStep sc) afterModules (Map.toList names)
  pure SearchResult {matches = count, hits = Map.elems best}
  where
    -- Every pattern must fit the scratch: Note [Search].
    longestPattern =
      maximum
        ( VU.length q.plain.chars
            : concat
              [ [VU.length qq.qual.chars, VU.length qq.base.chars]
              | Just qq <- [q.qualified]
              ]
        )
    longest =
      max
        (Map.foldl' (\a m -> max a (lengthWord8 m.name)) 0 modules)
        (Map.foldl' (foldl' (\a e -> max a (lengthWord8 e.name))) 0 names)
    moduleStep :: Scratch s -> Found -> ModuleCandidate -> ST s Found
    moduleStep sc acc m =
      rankWith sc q.plain m.name >>= \case
        Nothing -> pure acc
        Just r ->
          pure $!
            offer
              limit
              acc
              Offered
                { rank = r,
                  name = m.name,
                  visibility = Exported,
                  kind = OfModule,
                  candidate = m,
                  place = Nothing,
                  hit = ModuleHit m
                }
    namesStep :: Scratch s -> Found -> (ModuleId, [NameEntry]) -> ST s Found
    namesStep sc acc (i, entries) = case Map.lookup i modules of
      Nothing -> pure acc
      Just m -> do
        -- The module part, once per module.
        qualScore <- case q.qualified of
          Just qq -> scoreWith sc qq.qual m.name
          Nothing -> pure Nothing
        let go !a _ [] = pure a
            go !a !ix (e : more) = case declKind e.decl of
              Nothing -> go a (ix + 1) more
              Just kind ->
                declRank sc qualScore e.name >>= \case
                  Just r ->
                    let offered =
                          Offered
                            { rank = r,
                              name = e.name,
                              visibility = if e.exported then Exported else Private,
                              kind = kind,
                              candidate = m,
                              place = Just ix,
                              hit = DeclHit m e
                            }
                     in go (offer limit a offered) (ix + 1) more
                  Nothing -> go a (ix + 1) more
        go acc (0 :: Int) entries
    declRank :: Scratch s -> Maybe Int -> Text -> ST s (Maybe Rank)
    declRank sc qualScore name = do
      plain <- rankWith sc q.plain name
      qualified <- case (q.qualified, qualScore) of
        (Just qq, Just s) ->
          rankWith sc qq.base name >>= \case
            Just (Rank t (Down b)) -> pure $! Just $! Rank t (Down (b + s))
            Nothing -> pure Nothing
        _ -> pure Nothing
      pure $! case (plain, qualified) of
        (Just a, Just b) -> Just (min a b)
        (a, Nothing) -> a
        (Nothing, b) -> b

-- | The hits so far: how many, and the best ones.
data Found = Found Int (Map Key Hit)

-- | A hit that matched, before it's counted.
data Offered = Offered
  { rank :: Rank,
    name :: Text,
    visibility :: Visibility,
    kind :: Kind,
    candidate :: ModuleCandidate,
    -- | Place in the module. 'Nothing' for the module itself.
    place :: Maybe Int,
    hit :: Hit
  }

-- | Exported first.
data Visibility = Exported | Private
  deriving (Eq, Ord)

-- | Count a hit, and keep it if it's among the best @limit@. The key is
-- only built if the tier and score can get it in.
offer :: Int -> Found -> Offered -> Found
-- Inlined, so the record is never built: this runs for every hit.
{-# INLINE offer #-}
offer limit (Found n best) o
  | Map.size best < limit = Found (n + 1) (Map.insert k o.hit best)
  | Just (worst, _) <- Map.lookupMax best,
    o.rank <= worst.rank,
    k < worst =
      Found (n + 1) (Map.insert k o.hit (Map.deleteMax best))
  | otherwise = Found (n + 1) best
  where
    k =
      Key
        { rank = o.rank,
          visibility = o.visibility,
          len = Text.length o.name,
          kind = o.kind,
          ref = o.candidate.ref,
          moduleId = o.candidate.id,
          place = o.place
        }

-- | Tier and score: lower is better.
data Rank = Rank Tier (Down Int)
  deriving (Eq, Ord)

-- | How close a name is to the query.
data Tier = Exact | ExactIgnoringCase | Other
  deriving (Eq, Ord)

-- | Sort key, best first. See Note [Search].
data Key = Key
  { rank :: Rank,
    visibility :: Visibility,
    len :: Int,
    kind :: Kind,
    ref :: Text,
    moduleId :: ModuleId,
    -- | Place in the module. 'Nothing' for the module itself.
    place :: Maybe Int
  }
  deriving (Eq, Ord)

-- | Kinds, in sort order.
data Kind = OfModule | OfType | OfClass | OfCon | OfPat | OfVal | OfField
  deriving (Eq, Ord)

-- | 'Nothing' for instances: they have no name.
declKind :: DeclId -> Maybe Kind
declKind = \case
  Named DType _ -> Just OfType
  Named DClass _ -> Just OfClass
  Named DCon _ -> Just OfCon
  Named DPat _ -> Just OfPat
  Named DVal _ -> Just OfVal
  Field _ _ -> Just OfField
  Inst _ -> Nothing

-- | 'Nothing' if it doesn't match.
rankWith :: Scratch s -> Pattern -> Text -> ST s (Maybe Rank)
rankWith sc p t =
  -- Strict: a lazy answer is a thunk for every name.
  scoreWith sc p t >>= \case
    Just s -> pure $! Just $! Rank (tier p t) (Down s)
    Nothing -> pure Nothing

tier :: Pattern -> Text -> Tier
tier p t
  | Text.compareLength t (VU.length p.chars) /= EQ = Other
  | t == p.text = Exact
  | Text.map toLower t == Text.map toLower p.text = ExactIgnoringCase
  | otherwise = Other

-- * Scoring

-- | Longer texts use V1. See Note [Search].
maxV2Length :: Int
maxV2Length = 1024

-- | Texts with more DP cells (n*m) use V1. See Note [Search].
maxV2Cells :: Int
maxV2Cells = 4096

-- | How well the pattern matches: 'Nothing' if it's not a subsequence.
-- Higher is better. See Note [Search].
scoreText :: Pattern -> Text -> Maybe Int
scoreText p t = runST $ do
  sc <- newScratch (VU.length p.chars) (lengthWord8 t)
  scoreWith sc p t

-- | fzf's FuzzyMatchV2 alone, whatever the length, for tests.
scoreV2 :: Pattern -> Text -> Maybe Int
scoreV2 p t = runST $ do
  sc <- newScratch (VU.length p.chars) (lengthWord8 t)
  loaded sc p t >>= \case
    Nothing -> pure Nothing
    Just l -> Just <$> v2 sc p.chars l.lastIdx

-- | fzf's FuzzyMatchV1 alone, whatever the length, for tests.
scoreV1 :: Pattern -> Text -> Maybe Int
scoreV1 p t = runST $ do
  sc <- newScratch (VU.length p.chars) (lengthWord8 t)
  loaded sc p t >>= \case
    Nothing -> pure Nothing
    Just l -> v1 sc p.chars l.count

-- | Per-search buffers. See Note [Search].
data Scratch s = Scratch
  { -- | The text's chars, folded.
    txt :: VUM.MVector s Char,
    -- | Each char's bonus.
    bonuses :: VUM.MVector s Int,
    -- | Where each query char first matches, greedily.
    firsts :: VUM.MVector s Int,
    -- | The DP column: score, run length, in a gap.
    hs :: VUM.MVector s Int,
    cs :: VUM.MVector s Int,
    gaps :: VUM.MVector s Bool
  }

-- | For queries up to @m@ chars and texts up to @n@ bytes.
newScratch :: Int -> Int -> ST s (Scratch s)
newScratch m n = do
  txt <- VUM.new (max 1 n)
  bonuses <- VUM.new (max 1 n)
  firsts <- VUM.new (max 1 m)
  hs <- VUM.new (max 1 m)
  cs <- VUM.new (max 1 m)
  gaps <- VUM.new (max 1 m)
  pure
    Scratch
      { txt = txt,
        bonuses = bonuses,
        firsts = firsts,
        hs = hs,
        cs = cs,
        gaps = gaps
      }

-- | The score, picking the algorithm. The scratch must fit.
scoreWith :: Scratch s -> Pattern -> Text -> ST s (Maybe Int)
scoreWith sc p t
  | m == 0 = pure Nothing
  | m == 1 = pure $! single p t
  | otherwise =
      loaded sc p t >>= \case
        Nothing -> pure Nothing
        Just Loaded {count = n, lastIdx = lastIdx}
          | n > maxV2Length || n * m > maxV2Cells -> v1 sc p.chars n
          | otherwise -> v2 sc p.chars lastIdx >>= \s -> pure $! Just s
  where
    m = VU.length p.chars

-- | A one-char query: the best occurrence. No arrays.
single :: Pattern -> Text -> Maybe Int
single p t = go 0 White 0
  where
    p0 = VU.unsafeIndex p.chars 0
    end = lengthWord8 t
    go !i !prev !best
      | i >= end = if best > 0 then Just best else Nothing
      | otherwise =
          let Iter c d = iter t i
              cls = charClass c
              best'
                | fold p.matchCase c == p0 = max best (scoreMatch + 2 * bonusFor prev cls)
                | otherwise = best
           in go (i + d) cls best'

-- | The pattern's chars appear in this order. No writes.
subsequence :: Pattern -> Text -> Bool
subsequence p t = Text.foldl' step 0 t == m
  where
    m = VU.length p.chars
    step !k c
      | k < m && fold p.matchCase c == VU.unsafeIndex p.chars k = k + 1
      | otherwise = k

-- | What 'load' found.
data Loaded = Loaded
  { -- | The text's length, in chars.
    count :: Int,
    -- | Where the query's last char last matches.
    lastIdx :: Int
  }

-- | If it's a subsequence, the text loaded.
loaded :: Scratch s -> Pattern -> Text -> ST s (Maybe Loaded)
loaded sc p t
  | VU.null p.chars || not (subsequence p t) = pure Nothing
  | otherwise = Just <$> load sc p t

-- | Write the text's folded chars, their bonuses and the greedy first
-- matches into the scratch. The pattern must be a subsequence.
load :: Scratch s -> Pattern -> Text -> ST s Loaded
load sc p t = go 0 0 White 0 0
  where
    pat = p.chars
    m = VU.length pat
    lastChar = VU.unsafeLast pat
    end = lengthWord8 t
    go !i !j !prev !k !lastIdx
      | i >= end = pure Loaded {count = j, lastIdx = lastIdx}
      | otherwise = do
          let Iter c d = iter t i
              cls = charClass c
              f = fold p.matchCase c
          VUM.unsafeWrite sc.txt j f
          VUM.unsafeWrite sc.bonuses j (bonusFor prev cls)
          k' <-
            if k < m && f == VU.unsafeIndex pat k
              then VUM.unsafeWrite sc.firsts k j >> pure (k + 1)
              else pure k
          go (i + d) (j + 1) cls k' (if f == lastChar then j else lastIdx)

fold :: MatchCase -> Char -> Char
fold = \case
  CaseSensitive -> id
  IgnoreCase -> toLower

-- | fzf's char classes, in fzf's order: 'bonusFor' compares them.
data CharClass = White | NonWord | Delimiter | Lower | Upper | Letter | Number
  deriving (Eq, Ord)

charClass :: Char -> CharClass
charClass c
  | isLower c = Lower
  | isUpper c = Upper
  | isNumber c = Number
  | isAlpha c || c == '\'' = Letter
  | isSpace c = White
  | c == '.' || c == '/' = Delimiter
  | otherwise = NonWord

-- | The bonus of a char, by its class and the one before it.
bonusFor :: CharClass -> CharClass -> Int
bonusFor prev cls
  | cls > NonWord, prev == White = 10
  | cls > NonWord, prev == Delimiter = 9
  | cls > NonWord, prev == NonWord = 8
  | prev == Lower && cls == Upper = 7
  | prev /= Number && cls == Number = 7
  | cls == NonWord || cls == Delimiter = 8
  | cls == White = 10
  | otherwise = 0

-- | fzf's FuzzyMatchV2, score only, one column at a time, on a loaded
-- text. See Note [Search].
v2 :: Scratch s -> VU.Vector Char -> Int -> ST s Int
v2 sc pat lastIdx = do
  reset 0
  f0 <- VUM.unsafeRead sc.firsts 0
  column f0 0 0
  where
    m = VU.length pat
    reset i
      | i >= m = pure ()
      | otherwise = do
          VUM.unsafeWrite sc.hs i 0
          VUM.unsafeWrite sc.cs i 0
          VUM.unsafeWrite sc.gaps i False
          reset (i + 1)
    column !j !top !best
      | j > lastIdx = pure best
      | otherwise = do
          -- The rows that started: row i starts at its first match.
          top' <- grow j top
          best' <- rows j top' best
          column (j + 1) top' best'
    grow !j !r
      | r + 1 < m = do
          f <- VUM.unsafeRead sc.firsts (r + 1)
          if f <= j then grow j (r + 1) else pure r
      | otherwise = pure r
    -- Bottom up, so row i - 1 still has column j - 1.
    rows !j !i !best
      | i < 0 = pure best
      | otherwise = do
          score <- cell j i
          rows j (i - 1) (if i == m - 1 then max best score else best)
    cell j i = do
      ch <- VUM.unsafeRead sc.txt j
      b <- VUM.unsafeRead sc.bonuses j
      let matched = ch == VU.unsafeIndex pat i
      if i == 0
        then
          if matched
            then do
              let s = scoreMatch + b * 2
              write i s 1 False
              pure s
            else do
              prev <- VUM.unsafeRead sc.hs 0
              inGap <- VUM.unsafeRead sc.gaps 0
              let s = max 0 (prev + gapScore inGap)
              write i s 0 True
              pure s
        else do
          left <- VUM.unsafeRead sc.hs i
          inGap <- VUM.unsafeRead sc.gaps i
          diag <- VUM.unsafeRead sc.hs (i - 1)
          cdiag <- VUM.unsafeRead sc.cs (i - 1)
          let s2 = left + gapScore inGap
          -- No tuples here: this runs for every cell.
          if not matched
            then do
              let s = max 0 s2
              write i s 0 (0 < s2)
              pure s
            else do
              let c0 = cdiag + 1
              chunkStart <- VUM.unsafeRead sc.bonuses (j - c0 + 1)
              -- A boundary in the middle of a run starts a new run.
              let newRun = c0 > 1 && b >= 8 && b > chunkStart
                  b'
                    | c0 <= 1 || newRun = b
                    | otherwise = max b (max bonusConsecutive chunkStart)
                  c1 = if newRun then 1 else c0
                  run = diag + scoreMatch + b'
                  keepRun = run >= s2
                  s1 = if keepRun then run else diag + scoreMatch + b
                  s = max 0 (max s1 s2)
              write i s (if keepRun then c1 else 0) (s1 < s2)
              pure s
    write i s c g = do
      VUM.unsafeWrite sc.hs i s
      VUM.unsafeWrite sc.cs i c
      VUM.unsafeWrite sc.gaps i g

-- | fzf's FuzzyMatchV1 on a loaded text of @n@ chars: the first match
-- going forward, shrunk going back, then scored like V2 scores one
-- path. O(n).
v1 :: Scratch s -> VU.Vector Char -> Int -> ST s (Maybe Int)
v1 sc pat n =
  forward 0 0 >>= \case
    Nothing -> pure Nothing
    Just end ->
      backward (m - 1) end >>= \case
        Nothing -> pure Nothing
        Just start -> Just <$> window start (end + 1)
  where
    m = VU.length pat
    at j = VUM.unsafeRead sc.txt j
    -- The index of the last char of the first greedy match.
    forward !i !j
      | j >= n = pure Nothing
      | otherwise = do
          c <- at j
          if c == VU.unsafeIndex pat i
            then if i == m - 1 then pure (Just j) else forward (i + 1) (j + 1)
            else forward i (j + 1)
    backward !i !j
      | j < 0 = pure Nothing
      | otherwise = do
          c <- at j
          if c == VU.unsafeIndex pat i
            then if i == 0 then pure (Just j) else backward (i - 1) (j - 1)
            else backward i (j - 1)
    window start end = go start 0 0 False 0 0
      where
        go !j !pidx !score !inGap !consecutive !firstBonus
          | j >= end || pidx >= m = pure score
          | otherwise = do
              c <- at j
              if c == VU.unsafeIndex pat pidx
                then do
                  b <- VUM.unsafeRead sc.bonuses j
                  let (bonus, firstBonus')
                        | consecutive == 0 = (b, b)
                        | otherwise =
                            let fb = if b >= 8 && b > firstBonus then b else firstBonus
                             in (max (max b fb) bonusConsecutive, fb)
                      add = if pidx == 0 then bonus * 2 else bonus
                      score' = score + scoreMatch + add
                  go (j + 1) (pidx + 1) score' False (consecutive + 1) firstBonus'
                else go (j + 1) pidx (score + gapScore inGap) True (0 :: Int) 0

scoreMatch, bonusConsecutive :: Int
scoreMatch = 16
bonusConsecutive = 4

-- | Gap start -3, gap extension -1.
gapScore :: Bool -> Int
gapScore inGap = if inGap then -1 else -3
