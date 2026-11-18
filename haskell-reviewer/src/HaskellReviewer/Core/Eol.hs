{-# LANGUAGE StrictData #-}

-- | git's line-end rules for the clean step ("git add"), as convert.c
-- has them, and the config they read. Pure: the attributes come from
-- @git check-attr@, the config from @git config@ (D-311, D-318). See
-- Note [Clean step].
module HaskellReviewer.Core.Eol
  ( -- * Attributes
    AttrState (..),
    Attrs (..),
    unspecifiedAttrs,
    attrNames,
    parseCheckAttr,
    Filtering (..),
    filtering,

    -- * Config
    AutoCrlf (..),
    ExecBit (..),
    Symlinks (..),
    CoreConfig (..),
    defaultCoreConfig,
    parseCoreConfig,
    gitBool,

    -- * Rules
    EolRule (..),
    eolRule,
    TextStats (..),
    textStats,
    convertIsBinary,
    hasCrlfInIndex,
    CleanForms (..),
    cleanForms,
    eolSure,
    CleanEol (..),
    applyEol,
    dropCrBeforeLf,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Unsafe qualified as BS.Unsafe
import Data.Char (isDigit, isHexDigit, isOctDigit, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Numeric (readHex, readOct)

{- Note [Clean step]

git hashes a work-tree file after its clean step: the filter driver,
working-tree-encoding, then line ends, then ident. We never run a
driver (SPEC §9.1). We do the line-end part, which is plain code in
convert.c:

- The action comes from the attributes: @text@, or @crlf@ if @text@ is
  unspecified (set: text; unset: binary; @input@: text; @auto@: auto).
  Then @eol@, unless binary: with auto, @lf@ or @crlf@ keep it auto;
  otherwise either makes it text. Still nothing: @core.autocrlf@ false
  is binary, true or @input@ is auto.
- Text drops each CR that comes right before an LF. Lone CRs stay.
- Auto does the same, unless the file looks binary
  ('convertIsBinary'), or the index blob has CRLF already
  ('hasCrlfInIndex'). Not binary means no lone CR, so then every CR is
  before an LF.

check-attr prints @set@, @unset@ and @unspecified@ for the three
states, and the value otherwise. A value that is one of those words
(@text=set@) looks like the state, and git reads it as a value: no
action, so @crlf@ or core.autocrlf decides. When the readings clean
the bytes differently ('eolSure'), the file can't be clean by hash:
it is dirty (D-318). The same goes for a filter driver named @unset@
or @unspecified@ ('stateDrivers').

filter, ident and working-tree-encoding: anything but unset or
unspecified makes the file "filtered". That is stricter than git (a
driver with no config does nothing there). We can't run those steps,
so a filtered file is never clean by hash: it is read from disk.
-}

-- * Attributes

-- | One attribute of one path, as check-attr prints it.
data AttrState
  = AttrSet
  | AttrUnset
  | AttrUnspecified
  | AttrValue ByteString
  deriving (Eq, Show)

-- | The attributes we ask for.
data Attrs = Attrs
  { text :: AttrState,
    eol :: AttrState,
    crlf :: AttrState,
    filter :: AttrState,
    ident :: AttrState,
    encoding :: AttrState
  }
  deriving (Eq, Show)

-- | Nothing set.
unspecifiedAttrs :: Attrs
unspecifiedAttrs =
  Attrs
    { text = AttrUnspecified,
      eol = AttrUnspecified,
      crlf = AttrUnspecified,
      filter = AttrUnspecified,
      ident = AttrUnspecified,
      encoding = AttrUnspecified
    }

-- | The names, in the order we pass them to check-attr.
attrNames :: [String]
attrNames = ["text", "eol", "crlf", "filter", "ident", "working-tree-encoding"]

-- | Parse @check-attr -z@: @\<path>\\0\<attr>\\0\<info>\\0@ per
-- attribute and path. An attribute we didn't ask for, or a cut record,
-- is an error.
parseCheckAttr :: ByteString -> Either Text (Map ByteString Attrs)
parseCheckAttr out = go Map.empty (fields out)
  where
    fields b = case BS.split 0 b of
      xs | Just (ys, "") <- unsnoc xs -> ys
      xs -> xs
    go acc = \case
      [] -> Right acc
      path : name : info : rest -> do
        let old = Map.findWithDefault unspecifiedAttrs path acc
        new <- set name (state info) old
        go (Map.insert path new acc) rest
      _ -> Left "check-attr: cut record"
    state = \case
      "set" -> AttrSet
      "unset" -> AttrUnset
      "unspecified" -> AttrUnspecified
      v -> AttrValue v
    set name v a = case name of
      "text" -> Right a {text = v}
      "eol" -> Right a {eol = v}
      "crlf" -> Right a {crlf = v}
      "filter" -> Right a {filter = v}
      "ident" -> Right a {ident = v}
      "working-tree-encoding" -> Right a {encoding = v}
      _ -> Left ("check-attr: unknown attribute " <> lenient name)
    unsnoc xs = case reverse xs of
      [] -> Nothing
      y : ys -> Just (reverse ys, y)

-- | Whether a file goes through more than line ends when git adds it.
data Filtering = Unfiltered | Filtered
  deriving (Eq, Show)

-- | A filter driver, ident or working-tree-encoding. See Note [Clean
-- step].
filtering :: CoreConfig -> Attrs -> Filtering
filtering cfg a
  | a.filter `elem` cfg.stateDrivers = Filtered
  | any on [a.filter, a.ident, a.encoding] = Filtered
  | otherwise = Unfiltered
  where
    on = \case
      AttrUnset -> False
      AttrUnspecified -> False
      _ -> True

-- * Config

-- | @core.autocrlf@.
data AutoCrlf = AutoCrlfFalse | AutoCrlfTrue | AutoCrlfInput
  deriving (Eq, Show)

-- | @core.filemode@: does the exec bit count?
data ExecBit = TrustExecBit | IgnoreExecBit
  deriving (Eq, Show)

-- | @core.symlinks@: off means a symlink can be checked out as a
-- plain file.
data Symlinks = SymlinksOn | SymlinksOff
  deriving (Eq, Show)

-- | The config the stat check and the clean step read.
data CoreConfig = CoreConfig
  { autocrlf :: AutoCrlf,
    fileMode :: ExecBit,
    symlinks :: Symlinks,
    -- | 'AttrUnset' or 'AttrUnspecified' when a filter driver of that
    -- name is configured: check-attr prints the same for the state and
    -- for @filter=unset@, so a file showing it may be filtered.
    stateDrivers :: [AttrState]
  }
  deriving (Eq, Show)

-- | git's defaults, when a key is not set.
defaultCoreConfig :: CoreConfig
defaultCoreConfig =
  CoreConfig
    { autocrlf = AutoCrlfFalse,
      fileMode = TrustExecBit,
      symlinks = SymlinksOn,
      stateDrivers = []
    }

-- | Parse @config -z --get-regexp@: @\<key>\\n\<value>\\0@, or
-- @\<key>\\0@ for a key with no value. The last one wins. A value git
-- can't read as a boolean is an error: git dies on it too. Keys we
-- don't use (trustctime, checkstat, ignorecase) are skipped. A
-- @filter.unset.*@ or @filter.unspecified.*@ key is a driver named like
-- a check-attr state (see 'stateDrivers').
parseCoreConfig :: ByteString -> Either Text CoreConfig
parseCoreConfig out = foldl' step (Right defaultCoreConfig) records
  where
    records = filter (not . BS.null) (BS.split 0 out)
    step acc r = do
      c <- acc
      let (key, rest) = Char8.break (== '\n') r
          value = snd <$> Char8.uncons rest
      case key of
        "core.autocrlf"
          | Just v <- value,
            Char8.map toLower v == "input" ->
              Right c {autocrlf = AutoCrlfInput}
          | otherwise -> do
              b <- bool key value
              Right c {autocrlf = if b then AutoCrlfTrue else AutoCrlfFalse}
        "core.filemode" -> do
          b <- bool key value
          Right c {fileMode = if b then TrustExecBit else IgnoreExecBit}
        "core.symlinks" -> do
          b <- bool key value
          Right c {symlinks = if b then SymlinksOn else SymlinksOff}
        _
          | "filter.unset." `BS.isPrefixOf` key -> Right (driver AttrUnset c)
          | "filter.unspecified." `BS.isPrefixOf` key -> Right (driver AttrUnspecified c)
          | otherwise -> Right c
    driver st c
      | st `elem` c.stateDrivers = c
      | otherwise = c {stateDrivers = st : c.stateDrivers}
    bool key value = case gitBool value of
      Just b -> Right b
      Nothing ->
        Left
          ( "bad boolean config value for "
              <> lenient key
              <> ": "
              <> lenient (fromMaybe "" value)
          )

-- | A config boolean, as git reads it: no value is true; an empty
-- value is false; true, yes, on, false, no, off in any case; else an
-- int (C syntax: leading blanks, a sign, 0x for hex, 0 for octal, then
-- k, m or g, within an int), true unless 0. 'Nothing' if git would refuse it.
gitBool :: Maybe ByteString -> Maybe Bool
gitBool = \case
  Nothing -> Just True
  Just v
    | BS.null v -> Just False
    | lower `elem` ["true", "yes", "on"] -> Just True
    | lower `elem` ["false", "no", "off"] -> Just False
    | otherwise -> (/= 0) <$> gitInt (Char8.unpack v)
    where
      lower = Char8.map toLower v

-- | git_parse_int: strtoimax with base 0, a unit, and the int range.
gitInt :: String -> Maybe Integer
gitInt s0 = do
  let s1 = dropWhile (`elem` (" \t\n\v\f\r" :: String)) s0
      (sign, s2) = case s1 of
        '-' : r -> (-1, r)
        '+' : r -> (1, r)
        r -> (1, r)
  (n, unit) <- number s2
  factor <- case map toLower unit of
    "" -> Just 1
    "k" -> Just 1024
    "m" -> Just (1024 * 1024)
    "g" -> Just (1024 * 1024 * 1024)
    _ -> Nothing
  let v = sign * n * factor
  if v > 2 ^ (31 :: Int) - 1 || v < -(2 ^ (31 :: Int)) then Nothing else Just v
  where
    number = \case
      '0' : x : r@(h : _)
        | x `elem` ("xX" :: String),
          isHexDigit h ->
            let (ds, rest) = span isHexDigit r
             in (,rest) <$> readWith readHex ds
      '0' : r ->
        let (ds, rest) = span isOctDigit r
         in Just (if null ds then 0 else fromMaybe 0 (readWith readOct ds), rest)
      r@(c : _)
        | isDigit c ->
            let (ds, rest) = span isDigit r
             in Just (read ds, rest)
      _ -> Nothing
    readWith f ds = case f ds of
      [(n, "")] -> Just n
      _ -> Nothing

-- * Rules

-- | What the clean step does to line ends. See Note [Clean step].
data EolRule
  = -- | Nothing.
    EolKeep
  | -- | Drop each CR before an LF.
    EolText
  | -- | The same, unless binary or the index blob has CRLF.
    EolAuto
  deriving (Eq, Show)

-- | convert.c's crlf_action, before it is folded into 'EolRule'.
data Action = ActUndefined | ActBinary | ActText | ActAuto | ActAutoEol
  deriving (Eq)

-- | The rule for a path. See Note [Clean step].
eolRule :: AutoCrlf -> Attrs -> EolRule
eolRule autocrlf a = case final of
  ActBinary -> EolKeep
  ActText -> EolText
  ActAuto -> EolAuto
  ActAutoEol -> EolAuto
  ActUndefined -> EolKeep
  where
    first = case action a.text of
      ActUndefined -> action a.crlf
      x -> x
    withEol
      | first == ActBinary = ActBinary
      | otherwise = case a.eol of
          AttrValue v
            | v `elem` ["lf", "crlf"] ->
                if first == ActAuto then ActAutoEol else ActText
          _ -> first
    final = case withEol of
      ActUndefined -> case autocrlf of
        AutoCrlfFalse -> ActBinary
        AutoCrlfTrue -> ActAuto
        AutoCrlfInput -> ActAuto
      x -> x
    action = \case
      AttrSet -> ActText
      AttrUnset -> ActBinary
      AttrUnspecified -> ActUndefined
      AttrValue "input" -> ActText
      AttrValue "auto" -> ActAuto
      AttrValue _ -> ActUndefined

-- | Whether the rule is sure for these bytes. check-attr prints
-- @set@ and @unset@ for the states and for those values; git reads a
-- value as no action (then @crlf@, then core.autocrlf). So we try each
-- reading of a set or unset @text@ and @crlf@ (at most 4), and the
-- rule is sure when they all clean the bytes the same way. See Note
-- [Clean step].
eolSure :: AutoCrlf -> Attrs -> ByteString -> Bool
eolSure autocrlf a b = case readings of
  [] -> True
  [_] -> True
  r : rs -> all (== forms r) (map forms rs)
  where
    readings = [a {text = t, crlf = c} | t <- literal a.text, c <- literal a.crlf]
    literal = \case
      AttrSet -> [AttrSet, AttrValue "set"]
      AttrUnset -> [AttrUnset, AttrValue "unset"]
      x -> [x]
    forms r = cleanForms (eolRule autocrlf r) b

-- | convert.c's gather_stats.
data TextStats = TextStats
  { nul :: Int,
    loneCr :: Int,
    loneLf :: Int,
    crlfs :: Int,
    printable :: Int,
    nonPrintable :: Int
  }
  deriving (Eq, Show)

-- | Count as gather_stats does. A CR then LF counts once, as CRLF. A
-- ^Z at the very end is not counted as non-printable.
textStats :: ByteString -> TextStats
textStats b = go 0 0 0 0 0 0 0
  where
    n = BS.length b
    at = BS.Unsafe.unsafeIndex b
    go :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> TextStats
    go !i !nl !lcr !llf !cl !pr !np
      | i >= n =
          TextStats
            { nul = nl,
              loneCr = lcr,
              loneLf = llf,
              crlfs = cl,
              printable = pr,
              nonPrintable = if n >= 1 && at (n - 1) == 26 then np - 1 else np
            }
      | otherwise = case at i of
          13
            | i + 1 < n && at (i + 1) == 10 -> go (i + 2) nl lcr llf (cl + 1) pr np
            | otherwise -> go (i + 1) nl (lcr + 1) llf cl pr np
          10 -> go (i + 1) nl lcr (llf + 1) cl pr np
          127 -> go (i + 1) nl lcr llf cl pr (np + 1)
          c
            | c >= 32 -> go (i + 1) nl lcr llf cl (pr + 1) np
            -- BS, HT, ESC, FF.
            | c `elem` [8, 9, 27, 12] -> go (i + 1) nl lcr llf cl (pr + 1) np
            | c == 0 -> go (i + 1) (nl + 1) lcr llf cl pr (np + 1)
            | otherwise -> go (i + 1) nl lcr llf cl pr (np + 1)

-- | convert.c's convert_is_binary: a lone CR, a NUL, or too many
-- non-printable bytes.
convertIsBinary :: TextStats -> Bool
convertIsBinary s =
  s.loneCr > 0 || s.nul > 0 || (s.printable `div` 128) < s.nonPrintable

-- | convert.c's has_crlf_in_index, on the index blob's bytes: it has
-- CRLF and is not binary.
hasCrlfInIndex :: ByteString -> Bool
hasCrlfInIndex b =
  BS.elem 13 b && not (convertIsBinary s) && s.crlfs > 0
  where
    s = textStats b

-- | The bytes git may hash for a file, before we know the index blob.
data CleanForms
  = -- | One answer.
    OneForm ByteString
  | -- | 'EolAuto' with CRLF and not binary: raw if the index blob has
    -- CRLF (git keeps the CRs), converted if not.
    AutoForms ByteString ByteString
  deriving (Eq, Show)

-- | The bytes git may hash for this file under this rule. For
-- 'AutoForms' a match on either is git's answer: if the raw bytes
-- match, the blob has CRLF, so git keeps them raw; if the converted
-- ones match, the blob has no CR, so git converts.
cleanForms :: EolRule -> ByteString -> CleanForms
cleanForms rule b = case rule of
  EolKeep -> OneForm b
  EolText -> OneForm (dropCrBeforeLf b)
  EolAuto
    | not (BS.elem 13 b) -> OneForm b
    | convertIsBinary (textStats b) -> OneForm b
    | otherwise -> AutoForms b (dropCrBeforeLf b)

-- | How to clean a file read from disk, decided at listing.
data CleanEol
  = -- | As it is.
    KeepRaw
  | -- | 'EolText'.
    DropCrBeforeLf
  | -- | 'EolAuto' with no CRLF in the index: decided by the bytes read.
    AutoOnRead
  deriving (Eq, Show)

-- | Clean bytes read from disk. Give it the whole file.
applyEol :: CleanEol -> ByteString -> ByteString
applyEol rule b = case rule of
  KeepRaw -> b
  DropCrBeforeLf -> dropCrBeforeLf b
  AutoOnRead
    | not (BS.elem 13 b) -> b
    | convertIsBinary (textStats b) -> b
    | otherwise -> dropCrBeforeLf b

-- | Drop each CR right before an LF.
dropCrBeforeLf :: ByteString -> ByteString
dropCrBeforeLf b = case BS.breakSubstring "\r\n" b of
  (_, rest) | BS.null rest -> b
  _ -> BS.intercalate "\n" (splitCrlf b)
  where
    splitCrlf s = case BS.breakSubstring "\r\n" s of
      (pre, rest)
        | BS.null rest -> [pre]
        | otherwise -> pre : splitCrlf (BS.drop 2 rest)

-- * Internal

lenient :: ByteString -> Text
lenient = Text.Encoding.decodeUtf8Lenient
