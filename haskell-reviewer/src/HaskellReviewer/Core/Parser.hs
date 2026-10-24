{-# LANGUAGE StrictData #-}
-- fakeSettings leaves most fields out. See Note [Parser settings].
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Parse one module with ghc-lib-parser, the way GHC would after Cabal
-- set the component's flags (SPEC §3, §3.1). Pure: GHC's parser runs in
-- its own pure monad, and we set the flags ourselves.
module HaskellReviewer.Core.Parser
  ( -- * Settings
    ParseSettings (..),
    FileKind (..),
    defaultParseSettings,
    moduleParseSettings,

    -- * Parsing
    parseSource,

    -- * Helpers
    canonicalExtensionNames,
  )
where

import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (StringBuffer, stringBufferFromByteString)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.DynFlags (DynFlags, language, safeHaskell)
import GHC.Driver.Flags
  ( GeneralFlag (Opt_Haddock),
    OnOff (..),
    extensionName,
    impliedXFlags,
  )
import GHC.Driver.Flags qualified as Ghc (Language (..))
import GHC.Driver.Session
  ( FlagSpec (..),
    defaultDynFlags,
    gopt_set,
    lang_set,
    supportedLanguagesAndExtensions,
    xFlags,
    xopt,
    xopt_set,
    xopt_unset,
  )
import GHC.LanguageExtensions qualified as LangExt
import GHC.Parser qualified as Ghc (parseModule, parseSignature)
import GHC.Parser.Errors.Types (PsHeaderMessage (..), PsMessage (..))
import GHC.Parser.Header (toArgs)
import GHC.Parser.Lexer
  ( P (..),
    PState (..),
    ParseResult (..),
    ParserOpts,
    Token (..),
    getPsErrorMessages,
    initParserState,
    initPragState,
    lexer,
  )
import GHC.Platform (genericPlatform, platformArchOS)
import GHC.Settings
  ( FileSettings (..),
    GhcNameVersion (..),
    PlatformMisc (..),
    ToolSettings (..),
    UnitSettings (..),
  )
import GHC.Settings qualified as Ghc (Settings (..))
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Error
  ( Diagnostic (..),
    MsgEnvelope (..),
    defaultDiagnosticOpts,
    getMessages,
    unDecorated,
  )
import GHC.Types.SafeHaskell (SafeHaskellMode (..))
import GHC.Types.SrcLoc
  ( GenLocated (..),
    RealSrcLoc,
    SrcSpan (..),
    combineSrcSpans,
    getLoc,
    mkRealSrcLoc,
    mkSrcSpanPs,
    realSrcSpanStart,
    srcSpanToRealSrcSpan,
    unLoc,
  )
import GHC.Unit.Types (stringToUnitId)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
import HaskellReviewer.Core.Cpp (CppEnv (..), CppOutput (..), runCpp)
import HaskellReviewer.Core.Cpp qualified as Cpp (CppProblem (..))
import HaskellReviewer.Core.ModuleModel
  ( Extensions (..),
    ModuleParse (..),
    ParseError (..),
    ParseWarning (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    forceModuleParse,
  )
import HaskellReviewer.Core.Modules
  ( ComponentSettings (..),
    HaskellFile (..),
    Literate (..),
    Tools (..),
    toolsFrom,
  )
import HaskellReviewer.Core.Parser.Convert (convertModule, toSpan)
import HaskellReviewer.Core.Project
  ( Extension (..),
    GhcVersion,
    Language,
    hostPlatform,
    parserGhcVersion,
  )
import HaskellReviewer.Core.Project qualified as Project (Language (..))
import HaskellReviewer.Core.RelPath (relPathName)
import HaskellReviewer.Core.SourceText
  ( ColumnMap,
    columnMap,
    stripBom,
    unlitBirdLatex,
    unlitMarkdown,
  )

{- Note [Parser settings]

GHC's flags come from three places, in this order (Cabal's):

1. The component: default-language, then default-extensions, then the
   -X... and -cpp words of ghc-options. A name GHC doesn't know: a
   warning, skipped. An unknown language: a warning, Haskell98 (D-84).
2. The module's LANGUAGE and OPTIONS_GHC pragmas, read by our copy of
   GHC's getOptions (readPragmas). Only -X..., -cpp and the LANGUAGE
   names are applied. -F and -pgmF count with ghc-options for "is this
   preprocessed?" (D-103). Any other OPTIONS_GHC word never reaches
   GHC's flag code: it's an IgnoredPragmaFlag warning. -pgmL in a pragma
   does nothing, like in GHC (unlit runs before GHC reads the pragmas).
3. Haddock is always on, so we get the doc comments.
4. Safe Haskell, once all flags are in, like GHC's safeFlagCheck: under
   Safe, TemplateHaskell, GeneralizedNewtypeDeriving and DerivingVia are
   turned off, with a warning each. Two different modes (Safe, then
   Unsafe) is an error in GHC, which keeps the first. We keep the first
   too, with a warning, and go on.

Only names that were applied go in fromPragmas. Unknown ones are
warnings.

We set the flags ourselves (xopt_set and friends, plus the implied
flags) instead of calling parseDynamicFlags: that one is IO, and it
would act on any flag the code under review gives it.

An unknown name in a LANGUAGE pragma fails the module, with GHC's
error: GHC fails it too. getOptions reports that by throwing in pure
code, so readPragmas is a copy that returns an Either. A lexer error in
the header ends the tokens with an EOF, as in GHC, so a broken LANGUAGE
pragma is an error too.

fakeSettings is from ghc-lib's mini-hlint example. Only the fields the
parser reads are set; the others are left out (-Wno-missing-fields) and
would throw if read. The parser never reads them.
-}

-- * Settings

-- | All the parse depends on, apart from the text. Part of the parse
-- key (SPEC §11.4.4).
data ParseSettings = ParseSettings
  { language :: Language,
    extensions :: [Extension],
    -- | The component's, as written.
    ghcOptions :: [Text],
    literate :: Literate,
    kind :: FileKind,
    cpp :: CppEnv
  }
  deriving (Eq, Show)

-- | How GHC parses the file.
data FileKind
  = ModuleFile
  | -- | A Backpack signature (.hsig, .lhsig).
    SignatureFile
  deriving (Eq, Show)

-- | GHC2021 and nothing else, as for files outside any component (D-84).
defaultParseSettings :: ParseSettings
defaultParseSettings =
  ParseSettings
    { language = Project.GHC2021,
      extensions = [],
      ghcOptions = [],
      literate = NotLiterate,
      kind = ModuleFile,
      cpp =
        CppEnv
          { ghc = parserGhcVersion,
            platform = hostPlatform,
            cppOptions = [],
            buildDepends = []
          }
    }

-- | The settings for a file of the module index, with the project's
-- GHC. A .hsig or .lhsig file is a signature, as for GHC.
moduleParseSettings :: GhcVersion -> ComponentSettings -> HaskellFile -> ParseSettings
moduleParseSettings ghc c file =
  ParseSettings
    { language = c.language,
      extensions = c.extensions,
      ghcOptions = c.ghcOptions,
      literate = file.literate,
      kind = if signature then SignatureFile else ModuleFile,
      cpp =
        CppEnv
          { ghc = ghc,
            platform = hostPlatform,
            cppOptions = c.cppOptions,
            buildDepends = c.buildDepends
          }
    }
  where
    signature = any (`Text.isSuffixOf` relPathName file.path) [".hsig", ".lhsig"]

-- * Parsing

-- | Parse a module's text. The result is fully forced once it's in WHNF,
-- so nothing in it points at GHC's AST (D-61).
--
-- GHC can still throw on a bug of its own: callers catch that (see
-- "HaskellReviewer.Effects.Parser").
parseSource :: ParseSettings -> Text -> ModuleParse
parseSource settings input = forceModuleParse r `seq` r
  where
    r = run settings input

run :: ParseSettings -> Text -> ModuleParse
run settings input = case readPragmas (initParserOpts comp.dflags) (toBuffer text) of
  Left e -> textOnly (PragmaFailed (headerError text e)) []
  Right args
    | Just tool <- (toolsFrom (settings.ghcOptions <> args)).pgmF ->
        textOnly (Preprocessed tool) []
    | not (xopt LangExt.Cpp pragmas.dflags) -> parseWith pragmas text
    | otherwise -> case runCpp settings.cpp text of
        -- GHC reads the pragmas again from cpp's output. runCpp gives
        -- back the input, so there's nothing new to read.
        Left Cpp.CppNotSupported -> textOnly CppNotSupported pragmas.warnings
        Right out -> parseWith pragmas out.text
    where
      pragmas = applyPragmas comp.dflags args
  where
    (text, unlitWarnings) = case settings.literate of
      NotLiterate -> (stripBom input, [])
      BirdOrLatex -> (unlitBirdLatex (stripBom input), [])
      MarkdownUnlit -> case unlitMarkdown (stripBom input) of
        Just t -> (t, [])
        Nothing -> ("", [NoMarkdownCode])
    comp = componentFlags settings
    early = unlitWarnings <> comp.warnings
    textOnly reason later =
      TextOnly TextOnlyModule {reason = reason, warnings = early <> later}
    parseWith pragmas t =
      case unP parser (initParserState opts (toBuffer t) startLoc) of
        PFailed st -> failed (errorsOf st)
        POk st m -> case errorsOf st of
          [] -> cols `seq` Parsed (convertModule cols exts (early <> pragmas.warnings) m)
          errs -> failed errs
      where
        parser = case settings.kind of
          ModuleFile -> Ghc.parseModule
          SignatureFile -> Ghc.parseSignature
        opts = initParserOpts pragmas.dflags
        cols = columnMap t
        exts = extensionsOf pragmas.dflags pragmas.names
        errorsOf st = toList (getMessages (getPsErrorMessages st))
        failed errs = textOnly (ParseFailed (firstError errs)) pragmas.warnings
        firstError = \case
          e : _ -> diagnosticError cols (errMsgSpan e) (errMsgDiagnostic e)
          -- PFailed always has an error. Just in case.
          [] -> ParseError {message = "parse failed", code = Nothing, span = Nothing}

-- | The text as GHC's buffer.
toBuffer :: Text -> StringBuffer
toBuffer = stringBufferFromByteString . Text.Encoding.encodeUtf8

-- | GHC wants a file name in its locations. We never show it.
startLoc :: RealSrcLoc
startLoc = mkRealSrcLoc (mkFastString "input") 1 1

-- * Flags

-- | Flags and what we had to work around.
data Flags = Flags
  { dflags :: DynFlags,
    warnings :: [ParseWarning]
  }

-- | The component's flags. See Note [Parser settings].
componentFlags :: ParseSettings -> Flags
componentFlags settings =
  Flags {dflags = withExtensions, warnings = langWarnings <> reverse extWarnings}
  where
    base = gopt_set (defaultDynFlags fakeSettings) Opt_Haddock
    (lang, langWarnings) = case settings.language of
      Project.Haskell98 -> (Ghc.Haskell98, [])
      Project.Haskell2010 -> (Ghc.Haskell2010, [])
      Project.GHC2021 -> (Ghc.GHC2021, [])
      Project.GHC2024 -> (Ghc.GHC2024, [])
      Project.UnknownLanguage t -> (Ghc.Haskell98, [UnknownLanguage t])
    withLanguage = lang_set base (Just lang)
    names =
      [if e.enabled then e.name else "No" <> e.name | e <- settings.extensions]
        <> [n | o <- settings.ghcOptions, Just n <- [Text.stripPrefix "-X" o]]
        <> ["CPP" | "-cpp" `elem` settings.ghcOptions]
    -- Warnings in reverse.
    (withExtensions, extWarnings) = foldl' step (withLanguage, []) names
    step (df, ws) n = case applyName n df of
      Applied df' -> (df', ws)
      AppliedWith w df' -> (df', w : ws)
      Unknown -> (df, UnknownExtension n : ws)

-- | All the module's flags: the component's, then its pragmas'.
data PragmaFlags = PragmaFlags
  { dflags :: DynFlags,
    -- | The names applied from the pragmas, as written.
    names :: [Text],
    warnings :: [ParseWarning]
  }

-- | Apply what readPragmas found, then the Safe Haskell check. See
-- Note [Parser settings].
applyPragmas :: DynFlags -> [Text] -> PragmaFlags
applyPragmas = go [] []
  where
    -- Names and warnings in reverse.
    go names ws df = \case
      [] ->
        let (df', safeWarnings) = safeCheck df
         in PragmaFlags
              { dflags = df',
                -- A name given twice is listed once, first place.
                names = nub (reverse names),
                warnings = reverse ws <> safeWarnings
              }
      a : rest
        | Just n <- Text.stripPrefix "-X" a -> named n rest
        | a == "-cpp" -> named "CPP" rest
        | a == "-F" -> go names ws df rest
        | a `elem` ["-pgmF", "-haddock-opts", "-#include"] -> go names ws df (drop 1 rest)
        | "-pgmF" `Text.isPrefixOf` a -> go names ws df rest
        | otherwise ->
            -- With its arguments, if any: -pgmL tool.
            let (args, rest') = break ("-" `Text.isPrefixOf`) rest
             in go names (IgnoredPragmaFlag (Text.unwords (a : args)) : ws) df rest'
      where
        named n rest = case applyName n df of
          Applied df' -> go (n : names) ws df' rest
          AppliedWith w df' -> go (n : names) (w : ws) df' rest
          Unknown -> go names (UnknownExtension n : ws) df rest

-- | Like GHC's safeFlagCheck: under -XSafe, the extensions Safe Haskell
-- forbids are turned off, with a warning each.
safeCheck :: DynFlags -> (DynFlags, [ParseWarning])
safeCheck df
  | safeHaskell df /= Sf_Safe = (df, [])
  | otherwise = foldl' check (df, []) unsafeExtensions
  where
    check (d, ws) e
      | xopt e d = (xopt_unset d e, ws <> [SafeTurnsOff (canonical e)])
      | otherwise = (d, ws)
    -- GHC's unsafeFlags.
    unsafeExtensions =
      [LangExt.GeneralizedNewtypeDeriving, LangExt.DerivingVia, LangExt.TemplateHaskell]

-- | What a -X name does.
data Setting
  = SetLanguage Ghc.Language
  | SetSafe SafeHaskellMode
  | SetExtension LangExt.Extension
  | UnsetExtension LangExt.Extension

-- | The result of one -X name.
data Applied
  = Applied DynFlags
  | -- | Applied, but with something to say.
    AppliedWith ParseWarning DynFlags
  | -- | GHC doesn't know the name.
    Unknown

-- | Apply a -X name: a language, a Safe Haskell mode, @Foo@ or @NoFoo@.
applyName :: Text -> DynFlags -> Applied
applyName n df = maybe Unknown apply (Map.lookup n settingNames)
  where
    apply = \case
      SetLanguage l -> Applied (lang_set df (Just l))
      SetSafe m
        -- GHC's combineSafeFlags: a second, different mode is an error,
        -- and the first one stays.
        | safeHaskell df == Sf_None || safeHaskell df == m ->
            Applied df {safeHaskell = m}
        | otherwise ->
            AppliedWith (SafeModesClash (safeName (safeHaskell df)) n) df
      SetExtension e -> Applied (setExtension e df)
      UnsetExtension e -> Applied (xopt_unset df e)
    safeName = \case
      Sf_Safe -> "Safe"
      Sf_Trustworthy -> "Trustworthy"
      Sf_Unsafe -> "Unsafe"
      _ -> "none"

-- | Like GHC's setExtensionFlag': with the extensions it implies.
setExtension :: LangExt.Extension -> DynFlags -> DynFlags
setExtension e df = foldr implied (xopt_set df e) [x | (e', x) <- impliedXFlags, e' == e]
  where
    implied x d = case x of
      On e2 -> setExtension e2 d
      Off e2 -> xopt_unset d e2

-- | Every -X name GHC knows, with its alternate spellings.
settingNames :: Map Text Setting
settingNames =
  Map.fromList $
    [(Text.pack (show l), SetLanguage l) | l <- [minBound .. maxBound]]
      <> [ ("Safe", SetSafe Sf_Safe),
           ("Trustworthy", SetSafe Sf_Trustworthy),
           ("Unsafe", SetSafe Sf_Unsafe)
         ]
      <> concat
        [ [(n, SetExtension e), ("No" <> n, UnsetExtension e)]
        | f <- xFlags,
          let n = Text.pack (flagSpecName f)
              e = flagSpecFlag f
        ]

-- | The extensions part of the model.
extensionsOf :: DynFlags -> [Text] -> Extensions
extensionsOf df names =
  Extensions
    { language = maybe "Haskell98" (Text.pack . show) (language df),
      enabled = sort [canonical e | e <- [minBound .. maxBound], xopt e df],
      fromPragmas = names
    }

-- | GHC's main name for an extension (the first of its xFlags names).
canonical :: LangExt.Extension -> Text
canonical = Text.pack . extensionName

-- | Every extension's main name, for tests.
canonicalExtensionNames :: [Text]
canonicalExtensionNames = map canonical [minBound .. maxBound]

-- * Pragmas

-- | A header error: GHC's message and where.
data HeaderError = HeaderError SrcSpan PsHeaderMessage

-- | Our copy of GHC's getOptions: the LANGUAGE names (as @-XFoo@) and
-- the OPTIONS_GHC words of the header, in order. GHC's throws on errors;
-- this one returns them. See Note [Parser settings].
--
-- Adapted from GHC's GHC.Parser.Header.getOptions' (BSD-3, © The
-- University Court of the University of Glasgow; see ghc-lib-parser's
-- LICENSE).
readPragmas :: ParserOpts -> StringBuffer -> Either HeaderError [Text]
readPragmas opts buf = go (lexAll (initPragState opts buf startLoc))
  where
    -- Like GHC's getToks: a lexer error ends the list with an EOF.
    lexAll st = case unP (lexer False pure) st of
      POk st' t -> case unLoc t of
        ITeof -> [t]
        _ -> t : lexAll st'
      PFailed st' -> [L (mkSrcSpanPs (last_loc st')) ITeof]
    go = \case
      open : close : rest
        | IToptions_prag str <- unLoc open,
          ITclose_prag <- unLoc close ->
            case srcSpanToRealSrcSpan (getLoc open) of
              Just real -> case toArgs (realSrcSpanStart real) str of
                Left _ ->
                  let loc = combineSrcSpans (getLoc open) (getLoc close)
                   in Left (HeaderError loc (PsErrParseOptionsPragma str))
                Right args -> (map (Text.pack . unLoc) args <>) <$> go rest
              Nothing -> go rest
        | ITinclude_prag str <- unLoc open,
          ITclose_prag <- unLoc close ->
            (["-#include", Text.pack str] <>) <$> go rest
        | ITdocOptions str _ <- unLoc open,
          ITclose_prag <- unLoc close ->
            (["-haddock-opts", Text.pack str] <>) <$> go rest
      open : rest
        | ITlanguage_prag <- unLoc open -> language rest
      t : rest
        | isComment (unLoc t) -> go rest
      _ -> Right []
    language = \case
      L loc (ITconid fs) : rest -> do
        let name = unpackFS fs
        if Set.member name supportedNames
          then ("-X" <> Text.pack name :) <$> languageRest rest
          else Left (HeaderError loc (PsErrUnsupportedExt name []))
      t : _ -> Left (HeaderError (getLoc t) PsErrParseLanguagePragma)
      [] -> Right []
    languageRest = \case
      L _ ITcomma : more -> language more
      L _ ITclose_prag : more -> go more
      t : _ -> Left (HeaderError (getLoc t) PsErrParseLanguagePragma)
      [] -> Right []
    isComment = \case
      ITlineComment {} -> True
      ITblockComment {} -> True
      ITdocComment {} -> True
      _ -> False

-- | What GHC accepts in a LANGUAGE pragma.
supportedNames :: Set String
supportedNames =
  Set.fromList (supportedLanguagesAndExtensions (platformArchOS genericPlatform))

-- * Errors

-- | A header error as a 'ParseError', worded like GHC.
headerError :: Text -> HeaderError -> ParseError
headerError text (HeaderError loc msg) =
  diagnosticError (columnMap text) loc (PsHeaderMessage msg)

-- | GHC's message, code and span.
diagnosticError :: ColumnMap -> SrcSpan -> PsMessage -> ParseError
diagnosticError cols loc msg =
  ParseError
    { message = Text.intercalate "\n" (map render blocks),
      code = Text.pack . show <$> diagnosticCode msg,
      span = toSpan cols loc
    }
  where
    blocks = unDecorated (diagnosticMessage (defaultDiagnosticOpts @PsMessage) msg)
    render = Text.strip . Text.pack . renderWithContext defaultSDocContext

-- * GHC settings

-- | See Note [Parser settings].
--
-- From ghc-lib's mini-hlint example (github.com/digital-asset/ghc-lib,
-- BSD-3).
fakeSettings :: Ghc.Settings
fakeSettings =
  Ghc.Settings
    { Ghc.sGhcNameVersion =
        GhcNameVersion
          { ghcNameVersion_programName = "ghc",
            ghcNameVersion_projectVersion = cProjectVersion
          },
      Ghc.sFileSettings = FileSettings {},
      Ghc.sTargetPlatform = genericPlatform,
      Ghc.sPlatformMisc = PlatformMisc {},
      Ghc.sToolSettings = ToolSettings {toolSettings_opt_P_fingerprint = fingerprint0},
      Ghc.sUnitSettings = UnitSettings {unitSettings_baseUnitId = stringToUnitId "base"}
    }
