{-# OPTIONS_GHC -Wno-missing-fields #-}
-- | Parse one file with ghc-lib-parser, the way GHC would after Cabal set
-- the component flags.
module Parse
  ( Env (..)
  , newEnv
  , compFlags
  , parseFile
  , otherSettings
  , unlitMarkdown
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser (initParserOpts, supportedLanguagePragmas)
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser (parseModule, parseSignature)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
import GHC.Platform (genericPlatform)
import GHC.Settings (ToolSettings (..), UnitSettings (..))
import qualified GHC.Settings
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Unit.Types (stringToUnitId)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Logger (Logger, initLogger)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext)
import Cpp
import Types

newtype Env = Env { envLogger :: Logger }

newEnv :: IO Env
newEnv = Env <$> initLogger

-- | Settings for files outside any component: GHC's own default.
otherSettings :: Types.Settings
otherSettings = Types.Settings "GHC2021" [] [] []

-- | Copied from ghc-lib's examples/ghc-lib-test-mini-hlint (9.14).
fakeSettings :: Settings'
fakeSettings = GHC.Settings.Settings
  { sGhcNameVersion = GhcNameVersion
      { ghcNameVersion_programName = "ghc"
      , ghcNameVersion_projectVersion = cProjectVersion }
  , sFileSettings = FileSettings {}
  , sTargetPlatform = genericPlatform
  , sPlatformMisc = PlatformMisc {}
  , sToolSettings = ToolSettings { toolSettings_opt_P_fingerprint = fingerprint0 }
  , sUnitSettings = UnitSettings { unitSettings_baseUnitId = stringToUnitId "base" }
  }

type Settings' = GHC.Settings.Settings

-- | Component flags: language, default-extensions, then ghc-options.
-- Left = GHC refused a flag.
compFlags :: Env -> Types.Settings -> IO (Either String DynFlags)
compFlags env s = do
  let args = ("-X" ++ setLanguage s) : setExts s
             ++ [o | o <- setGhcOpts s, "-X" `isPrefixOf` o || o == "-cpp"]
  r <- try (parseDynamicFlagsCmdLine (envLogger env) (defaultDynFlags fakeSettings)
              (map noLoc args))
  pure $ case r of
    Left e -> Left (firstLine (displayException (e :: SomeException)))
    Right (df, left, _)
      | null left -> Right df
      | otherwise -> Left ("unknown flags: " ++ unwords (map unLoc left))

firstLine :: String -> String
firstLine = takeWhile (/= '\n') . dropWhile (== '\n')

-- | Parse one file. @abs@ is the path to read, @rel@ the path we report.
parseFile :: Env -> Types.Settings -> Either String DynFlags -> FilePath -> FilePath -> IO ModResult
parseFile env s edf absPath rel = do
  t0 <- getMonotonicTimeNSec
  bytes <- BS.readFile absPath
  let isLhs = ".lhs" `isSuffixOf` rel || ".lhsig" `isSuffixOf` rel
      isSig = ".hsig" `isSuffixOf` rel || ".lhsig" `isSuffixOf` rel
      txt = T.unpack (TE.decodeUtf8With TE.lenientDecode bytes)
      mdUnlit = any ("markdown-unlit" `isInfixOf`) (setGhcOpts s)
      src | isLhs && mdUnlit = unlitMarkdown txt
          | isLhs = unlit txt
          | otherwise = txt
      nLines = length (lines txt)
  _ <- evaluate (length src)
  t1 <- getMonotonicTimeNSec
  let base = ModResult rel nLines (BS.length bytes) Nothing True Nothing [] (ns t0 t1) 0 0
  case edf of
    Left err -> pure base { mrFailure = Just (Failure "config" "-" err 0) }
    Right cdf -> do
      let buf = if isLhs then stringToStringBuffer src else stringBufferFromByteString bytes
      hdr <- fileFlags env cdf buf rel
      t2 <- getMonotonicTimeNSec
      case hdr of
        Left err -> pure base { mrFailure = Just (Failure "config" "-" err 0), mrHeaderNs = ns t1 t2 }
        Right df -> do
          let tags = ["pgmF" | gopt Opt_Pp df]
                     ++ ["pgmP" | any ("-pgmP" `isPrefixOf`) (setGhcOpts s)]
                     ++ ["pgmL" | isLhs, any ("-pgmL" `isPrefixOf`) (setGhcOpts s)]
              isCpp = xopt LangExt.Cpp df
          if not isCpp
            then do
              f <- evaluate (forceF (runParse df isSig rel buf))
              t3 <- getMonotonicTimeNSec
              pure base { mrFailure = f, mrLexes = lexes df rel buf f, mrTags = tags
                        , mrHeaderNs = ns t1 t2, mrParseNs = ns t2 t3 }
            else do
              let srcCpp = if xopt LangExt.Cpp cdf
                    then (if "-XCPP" `elem` setExts s then CppDefaultExt else CppGhcOptions)
                    else CppPragma
                  noCpp = xopt_unset df LangExt.Cpp
                  raw = forceF (runParse noCpp isSig rel buf)
                  stripped = stringToStringBuffer (stripCpp src)
                  st = forceF (runParse df isSig rel stripped)
                  (basicTxt, feats) = basicCpp (setCppDefs s) src
                  basicBuf = stringToStringBuffer basicTxt
              -- GHC reads the pragmas again after CPP
              dfC <- either (const df) id <$> fileFlags env cdf basicBuf rel
              let dfC' = if xopt LangExt.Cpp dfC then dfC else xopt_set dfC LangExt.Cpp
              c <- evaluate (forceF (runParse dfC' isSig rel basicBuf))
              t3 <- getMonotonicTimeNSec
              a <- evaluate raw
              b <- evaluate st
              let feats' = feats { cfDirectives = cfDirectives feats }
              pure base { mrFailure = c, mrLexes = lexes dfC' rel basicBuf c
                        , mrCpp = Just (srcCpp, CppOutcome a b c feats')
                        , mrTags = tags, mrHeaderNs = ns t1 t2, mrParseNs = ns t2 t3 }
  where
    ns a b = toInteger (b - a)
    forceF f = case f of
      Nothing -> Nothing
      Just x@(Failure p c m l) -> length p `seq` length c `seq` length m `seq` l `seq` Just x

-- | OPTIONS_GHC and LANGUAGE pragmas of the file on top of the component.
fileFlags :: Env -> DynFlags -> StringBuffer -> FilePath -> IO (Either String DynFlags)
fileFlags env df buf rel = do
  r <- try $ do
    let (_, opts) = getOptions (initParserOpts df) (supportedLanguagePragmas df) buf rel
    (df', left, _) <- parseDynamicFilePragma (envLogger env) df opts
    _ <- evaluate df'
    pure (df', left)
  pure $ case r of
    Left e -> Left (firstLine (displayException (e :: SomeException)))
    Right (df', left)
      | null left -> Right df'
      | otherwise -> Left ("unknown flags: " ++ unwords (map unLoc left))

runParse :: DynFlags -> Bool -> FilePath -> StringBuffer -> Maybe Failure
runParse df isSig rel buf =
  let loc = mkRealSrcLoc (mkFastString rel) 1 1
      st0 = initParserState (initParserOpts df) buf loc
      p = if isSig then parseSignature else parseModule
  in case unP p st0 of
       PFailed st -> firstFailure (getPsErrorMessages st)
       POk st _ -> firstFailure (getPsErrorMessages st)

firstFailure :: Messages PsMessage -> Maybe Failure
firstFailure msgs = case toList (getMessages msgs) of
  [] -> Nothing
  (e : _) ->
    let d = errMsgDiagnostic e
        code = maybe "-" show (diagnosticCode d)
        txt = case unDecorated (diagnosticMessage (defaultDiagnosticOpts @PsMessage) d) of
          (sd : _) -> firstLine (renderWithContext defaultSDocContext sd)
          [] -> ""
        line = case errMsgSpan e of
          RealSrcSpan r _ -> srcSpanStartLine r
          _ -> 0
    in Just (Failure "parser" code txt line)

-- | Does the text lex? Tried only when the parse failed.
lexes :: DynFlags -> FilePath -> StringBuffer -> Maybe Failure -> Bool
lexes _ _ _ Nothing = True
lexes df rel buf (Just _) =
  case lexTokenStream (initParserOpts df) buf (mkRealSrcLoc (mkFastString rel) 1 1) of
    POk _ _ -> True
    PFailed _ -> False

-- | Line-preserving unlit: bird tracks and \begin{code} blocks.
unlit :: String -> String
unlit = unlines . go False . lines
  where
    go _ [] = []
    go inCode (l : ls)
      | "\\begin{code}" `isPrefixOf` l = "" : go True ls
      | "\\end{code}" `isPrefixOf` l = "" : go False ls
      | inCode = l : go inCode ls
      | ('>' : rest) <- l = (' ' : rest) : go inCode ls
      | otherwise = "" : go inCode ls

-- | Line-preserving markdown-unlit: code is in ```haskell fences
-- (not marked "ignore").
unlitMarkdown :: String -> String
unlitMarkdown = unlines . go False . lines
  where
    go _ [] = []
    go inCode (l : ls)
      | "```" `isPrefixOf` dropWhile isSpace l =
          let info = drop 3 (dropWhile isSpace l)
              open = not inCode && "haskell" `isInfixOf` info && not ("ignore" `isInfixOf` info)
          in "" : go (if inCode then False else open) ls
      | inCode = l : go inCode ls
      | otherwise = "" : go inCode ls
