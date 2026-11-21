-- | Read the highlighting rules at start-up. See Note [Language rules]
-- in "HaskellReviewer.Core.Lang".
module HaskellReviewer.Effects.Lang
  ( -- * Loading
    loadLangs,

    -- * Warm-up
    warmUpLangs,
  )
where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Config (ConfigWarning (..), renderConfigWarning)
import HaskellReviewer.Core.Lang
  ( Lang,
    Langs,
    ParsedLang (..),
    buildLangs,
    forceLangs,
    langCount,
    noLangs,
    parseLang,
    scanLang,
  )
import HaskellReviewer.Core.Xdg (highlightDir)
import HaskellReviewer.Effects.Config (configSizeLimit, readRegularFile)
import HaskellReviewer.Effects.Log (Logger, ioErrorMessage, logInfo, logVerbose)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Error (isDoesNotExistError)

-- | Read every @*.toml@ of the highlight dir, in name order, and log
-- their problems (one line each, default level). Never fails: no dir
-- is no rules, silently, and a bad file is skipped with a warning. Two
-- files with the same extension: the first one keeps it.
--
-- Files are read like config.toml: regular files only, 64 KiB at most.
-- The result is fully built.
loadLangs :: Logger -> (String -> Maybe String) -> IO Langs
loadLangs logger env = case highlightDir env of
  -- loadConfig says why with --verbose.
  Left _ -> pure noLangs
  Right dir ->
    try @IOException (listDirectory dir) >>= \case
      Left e
        | isDoesNotExistError e -> do
            logVerbose logger ("highlight: " <> Text.pack dir <> " (not found)")
            pure noLangs
        | otherwise -> do
            warn dir ("cannot read: " <> ioErrorMessage e)
            pure noLangs
      Right names -> do
        let files = sort [dir </> n | n <- names, takeExtension n == ".toml"]
        (langs, lost) <- buildLangs . catMaybes <$> mapM load files
        forM_ lost $ \(path, ext) ->
          warn path ("extension \"" <> ext <> "\" already has rules; ignored")
        () <- evaluate (forceLangs langs)
        logVerbose logger $
          "highlight: " <> Text.pack dir <> " (" <> count (langCount langs) <> ")"
        pure langs
  where
    warn path msg = logInfo logger (prefixed (Text.pack path <> ": " <> msg))
    count n = Text.pack (show n) <> " extensions"
    load :: FilePath -> IO (Maybe (FilePath, Lang))
    load path =
      try @IOException (readRegularFile configSizeLimit path) >>= \case
        Left e -> Nothing <$ warn path ("cannot read: " <> ioErrorMessage e)
        Right Nothing -> Nothing <$ warn path "not a regular file; ignored"
        Right (Just bytes)
          | BS.length bytes > configSizeLimit -> Nothing <$ warn path "over 64 KiB; ignored"
          | otherwise -> case Text.Encoding.decodeUtf8' bytes of
              Left _ -> Nothing <$ warn path "not UTF-8; ignored"
              Right text -> do
                let parsed = parseLang text
                mapM_ (logInfo logger . prefixed . renderConfigWarning path) parsed.warnings
                pure ((path,) <$> parsed.lang)

-- | Parse rules with every kind of warning and scan with them, so the
-- shared values are built before threads race on them
-- (Q-RTS-BLACKHOLE).
warmUpLangs :: IO ()
warmUpLangs = forM_ samples $ \t -> do
  let p = parseLang t
  void (evaluate (sum (map size p.warnings)))
  forM_ p.lang $ \l ->
    void (evaluate (length (scanLang l "#| a #| b |# |# ; c\r\n(define #\\a x \"s\\\"\" 12 #t)\n\"")))
  where
    size w = Text.length w.message
    samples =
      [ "extensions = [\"scm\", \".SS\"]\nword-chars = \"-#\"\nline-comments = [\";\"]\n\
        \numbers = true\nchar-prefixes = [\"#\\\\\"]\n[[block-comments]]\nopen = \"#|\"\nclose = \"|#\"\nnested = true\n\
        \[[strings]]\nopen = \"\\\"\"\nclose = \"\\\"\"\nescape = \"\\\\\"\n\
        \[words]\nkeyword = [\"define\"]\ntypeCon = []\nvariable = []\noperator = []\n\
        \literal = [\"#t\"]\ncomment = []\npragma = []\n",
        "extensions = [1, \"a.b\"]\nword-chars = \" \"\nline-comments = [\"\", \"a b\"]\n\
        \numbers = 1\nx = 1\nblock-comments = [1, {open = \"a\"}, {open = 1, close = \"b\", \
        \nested = 1, y = 2}]\nstrings = [{open = \"a\", close = \"b\", escape = \"ab\"}, \
        \{open = \"a\", close = \"b\", escape = 1}]\n[words]\nnope = []\nkeyword = [\"\", 1]\n\
        \[t]\n",
        "extensions = 1\nword-chars = 1\nline-comments = 1\nblock-comments = 1\n\
        \strings = 1\nwords = 1\n",
        "extensions = [\"a\"]\nextensions = [\"b\"]\n",
        "extensions = \x01\n",
        ""
      ]

-- * Internal

prefixed :: Text -> Text
prefixed = ("haskell-reviewer: " <>)
