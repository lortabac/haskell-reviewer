-- | Running the parser from IO: GHC's own crashes become text only, and
-- a warm-up for its shared values.
module HaskellReviewer.Effects.Parser
  ( parseGuarded,
    warmUpParser,
  )
where

import Control.Exception
  ( SomeAsyncException,
    SomeException,
    displayException,
    evaluate,
    fromException,
    throwIO,
    try,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
    parseSource,
  )
import HaskellReviewer.Core.Project (Extension (..))

-- | 'parseSource', fully forced. If GHC throws (a bug, in GHC or ours),
-- the module is text only. Async exceptions go through, and so do
-- StackOverflow and HeapOverflow: they are async exceptions too.
parseGuarded :: ParseSettings -> Text -> IO ModuleParse
parseGuarded settings text = do
  r <- try (evaluate (parseSource settings text))
  case r of
    Right p -> pure p
    Left e -> do
      rethrowAsync e
      -- Showing the exception can throw too, and a lazy text would keep
      -- it alive: build the text here, in its own try.
      shown <- try (evaluate (Text.pack (displayException e)))
      message <- case shown of
        Right t -> pure t
        Left e2 -> "parser crashed" <$ rethrowAsync e2
      pure (TextOnly TextOnlyModule {reason = ParserCrashed message, warnings = []})

-- | Throw it again if it's async.
rethrowAsync :: SomeException -> IO ()
rethrowAsync e = case fromException e of
  Just (_ :: SomeAsyncException) -> throwIO e
  Nothing -> pure ()

-- | Parse a few tiny modules once, so the shared top-level values (ours
-- and ghc-lib-parser's) are built before parallel threads use them
-- (Q-RTS-BLACKHOLE). Each one takes a different path: pragmas and
-- extension names, Haddock, tabs, a parse error, a pragma error, a
-- signature.
warmUpParser :: IO ()
warmUpParser = do
  mapM_
    (parseGuarded defaultParseSettings {extensions = [Extension "LambdaCase" True]})
    [ "{-# LANGUAGE CPP, Safe #-}\n{-# OPTIONS_GHC -Wall -XMultiWayIf #-}\n"
        <> "-- | Doc.\nmodule M (x, T (..)) where\nimport A (b)\nx =\t1\n",
      "module M where\nx = = 1\n",
      "{-# LANGUAGE NoSuchThing #-}\nmodule M where\n"
    ]
  _ <-
    parseGuarded
      defaultParseSettings {kind = SignatureFile}
      "signature S (T) where\ndata T\n"
  pure ()
