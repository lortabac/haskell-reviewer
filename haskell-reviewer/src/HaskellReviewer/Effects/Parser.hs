-- | Running the parser from IO: GHC's own crashes become text only, a
-- limit of one parse at a time, and a warm-up for its shared values.
module HaskellReviewer.Effects.Parser
  ( parseGuarded,
    oneAtATime,
    warmUpParser,
  )
where

import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket_,
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

{- Note [One parse at a time]

A parse can take a lot of memory: a hostile 1 MiB module peaked at
0.7-1.2 GB of RSS, and 8 such requests at once at 8.3 GB. The budget is
2 GiB for a large project (SPEC §10.2). So requests parse one at a time
('oneAtATime'). A parse of a normal module takes a few ms, so waiting
costs little.

The warm-up doesn't go through the limit: it runs before any request
can parse, so it can't wait on one.
-}

-- | Wrap a parse function so only one call runs at a time. See
-- Note [One parse at a time].
oneAtATime :: (a -> b -> IO c) -> IO (a -> b -> IO c)
oneAtATime f = do
  sem <- newQSem 1
  pure (\a b -> bracket_ (waitQSem sem) (signalQSem sem) (f a b))

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
