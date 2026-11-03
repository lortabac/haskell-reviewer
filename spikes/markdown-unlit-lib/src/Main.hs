-- | Spike (M2/T02): markdown-unlit 0.6.0 as a library. See README.md.
module Main (main) where

import Census (census, codeLines)
import Check (Result (..), badNames, nameCount, parseResult, renderSpan)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Clock (getMonotonicTime)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import HaskellReviewer.Core.Cpp (CppOutput (..), Region (..), runCpp)
import HaskellReviewer.Core.Cpp.Positions (PositionMap (..), mapSpan)
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import HaskellReviewer.Core.SourceText (stripBom, unlitLoose, unlitMarkdown)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (Token (..))
import LibUnlit
  ( Block (..),
    layoutMatchesLib,
    libLayout,
    libUnlitLooseRaw,
    libUnlitMarkdownRaw,
    selectedBlocks,
  )
import Shapes (shape)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Mem (getAllocationCounter)
import Text.Printf (printf)

main :: IO ()
main =
  getArgs >>= \case
    "linemap" : files -> mapM_ linemap files
    ["alloc", impl, name, size] -> alloc impl name (read size)
    "census" : out : dirs -> census out dirs
    ["diff", file] -> diffFile file
    _ -> do
      hPutStrLn stderr $
        "usage: linemap FILE... | alloc IMPL SHAPE BYTES"
          <> " | census OUT DIR... | diff FILE"
      exitFailure

-- * Line mapping

readText :: FilePath -> IO Text
readText f = stripBom . Text.Encoding.decodeUtf8Lenient <$> BS.readFile f

-- | P0 ours, P1 the library's text as is, P2 our layout of the
-- library's blocks.
linemap :: FilePath -> IO ()
linemap file = do
  t <- readText file
  putStrLn ("=== " <> file)
  let md = defaultParseSettings {literate = MarkdownUnlit}
      plain = defaultParseSettings
      p0 = parseResult md id t
      p1 = parseResult plain id <$> libUnlitMarkdownRaw file t
      lay = libLayout t
      p2 = (\(o, pm) -> parseResult plain (mapSpan pm) o) <$> lay
  report "P0 ours" t (Just p0)
  report "P1 lib-raw" t p1
  report "P2 lib-layout" t p2
  putStrLn
    ( "  layout: "
        <> maybe "none" (\(_, PositionMap m) -> placement m) lay
        <> ", same code as lib: "
        <> show (layoutMatchesLib t)
    )
  case p2 of
    Just r
      | p0.outcome == "parsed" ->
          putStrLn
            ( "  P2 tokens == P0 tokens: "
                <> show (map show r.tokens == map show p0.tokens)
                <> (" (" <> show (length r.tokens) <> " vs ")
                <> (show (length p0.tokens) <> ")")
            )
    _ -> pure ()
  -- CPP regions, as file lines.
  let regionsOf txt = [(r.firstLine, r.lastLine) | r <- (runCpp plain.cpp [] txt).regions]
      lineOf pm l =
        let at = Line (fromIntegral l)
            Span (Line a) _ _ _ = mapSpan pm (Span at (Col 1) at (Col 1))
         in a
  forM_ (unlitMarkdown t) $ \o -> do
    let rs = regionsOf o
    unless (null rs) (putStrLn ("  CPP regions P0: " <> show rs))
  forM_ lay $ \(o, pm) -> do
    let rs = regionsOf o
    unless (null rs) $
      putStrLn
        ( "  CPP regions P2 (layout lines -> file lines): "
            <> show [((a, b), (lineOf pm a, lineOf pm b)) | (a, b) <- rs]
        )

report :: String -> Text -> Maybe Result -> IO ()
report label t = \case
  Nothing -> putStrLn ("  " <> label <> ": no block")
  Just r -> do
    let bad = badNames t r
    printf
      "  %s: %s, names %d, wrong %d\n"
      label
      (Text.unpack r.outcome)
      (nameCount r)
      (length bad)
    forM_ bad $ \(n, s, got) ->
      putStrLn $
        "    wrong: "
          <> Text.unpack n
          <> (" at " <> Text.unpack (renderSpan s) <> " cuts " <> show got)
    unless (null r.warnings) (putStrLn ("    warnings: " <> show r.warnings))
    unless (null r.fromPragmas) (putStrLn ("    pragmas: " <> show r.fromPragmas))

-- * Cost

-- | One sample: build the input, then measure one implementation.
alloc :: String -> String -> Int -> IO ()
alloc impl name size = do
  seed <-
    if name == "S1"
      then lookupEnv "SEED" >>= maybe noSeed readText
      else pure ""
  input <- case shape seed name size of
    Just x -> pure x
    Nothing -> hPutStrLn stderr ("unknown shape " <> name) >> exitFailure
  inLen <- evaluate (Text.length input)
  run <- case impl of
    "none" -> pure (pure 0)
    "ours-md" -> pure (pure (maybe 0 Text.length (unlitMarkdown input)))
    "lib-raw" -> pure (pure (maybe 0 Text.length (libUnlitMarkdownRaw "input" input)))
    "lib-layout" ->
      pure (pure (maybe 0 (\(o, pm) -> Text.length o + pmSize pm) (libLayout input)))
    "ours-loose" -> pure (pure (Text.length (fst (unlitLoose input))))
    "lib-loose" -> pure (pure (Text.length (fst (libUnlitLooseRaw input))))
    "lib-has-block" -> pure (pure (length (take 1 (selectedBlocks input))))
    "parse-ours" ->
      let md = defaultParseSettings {literate = MarkdownUnlit}
       in pure (pure (forceResult (parseResult md id input)))
    "parse-lib" ->
      pure $
        pure $ case libLayout input of
          Just (o, pm) -> forceResult (parseResult defaultParseSettings (mapSpan pm) o)
          Nothing -> 0
    _ -> hPutStrLn stderr ("unknown impl " <> impl) >> exitFailure
  a0 <- getAllocationCounter
  t0 <- getMonotonicTime
  n <- evaluate =<< run
  t1 <- getMonotonicTime
  a1 <- getAllocationCounter
  enabled <- getRTSStatsEnabled
  (live, mem) <-
    if enabled
      then (\s -> (s.max_live_bytes, s.max_mem_in_use_bytes)) <$> getRTSStats
      else pure (0, 0)
  printf
    "%s\t%s\t%d\t%d\t%d\t%.2f\t%d\t%d\t%d\n"
    impl
    name
    size
    inLen
    (a0 - a1)
    ((t1 - t0) * 1000)
    live
    mem
    n

-- | Missing S1 input.
noSeed :: IO a
noSeed = hPutStrLn stderr "S1 needs SEED (servant's Server.lhs)" >> exitFailure

-- | In place (no map) or moved.
placement :: IntMap.IntMap a -> String
placement m = if IntMap.null m then "in place" else "moved"

pmSize :: PositionMap -> Int
pmSize (PositionMap m) = sum (map IntMap.size (IntMap.elems m))

forceResult :: Result -> Int
forceResult r =
  length r.tokens
    + sum [fromIntegral a + fromIntegral b | (_, Span (Line a) (Col b) _ _) <- r.names]
    + sum [fromIntegral a | tok <- r.tokens, let Span (Line a) _ _ _ = tok.span]

-- * Diff

-- | Our code lines vs the library's, by file line.
diffFile :: FilePath -> IO ()
diffFile file = do
  t <- readText file
  let ours = codeLines t
      lib =
        Map.fromList
          [ (b.start + j, Text.pack l)
          | b <- selectedBlocks t,
            (j, l) <- zip [0 ..] b.content,
            not (Text.null (Text.strip (Text.pack l)))
          ]
  forM_ (Map.keys (Map.union ours lib)) $ \n ->
    case (Map.lookup n ours, Map.lookup n lib) of
      (a, b)
        | a == b -> pure ()
        | otherwise ->
            putStrLn (show n <> "\n  ours: " <> show a <> "\n  lib:  " <> show b)
