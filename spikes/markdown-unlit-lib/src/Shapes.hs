-- | Inputs of about a given size in bytes, normal and hostile.
module Shapes (shape) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | A shape, cut or repeated to about @n@ bytes. S1 repeats the seed
-- (servant's tutorial Server.lhs).
shape :: Text -> String -> Int -> Maybe Text
shape seed name n = case name of
  -- servant's tutorial, repeated.
  "S1" -> Just (fill (seed <> "\n"))
  -- One block of lines like xN = N.
  "S2" ->
    let body = Text.take (n - 20) (lines' [eq i | i <- [1 .. n `div` 6 + 1]])
     in Just ("```haskell\n" <> Text.dropWhileEnd (/= '\n') body <> "```\n")
  -- Tiny blocks.
  "S3" -> Just (fill "```haskell\nx\n```\n")
  -- Fences only.
  "S4" -> Just (fill "```\n")
  -- Prose, no fence.
  "S5" -> Just (fill "Some prose, no code here at all.\n")
  -- One fence with a huge info string.
  "S6" -> Just ("```haskell " <> wordsTo (n - 20) <> "\nx = 1\n```\n")
  -- An open brace and no close.
  "S7" -> Just ("```{haskell " <> wordsTo (n - 20) <> "\nx = 1\n```\n")
  -- top: and a huge number.
  "S8" -> Just ("```haskell top:" <> Text.replicate (n - 30) "9" <> "\nx = 1\n```\n")
  -- top: and deep parens.
  "S9" ->
    let k = (n - 30) `div` 2
     in Just
          ( "```haskell top:"
              <> Text.replicate k "("
              <> "1"
              <> Text.replicate k ")"
              <> "\nx = 1\n```\n"
          )
  -- Many blocks, top:i with i going down.
  "S10" ->
    let k = n `div` 40
     in Just
          ( Text.concat
              [ "```haskell top:" <> num (k - i) <> "\nx" <> num i <> " = 1\n```\n"
              | i <- [1 .. k]
              ]
          )
  -- Two fences indented by a quarter of the size each, code between.
  "S11" ->
    let k = n `div` 4
     in Just
          ( Text.replicate k " "
              <> "```haskell\n"
              <> fillTo (n - 2 * k) (Text.replicate 20 " " <> "x = 1\n")
              <> Text.replicate k " "
              <> "```\n"
          )
  -- One line, no newline.
  "S12" -> Just (Text.replicate n "a")
  -- Alternating ``` and ~~~.
  "S13" -> Just (fill "```\n~~~\n")
  _ -> Nothing
  where
    fill = fillTo n
    num = Text.pack . show
    eq i = "x" <> num i <> " = " <> num i
    lines' = Text.unlines

-- | Repeat, then cut to n chars (the last copy can be cut short).
fillTo :: Int -> Text -> Text
fillTo n unit
  | Text.null unit = ""
  | otherwise = Text.take n (Text.replicate (n `div` Text.length unit + 1) unit)

-- | Words w1 w2 ... up to about n chars.
wordsTo :: Int -> Text
wordsTo n = Text.take n (Text.unwords [w i | i <- [1 :: Int .. n `div` 3 + 1]])
  where
    w i = "w" <> Text.pack (show i)
