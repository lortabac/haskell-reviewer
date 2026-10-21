module NondecIndent where
f = do
  x <- pure 1
  case x of
   _ -> do
   pure x
