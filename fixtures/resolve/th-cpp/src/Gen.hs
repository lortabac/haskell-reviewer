{-# LANGUAGE TemplateHaskell #-}
module Gen (made, helper) where

import Language.Haskell.TH

helper :: Int
helper = 1

$(do d <- funD (mkName "made") [clause [] (normalB [|2 :: Int|]) []]; pure [d])
