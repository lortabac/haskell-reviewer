{-# LANGUAGE TemplateHaskell #-}
module GenNoList where

import Language.Haskell.TH

plain :: Int
plain = 1

$(do d <- funD (mkName "fromSplice") [clause [] (normalB [|3 :: Int|]) []]; pure [d])
