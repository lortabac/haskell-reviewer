{-# LANGUAGE DataKinds, OverloadedRecordDot, QuasiQuotes, RecordWildCards #-}
module Use where

import Data.Proxy (Proxy (..))
import Defs

instance Cls Bool where
  cm _ = 1

instance Show Rec where
  show r = show r.ra

mk :: Rec
mk = Rec {ra = 1, rb = 2}

upd :: Rec -> Rec
upd r = r {ra = 3}

dot :: Rec -> Int
dot r = r.rb + px (Point 1 2)

wild :: Rec -> Int
wild Rec {..} = ra + rb

prox :: Proxy (S Z)
prox = Proxy

q :: String
q = [qq|hello|]
