module Jak.Frontend.Vty where

import Jak.Core

import Control.FRPNow
import qualified Graphics.Vty as Vty
import Control.Applicative
import System.IO

data MyCursor = MyCursor !Int !Int deriving Eq

toVtyCursor :: MyCursor -> Vty.Cursor
toVtyCursor (MyCursor x y) = Vty.Cursor x y

vtyFrontend :: (Show a, Eq a) => Vty.Config -> (a -> ((Int,Int), [String])) -> Frontend Vty.Event a
vtyFrontend cfg f = mkFrontend $ IOFrontend
  (writeFile "eventlog" "" *> writeFile "modellog" "" *> Vty.mkVty cfg)
  Vty.shutdown 
  Vty.nextEvent 
  (\vty (cur,img) -> Vty.update vty ((Vty.picForImage img) { Vty.picCursor = toVtyCursor cur })) 
  (g . f)
  (appendFile "eventlog" . (++ "\n") . show)
  (\_ -> return ()) -- appendFile "modellog" . (++ "\n") . show)
  where
    g ((c,r),strs) = (MyCursor c r, mconcat (map (Vty.string Vty.defAttr) strs))
