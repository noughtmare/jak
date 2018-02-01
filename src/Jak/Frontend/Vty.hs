module Jak.Frontend.Vty where

import Jak.Core

import Control.FRPNow
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Mouse
import Control.Applicative
import System.IO

vtyFrontend :: Vty.Config -> Frontend Vty.Event Vty.Picture
vtyFrontend cfg = mkFrontend $ IOFrontend
  (Vty.mkVty cfg <* putStrLn requestMouseEvents)
  (\vty -> Vty.shutdown vty <* putStrLn disableMouseEvents)
  Vty.nextEvent 
  Vty.update
