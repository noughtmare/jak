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
  (\vty -> putStrLn disableMouseEvents *> Vty.shutdown vty)
  Vty.nextEvent
  Vty.update

vtyFrontend' :: Vty.Vty -> Frontend Vty.Event Vty.Picture
vtyFrontend' vty = mkFrontend $ IOFrontend
  (vty <$ putStrLn requestMouseEvents)
  (\vty -> putStrLn disableMouseEvents *> Vty.shutdown vty)
  Vty.nextEvent 
  Vty.update
