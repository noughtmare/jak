module Jak.Viewport (viewport) where

import Jak.Types
import Control.FRPNow
import Control.FRPNow.Util
import Control.Lens
import Data.Monoid ((<>))

viewport :: Viewport
         -> EvStream Position
         -> EvStream Size
         -> Behavior (EvStream Viewport)
viewport vpt0 pos size = fromUpdates vpt0 $
  fmap (set viewportSize) size <> fmap scrollViewport pos

scrollViewport :: Position -- ^ The new cursor position
               -> Viewport -- ^ The old viewport
               -> Viewport -- ^ The updated viewport
scrollViewport (Position cc cr) (Viewport (Position vc vr) size@(Size w h))
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in Viewport (Position c r) size
