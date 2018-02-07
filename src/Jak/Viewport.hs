module Jak.Viewport (viewport) where

import Jak.Types
import Control.FRPNow
import Control.FRPNow.Util

viewport :: Viewport
         -> EvStream Cursor
         -> EvStream Size
         -> Behavior (EvStream Viewport)
viewport s cur size = fromUpdates s viewportEvs
  where
    viewportEvs = fmap (flip resizeViewport) size `merge` fmap (flip scrollViewport . _cursorPos) cur

scrollViewport :: Viewport -> Position -> Viewport
scrollViewport (Viewport (Position vc vr) size@(Size w h)) (Position cc cr)
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in  Viewport (Position c r) size

resizeViewport :: Viewport -> Size -> Viewport
resizeViewport (Viewport pos _) size = Viewport pos size
