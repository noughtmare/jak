module Jak.Viewport where

import Jak.Types
import Control.FRPNow

viewportBehavior :: Viewport
                 -> EvStream Cursor
                 -> EvStream Size
                 -> Behavior (EvStream Viewport)
viewportBehavior s cur size = scanlEv scrollViewport s viewportEvs
  where
    viewportEvs = fmap Resize size `merge` fmap (Moved . _cursorPos) cur

scrollViewport :: Viewport -> ViewportEvent -> Viewport
scrollViewport (Viewport (Position vc vr) size@(Size w h)) (Moved (Position cc cr))
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in  Viewport (Position c r) size
scrollViewport (Viewport pos _) (Resize size) = Viewport pos size

