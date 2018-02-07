module Jak.Viewport (viewport) where

import Jak.Types
import Control.FRPNow

viewport :: Viewport
         -> EvStream Cursor
         -> EvStream Size
         -> Behavior (EvStream Viewport)
viewport s cur size = scanlEv scrollViewport s viewportEvs
  where
    viewportEvs = fmap Left size `merge` fmap (Right . _cursorPos) cur

scrollViewport :: Viewport -> Either Size Position -> Viewport
scrollViewport (Viewport (Position vc vr) size@(Size w h)) (Right (Position cc cr))
  = let c | cc < vc          = cc
          | cc >= vc + w2c w = cc - w2c w + 1
          | otherwise        = vc
        r | cr < vr          = cr
          | cr >= vr + h2r h = cr - h2r h + 1
          | otherwise        = vr
    in  Viewport (Position c r) size
scrollViewport (Viewport pos _) (Left size) = Viewport pos size
