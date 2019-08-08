module Jak.Viewport (viewport) where

import Jak.Types
import FRP.Yampa

viewport :: Viewport -> SF (Position, Event Size) Viewport
viewport = sscan $ \v (p,s) -> scrollViewport p $ case s of
  Event s' -> v { viewportSize = s' }
  NoEvent -> v

scrollViewport :: Position -> Viewport -> Viewport
scrollViewport (Position cc cr) (Viewport (Position vc vr) size@(Size w h)) =
  let
    c | cc < vc          = cc
      | cc >= vc + w2c w = cc - w2c w + 1
      | otherwise        = vc
    r | cr < vr          = cr
      | cr >= vr + h2r h = cr - h2r h + 1
      | otherwise        = vr
  in Viewport (Position c r) size
