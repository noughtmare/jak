module Jak.Viewport (Viewport, viewport, emptyViewport) where

import Jak.Types
import FRP.Yampa

data Viewport = Viewport !Position !Size

setViewportSize :: Size -> Viewport -> Viewport
setViewportSize s (Viewport x _) = Viewport x s

instance HasView Viewport where
  projectView (Viewport a b) = (a, b)

emptyViewport :: Size -> Viewport
emptyViewport = Viewport (Position 0 0)

viewport :: Viewport -> SF (Position, Event Size) Viewport
viewport = sscan $ \v (p,s) -> scrollViewport p $ case s of
  Event s' -> setViewportSize s' v
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
