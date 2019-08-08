{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
module Jak.Editor (editor) where

import Jak.Types
import Jak.Viewport (viewport)
import Jak.Cursor   (cursor)
import Jak.Content  (content)

import FRP.Yampa

editor :: Editor -> SF (Event EditorEvent) (Maybe Editor)
editor x@(Editor v0 cu0 co0) = loopPre x $
  proc (e, x'@(Editor _v' _cu' co')) -> case e of
    Event EditorExit -> returnA -< (Nothing, x')
    _ -> do
      cu <- cursor  cu0 -< (e, contentShape co')
      co <- content co0 -< (e, cursorPosition cu)
      v  <- viewport v0 -< (cursorPosition cu, e >>= projectSize)
      first (arr Just) -< dup (Editor v cu co)
  where
    projectSize (EditorResize s) = Event s
    projectSize _ = NoEvent
