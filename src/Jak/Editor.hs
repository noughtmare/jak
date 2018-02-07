{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Jak.Editor (editor) where

import Jak.Types
import Jak.Viewport (viewport)
import Jak.Cursor   (cursor)
import Jak.Content  (content)

import Control.FRPNow.Util (fromUpdates)
import Control.Lens (view, set)
import Control.FRPNow (EvStream, Behavior, filterMapEs)

editor :: Editor
       -> EvStream EditorEvent
       -> Behavior (EvStream Editor)
editor editor@(Editor vpt0 cur0 con0) evs = mdo
  cur <- cursor
           cur0
           evs
           (view contentShape con0)
           (fmap (view contentShape) con)
  con <- content
           con0
           evs
           (view cursorPos cur0)
           (fmap (view cursorPos) cur)
  vpt <- viewport vpt0 cur (sizeEvs evs)
  fromUpdates editor
    (mconcat [ fmap (set editorViewport) vpt
             , fmap (set editorContent ) con
             , fmap (set editorCursor  ) cur ])
  where
    sizeEvs :: EvStream EditorEvent -> EvStream Size
    sizeEvs = filterMapEs $ \case
      EditorResize s -> Just s
      _ -> Nothing
