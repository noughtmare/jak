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
           (moveEvs evs)
           (fmap (view contentShape) con)
           (view contentShape con0)
  con <- content
           con0
           evs
           (fmap (view cursorPos) cur)
           (view cursorPos cur0)
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

    moveEvs :: EvStream EditorEvent -> EvStream CursorEvent
    moveEvs = filterMapEs $ \case
      EditorInsert _    -> Just MoveInsert
      EditorBackspace   -> Just MoveBackspace
      EditorMoveDir dir -> Just (Move dir)
      EditorMoveAbs pos -> Just (MoveAbs pos)
      _ -> Nothing
