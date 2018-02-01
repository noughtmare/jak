{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Jak.Editor where

import Jak.Types
import Jak.Viewport (viewportBehavior)
import Jak.Cursor   (cursorBehavior)
import Jak.Content  (contentBehavior)

import Control.FRPNow.Util (fromUpdates)
import Control.Lens (view, set)
import Control.FRPNow (EvStream, Behavior, filterMapEs)

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

editorBehavior :: Editor
               -> EvStream EditorEvent
               -> Behavior (EvStream Editor)
editorBehavior editor@(Editor vpt0 cur0 con0) evs = mdo
  cur <- cursorBehavior
           cur0
           (moveEvs evs)
           (fmap (view contentShape) con)
           (view contentShape con0)
  con <- contentBehavior
           con0
           evs
           (fmap (view cursorPos) cur)
           (view cursorPos cur0)
  vpt <- viewportBehavior
           vpt0
           cur
           (sizeEvs evs)
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
