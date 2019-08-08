{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
module Jak.Editor (Editor, EditorEvent (..), editor, emptyEditor) where

import Jak.Types
import Jak.Viewport
import Jak.Cursor
import Jak.Content

import FRP.Yampa

data Editor = Editor !Viewport !Cursor !Content

instance Render Editor where
  renderCursor (Editor v c _) = viewCursor c (projectView v)
  renderContent (Editor v _ c) = viewContent c (projectView v)

emptyEditor :: Size -> Editor
emptyEditor s = Editor (emptyViewport s) emptyCursor emptyContent

data EditorEvent
  = EditorInsert    !Char
  | EditorBackspace
  | EditorDelete
  | EditorMoveDir   !Direction
  | EditorMoveAbs   !Position
  | EditorResize    !Size
  | EditorExit

editor :: Editor -> SF (Event EditorEvent) (Maybe Editor)
editor x@(Editor v0 cu0 co0) = loopPre x $
  proc (e, x'@(Editor _v' _cu' co')) -> case e of
    Event EditorExit -> returnA -< (Nothing, x')
    _ -> do
      cu <- cursor  cu0 -< (e >>= projectCursorEvent, projectShape co')
      co <- content co0 -< (e >>= projectContentEvent, projectPosition cu)
      v  <- viewport v0 -< (projectPosition cu, e >>= projectSize)
      first (arr Just) -< dup (Editor v cu co)
  where
    projectSize (EditorResize s) = Event s
    projectSize _ = NoEvent

    projectCursorEvent = \case
      EditorInsert c -> Event (CursorInsert c)
      EditorBackspace -> Event CursorBackspace
      EditorMoveDir d -> Event (CursorMoveDir d)
      EditorMoveAbs p -> Event (CursorMoveAbs p)
      _ -> NoEvent

    projectContentEvent = \case
      EditorInsert c -> Event (ContentInsert c)
      EditorBackspace -> Event ContentBackspace
      EditorDelete -> Event ContentDelete
      _ -> NoEvent
