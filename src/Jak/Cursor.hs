{-# LANGUAGE LambdaCase #-}
module Jak.Cursor (cursor) where

import Jak.Types

import Data.Sequence (index)
import FRP.Yampa

cursor :: Cursor -> SF (Event EditorEvent, Shape) Cursor
cursor = sscan $ \prev (e,s) -> case e of
  NoEvent -> prev
  Event e'
    | isCursorEvent e' && changesCursor prev s e' -> moveCursor prev s e'
    | otherwise -> prev
  where
    isCursorEvent = \case
      EditorMoveDir _ -> True
      EditorMoveAbs _ -> True
      EditorBackspace -> True
      EditorInsert _  -> True
      _               -> False
    changesCursor :: Cursor -> Shape -> EditorEvent -> Bool
    changesCursor (Cursor p _) s = \case
      EditorMoveDir d -> canMove p s d
      EditorBackspace -> canBackspace p
      EditorMoveAbs p' -> p /= p'
      _ -> True

canBackspace :: Position -> Bool
canBackspace (Position c r) = r > 0 || c > 0

canMove :: Position -> Shape -> Direction -> Bool
canMove (Position c r) (Shape s) = \case
  North -> r > 0
  East  -> C (index s (fromIntegral r)) > c
  South -> R (length s) - 1 > r
  West  -> c > 0

moveCursor :: Cursor -> Shape -> EditorEvent -> Cursor
moveCursor ic@(Cursor (Position c r) v) (Shape shape) move =
  case move of
    EditorMoveDir North -> clampCol $ Position (v2c v) (r - 1)
    EditorMoveDir East  -> mkCursor $ Position (c + 1) r
    EditorMoveDir South -> clampCol $ Position (v2c v) (r + 1)
    EditorMoveDir West  -> mkCursor $ Position (c - 1) r
    EditorMoveAbs p     -> clampCol (clampRow p)
    EditorBackspace
      | c > 0           -> mkCursor $ Position (c - 1) r
      | r > 0           -> mkCursor $ Position (C (index shape (fromIntegral (r - 1)))) (r - 1)
    EditorInsert '\n'   -> mkCursor $ Position 0 (r + 1)
    EditorInsert _      -> mkCursor $ Position (c + 1) r
    -- The following catchall will also catch any events that are defined in
    -- the future. Is that desirable?
    -- TODO: I think that the best solution would be to define a new datatype
    -- for CursorEvents and then project EditorEvents to those CursorEvents.
    _                   -> ic
  where
    between :: Ord a => a -> a -> a -> a
    between low high a
      | a < low   = low
      | a > high  = high
      | otherwise = a
    clampCol (Position cc rr) = Cursor (Position (between 0 cc (C (index shape (fromIntegral rr)))) rr) (c2v cc)
    clampRow (Position cc rr) = Position cc (between 0 (documentLength - 1) rr)
    mkCursor p@(Position cc _) = Cursor p (c2v cc)
    documentLength = R $ length shape
