{-# LANGUAGE LambdaCase #-}
module Jak.Cursor (cursor) where

import Jak.Types

import Control.FRPNow
import Control.FRPNow.Util
import Data.Sequence (index)
import Data.Bifunctor (bimap)

cursor :: Cursor
       -> EvStream EditorEvent
       -> Shape
       -> EvStream Shape
       -> Behavior (EvStream Cursor)
cursor cur0 evs shape0 shapeEs = do
  shape  <- fromChanges shape0 shapeEs
  dshape <- delay evs shape0 shape
  scanlFilterEv (\a (b,c) -> if changesCursor a b c then Just (moveCursor a b c) else Nothing) cur0
    $ uncurry merge
    $ bimap (f dshape) (f shape)
    $ partitionEs isBefore
    $ filterEs isCursorEvent evs
  where
    changesCursor :: Cursor -> Shape -> EditorEvent -> Bool
    changesCursor (Cursor p _) s = \case
      EditorMoveDir d -> canMove p s d
      EditorBackspace -> canBackspace p
      EditorMoveAbs p' -> p /= p'
      e -> True
    f s = (fmap (,) s <@@>)
    isBefore = \case
      EditorInsert _ -> False
      _ -> True
    isCursorEvent = \case
      EditorMoveDir _ -> True
      EditorMoveAbs _ -> True
      EditorBackspace -> True
      EditorInsert _  -> True
      _               -> False

canBackspace :: Position -> Bool
canBackspace (Position c r) = r > 0 || c > 0

canMove :: Position -> Shape -> Direction -> Bool
canMove (Position c r) (Shape s) = \case
  North -> r > 0
  East  -> C (index s (fromIntegral r)) > c
  South -> R (length s) - 1 > r
  West  -> c > 0

moveCursor :: Cursor -> Shape -> EditorEvent -> Cursor
moveCursor (Cursor (Position c r) v) (Shape shape) move =
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
  where
    between :: Ord a => a -> a -> a -> a
    between low high a
      | a < low   = low
      | a > high  = high
      | otherwise = a
    clampCol (Position cc rr) = Cursor (Position (between 0 cc (C (index shape (fromIntegral rr)))) rr) (c2v cc)
    clampRow (Position cc rr) = Position cc (between 0 (documentLength - 1) rr)
    mkCursor p@(Position c _) = Cursor p (c2v c)
    lineLength = C $ index shape (fromIntegral r)
    documentLength = R $ length shape
