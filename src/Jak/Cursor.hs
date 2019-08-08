{-# LANGUAGE LambdaCase #-}
module Jak.Cursor (Cursor, CursorEvent (..), cursor, emptyCursor) where

import Jak.Types

import Data.Sequence (index)
import FRP.Yampa

data Cursor = Cursor !Position !VirtualColumn

instance HasPosition Cursor where
  projectPosition (Cursor x _) = x

instance ViewCursor Cursor where
  viewCursor (Cursor (Position cc cr) _) (Position vc vr, _) = Position
    (fromIntegral (cc - vc)) (fromIntegral (cr - vr))

emptyCursor :: Cursor
emptyCursor = Cursor (Position 0 0) (V 0)

data CursorEvent
  = CursorMoveDir   !Direction
  | CursorMoveAbs   !Position
  | CursorBackspace
  | CursorInsert    !Char

cursor :: Cursor -> SF (Event CursorEvent, Shape) Cursor
cursor = sscan $ \prev (e,s) -> case e of
  Event e' | changesCursor prev s e' -> moveCursor prev s e'
  _ -> prev
  where
    changesCursor (Cursor p _) s = \case
      CursorMoveDir d -> canMove p s d
      CursorBackspace -> canBackspace p
      CursorMoveAbs p' -> p /= p'
      _ -> True

canBackspace :: Position -> Bool
canBackspace (Position c r) = r > 0 || c > 0

canMove :: Position -> Shape -> Direction -> Bool
canMove (Position c r) (Shape s) = \case
  North -> r > 0
  East  -> C (index s (fromIntegral r)) > c
  South -> R (length s) - 1 > r
  West  -> c > 0

moveCursor :: Cursor -> Shape -> CursorEvent -> Cursor
moveCursor ic@(Cursor (Position c r) v) (Shape shape) move =
  case move of
    CursorMoveDir North -> clampCol $ Position (v2c v) (r - 1)
    CursorMoveDir East  -> mkCursor $ Position (c + 1) r
    CursorMoveDir South -> clampCol $ Position (v2c v) (r + 1)
    CursorMoveDir West  -> mkCursor $ Position (c - 1) r
    CursorMoveAbs p     -> clampCol (clampRow p)
    CursorBackspace
      | c > 0           -> mkCursor $ Position (c - 1) r
      | r > 0           -> mkCursor $ Position (C (index shape (fromIntegral (r - 1)))) (r - 1)
      | otherwise       -> ic
    CursorInsert '\n'   -> mkCursor $ Position 0 (r + 1)
    CursorInsert _      -> mkCursor $ Position (c + 1) r
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
