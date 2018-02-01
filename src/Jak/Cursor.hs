{-# LANGUAGE LambdaCase #-}
module Jak.Cursor (cursor) where

import Jak.Types

import Control.FRPNow
import Control.FRPNow.Util
import Data.Sequence (index)
import Data.Bifunctor (bimap)

cursor :: Cursor
       -> EvStream CursorEvent
       -> EvStream Shape
       -> Shape
       -> Behavior (EvStream Cursor)
cursor a evs shapeEs shape0 = do
  shape  <- fromChanges shape0 shapeEs
  dshape <- delay evs shape0 shape
  scanlEv moveCursor a
    $ uncurry merge
    $ bimap (f dshape) (f shape)
    $ partitionEs isBefore evs
  where
    f s = (fmap (,) s <@@>)
    isBefore = \case
      MoveInsert -> False
      _ -> True

moveCursor :: Cursor -> (Shape,CursorEvent) -> Cursor
moveCursor (Cursor (Position c r) v) (Shape shape, move) =
  case move of
    Move North    -> clampCol $ Position (v2c v) (max 0 (r - 1))
    Move East     -> mkCursor $ Position (min lineLength (c + 1)) r
    Move South    -> clampCol $ Position (v2c v) (min (R (length shape - 1)) (r + 1))
    Move West     -> mkCursor $ Position (max 0 (c - 1)) r
    MoveAbs p     -> clampCol (clampRow p)
    MoveBackspace
      | c > 0 -> mkCursor $ Position (c - 1) r
      | r > 0 -> mkCursor $ Position (C (index shape (fromIntegral (r - 1)))) (r - 1)
      | otherwise -> mkCursor $ Position 0 0
    MoveInsert
      | c < lineLength -> mkCursor $ Position (c + 1) r
      | otherwise      -> mkCursor $ Position 0 (r + 1)
  where
    clampCol (Position cc rr) = Cursor (Position (max 0 (min cc (C (index shape (fromIntegral rr))))) rr) (c2v cc)
    clampRow (Position cc rr) = Position cc (min (R (length shape - 1)) (max 0 rr))
    lineLength = C $ index shape (fromIntegral r)
    mkCursor p@(Position c _) = Cursor p (c2v c)

