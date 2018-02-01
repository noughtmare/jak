{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.FRPNow
import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Data.Foldable                 (toList, fold)
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                 (fromList, singleton)
import qualified Data.Sequence          as S
import qualified Graphics.Vty           as Vty
import           Jak.Editor
import           Jak.Core
import           Jak.Frontend.Vty
import           System.Exit                   (ExitCode (ExitSuccess))

editorToPicture :: Editor -> Vty.Picture
editorToPicture (Editor (Viewport (Position vc vr) (Size vw vh)) (Cursor (Position cc cr) _) (Content con))
  = Vty.Picture actualCursor [contentImage] Vty.ClearBackground
 where
  contentImage =
    mconcat
      . map
          ( Vty.string Vty.defAttr . toList . S.take (fromIntegral vw) . S.drop
            (fromIntegral vc)
          )
      . (toList . S.take (fromIntegral vh) . S.drop (fromIntegral vr))
      $ con
  actualCursor = Vty.Cursor (fromIntegral (cc - vc)) (fromIntegral (cr - vr))

myHandler :: Handler Vty.Event Vty.Picture
myHandler = Handler $ \evs pic -> do
  escEvent <- sample (next (filterEs (== Vty.EvKey Vty.KEsc []) evs))
  pictures <- sample (fmap editorToPicture <$> editor (emptyEditor (Size 50 50)) (filterMapEs toEditorEvent evs))
  pure (pic, pictures, (ExitSuccess <$ escEvent))

toEditorEvent :: Vty.Event -> Maybe EditorEvent
toEditorEvent = \case
  Vty.EvResize w h           -> Just (EditorResize (Size (W w) (H h)))
  Vty.EvKey Vty.KEnter    [] -> Just (EditorInsert '\n')
  Vty.EvKey (Vty.KChar c) [] -> Just (EditorInsert c)
  Vty.EvKey Vty.KBS       [] -> Just (EditorBackspace)
  Vty.EvKey Vty.KDel      [] -> Just (EditorDelete)
  Vty.EvKey Vty.KLeft     [] -> Just (EditorMoveDir West)
  Vty.EvKey Vty.KRight    [] -> Just (EditorMoveDir East)
  Vty.EvKey Vty.KUp       [] -> Just (EditorMoveDir North)
  Vty.EvKey Vty.KDown     [] -> Just (EditorMoveDir South)
  Vty.EvMouseDown c r Vty.BLeft [] -> Just (EditorMoveAbs (Position (C c) (R r)))
  _                          -> Nothing

aSquare n = fromList (unlines (replicate n (replicate n 'a')))
aSquare' n = fromList (map fromList (replicate n (replicate n 'a')))
aLine' n = singleton (fromList (replicate n 'a'))

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  run (vtyFrontend (cfg { Vty.mouseMode = Just True }))
      myHandler
      ((Vty.picForImage mempty) { Vty.picCursor = Vty.Cursor 0 0 })
