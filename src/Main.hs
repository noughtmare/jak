{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.FRPNow
import           Data.Foldable           (toList)
import           Data.Sequence           (fromList, singleton)
import qualified Data.Sequence    as S
import qualified Graphics.Vty     as Vty
import           Jak.Editor              (editor)
import           Jak.Types
import           Jak.Core                (run)
import           Jak.Frontend.Vty        (vtyFrontend')
import           System.Exit             (ExitCode (ExitSuccess))

editorToPicture :: Editor -> Vty.Picture
editorToPicture (Editor (Viewport (Position vc vr) (Size vw vh))
                        (Cursor (Position cc cr) _)
                        (Content con)) =
  Vty.Picture actualCursor [contentImage] Vty.ClearBackground
  where
    contentImage =
      mconcat
        $ map
            (Vty.string Vty.defAttr . toList . S.take (fromIntegral vw) . S.drop
              (fromIntegral vc))
        $ (toList . S.take (fromIntegral vh) . S.drop (fromIntegral vr)) con
    actualCursor = Vty.Cursor (fromIntegral (cc - vc)) (fromIntegral (cr - vr))

myHandler :: Size -> Handler Vty.Event Vty.Picture
myHandler s = Handler $ \evs pic -> do
  escEvent <- sample (next (filterEs (== Vty.EvKey Vty.KEsc []) evs))
  pictures <- sample (fmap editorToPicture <$> editor (emptyEditor s) (filterMapEs toEditorEvent evs))
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
  vty <- Vty.mkVty (cfg { Vty.mouseMode = Just True })
  s <- (\(w,h) -> Size (W w) (H h)) <$> Vty.displayBounds (Vty.outputIface vty)
  run (vtyFrontend' vty)
      (myHandler s)
      ((Vty.picForImage mempty) { Vty.picCursor = Vty.Cursor 0 0 })
