{-# LANGUAGE LambdaCase #-}
module Main where

import           FRP.Yampa
import           Data.Foldable            (toList)
import qualified Data.Sequence            as S
import qualified Graphics.Vty             as Vty
import qualified Graphics.Vty.Input.Mouse as Vty
import           Jak.Editor               (editor)
import           Jak.Types
import           Control.Monad            (when)
import           Control.Exception        (bracket)

editorToPicture :: Editor -> Vty.Picture
editorToPicture (Editor (Viewport (Position vc vr) (Size vw vh))
                        (Cursor (Position cc cr) _)
                        (Content con)) =
  Vty.Picture actualCursor [contentImage] Vty.ClearBackground
  where
    contentImage =
      mconcat $ map
        (Vty.string Vty.defAttr . toList . S.take (fromIntegral vw) . S.drop
           (fromIntegral vc))
        $ toList $ S.take (fromIntegral vh) $ S.drop (fromIntegral vr) con
    actualCursor = Vty.Cursor (fromIntegral (cc - vc)) (fromIntegral (cr - vr))

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
  Vty.EvKey Vty.KEsc      [] -> Just EditorExit
  _                          -> Nothing

-- TODO: move the following to a test module
--
-- import           Data.Sequence            (fromList, singleton)
--
-- aSquare n = fromList (unlines (replicate n (replicate n 'a')))
-- aSquare' n = fromList (map fromList (replicate n (replicate n 'a')))
-- aLine' n = singleton (fromList (replicate n 'a'))

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty (cfg { Vty.mouseMode = Just True })
  s <- (\(w,h) -> Size (W w) (H h)) <$> Vty.displayBounds (Vty.outputIface vty)

  -- TODO: clean this up
  bracket
    (putStrLn Vty.requestMouseEvents)
    (\_ -> putStrLn Vty.disableMouseEvents *> Vty.shutdown vty)
    (\_ -> reactimate
      (return NoEvent)
      (\_ -> (\e -> (1, Just (maybeToEvent (toEditorEvent e)))) <$> Vty.nextEvent vty)
      (\b ed -> case ed of
        Nothing -> return True
        Just ed' -> False <$ when b (Vty.update vty (editorToPicture ed')))
      (editor (Editor (Viewport (Position 0 0) s) (Cursor (Position 0 0) (V 0)) (Content (S.singleton S.empty)))))
