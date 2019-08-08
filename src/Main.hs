{-# LANGUAGE LambdaCase #-}
module Main where

import           FRP.Yampa
import qualified Graphics.Vty             as Vty
import qualified Graphics.Vty.Input.Mouse as Vty
import           Jak.Types
import           Jak.Editor
import           Control.Monad            (when)
import           Control.Exception        (bracket)

renderPicture :: Render a => a -> Vty.Picture
renderPicture ed = Vty.Picture cursor [image] Vty.ClearBackground
  where
    image = foldMap (Vty.string Vty.defAttr) (renderContent ed)
    cursor = let (Position (C x) (R y)) = renderCursor ed in Vty.Cursor x y

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
        Just ed' -> False <$ when b (Vty.update vty (renderPicture ed')))
      (editor (emptyEditor s)))
