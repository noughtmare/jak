{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Jak.Core
import Jak.Frontend.Vty
import Jak.Content.DoubleSeq

import Data.Foldable (toList, fold)
import Control.FRPNow
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Data.Maybe
import Control.Applicative
import qualified Graphics.Vty as Vty
import Data.Sequence (fromList, singleton)

myHandler :: Content a => Handler Vty.Event a
myHandler = Handler $ \evs i -> do
  escEvent <- sample (next (filterEs (== Vty.EvKey Vty.KEsc []) evs))
  i' <- sample (foldEs (\a b -> handleEvent b a) i evs)
  pure (i' `Until` (ExitSuccess <$ escEvent))

addLineNumbers :: Vty.Image -> Vty.Image
addLineNumbers img = foldr1 (<>) (
  [ Vty.string Vty.defAttr (replicate (max 3 (length (show (Vty.imageHeight img))) - length (show i)) ' ' ++ show i ++ " ")
     | i <- [1..Vty.imageHeight img]]) Vty.<|> img

handleEvent :: Content a => Vty.Event -> a -> a
handleEvent (Vty.EvKey Vty.KEnter    []) = insert '\n'
handleEvent (Vty.EvKey (Vty.KChar i) []) = insert i
handleEvent (Vty.EvKey Vty.KBS       []) = backspace
handleEvent (Vty.EvKey Vty.KDel      []) = delete
handleEvent (Vty.EvKey Vty.KLeft     []) = moveLeft False
handleEvent (Vty.EvKey Vty.KRight    []) = moveRight False
handleEvent (Vty.EvKey Vty.KUp       []) = moveUp
handleEvent (Vty.EvKey Vty.KDown     []) = moveDown
handleEvent (Vty.EvMouseDown c r Vty.BLeft []) = move (r,c)
handleEvent (Vty.EvResize c r)           = resize (r,c)
handleEvent _                            = id

aSquare  n = fromList (unlines (replicate n (replicate n 'a')))
aSquare' n = fromList (map fromList (replicate n (replicate n 'a')))
aLine'   n = singleton (fromList (replicate n 'a'))

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  run (vtyFrontend (cfg {Vty.mouseMode = Just True}) renderContent) myHandler (DoubleSeq (aSquare' 1000) 0 0 0 0 (50,50))
