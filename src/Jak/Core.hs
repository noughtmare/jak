{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Jak.Core
  ( module Jak.Types
  , mkFrontend
  , run)
  where

import Jak.Types

import Control.FRPNow
import Data.Maybe
import Control.Monad
import Data.Function
import Control.Concurrent
import Control.Exception (bracket)
import System.Exit (ExitCode, exitWith)
import Control.Applicative

repeatIO :: IO a -> Now (EvStream a)
repeatIO get = do
  (evs,callback) <- callbackStream
  async (forever (get >>= callback))
  pure evs

mkFrontend :: forall r e m . IOFrontend r e m -> Frontend e m
mkFrontend (IOFrontend acquire release getEvent draw) = Frontend $ \f -> do
  exitCode <- bracket acquire release $ \a -> runNowMaster $ do
    evs <- repeatIO (getEvent a)
    (modelNow, modelStream, exitEvent) <- f evs
    sync (draw a modelNow)
    callIOStream (draw a) modelStream
    pure exitEvent
  exitWith exitCode

run :: Frontend e m -> Handler e m -> m -> IO ()
run frontend handler model = runNow frontend (\evs -> handle handler evs model)
