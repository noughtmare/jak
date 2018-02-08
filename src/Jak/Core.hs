module Jak.Core
  ( module Jak.Types
  , mkFrontend
  , run
  ) where

import Jak.Types
import Control.FRPNow.Util

import Control.FRPNow
import Control.Exception (bracket)
import System.Exit (ExitCode, exitWith)

mkFrontend :: IOFrontend r e m -> Frontend e m
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
