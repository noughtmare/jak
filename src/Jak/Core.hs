{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Jak.Core where

import Control.FRPNow
import Data.Maybe
import Control.Monad
import Data.Function
import Control.Concurrent
import Control.Exception (bracket)
import System.Exit (ExitCode, exitWith)
import Control.Applicative

newtype Frontend event model = Frontend
  { runNow :: (EvStream event -> Now (model, EvStream model, Event ExitCode)) -> IO () }

emptyFrontend :: Frontend e m
emptyFrontend = Frontend $ \f -> runNowMaster ((\(_,_,x) -> x) <$> f mempty) >>= exitWith

newtype Handler event model = Handler
  { handle :: EvStream event -> model -> Now (model, EvStream model, Event ExitCode) }

emptyHandler :: Handler e m
emptyHandler = Handler $ \_ m -> pure (m, mempty, never)

data IOFrontend resource event model = IOFrontend
  { acquire  :: IO resource
  , release  :: resource -> IO ()
  , getEvent :: resource -> IO event
  , draw     :: resource -> model -> IO ()
  }

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
