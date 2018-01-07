{-# LANGUAGE ScopedTypeVariables #-}
module Jak.Core where

import Control.FRPNow
import Data.Maybe
import Control.Monad
import Data.Function
import Control.Concurrent
import Control.Exception (bracket)
import System.Exit (ExitCode, exitWith)

class Content a where
  move      :: (Int,Int) -> a -> a
  moveUp    :: a -> a
  moveDown  :: a -> a
  moveRight :: Bool -> a -> a
  moveLeft  :: Bool -> a -> a
  insert    :: Char -> a -> a
  delete    :: a -> a
  backspace :: a -> a
  renderContent :: a -> ((Int,Int),[String])
  emptyContent :: a
  resize    :: (Int,Int) -> a -> a

newtype Frontend event model = Frontend { runNow :: (EvStream event -> Now (BehaviorEnd model ExitCode)) -> IO () }

emptyFrontend :: Frontend e m
emptyFrontend = Frontend $ \f -> runNowMaster (end <$> f mempty) >>= exitWith

newtype Handler event model = Handler { handle :: EvStream event -> model -> Now (BehaviorEnd model ExitCode) }

emptyHandler :: Handler e m
emptyHandler = Handler $ \_ b -> pure (pure b `Until` never)

data IOFrontend resource event model rendered = IOFrontend
  { acquire  :: IO resource
  , release  :: resource -> IO ()
  , getEvent :: resource -> IO event
  , draw     :: resource -> rendered -> IO ()
  , render   :: model -> rendered
  , logEvent :: event -> IO ()
  , logModel :: model -> IO ()
  }

mkFrontend :: forall a e m c . Eq m => IOFrontend a e m c -> Frontend e m
mkFrontend (IOFrontend acquire release getEvent draw render logEvent logModel) = Frontend $ \f -> do
  exitCode <- bracket acquire release $ \a -> runNowMaster $ do
    (evs,callback) <- callbackStream
    async (forever (getEvent a >>= callback))
    callIOStream logEvent evs
    (Until modelBehavior exitEvent) <- f evs 
    modelNow <- sample modelBehavior
    sync (drawModel a modelNow)
    callIOStream (drawModel a) (toChanges modelBehavior)
    pure exitEvent
  exitWith exitCode
  where
    drawModel :: a -> m -> IO ()
    drawModel a m = logModel m *> draw a (render m)

run :: Frontend e m -> Handler e m -> m -> IO ()
run frontend handler model = runNow frontend (\evs -> handle handler evs model)
