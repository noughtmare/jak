module Control.FRPNow.Util where

import Control.FRPNow
import Control.Monad.Fix

partitionEs :: (a -> Bool) -> EvStream a -> (EvStream a, EvStream a)
partitionEs goesLeft evs = (filterEs goesLeft evs, filterEs (not . goesLeft) evs)

fromUpdates :: a -> EvStream (a -> a) -> Behavior (EvStream a)
fromUpdates a evs = scanlEv (flip id) a evs

scanlFilterEv :: (a -> b -> Maybe a) -> a -> EvStream b -> Behavior (EvStream a)
scanlFilterEv f a es = mfix $ \es' -> do
  b <- fromChanges a es' >>= delay es a
  pure (catMaybesEs (fmap f b <@@> es))
