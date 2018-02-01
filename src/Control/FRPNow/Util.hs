module Control.FRPNow.Util where

import Control.FRPNow

partitionEs :: (a -> Bool) -> EvStream a -> (EvStream a, EvStream a)
partitionEs goesLeft evs = (filterEs goesLeft evs, filterEs (not . goesLeft) evs)

fromUpdates :: a -> EvStream (a -> a) -> Behavior (EvStream a)
fromUpdates a evs = scanlEv (flip id) a evs

