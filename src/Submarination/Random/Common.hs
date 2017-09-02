module Submarination.Random.Common
  ( MonadRandomSupply(..) )
  where

import Protolude

class Monad m => MonadRandomSupply m where
  randomInt    :: (Int, Int)       -> m Int
  randomDouble :: (Double, Double) -> m Double

instance MonadRandomSupply m => MonadRandomSupply (StateT s m) where
  randomInt    = lift . randomInt
  {-# INLINE randomInt #-}
  randomDouble = lift . randomDouble
  {-# INLINE randomDouble #-}

instance MonadRandomSupply m => MonadRandomSupply (ReaderT s m) where
  randomInt    = lift . randomInt
  {-# INLINE randomInt #-}
  randomDouble = lift . randomDouble
  {-# INLINE randomDouble #-}

