module Submarination.Random.GHCJS
  ( RandomSupplyT()
  , runWithRandomSupply )
  where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Trans.Class
import Data.Word
import Protolude

import Submarination.Random.Common

foreign import javascript unsafe "$r = Math.random();" math_random :: IO Double

newtype RandomSupplyT m a = RandomSupplyT (m a)
  deriving ( Functor, Applicative, Monad, Typeable, Generic )

instance MonadIO m => MonadIO (RandomSupplyT m) where
  liftIO = RandomSupplyT . liftIO

runWithRandomSupply :: PrimMonad m => Word32 -> RandomSupplyT m a -> m a
runWithRandomSupply _ (RandomSupplyT internal) = internal
{-# INLINE runWithRandomSupply #-}

instance MonadTrans RandomSupplyT where
  lift = RandomSupplyT
  {-# INLINE lift #-}

randomDoubleST :: (Double, Double) -> ST s Double
randomDoubleST (mi, ma) = do
  mr <- unsafeIOToST math_random
  mr `seq` return ((mr*(ma-mi)) + mi)
{-# INLINE randomDoubleST #-}

randomIntST :: (Int, Int) -> ST s Int
randomIntST (mi, ma') = do
  let ma = ma'+1
      mid = fromIntegral mi
      mad = fromIntegral ma
  mr <- unsafeIOToST math_random
  mr `seq` return (max mi $ min ma' (floor $ mr * (mad-mid) + mid))
{-# INLINE randomIntST #-}

instance MonadRandomSupply (RandomSupplyT (StateT t (ST s))) where
  randomDouble = RandomSupplyT . lift . randomDoubleST
  {-# INLINE randomDouble #-}

  randomInt = RandomSupplyT . lift . randomIntST
  {-# INLINE randomInt #-}

instance MonadRandomSupply (RandomSupplyT (ST s)) where
  randomDouble = RandomSupplyT . randomDoubleST
  {-# INLINE randomDouble #-}

  randomInt = RandomSupplyT . randomIntST
  {-# INLINE randomInt #-}

instance MonadState s m => MonadState s (RandomSupplyT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

