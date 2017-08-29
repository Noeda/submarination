module Submarination.Random
  ( runWithRandomSupply
  , MonadRandomSupply(..)
  , randomCoinToss
  , randomV2Spherical
  , randomOf
  , randomV2In
  , RandomSupplyT() )
  where

import Control.Lens ( (<&>) )
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import Data.Word
import Linear.V2
import Protolude
import System.Random.MWC

newtype RandomSupplyT m a = RandomSupplyT { unwrapRandomSupplyT :: ReaderT (Gen (PrimState m)) m a }
  deriving ( Functor, Applicative, Monad, Typeable, Generic )

class Monad m => MonadRandomSupply m where
  randomInt    :: (Int, Int)       -> m Int
  randomDouble :: (Double, Double) -> m Double

instance MonadTrans RandomSupplyT where
  lift = RandomSupplyT . lift
  {-# INLINE lift #-}

instance PrimMonad m => MonadRandomSupply (RandomSupplyT m) where
  randomInt = RandomSupplyT . (ask >>=) . uniformR
  {-# INLINE randomInt #-}

  randomDouble = RandomSupplyT . (ask >>=) . uniformR
  {-# INLINE randomDouble #-}

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

runWithRandomSupply :: PrimMonad m => Word32 -> RandomSupplyT m a -> m a
runWithRandomSupply seed =
  (initialize (V.singleton seed) >>=) . runReaderT . unwrapRandomSupplyT

randomV2Spherical :: MonadRandomSupply m => Double -> m (V2 Double)
randomV2Spherical scale = do
  dist <- randomDouble (0, scale)
  angle <- randomDouble (0, pi*2)
  return $ V2 (dist * cos angle) (dist * sin angle)

randomV2In :: MonadRandomSupply m => (V2 Int, V2 Int) -> m (V2 Int)
randomV2In (V2 x1 y1, V2 x2 y2) =
  V2 <$> randomInt (mi_x, ma_x) <*> randomInt (mi_y, ma_y)
 where
  mi_x = min x1 x2
  ma_x = max x1 x2
  mi_y = min y1 y2
  ma_y = max y1 y2

randomOf :: MonadRandomSupply m => VB.Vector a -> m a
randomOf vec = (vec VB.!) <$> randomInt (0, num_elems-1)
 where
  num_elems = VB.length vec

randomCoinToss :: MonadRandomSupply m => m Bool
randomCoinToss = randomInt (0, 1) <&> \case
  0 -> False
  _ -> True

