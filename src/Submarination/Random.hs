module Submarination.Random
  ( runWithRandomSupply
  , randomCoinToss
  , randomInt
  , randomDouble
  , randomV2Spherical
  , randomOf
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

instance MonadTrans RandomSupplyT where
  lift = RandomSupplyT . lift
  {-# INLINE lift #-}

runWithRandomSupply :: PrimMonad m => Word32 -> RandomSupplyT m a -> m a
runWithRandomSupply seed =
  (initialize (V.singleton seed) >>=) . runReaderT . unwrapRandomSupplyT

randomInt :: PrimMonad m => (Int, Int) -> RandomSupplyT m Int
randomInt = RandomSupplyT . (ask >>=) . uniformR

randomDouble :: PrimMonad m => (Double, Double) -> RandomSupplyT m Double
randomDouble = RandomSupplyT . (ask >>=) . uniformR

randomV2Spherical :: PrimMonad m => Double -> RandomSupplyT m (V2 Double)
randomV2Spherical scale = do
  angle <- randomDouble (0, pi*2)
  dist <-  randomDouble (0, scale)
  return $ V2 (cos angle * dist) (sin angle * dist)

randomOf :: PrimMonad m => VB.Vector a -> RandomSupplyT m a
randomOf vec = (vec VB.!) <$> randomInt (0, num_elems-1)
 where
  num_elems = VB.length vec

randomCoinToss :: PrimMonad m => RandomSupplyT m Bool
randomCoinToss = randomInt (0, 1) <&> \case
  0 -> False
  _ -> True

