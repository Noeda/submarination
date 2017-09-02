{-# LANGUAGE CPP #-}

module Submarination.Random
  ( runWithRandomSupply
  , MonadRandomSupply(..)
  , randomCoinToss
  , randomV2Spherical
  , randomOf
  , randomV2In
  , randomDirection
  , RandomSupplyT() )
  where

import Control.Lens ( (<&>) )
import qualified Data.Vector as VB
import Linear.V2
import Protolude

import Submarination.Direction
import Submarination.Random.Common
#ifdef GHCJS_BROWSER
import Submarination.Random.GHCJS
#else
import Submarination.Random.Native
#endif

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

randomDirection :: MonadRandomSupply m => m Direction
randomDirection = randomInt (0, 7) <&> \case
  0 -> D1
  1 -> D2
  2 -> D3
  3 -> D4
  4 -> D6
  5 -> D7
  6 -> D8
  _ -> D9
{-# INLINEABLE randomDirection #-}

