{-# LANGUAGE CPP #-}

module Submarination.Biome.AncientCaves
  ( ancientCaves
  , ancientCavesGen )
  where

-- Level with very regular tubular caves
--

import Control.Lens hiding ( Level, elements )
import Data.Maybe
import qualified Data.Vector as V
import Linear.V2
import Protolude

import Submarination.Biome.Common
#ifdef USE_BAKED_LEVELS
import Submarination.Biome.AncientCavesGen
#endif
import Submarination.Creature
import Submarination.Direction
import Submarination.Item
import Submarination.Level
import Submarination.Random
import Submarination.Turn
import Submarination.Voronoi

ancientCaves :: Level
#ifdef USE_BAKED_LEVELS
ancientCaves = biome
#else
ancientCaves = ancientCavesGen
#endif
{-# NOINLINE ancientCaves #-}

zoneSize :: Int
zoneSize = 130

ancientCavesGen :: Level
ancientCavesGen = runST $
  execStateT
    (runWithRandomSupply 15938 layout)
    (emptyLevel Rock)
 where
  carveSubSurroundings :: RandomSupplyT (StateT Level (ST s)) ()
  carveSubSurroundings = do
    for_ [V2 x y | x <- [-9,-8..9], y <- [-9,-8..9]] $ \ipos -> do
      let pos = ipos + V2 103 70
      lift $ cellAt pos .= Water

  shootCarver :: RandomSupplyT (StateT Level (ST s)) ()
  shootCarver = do
    points <- replicateM 5 $
      fmap fromIntegral <$> randomV2In (V2 20 20, V2 (zoneSize-20) (zoneSize-20))

    for_ [0,0.001..1.0] $ \theta -> do
      let Just p' = bezierRing points theta
          p = floor <$> p'
      for_ [V2 x y | x <- [-1,0,1], y <- [-1,0,1]] $ \ipos -> do
        let pos = ipos + p
        lift $ cellAt pos .= Water

  layout :: RandomSupplyT (StateT Level (ST s)) ()
  layout = do
    shootCarver
    shootCarver
    shootCarver
    carveSubSurroundings

    lift $ modify $ darkenRocks zoneSize

