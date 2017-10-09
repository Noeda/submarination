{-# LANGUAGE CPP #-}

module Submarination.Biome.KelpForest
  ( kelpForest
  , kelpForestGen )
  where

-- Kelp forest happens in shallow muddy places
-- 1) Kelp and green stuff
-- 2) Ugly mud fish
--

import Control.Lens hiding ( Level, elements )
import Data.Maybe
import qualified Data.Vector as V
import Linear.V2
import Protolude

import Submarination.Biome.Common
#ifdef USE_BAKED_LEVELS
import Submarination.Biome.KelpForestGen
#endif
import Submarination.Creature
import Submarination.Direction
import Submarination.Item
import Submarination.Level
import Submarination.Random
import Submarination.Turn
import Submarination.Voronoi

kelpForest :: Level
#ifdef USE_BAKED_LEVELS
kelpForest = biome
#else
kelpForest = kelpForestGen
#endif
{-# NOINLINE kelpForest #-}

zoneSize :: Int
zoneSize = 120

kelpForestGen :: Level
kelpForestGen = runST $
  execStateT
    (runWithRandomSupply 12938 layout)
    (emptyLevel DeepRock)
 where
  -- We put in a grid of points until the edges of the level
  -- Then we'll perturb them a bit
  -- Those will be where kelp is growing
  kelpPoints :: VoronoiGrid ()
  kelpPoints =
    perturbGrid $
    perturbGrid $
    regularGrid (V2 5 5) (zoneSize `div` 5) (zoneSize `div` 5) ()

  voronoi :: VoronoiGrid LevelCell
  voronoi =
    combineNeighbours' (\_ e3 (V2 x y) -> if x <= 12 || y <= 12 || x >= fromIntegral zoneSize-12 || y >= fromIntegral zoneSize-12
                                            then Rock
                                            else e3) $
    elementize [Water, Rock, Soil, Seagrass, Seagrass, Seagrass, Seagrass, Seagrass] $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    perturbGrid $
    regularGrid (V2 10 10) (zoneSize `div` 10) (zoneSize `div` 10) Water

  layout :: RandomSupplyT (StateT Level (ST s)) ()
  layout = do
    forOf_ points kelpPoints $ \pos -> do
      toss <- randomInt (0, 8)
      let fpos = floor <$> pos
      lift $ cellAt fpos .= (if toss == 7 then KelpFresh else Kelp)

      let put_r = do p <- randomV2In (fpos - V2 1 1, fpos + V2 1 1)
                     lift $ cellAt p .= (if | toss == 7
                                              -> KelpFresh
                                            | toss >= 5
                                              -> KelpFlutter
                                            | otherwise
                                              -> Kelp)
      replicateM_ 5 put_r

    for_ [0..zoneSize :: Int] $ \x ->
      for_ [0..zoneSize :: Int] $ \y -> do
        let p = pointAt (V2 (fromIntegral x) (fromIntegral y)) voronoi
            pos = V2 x y
        old_p <- lift $ use $ cellAt pos
        unless ( (old_p == Kelp || old_p == KelpFresh) &&
                 p /= Rock ) $
          lift $ cellAt pos .= p

    lift $ modify $ darkenRocks zoneSize

