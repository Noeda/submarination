module Submarination.Biome.IntertidalZone
  ( intertidalZone
  , intertidalZoneGen )
  where

-- Intertidal zone is where ocean meets land.
-- 1) Abundant sunlight
-- 2) SEAWEED
-- 3) Steep cliffs
-- 4) Some coral
-- 5) Rocks
--

import Control.Lens hiding ( Level, elements )
import Data.Maybe
import Linear.V2
import Protolude

import Submarination.Biome.IntertidalZoneGen
import Submarination.Level
import Submarination.Random
import Submarination.Voronoi

intertidalZone :: Level
intertidalZone = biome

intertidalZoneGen :: Level
intertidalZoneGen = runST $
  execStateT
    (runWithRandomSupply 123 layout)
    (emptyLevel Rock)
 where
  voronoi :: VoronoiGrid LevelCell
  voronoi =
    combineNeighbours (\neighbours e3 -> case neighbours of
                         [] -> e3
                         n | all (== (fromJust $ head n)) n -> fromJust $ head n
                         _ -> e3) $
    combineNeighbours (\neighbours e3 -> if length (filter (== Water) neighbours) >= 4
                                           then Water
                                           else e3) $
    combineNeighbours (\neighbours e3 -> if length (filter (== Rock) neighbours) >= 2
                                           then Rock
                                           else e3) $
    combineNeighbours' (\_ e3 (V2 x y) -> if x <= 6 || y <= 6 || x >= 234 || y >= 234
                                            then Rock
                                            else e3) $
    elementize [Water, Water, Water, HappyCoral, Soil, Soil, Rock] $
    perturbGrid $
    perturbGrid $
    regularGrid (V2 3 3) 80 80 Water

  layout :: RandomSupplyT (StateT Level (ST s)) ()
  layout =
    for_ [0..80*3 :: Int] $ \x ->
      for_ [0..80*3 :: Int] $ \y -> do
        let p = pointAt (V2 (fromIntegral x) (fromIntegral y)) voronoi
            pos = V2 x y
        if p == Rock
          then do toss <- randomInt (0, 4)
                  lift $ cellAt pos .= (if toss < 4 then Rock else MountainRock)
          else lift $ cellAt pos .= p


