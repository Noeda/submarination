module Submarination.Biome.Common
  ( darkenRocks
  , bezierRing )
  where

import Control.Lens hiding ( Level )
import Data.List ( (!!) )
import Linear.V2
import Linear.Vector
import Protolude

import Submarination.Direction
import Submarination.Level

darkenRocks :: Int -> Level -> Level
darkenRocks zoneSize lvl = flip execState lvl $
  for_ [0..zoneSize] $ \x -> for_ [0..zoneSize] $ \y -> do
    let old_cell = lvl^.cellAt (V2 x y)
    when (old_cell == Rock || old_cell == MountainRock) $
      when (all (\pos -> let c = lvl^.cellAt pos
                          in c == Rock || c == MountainRock)
                (allNeighbours (V2 x y))) $
        cellAt (V2 x y) %= \case
          Rock -> DeepRock
          MountainRock -> DeepMountainRock
          something_else -> something_else

bezierRing :: [V2 Double] -> Double -> Maybe (V2 Double)
bezierRing [] _ = Nothing
bezierRing [point] _ = Just point
bezierRing [point1, point2] theta = Just $
  ((1-theta) *^ point1) +
  (theta *^ point2)
bezierRing lst theta'' = Just $
    let p0 = prev_point
        p1 = (control_point - (0.25 *^ p0) - (0.25 *^ p2)) / 0.5

        p2 = next_point
        t = 1 - (theta' / 2)

     in ((t**2) *^ p0) +
        (((1-t)**2) *^ p2) +
        ((2*t*(1-t)) *^ p1)
 where
  num_points = length lst

  prev_point = lst !! idx
  control_point = lst !! next_idx
  next_point = lst !! next_next_idx

  theta = theta'' - fromIntegral (floor theta'' :: Integer)

  theta' :: Double
  theta' = (theta - (fromIntegral idx*division)) / division

  next_idx = (idx+1) `mod` num_points
  next_next_idx = (idx+2) `mod` num_points
  idx = (floor $ theta / division :: Int) `mod` num_points
  division = 1.0 / fromIntegral num_points

