{-# LANGUAGE CPP #-}

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
import qualified Data.Vector as V
import Linear.V2
import Protolude

#ifdef USE_BAKED_LEVELS
import Submarination.Biome.IntertidalZoneGen
#endif
import Submarination.Creature
import Submarination.Direction
import Submarination.Item
import Submarination.Level
import Submarination.Random
import Submarination.Voronoi

intertidalZone :: Level
#ifdef USE_BAKED_LEVELS
intertidalZone = biome
#else
intertidalZone = intertidalZoneGen
#endif
{-# NOINLINE intertidalZone #-}

zoneSize :: Int
zoneSize = 120

darkenRocks :: Level -> Level
darkenRocks lvl = flip execState lvl $
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

isAcceptableCreatureLocation :: V2 Int -> Level -> Bool
isAcceptableCreatureLocation (V2 x y) lvl =
  isWalkable (lvl^.cellAt (V2 x y)) &&
  isNothing (lvl^.creatureAt (V2 x y))

type Randomy s a = RandomSupplyT (StateT Level (ST s)) a

placeCreatures :: Level -> Randomy s Level
placeCreatures = execStateT $ do
  placeCorpseParty
  placeGators
  placeSnoatfish
  placeBiddy
  placeEnneapus
  placeCamobream
 where
  randomAcceptableCreatureLocation action = do
    pos <- randomV2In (V2 0 0, V2 zoneSize zoneSize)
    lvl <- get
    if isAcceptableCreatureLocation pos lvl
      then action pos
      else randomAcceptableCreatureLocation action

  placeGators = replicateM_ 20 $
    randomAcceptableCreatureLocation $ \pos ->
      creatureAt pos .= Just (fromType Gator)

  placeSnoatfish = replicateM_ 100 $
    randomAcceptableCreatureLocation $ \pos ->
      creatureAt pos .= Just (fromType Snoatfish)

  placeBiddy = replicateM_ 50 $
    randomAcceptableCreatureLocation $ \pos ->
      creatureAt pos .= Just (fromType Biddy)

  placeEnneapus = replicateM_ 10 $
    randomAcceptableCreatureLocation $ \pos ->
      creatureAt pos .= Just (fromType Enneapus)

  placeCamobream = replicateM_ 15 $
    randomAcceptableCreatureLocation $ \pos ->
      creatureAt pos .= Just (fromType Camobream)

  placeCorpseParty = randomAcceptableCreatureLocation $ actuallyPlaceCorpseParty (30 :: Int)

  actuallyPlaceCorpseParty 0 _ = return ()
  actuallyPlaceCorpseParty n coords = do
    lvl <- get
    when (null (lvl^.itemsAt coords) && isWalkable (lvl^.cellAt coords)) $ do
      corpse <- randomOf $ V.fromList [WoundedCorpse, WoundedCorpse, WoundedCorpse, MutilatedCorpse, MutilatedCorpse, BloatedCorpse, PartiallyEatenCorpse, SkeletonCorpse, PlantPersonCorpse]
      itemsAt coords .= [corpse]
      get >>= lift . placeBloodSpatter coords >>= put

    ncoords <- fmap round <$> randomV2Spherical 8
    actuallyPlaceCorpseParty (n-1) (coords + ncoords)

placeBloodSpatter :: V2 Int -> Level -> Randomy s Level
placeBloodSpatter loc = execStateT $ go loc (5 :: Int)
 where
  go _ 0 = return ()
  go loc n = do
    bloody <- lift $ randomOf (V.fromList [Blood, Blood, BloodCoagulated])
    old_loc <- use (cellAt loc)
    when (isWalkable old_loc) $
      cellAt loc .= bloody

    nloc <- fmap round <$> randomV2Spherical 4
    go (loc+nloc) (n-1)

intertidalZoneGen :: Level
intertidalZoneGen = runST $
  execStateT
    (runWithRandomSupply 12938 layout)
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
    combineNeighbours' (\_ e3 (V2 x y) -> if x <= 6 || y <= 6 || x >= fromIntegral zoneSize-6 || y >= fromIntegral zoneSize-6
                                            then Rock
                                            else e3) $
    elementize [Water, Water, Water, HappyCoral, Soil, Soil, Rock] $
    perturbGrid $
    perturbGrid $
    regularGrid (V2 3 3) (zoneSize `div` 3) (zoneSize `div` 3) Water

  layout :: RandomSupplyT (StateT Level (ST s)) ()
  layout = do
    for_ [0..zoneSize :: Int] $ \x ->
      for_ [0..zoneSize :: Int] $ \y -> do
        let p = pointAt (V2 (fromIntegral x) (fromIntegral y)) voronoi
            pos = V2 x y
        if p == Rock
          then do toss <- randomInt (0, 4)
                  lift $ cellAt pos .= (if toss < 4 then Rock else MountainRock)
          else lift $ cellAt pos .= p
    lift $ modify darkenRocks
    lift get >>= placeCreatures >>= lift . put


