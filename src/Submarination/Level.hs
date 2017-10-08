{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Level
  ( Level()
  , LevelCell(..)
  , LevelActiveMetadata(..)
  , cellAt
  , itemsAt
  , activeMetadataAt
  , defaultLevelCell
  , placeCreature
  , placeItem
  , creatures
  , creatureAt
  , emptyLevel
  , levelFromStrings
  , levelFromStringsPlacements
  , walkLevelActiveMetadata
  , rebase
  , isWalkable )
  where

import Control.Lens hiding ( Level )
import Data.Binary
import Data.Char ( isDigit )
import Data.Data
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Linear.V2
import Protolude hiding ( (&) )

import Submarination.Creature
import Submarination.Item
import qualified Submarination.QuadTree as Q
import Submarination.Turn

data Level = Level
  { _defaultLevelCell    :: !LevelCell
  , _levelCells          :: !(M.Map (V2 Int) LevelCell)
  , _levelActiveMetadata :: !(M.Map (V2 Int) LevelActiveMetadata)
  , _levelItems          :: !(M.Map (V2 Int) [Item])
  , _creatures           :: !(Q.IntQuadTree Creature) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

newtype LevelActiveMetadata
  = HatchAutoClose Turn
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data LevelCell
  = Water
  | SurfaceWater
  | SurfaceWaterSplashing
  | WoodenBoards
  | Rock
  | DeepRock
  | MountainRock
  | DeepMountainRock
  | Grass
  | Hull
  | Window
  | Hatch
  | OpenHatch
  | InteriorFloor
  | HappyCoral
  | Soil
  | Blood
  | BloodCoagulated
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )
makeLenses ''Level

isWalkable :: LevelCell -> Bool
isWalkable Water = True
isWalkable SurfaceWater = False
isWalkable SurfaceWaterSplashing = False
isWalkable WoodenBoards = True
isWalkable Rock = False
isWalkable MountainRock = False
isWalkable Grass = True
isWalkable Hull = False
isWalkable InteriorFloor = True
isWalkable Hatch = False
isWalkable OpenHatch = True
isWalkable Window = False
isWalkable HappyCoral = True
isWalkable Soil = True
isWalkable DeepRock = False
isWalkable DeepMountainRock = False
isWalkable Blood = True
isWalkable BloodCoagulated = True

walkLevelActiveMetadata :: Applicative f => Level -> (V2 Int -> LevelCell -> LevelActiveMetadata -> f (LevelCell, Maybe LevelActiveMetadata)) -> f Level
walkLevelActiveMetadata level action =
  injector <$>
  ifor (level^.levelActiveMetadata) (\coords metadata ->
     action coords (level^.cellAt coords) metadata)
 where
  injector :: M.Map (V2 Int) (LevelCell, Maybe LevelActiveMetadata) -> Level
  injector metadata_mapping = execState stater level
   where
    stater = ifor_ metadata_mapping $ \coords new_metadata -> case new_metadata of
      (new_cell, Nothing) -> do
        levelActiveMetadata %= M.delete coords
        cellAt coords .= new_cell
      (new_cell, Just mt) -> do
        levelActiveMetadata %= M.insert coords mt
        cellAt coords .= new_cell

activeMetadataAt :: V2 Int -> Lens' Level (Maybe LevelActiveMetadata)
activeMetadataAt coords = lens get_it set_it
 where
  get_it level = M.lookup coords (level^.levelActiveMetadata)

  set_it level Nothing = level & levelActiveMetadata %~ M.delete coords
  set_it level (Just new_metadata) = level & levelActiveMetadata %~ M.insert coords new_metadata

itemsAt :: V2 Int -> Lens' Level [Item]
itemsAt coords = lens get_it set_it
  where
   get_it level       = fromMaybe [] $ M.lookup coords (level^.levelItems)

   set_it level [] = level & levelItems.at coords .~ Nothing
   set_it level items = level & levelItems.at coords .~ Just items

cellAt :: V2 Int -> Lens' Level LevelCell
cellAt coords = lens get_it set_it
 where
  get_it level = fromMaybe (level^.defaultLevelCell) $ M.lookup coords (level^.levelCells)

  -- If some cell is set to default cell, we can remove it from map to save
  -- memory.
  set_it level new_cell | new_cell == level^.defaultLevelCell =
    level & (levelCells %~ M.delete coords)

  set_it level new_cell =
    level & (levelCells %~ M.insert coords new_cell)
{-# INLINEABLE cellAt #-}

placeCreature :: Creature -> V2 Int -> Level -> Level
placeCreature creature location =
  creatures.at location .~ Just creature

placeItem :: Item -> V2 Int -> Level -> Level
placeItem item location =
  levelItems.at location %~ \case
    Nothing -> Just [item]
    Just items -> Just (item:items)

emptyLevel :: LevelCell -> Level
emptyLevel default_cell = Level
  { _defaultLevelCell = default_cell
  , _levelCells = mempty
  , _levelItems = mempty
  , _creatures = Q.empty
  , _levelActiveMetadata = mempty }

levelFromStrings :: LevelCell -> [Text] -> Level
levelFromStrings default_cell = levelFromStringsPlacements default_cell []

levelFromStringsPlacements :: LevelCell -> [(Int, (Char, V2 Int -> Level -> Level))] -> [Text] -> Level
levelFromStringsPlacements default_cell settings strs =
  let (cells, integer_location_mapping) = toMapping strs
      base_level = Level { _defaultLevelCell    = default_cell
                         , _levelCells          = cells
                         , _levelItems          = M.empty
                         , _creatures           = Q.empty
                         , _levelActiveMetadata = M.empty }

   in foldr (placements_folder integer_location_mapping) base_level settings
 where
  placements_folder integer_location_mapping (index, (ch, level_modifier)) level =
    case IM.lookup index integer_location_mapping of
      Nothing -> level
      Just location ->
        level_modifier location (level & (cellAt location .~ chToFeature ch))

  chToFeature ch = case ch of
    '.' -> SurfaceWater
    ',' -> SurfaceWaterSplashing
    '+' -> InteriorFloor
    '=' -> WoodenBoards
    'S' -> Water
    '#' -> Rock
    'X' -> Hull
    'W' -> Window
    'h' -> Hatch
    '^' -> MountainRock
    'g' -> Grass
    'C' -> HappyCoral
    _   -> Water

  toMapping = go 0 
   where
    go _y [] = (M.empty, IM.empty)
    go y (line:rest) =
      let (rest_cells_mapping, rest_integer_location_mapping) = go (y+1) rest
       in (M.union rest_cells_mapping $
           M.fromList $
           zip [0..] (T.unpack line) <&> \(x, ch) -> (V2 x y, chToFeature ch)

          ,IM.union rest_integer_location_mapping $
           IM.fromList $ catMaybes $
           zip [0..] (T.unpack line) <&> \(x, ch) ->
             if isDigit ch
               then Just (ord ch - ord '0', V2 x y)
               else Nothing)

rebase :: V2 Int -> Level -> Level
rebase offset level = level
  { _levelCells          = M.mapKeys (\x -> x - offset) (_levelCells level)
  , _levelActiveMetadata = M.mapKeys (\x -> x - offset) (_levelActiveMetadata level)
  , _levelItems          = M.mapKeys (\x -> x - offset) (_levelItems level)
  , _creatures           = Q.mapKeys (\x -> x - offset) (_creatures level) }

creatureAt :: V2 Int -> Lens' Level (Maybe Creature)
creatureAt pos = lens get_it set_it
 where
  get_it lvl = lvl^.creatures.at pos
  set_it lvl Nothing = lvl & creatures.at pos .~ Nothing
  set_it lvl (Just creature) = lvl & creatures.at pos .~ Just creature

