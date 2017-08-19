module Submarination.Sub
  ( SubTopology()
  , standardRoom
  , airLock
  , bridge
  , composeHorizontally
  , composeVertically
  , subCell
  , subCellP
  , subActiveMetadataAt
  , subSize
  , subLevels )
  where

import Control.Lens hiding ( Level )
import Data.Data
import Linear.V2
import Protolude

import Submarination.Level

data SubTopology
  = StandardRoom !Level
  | Bridge !Level
  | AirLock !Level
  | Composition !SubTopology !SubTopology !(V2 Int) (V2 Int)
  | Rotate90 !SubTopology
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

subLevels :: Traversal' SubTopology Level
subLevels action (Rotate90 topo) = Rotate90 <$> subLevels action topo
subLevels action (StandardRoom lvl) = StandardRoom <$> action lvl
subLevels action (Bridge lvl) = Bridge <$> action lvl
subLevels action (AirLock lvl) = AirLock <$> action lvl
subLevels action (Composition topo1 topo2 offset sz) =
  Composition <$> subLevels action topo1 <*> subLevels action topo2 <*> pure offset <*> pure sz

standardRoom :: SubTopology
standardRoom = StandardRoom $
  levelFromStrings Hull
    ["XXhXX"
    ,"X+++X"
    ,"h+++h"
    ,"X+++X"
    ,"XXhXX"]

bridge :: SubTopology
bridge = Bridge $
  levelFromStrings Hull
    [".XXXX"
    ,"XX++X"
    ,"W+++h"
    ,"XX++X"
    ,".XXXX"]

airLock :: SubTopology
airLock = AirLock $ levelFromStrings Hull
 ["XhX"
 ,"X+X"
 ,"XhX"]

subLens :: (V2 Int -> Lens' Level a) -> V2 Int -> Traversal' SubTopology a
subLens level_lens coords@(V2 x y) action topo | x >= 0 && y >= 0 = case topo of
  StandardRoom lvl | x < 5 && y < 5 -> StandardRoom <$> forOf (level_lens coords) lvl action
  AirLock lvl | x < 3 && y < 3 -> AirLock <$> forOf (level_lens coords) lvl action
  Bridge lvl | not (x == 0 && y == 0) &&
               not (x == 0 && y == 4) &&
               x < 5 && y < 5 -> Bridge <$> forOf (level_lens coords) lvl action
  Composition topo1 topo2 offset@(V2 ox oy) (V2 w h) | x < w && y < h ->
    let V2 tw1 th1 = subSize topo1

     in if x < tw1 && y < th1 && execState (subLens (\_ -> identity) coords (\i -> put True >> pure i) topo1) False
          then Composition <$> (self coords action topo1) <*> pure topo2 <*> pure offset <*> pure (V2 w h)
          else Composition topo1 <$> (self (V2 (x-ox) (y-oy)) action topo2) <*> pure offset <*> pure (V2 w h)


  _ -> pure topo
 where
  self = subLens level_lens

subLens _ _ _ topo = pure topo
{-# INLINE subLens #-}

subActiveMetadataAt :: V2 Int -> Traversal' SubTopology (Maybe LevelActiveMetadata)
subActiveMetadataAt = subLens activeMetadataAt
{-# INLINE subActiveMetadataAt #-}

subCellP :: V2 Int -> Traversal' SubTopology LevelCell
subCellP = subLens cellAt
{-# INLINE subCellP #-}

subCell :: SubTopology -> V2 Int -> Maybe LevelCell
subCell topo coords = case topo^..subCellP coords of
  [cell] -> Just cell
  _ -> Nothing
{-# INLINEABLE subCell #-}

subSize :: SubTopology -> V2 Int
subSize (Composition _ _ _ sz) = sz
subSize topo = computeSubSize topo

computeSubSize :: SubTopology -> V2 Int
computeSubSize (Composition _ _ _ sz) = sz
computeSubSize (StandardRoom{}) = V2 5 5
computeSubSize (AirLock{}) = V2 3 3
computeSubSize (Bridge{}) = V2 5 5
computeSubSize (Rotate90 inner) =
  let V2 w h = computeSubSize inner
   in V2 h w

composeHorizontally :: SubTopology -> SubTopology -> SubTopology
composeHorizontally topo1 topo2 =
  let V2 w h = computeSubSize topo1
      V2 iw ih = computeSubSize topo2

      center_y = h `div` 2
      center_inner_y = ih `div` 2

      offset@(V2 ox oy) = V2 (w-1) (center_y - center_inner_y)

      topo1_x_max = w-1
      topo1_y_max = h-1

      topo2_x_max = ox+iw-1
      topo2_y_max = oy+ih-1

      rightest_extent = max topo2_x_max topo1_x_max
      bottommost_extent = max topo2_y_max topo1_y_max

   in Composition topo1 topo2 offset (V2 (rightest_extent+1) (bottommost_extent+1))

composeVertically :: SubTopology -> SubTopology -> SubTopology
composeVertically topo1 topo2 =
  let V2 w h = computeSubSize topo1
      V2 iw ih = computeSubSize topo2

      center_x = w `div` 2
      center_inner_x = iw `div` 2

      offset@(V2 ox oy) = V2 (center_x - center_inner_x) (h-1)

      topo1_x_max = w-1
      topo1_y_max = h-1

      topo2_x_max = ox+iw-1
      topo2_y_max = oy+ih-1

      rightest_extent = max topo2_x_max topo1_x_max
      bottommost_extent = max topo2_y_max topo1_y_max

   in Composition topo1 topo2 offset (V2 (rightest_extent+1) (bottommost_extent+1))

