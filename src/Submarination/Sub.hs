module Submarination.Sub
  ( SubTopology()
  , standardRoom
  , airLock
  , bridge
  , composeHorizontally
  , composeVertically
  , subCell
  , subSize )
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

subCell :: SubTopology -> V2 Int -> Maybe LevelCell
subCell (Composition topo1 topo2 (V2 ox oy) (V2 sw sh)) pos@(V2 x y) =
  -- First do a check if we are at all inside the sub (this way we don't
  -- compute tw1/th1/tw2/th2 until we have to check them).
  if | x < 0 || y < 0 || x >= sw || y >= sh
       -> Nothing

     | x < tw1 && y < th1
       -> subCell topo1 pos

     | x >= ox && y >= oy && x < ox+tw2 && y < oy+th2
       -> subCell topo2 (V2 (x-ox) (y-oy))

     | otherwise
       -> Nothing
 where
  V2 tw1 th1 = subSize topo1
  V2 tw2 th2 = subSize topo2
subCell (StandardRoom lvl) pos@(V2 x y) =
  if x >= 0 && y >= 0 && x < 5 && y < 5
    then Just $ lvl^.cellAt pos
    else Nothing
subCell (AirLock lvl) pos@(V2 x y) =
  if x >= 0 && y >= 0 && x < 3 && y < 3
    then Just $ lvl^.cellAt pos
    else Nothing
subCell (Bridge lvl) pos@(V2 x y) =
  if x >= 0 && y >= 0 && x < 5 && y < 5 && not (x == 0 && y == 0) && not (x == 0 && y == 4)
    then Just $ lvl^.cellAt pos
    else Nothing
subCell (Rotate90 inner) (V2 x y) =
  subCell inner (V2 y x)
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

