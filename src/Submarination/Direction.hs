module Submarination.Direction
  ( Direction(..)
  , Cornering(..)
  , allNeighbours
  , allCardinalNeighbours
  , directionToDelta
  , deltaToDirection
  , deltaToCornering
  , move1V2 )
  where

import Data.Data
import GHC.Generics
import Linear.V2
import Protolude

data Direction
  = D4
  | D8
  | D2
  | D6
  | D9
  | D7
  | D1
  | D3
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

allCardinalNeighbours :: V2 Int -> [V2 Int]
allCardinalNeighbours (V2 x y) =
  [V2 (x-1) y
  ,V2 (x+1) y
  ,V2 x (y-1)
  ,V2 x (y+1)]
{-# INLINEABLE allCardinalNeighbours #-}

allNeighbours :: V2 Int -> [V2 Int]
allNeighbours (V2 x y) =
  [V2 (x-1) y
  ,V2 (x+1) y
  ,V2 x (y-1)
  ,V2 x (y+1)
  ,V2 (x-1) (y-1)
  ,V2 (x+1) (y+1)
  ,V2 (x-1) (y+1)
  ,V2 (x+1) (y-1)]
{-# INLINEABLE allNeighbours #-}

move1V2 :: Direction -> V2 Int -> V2 Int
move1V2 dir = (+ directionToDelta dir)

directionToDelta :: Direction -> V2 Int
directionToDelta D2 = V2 0 1
directionToDelta D8 = V2 0 (-1)
directionToDelta D4 = V2 (-1) 0
directionToDelta D6 = V2 1 0
directionToDelta D7 = V2 (-1) (-1)
directionToDelta D9 = V2 1 (-1)
directionToDelta D3 = V2 1 1
directionToDelta D1 = V2 (-1) 1

data Cornering
 = Vertical
 | Horizontal
 | SECorner
 | SWCorner
 | NECorner
 | NWCorner
 | SWNE
 | SENW
 deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

deltaToCornering :: V2 Int -> V2 Int -> V2 Int -> Maybe Cornering
deltaToCornering a@(V2 ax ay) m b@(V2 bx by) =
  if | ax == bx && abs (ay-by) == 2 -> Just Vertical
     | ay == by && abs (ax-bx) == 2 -> Just Horizontal
     | b-m == V2 1 0    && a-m == V2 0 1       -> Just SECorner
     | b-m == V2 (-1) 0 && a-m == V2 0 1       -> Just SWCorner
     | b-m == V2 1 0    && a-m == V2 0 (-1)    -> Just NECorner
     | b-m == V2 (-1) 0 && a-m == V2 (-1) (-1) -> Just NWCorner
     | a-m == V2 1 0    && b-m == V2 0 1       -> Just SECorner
     | a-m == V2 (-1) 0 && b-m == V2 0 1       -> Just SWCorner
     | a-m == V2 1 0    && b-m == V2 0 (-1)    -> Just NECorner
     | a-m == V2 (-1) 0 && b-m == V2 (-1) (-1) -> Just NWCorner
     | a-m == V2 1 1    && b-m == V2 (-1) (-1) -> Just SENW
     | a-m == V2 1 (-1) && b-m == V2 (-1) 1    -> Just SWNE
     | b-m == V2 1 1    && a-m == V2 (-1) (-1) -> Just SENW
     | b-m == V2 1 (-1) && a-m == V2 (-1) 1    -> Just SWNE
     | otherwise -> Nothing

deltaToDirection :: V2 Int -> V2 Int -> Maybe Direction
deltaToDirection a b =
  if | a + V2 0 1 == b       -> Just D2
     | a + V2 0 (-1) == b    -> Just D8
     | a + V2 1 0 == b       -> Just D6
     | a + V2 (-1) 0 == b    -> Just D4
     | a + V2 1 1 == b       -> Just D3
     | a + V2 (-1) 1 == b    -> Just D1
     | a + V2 (-1) (-1) == b -> Just D7
     | a + V2 1 (-1) == b    -> Just D9
     | otherwise -> Nothing

