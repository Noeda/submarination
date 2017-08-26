module Submarination.Sub
  ( SubTopology()
  , removeNonAirLockDoors
  , standardRoom
  , airLock
  , bridge
  , composeHorizontally
  , composeVertically
  , composeHorizontally'
  , composeVertically'
  , getLocationNameInSub
  , getAtomTopologyAt
  , isAirLock
  , isBridge
  , subCell
  , subCellP
  , subItems
  , subItemsP
  , subActiveMetadataAt
  , subSize
  , subLevels
  , subLevelsWithOffset )
  where

import Control.Lens hiding ( Level )
import Data.Data
import Linear.V2
import qualified Prelude as E
import Protolude

import Submarination.Item
import Submarination.Level

data SubTopology
  = StandardRoom !Level
  | Bridge !Level
  | AirLock !Level
  | Composition !SubTopology !SubTopology !(V2 Int) (V2 Int)
  | Rotate90 !SubTopology
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

isAirLock :: SubTopology -> Bool
isAirLock AirLock{} = True
isAirLock _ = False

isBridge :: SubTopology -> Bool
isBridge Bridge{} = True
isBridge _ = False

cardinalNeighboursOf :: V2 Int -> [V2 Int]
cardinalNeighboursOf (V2 x y) =
  [V2 (x-1) y
  ,V2 (x+1) y
  ,V2 x (y-1)
  ,V2 x (y+1)]

removeNonAirLockDoors :: SubTopology -> SubTopology
removeNonAirLockDoors topo = flip execState topo $
  for_ [0..w-1] $ \x -> for_ [0..h-1] $ \y ->
    case subCell topo (V2 x y) of
      Just _feat | Just AirLock{} <- getAtomTopologyAt (V2 x y) topo -> return ()

      Just feat | feat == Hatch || feat == OpenHatch ->
        when (any (isNothing . subCell topo) (cardinalNeighboursOf (V2 x y))) $
          subCellP (V2 x y) .= Hull
      _ -> return ()
 where
  V2 w h = subSize topo

subLevelsWithOffset :: Traversal SubTopology SubTopology (Level, V2 Int) Level
subLevelsWithOffset action topo = go topo identity
 where
  go (Rotate90 topo) offset = Rotate90 <$> go topo (\(V2 x y) -> offset $ V2 y x)
  go (StandardRoom lvl) offset = StandardRoom <$> action (lvl, offset $ V2 0 0)
  go (Bridge lvl) offset = Bridge <$> action (lvl, offset $ V2 0 0)
  go (AirLock lvl) offset = AirLock <$> action (lvl, offset $ V2 0 0)
  go (Composition topo1 topo2 coffset sz) offset =
    Composition <$> go topo1 offset <*> go topo2 (\inner_coords -> offset $ inner_coords + coffset) <*> pure coffset <*> pure sz
{-# INLINE subLevelsWithOffset #-}

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

subLensI :: forall a. (V2 Int -> Lens' Level a) -> V2 Int -> Traversal SubTopology SubTopology (a, V2 Int) a
subLensI level_lens coords action topo = go coords topo identity
 where
  go coords@(V2 x y) topo ioffset | x >= 0 && y >= 0 = case topo of
    StandardRoom lvl | x < 5 && y < 5 ->
      StandardRoom <$> forOf (level_lens coords) lvl (\thing -> action (thing, ioffset (V2 x y)))

    AirLock lvl | x < 3 && y < 3 ->
      AirLock <$> forOf (level_lens coords) lvl (\thing -> action (thing, ioffset (V2 x y)))

    Bridge lvl | not (x == 0 && y == 0) &&
                 not (x == 0 && y == 4) &&
                 x < 5 && y < 5 ->
      Bridge <$> forOf (level_lens coords) lvl (\thing -> action (thing, ioffset (V2 x y)))

    Composition topo1 topo2 offset@(V2 ox oy) (V2 w h) | x < w && y < h ->
      let V2 tw1 th1 = subSize topo1

       in if x < tw1 && y < th1 && execState (subLens (const identity) coords (\i -> put True >> pure i) topo1) False
            then Composition <$> go coords topo1 (\inner_coords -> ioffset $ inner_coords + offset) <*> pure topo2 <*> pure offset <*> pure (V2 w h)
            else Composition topo1 <$> go (V2 (x-ox) (y-oy)) topo2 (\inner_coords -> ioffset $ inner_coords + offset) <*> pure offset <*> pure (V2 w h)

    Rotate90 topo ->
      go (V2 y x) topo (\(V2 ix iy) -> V2 iy ix)

    _ -> pure topo

  go _ _ _ = pure topo
{-# INLINE subLensI #-}

subLens :: (V2 Int -> Lens' Level a) -> V2 Int -> Traversal' SubTopology a
subLens level_lens coords action topo =
  forOf (subLensI level_lens coords) topo $ \(thing, _coords) -> action thing
{-# INLINE subLens #-}

topologyName :: SubTopology -> Maybe Text
topologyName Bridge{}         = Just "Bridge"
topologyName AirLock{}        = Just "Airlock"
topologyName StandardRoom{}   = Just "Standard Compartment"
topologyName (Rotate90 inner) = topologyName inner
topologyName _                = Nothing

getLocationNameInSub :: V2 Int -> SubTopology -> Maybe Text
getLocationNameInSub coords topo =
  case getAtomTopologyAt' coords topo 1 of
    Just (topo, True)  -> topologyName topo
    Just (_topo, False) -> Just "Hatch"
    _ -> Nothing

getAtomTopologyAt :: V2 Int -> SubTopology -> Maybe SubTopology
getAtomTopologyAt coords topo = fst <$> getAtomTopologyAt' coords topo 0

getAtomTopologyAt' :: V2 Int -> SubTopology -> Int -> Maybe (SubTopology, Bool)
getAtomTopologyAt' (V2 x y) (Composition topo1 topo2 (V2 ox oy) (V2 w h)) threshold
  | x >= 0 && y >= 0 && x < w && y < h =
      getAtomTopologyAt' (V2 x y) topo1 threshold <|>
      getAtomTopologyAt' (V2 (x-ox) (y-oy)) topo2 threshold
  | otherwise = Nothing
getAtomTopologyAt' (V2 x y) (Rotate90 inner) threshold =
  getAtomTopologyAt' (V2 y x) inner threshold
getAtomTopologyAt' coord@(V2 x y) topo threshold = do
  let V2 w h = subSize topo
  case firstOf (subCellP coord) topo of
    Just{} ->
      return $ if x >= threshold && y >= threshold && x < w-threshold && y < h-threshold
        then (topo, True)
        else (topo, False)
    _ -> Nothing

subActiveMetadataAt :: V2 Int -> Traversal' SubTopology (Maybe LevelActiveMetadata)
subActiveMetadataAt = subLens activeMetadataAt
{-# INLINE subActiveMetadataAt #-}

subItemsP :: V2 Int -> Traversal' SubTopology [Item]
subItemsP = subLens itemsAt
{-# INLINE subItemsP #-}

subCellP :: V2 Int -> Traversal' SubTopology LevelCell
subCellP = subLens cellAt
{-# INLINE subCellP #-}

subCell :: SubTopology -> V2 Int -> Maybe LevelCell
subCell topo coords = firstOf (subCellP coords) topo
{-# INLINEABLE subCell #-}

subItems :: SubTopology -> V2 Int -> Maybe [Item]
subItems topo coords = firstOf (subItemsP coords) topo
{-# INLINE subItems #-}

subSize :: SubTopology -> V2 Int
subSize (Composition _ _ _ sz) = sz
subSize StandardRoom{} = V2 5 5
subSize AirLock{} = V2 3 3
subSize Bridge{} = V2 5 5
subSize (Rotate90 inner) =
  let V2 w h = subSize inner
   in V2 h w

compose :: V2 Int -> SubTopology -> SubTopology -> SubTopology
compose offset@(V2 ox oy) topo1 topo2 =
  let V2 w h = subSize topo1
      V2 iw ih = subSize topo2

      topo1_x_max = w-1
      topo1_y_max = h-1

      topo2_x_max = ox+iw-1
      topo2_y_max = oy+ih-1

      rightest_extent = max topo2_x_max topo1_x_max
      bottommost_extent = max topo2_y_max topo1_y_max

   in Composition topo1 topo2 offset (V2 (rightest_extent+1) (bottommost_extent+1))

composeHorizontally' :: [SubTopology] -> SubTopology
composeHorizontally' [] = E.error "composeHorizontally': must pass at least one topology."
composeHorizontally' [topo] = topo
composeHorizontally' (topo1:rest) =
  composeHorizontally topo1 (composeHorizontally' rest)

composeVertically' :: [SubTopology] -> SubTopology
composeVertically' [] = E.error "composeVertically': must pass at least one topology."
composeVertically' [topo] = topo
composeVertically' (topo1:rest) =
  composeVertically topo1 (composeVertically' rest)

composeHorizontally :: SubTopology -> SubTopology -> SubTopology
composeHorizontally topo1 topo2 =
  let V2 w h = subSize topo1
      V2 _iw ih = subSize topo2

      center_y = h `div` 2
      center_inner_y = ih `div` 2

      offset = V2 (w-1) (center_y - center_inner_y)

   in compose offset topo1 topo2

composeVertically :: SubTopology -> SubTopology -> SubTopology
composeVertically topo1 topo2 =
  let V2 w h = subSize topo1
      V2 iw _ih = subSize topo2

      center_x = w `div` 2
      center_inner_x = iw `div` 2

      offset = V2 (center_x - center_inner_x) (h-1)

   in compose offset topo1 topo2

