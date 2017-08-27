module Submarination.Voronoi
  ( VoronoiGrid()
  , initialVoronoiGrid
  , points
  , pointAt
  , elementize
  , regularGrid
  , perturbGrid
  , neighbourMap
  , neighbourElements
  , combineNeighbours
  , combineNeighbours'
  , combineNeighbours''

  , winding
  , isDinABC )
  where

import Control.Lens hiding ( Level, elements )
import Data.Data
import Data.List ( tail )
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.V2
import Graphics.Triangulation.Delaunay
import Linear.Metric
import Linear.V2
import Linear.V3
import Protolude hiding ( (&) )

import Submarination.Random

data VoronoiGrid a = VoronoiGrid
  { mainPoint    :: (V2 Double, a)
  , otherPoints  :: V.Vector (V2 Double, a)
  , neighbourMap :: IM.IntMap IS.IntSet }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

initialVoronoiGrid :: V2 Double -> a -> VoronoiGrid a
initialVoronoiGrid pos element =
  VoronoiGrid
  { mainPoint = (pos, element)
  , otherPoints = V.empty
  , neighbourMap = IM.empty }

recomputeNeighbourMap :: VoronoiGrid a -> VoronoiGrid a
recomputeNeighbourMap vg =
  vg { neighbourMap = nmap }
 where
  triangulation = triangulate (fromV2 (fst (mainPoint vg)):V.toList (fromV2 . fst <$> otherPoints vg))

  positionToIndex =
    foldr folder (M.singleton (fst $ mainPoint vg) 0) (zip [1..] $ V.toList $ otherPoints vg)
   where
    folder (idx, (opos, _)) = M.insert opos idx

  toV2 (Vector2 x y) = V2 x y
  fromV2 (V2 x y) = Vector2 x y

  insertEdge i1 i2 = do
    modify $ IM.alter (\case Nothing   -> Just (IS.singleton i1)
                             Just iset -> Just (IS.insert i1 iset))
                      i2
    modify $ IM.alter (\case Nothing   -> Just (IS.singleton i2)
                             Just iset -> Just (IS.insert i2 iset))
                      i1

  nmap = flip execState IM.empty $
    for_ triangulation $ \(v1, v2, v3) -> do
      let v1_i = fromJust $ M.lookup (toV2 v1) positionToIndex
          v2_i = fromJust $ M.lookup (toV2 v2) positionToIndex
          v3_i = fromJust $ M.lookup (toV2 v3) positionToIndex

      insertEdge v1_i v2_i
      insertEdge v1_i v3_i
      insertEdge v2_i v3_i

points :: Traversal' (VoronoiGrid a) (V2 Double)
points fun (VoronoiGrid mainpoint otherpoints neighbours) =
  fmap recomputeNeighbourMap $
  VoronoiGrid <$>
  traverseOf _1 fun mainpoint <*>
  traverseOf (each._1) fun otherpoints <*>
  pure neighbours

elements :: Traversal' (VoronoiGrid a) a
elements fun (VoronoiGrid mainpoint otherpoints neighbours) =
  VoronoiGrid <$>
  traverseOf _2 fun mainpoint <*>
  traverseOf (each._2) fun otherpoints <*>
  pure neighbours

regularGrid :: V2 Double -> Int -> Int -> a -> VoronoiGrid a
regularGrid (V2 dx dy) num_points_x num_points_y element =
  recomputeNeighbourMap
  VoronoiGrid
  { mainPoint   = fromJust $ head points
  , otherPoints = V.fromList $ tail points
  , neighbourMap = IM.empty }
 where
  points = do
    x <- [0..num_points_x-1]
    y <- [0..num_points_y-1]
    pure (V2 (dx*fromIntegral x) (dy*fromIntegral y), element)

perturbGrid :: VoronoiGrid a -> VoronoiGrid a
perturbGrid grid =
  runST $
  runWithRandomSupply 4000 $
  traverseOf points ((<$> randomV2Spherical 1.0) . (+)) grid

elementize :: [a] -> VoronoiGrid a -> VoronoiGrid a
elementize possible_elements grid =
  runST $
  runWithRandomSupply 4000 $
  traverseOf elements randomizer grid
 where
  vec_possible_elements = V.fromList possible_elements
  randomizer _ = randomOf vec_possible_elements

pointAt'' :: forall a. V2 Double -> VoronoiGrid a -> (V2 Double, a, Int)
pointAt'' point grid =
  fst $
  foldr
    folder
    ((mainpos, mainelem, 0), distance point (fst $ mainPoint grid))
    (zip [1..] $ V.toList $ otherPoints grid)
 where
  (mainpos, mainelem) = mainPoint grid

  folder :: (Int, (V2 Double, a)) -> ((V2 Double, a, Int), Double) -> ((V2 Double, a, Int), Double)
  folder (oidx, (otherpoint, otherelem)) o@(_, mdist) =
    let dist = distance otherpoint point
     in if dist <= mdist
          then ((otherpoint, otherelem, oidx), dist)
          else o
{-# INLINE pointAt'' #-}

pointAt' :: V2 Double -> VoronoiGrid a -> (V2 Double, a)
pointAt' point grid =
  let (pos, elem, _) = pointAt'' point grid
   in (pos, elem)
{-# INLINE pointAt' #-}

pointAt :: V2 Double -> VoronoiGrid a -> a
pointAt point grid = snd $ pointAt' point grid
{-# INLINE pointAt #-}

determinant2 :: V2 Double -> V2 Double -> Double
determinant2 (V2 a b) (V2 c d) = a*d - b*c
{-# INLINE determinant2 #-}

determinant3 :: V3 Double -> V3 Double -> V3 Double -> Double
determinant3 (V3 a b c) (V3 d e f) (V3 g h i) =
  a * determinant2 (V2 e f) (V2 h i) -
  b * determinant2 (V2 d f) (V2 g i) +
  c * determinant2 (V2 d e) (V2 g h)
{-# INLINE determinant3 #-}

data Winding
  = Clockwise
  | CounterClockwise
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

winding :: V2 Double -> V2 Double -> V2 Double -> Winding
winding (V2 ax ay) (V2 bx by) (V2 cx cy) =
  if cross (V3 ax ay 0 - V3 bx by 0)
           (V3 cx cy 0 - V3 ax ay 0) >= 0
    then Clockwise
    else CounterClockwise

isDinABC :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Bool
isDinABC a@(V2 ax ay)
         b@(V2 bx by)
         c@(V2 cx cy)
           (V2 dx dy) =
  let d = determinant3 (V3 (ax-dx) (ay-dy) (((ax-dx)**2)+((ay-dy)**2)))
                       (V3 (bx-dx) (by-dy) (((bx-dx)**2)+((by-dy)**2)))
                       (V3 (cx-dx) (cy-dy) (((cx-dx)**2)+((cy-dy)**2)))

   in case winding a b c of
        CounterClockwise -> d > 0
        Clockwise -> d <= 0

combineNeighbours :: forall a. ([a] -> a -> a) -> VoronoiGrid a -> VoronoiGrid a
combineNeighbours modifier =
  combineNeighbours' (\lst elem _ -> modifier lst elem)

combineNeighbours'' :: forall f a. Applicative f => ([a] -> a -> V2 Double -> f a) -> VoronoiGrid a -> f (VoronoiGrid a)
combineNeighbours'' modifier grid@(VoronoiGrid _main _other neighbours) =
  let new_main_elem = walk 0
      other_elems = traverse walk [1..V.length (otherPoints grid)]

   in VoronoiGrid <$>
      ((\x -> mainPoint grid & _2 .~ x) <$> new_main_elem) <*>
      ((V.zip (V.map fst (otherPoints grid)) . V.fromList) <$> other_elems) <*>
      pure neighbours
 where
  walk :: Int -> f a
  walk idx =
    let isset = fromMaybe IS.empty $ IM.lookup idx (neighbourMap grid)
        nelems = IS.toList isset <&> \tgt_idx ->
                   if tgt_idx == 0
                     then snd (mainPoint grid)
                     else snd (otherPoints grid V.! (tgt_idx-1))

     in modifier nelems
        (if idx == 0
           then snd $ mainPoint grid
           else snd $ otherPoints grid V.! (idx-1))
        (if idx == 0
           then fst $ mainPoint grid
           else fst $ otherPoints grid V.! (idx-1))

combineNeighbours' :: forall a. ([a] -> a -> V2 Double -> a) -> VoronoiGrid a -> VoronoiGrid a
combineNeighbours' modifier grid = runIdentity $
  combineNeighbours'' (\x y z -> pure $ modifier x y z) grid

neighbourElements :: V2 Double -> VoronoiGrid a -> [a]
neighbourElements point grid =
  let (_pos, _elem, idx) = pointAt'' point grid

   in case IM.lookup idx (neighbourMap grid) of
        Nothing -> []
        Just neighbours -> IS.toList neighbours <&> \case
          0 -> snd (mainPoint grid)
          _ -> snd (otherPoints grid V.! (idx-1))

