{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module implements a quadtree.
--
-- It's very similar to Map (V2 Int) a (in fact we provide an Iso for that)
-- but it supports efficient ways to get closest neighbours to point X.
module Submarination.QuadTree
  ( IntQuadTree()
  , lookup
  , insert
  , insertWith
  , delete
  , empty
  , fromList
  , toList
  , keys
  , findClosestNeighbour
  , fold
  , size
  , depth
  , mapKeys
  , filterWithKey
  , pegToPowerOfTwo
  , showInternal
  , showPretty

  , tests
  , benchmarks )
  where

import Control.Arrow
import Control.Exception
import Control.DeepSeq
import Control.Lens hiding ( Empty )
import Criterion
import Data.Binary
import Data.Data
import Data.List ( nub, (!!) )
import qualified Data.Set as S
import Linear.V2
import Prelude ( showsPrec, showParen, showString, shows, String )
import Protolude hiding ( empty, toList, (&), first )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Text.Read
import System.IO.Unsafe

import Submarination.Random

-- Some of the tests trigger this hlint thing
--{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | Mapping from 2d positions to values.
--
-- IntQuadTree v is isometric to Map (V2 Int) v
--
-- Spine of the tree is strict but values are not.
data IntQuadTree v
  = Empty
  | Leaf {-# UNPACK #-} !(V2 Int) v
  | Node {-# UNPACK #-} !(V2 Int)
         {-# UNPACK #-} !Int
                        !(IntQuadTree v)
                        !(IntQuadTree v)
                        !(IntQuadTree v)
                        !(IntQuadTree v)
  deriving ( Typeable, Data, Generic, Functor, Foldable, Traversable, NFData, Binary )

instance Monoid v => Monoid (IntQuadTree v) where
  mempty = Empty

  Empty `mappend` x = x
  x `mappend` Empty = x

  node1 `mappend` node2 =
    if size node1 > size node2
      then foldr folder node1 (toList node2)
      else foldr folder node2 (toList node1)
   where
    folder (key, value) = insert key value

instance Monoid v => Semigroup (IntQuadTree v) where
  node1 <> node2 = node1 `mappend` node2

-- | Shows the internal structure of a quadtree (ignoring values) in multi-line
-- string that's pretty to print out.
showPretty :: IntQuadTree v -> String
showPretty structure = go structure 0
 where
  go Empty _ = "Empty\n"
  go (Leaf (V2 lx ly) _) _ = "Leaf " <> show lx <> " " <> show ly <> "\n"
  go (Node (V2 ox oy) sidelength node1 node2 node3 node4) indentation =
    "Node (origin=" <> show ox <> ", " <> show oy <> ") (sidelength=" <> show sidelength <> ")\n" <>
    replicate (indentation+2) ' ' <> go node1 (indentation+2) <>
    replicate (indentation+2) ' ' <> go node2 (indentation+2) <>
    replicate (indentation+2) ' ' <> go node3 (indentation+2) <>
    replicate (indentation+2) ' ' <> go node4 (indentation+2)

-- | Shows the internal structure of a quad tree.
showInternal :: Show v => IntQuadTree v -> String
showInternal Empty = "Empty"
showInternal (Leaf leafpos leafvalue) = "Leaf (" <> show leafpos <> ") (" <> show leafvalue <> ")"
showInternal (Node origin side_length q1 q2 q3 q4) =
  "Node (" <> show origin <> ") " <> show side_length <> " (" <> showInternal q1 <> ") (" <> showInternal q2 <> ") (" <> showInternal q3 <> ") (" <> showInternal q4 <> ")"

-- | How deeply is the quadtree subdivided at maximum depth?
--
-- Minimum is 1.
--
-- Mostly useful for debugging.
depth :: IntQuadTree v -> Int
depth Empty = 0
depth Leaf{} = 0
depth (Node _ _ q1 q2 q3 q4) =
  1 + max (depth q1) (max (depth q2) (max (depth q3) (depth q4)))
{-# INLINEABLE depth #-}

instance Eq v => Eq (IntQuadTree v) where
  -- can't use auto-derived eq because quadtrees may have different spine and
  -- still be valid. Currently I use this naive method.
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  Leaf leafpos1 leafvalue1 == Leaf leafpos2 leafvalue2 = leafpos1 == leafpos2 && leafvalue1 == leafvalue2
  Leaf{} == _ = False
  _ == Leaf{} = False

  node1 == node2 = sortBy (comparing fst) (toList node1) ==
                   sortBy (comparing fst) (toList node2)
  {-# INLINEABLE (==) #-}

instance Ord v => Ord (IntQuadTree v) where
  node1 `compare` node2 = sort (toList node1) `compare` sort (toList node2)
  {-# INLINEABLE compare #-}

instance Show v => Show (IntQuadTree v) where
  showsPrec d quadtree =
    showParen (d > 10) $
      showString "fromList " . shows (sortBy (comparing fst) $ toList quadtree)

instance Read v => Read (IntQuadTree v) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return $ fromList xs

-- | Returns the empty quadtree
empty :: IntQuadTree v
empty = Empty
{-# INLINE empty #-}

--
--  q1 | q2
-- ----+-----
--  q3 | q4
--

-- | Looks up a point by its exact coordinates.
lookup :: V2 Int -> IntQuadTree v -> Maybe v
lookup _pos Empty = Nothing
lookup pos1 (Leaf pos2 v) = if pos1 == pos2 then Just v else Nothing
lookup pos@(V2 px py) (Node origin@(V2 ox oy) side_length q1 q2 q3 q4) =
  if px >= ox && py >= oy &&
     px < ox+side_length && py < oy+side_length
    then lookup' (pos - origin)
    else Nothing
 where
  side_length_half = side_length `div` 2

  lookup' (V2 x y)
    | x < side_length_half =
      if y < side_length_half
        then lookup pos q1
        else lookup pos q3
    | y < side_length_half =
      lookup pos q2
    | otherwise =
      lookup pos q4
{-# INLINE lookup #-}

mapKeys :: (V2 Int -> V2 Int) -> IntQuadTree v -> IntQuadTree v
mapKeys modder quadtree =
  let lst = sortBy (comparing fst) $ toList quadtree
      lst_modded = fmap (first modder) lst

   in fromList lst_modded
{-# INLINEABLE mapKeys #-}

filterWithKey :: (V2 Int -> v -> Bool) -> IntQuadTree v -> IntQuadTree v
filterWithKey _fun Empty = Empty
filterWithKey fun leaf@(Leaf leafpos leafvalue) =
  if fun leafpos leafvalue
    then leaf
    else Empty
filterWithKey fun (Node origin side_length q1 q2 q3 q4) =
  collapse $
  Node origin side_length
    (filterWithKey fun q1)
    (filterWithKey fun q2)
    (filterWithKey fun q3)
    (filterWithKey fun q4)
{-# INLINEABLE filterWithKey #-}

pegToPowerOfTwo :: Int -> Int
pegToPowerOfTwo 0 = 1
pegToPowerOfTwo 1 = 2
pegToPowerOfTwo x = 2^(finiteBitSize x - countLeadingZeros x)
{-# INLINE pegToPowerOfTwo #-}

-- | Finds the closest neighbours to a point, not including point itself
--
-- The distance metric is manhattan.
--
-- If there are more than one point with equal distance, they are all returned
-- in the list.
findClosestNeighbour :: V2 Int -> IntQuadTree v -> [(V2 Int, v)]
findClosestNeighbour _pos Empty = []
findClosestNeighbour pos (Leaf leafpos leafvalue)
  | leafpos == pos = []
  | otherwise = [(leafpos, leafvalue)]
findClosestNeighbour pos node =
  let (_max_distance, candidates) = findClosestNeighbourRaw pos node maxBound
   in candidates

-- used just to benchmark against naive lists
findClosestNeighbourLst :: forall v. V2 Int -> [(V2 Int, v)] -> [(V2 Int, v)]
findClosestNeighbourLst pos lst = snd $ go lst maxBound
 where
  go :: [(V2 Int, v)] -> Int -> (Int, [(V2 Int, v)])
  go [] max_distance = (max_distance, [])
  go ((candpos, value):rest) max_distance | ndist <- manhattan pos candpos, ndist <= max_distance, candpos /= pos =
    let (rest_max_distance, rest_candidates) = go rest ndist

     in if rest_max_distance < ndist
          then (rest_max_distance, rest_candidates)
          else (ndist, (candpos, value):rest_candidates)

  go (_:rest) max_distance = go rest max_distance

findClosestNeighbourRaw :: V2 Int -> IntQuadTree v -> Int -> (Int, [(V2 Int, v)])
findClosestNeighbourRaw pos node max_distance = case node of
  Empty -> (max_distance, [])
  Leaf leafpos leafvalue | leafpos /= pos, ndist <- manhattan leafpos pos, ndist <= max_distance ->
    (ndist, [(leafpos, leafvalue)])
  Leaf{} -> (max_distance, [])

  Node origin side_length node1 node2 node3 node4 ->
    let side_length_half = side_length `div` 2
        dist_to_node1 = dist pos origin side_length_half
        dist_to_node2 = dist pos (origin + V2 side_length_half 0) side_length_half
        dist_to_node3 = dist pos (origin + V2 0 side_length_half) side_length_half
        dist_to_node4 = dist pos (origin + V2 side_length_half side_length_half) side_length_half

        -- We test quadrants that seem closest to requested position
        -- But we don't test any that are further than current maximum distance
        slist = sortBy (comparing fst) $
                filter (\(dist, _) -> dist <= max_distance)
                [(dist_to_node1, node1)
                ,(dist_to_node2, node2)
                ,(dist_to_node3, node3)
                ,(dist_to_node4, node4)]

     in case slist of
          []  -> (max_distance, [])
          lst -> go lst max_distance
 where
  dist (V2 px py) (V2 ox oy) side_length =
    -- If position is inside the quadrant, then distance is 0
    if | px >= ox && py >= oy && px < ox+side_length && py < oy+side_length
         -> 0
       | otherwise
         -> let travel_x
                  | px < ox = ox - px
                  | px >= ox+side_length = px-(ox+side_length)+1
                  | otherwise = 0

                travel_y
                  | py < oy = oy - py
                  | py >= oy+side_length = py-(oy+side_length)+1 
                  | otherwise = 0

             in max travel_x travel_y

  go [] max_distance = (max_distance, [])
  go ((dist, node):rest) max_distance | dist <= max_distance =
    let (new_max_distance, candidates) = findClosestNeighbourRaw pos node max_distance
        (rest_max_distance, rest_candidates) = go rest new_max_distance

     in if rest_max_distance < new_max_distance
          then (rest_max_distance, rest_candidates)
          else (new_max_distance, rest_candidates <> candidates)

  go (_:rest) max_distance = go rest max_distance
{-# INLINEABLE findClosestNeighbourRaw #-}

manhattan :: V2 Int -> V2 Int -> Int
manhattan (V2 x1 y1) (V2 x2 y2) =
  max (abs (x1-x2)) (abs (y1-y2))
{-# INLINE manhattan #-}

size :: IntQuadTree v -> Int
size Empty = 0
size Leaf{} = 1
size (Node _ _ q1 q2 q3 q4) =
  size q1 + size q2 + size q3 + size q4
{-# INLINE size #-}

insert :: V2 Int -> v -> IntQuadTree v -> IntQuadTree v
insert pos value =
  insertWith pos (const value) value
{-# INLINE insert #-}

posLens :: V2 Int -> Lens' (IntQuadTree v) (Maybe v)
posLens pos = lens get_it set_it
 where
  get_it = lookup pos

  set_it qt Nothing = delete pos qt
  set_it qt (Just v) = insert pos v qt
{-# INLINE posLens #-}

type instance Index (IntQuadTree v) = V2 Int
type instance IxValue (IntQuadTree v) = v

instance Ixed (IntQuadTree v) where
  ix pos = posLens pos._Just
  {-# INLINE ix #-}

instance At (IntQuadTree v) where
  at = posLens
  {-# INLINE at #-}

instance FoldableWithIndex (V2 Int) IntQuadTree where
  ifoldMap _fun Empty = mempty
  ifoldMap fun (Leaf leafpos leafvalue) = fun leafpos leafvalue
  ifoldMap fun (Node _ _ q1 q2 q3 q4) =
    ifoldMap fun q1 `mappend`
    (ifoldMap fun q2 `mappend`
     (ifoldMap fun q3 `mappend`
      ifoldMap fun q4))
  {-# INLINE ifoldMap #-}

-- | Inserts a new point to quadtree. If there is already a point in the exact
-- coordinates given, it'll be replaced with the given function.
insertWith :: V2 Int -> (v -> v) -> v -> IntQuadTree v -> IntQuadTree v
insertWith pos _valuemod valuedef Empty = Leaf pos valuedef
insertWith pos@(V2 px py) valuemod valuedef leaf@(Leaf leafpos@(V2 lx ly) old_value)
  | leafpos == pos = Leaf pos (valuemod old_value)

  -- Case: We are inserting a point to a quadtree that only has a leaf.
  | otherwise =
    let side_length = pegToPowerOfTwo $ max (abs (lx-px)) (abs (ly-py))
        origin = V2 (min px lx) (min py ly)
        smap = selectQuadrant pos
                 (Node origin side_length Empty Empty Empty Empty)
                 (const $ Leaf pos valuedef)
        rmap = insertRaw leafpos (const old_value) old_value smap origin side_length leaf

     in collapse rmap

insertWith pos@(V2 px py) valuemod valuedef node@(Node origin@(V2 ox oy) side_length node1 node2 node3 node4)
  | px >= ox && px < ox+side_length && py >= oy && py < oy+side_length = collapse $
    if x < side_length_half
      then (if y < side_length_half
              then Node origin side_length (insertRaw pos valuemod valuedef node1 origin side_length_half node) node2 node3 node4
              else Node origin side_length node1 node2 (insertRaw pos valuemod valuedef node3 (V2 ox (oy+side_length_half)) side_length_half node) node4)
      else (if y < side_length_half
              then Node origin side_length node1 (insertRaw pos valuemod valuedef node2 (V2 (ox+side_length_half) oy) side_length_half node) node3 node4
              else Node origin side_length node1 node2 node3 (insertRaw pos valuemod valuedef node4 (V2 (ox+side_length_half) (oy+side_length_half)) side_length_half node))

  | otherwise = collapse $
      -- Position inserted outside quadtree; grow it
      let new_side_length = side_length*2
       in (if px < ox
             then (if py < oy
                     then insertWith pos valuemod valuedef (Node (origin - V2 side_length side_length) new_side_length Empty Empty Empty node')
                     else insertWith pos valuemod valuedef (Node (origin - V2 side_length 0) new_side_length Empty node' Empty Empty))
             else (if py < oy
                     then insertWith pos valuemod valuedef (Node (origin - V2 0 side_length) new_side_length Empty Empty node' Empty)
                     else insertWith pos valuemod valuedef (Node origin new_side_length node' Empty Empty Empty)))
 where
  node' = collapse node
  V2 x y = pos - origin
  side_length_half = side_length `div` 2
{-# INLINEABLE insertWith #-}

hasMisAlignedNode :: IntQuadTree v -> Maybe Text
hasMisAlignedNode Empty = Nothing
hasMisAlignedNode Leaf{} = Nothing
hasMisAlignedNode (Node origin sidelength q1 q2 q3 q4) =
  go q1 origin (sidelength `div` 2) <|>
  go q2 (origin + V2 sidelength_half 0) (sidelength `div` 2) <|>
  go q3 (origin + V2 0 sidelength_half) (sidelength `div` 2) <|>
  go q4 (origin + V2 sidelength_half sidelength_half) (sidelength `div` 2)
 where
  sidelength_half = sidelength `div` 2

  go Empty _ _ = Nothing
  go (Leaf (V2 lx ly) _) (V2 ox oy) sidelength =
    if lx >= ox && ly >= oy &&
       lx < ox + sidelength &&
       ly < oy + sidelength
      then Nothing
      else Just $ "Leaf " <> show (V2 lx ly) <> " not inside " <> show (V2 ox oy, sidelength)
  go (Node norigin nsidelength q1 q2 q3 q4) origin sidelength =
    (if norigin /= origin
       then Just $ "Origin " <> show norigin <> " not where expected: " <> show (origin, sidelength)
       else Nothing) <|>
    (if nsidelength /= sidelength
      then Just $ "Side length " <> show nsidelength <> " not what expected: " <> show sidelength
      else Nothing) <|>
        go q1 origin (sidelength `div` 2) <|>
        go q2 (origin + V2 sidelength_half 0) (sidelength `div` 2) <|>
        go q3 (origin + V2 0 sidelength_half) (sidelength `div` 2) <|>
        go q4 (origin + V2 sidelength_half sidelength_half) (sidelength `div` 2)
   where
    sidelength_half = sidelength `div` 2

hasEmptyNode :: IntQuadTree v -> Bool
hasEmptyNode Empty = False
hasEmptyNode Leaf{} = False
hasEmptyNode (Node _ _ Empty Empty Empty Empty) = True
hasEmptyNode (Node _ _ q1 q2 q3 q4) =
  hasEmptyNode q1 ||
  hasEmptyNode q2 ||
  hasEmptyNode q3 ||
  hasEmptyNode q4

insertRaw :: V2 Int -> (v -> v) -> v -> IntQuadTree v -> V2 Int -> Int -> IntQuadTree v -> IntQuadTree v
insertRaw pos _ valuedef Empty _ _ _ = Leaf pos valuedef
insertRaw pos valuemod valuedef (Leaf leafpos leafvalue) origin side_length original
  | leafpos == pos = Leaf pos (valuemod leafvalue)
  | otherwise =
      let s = selectQuadrant pos
               (Node origin side_length Empty Empty Empty Empty)
               (const $ Leaf pos valuedef)
          r = collapse $ insertRaw leafpos (const leafvalue) leafvalue s origin side_length original

       in r

insertRaw pos@(V2 px py) valuemod valuedef (Node origin@(V2 ox oy) side_length node1 node2 node3 node4) _origin _side_length original | px >= ox && py >= oy && px < ox+side_length && py < oy+side_length = collapse $
  if x < side_length_half
    then (if y < side_length_half
            then Node origin side_length (insertRaw pos valuemod valuedef node1 origin side_length_half original) node2 node3 node4
            else Node origin side_length node1 node2 (insertRaw pos valuemod valuedef node3 (V2 ox (oy+side_length_half)) side_length_half original) node4)
    else (if y < side_length_half
            then Node origin side_length node1 (insertRaw pos valuemod valuedef node2 (V2 (ox+side_length_half) oy) side_length_half original) node3 node4
            else Node origin side_length node1 node2 node3 (insertRaw pos valuemod valuedef node4 (V2 (ox+side_length_half) (oy+side_length_half)) side_length_half original))
 where
  V2 x y = pos - origin
  side_length_half = side_length `div` 2

insertRaw pos valuemod valuedef node@(Node node_origin@(V2 ox oy) node_side_length node1 node2 node3 node4) origin side_length original =

  let s = selectQuadrant node_origin
            (Node origin side_length Empty Empty Empty Empty)
            (const node)
      r = insertRaw pos valuemod valuedef s origin side_length original

   in collapse r
{-# INLINEABLE insertRaw #-}

-- Utility function that drills down to one quadrant in a node.
-- ignores leaves
selectQuadrant :: V2 Int -> IntQuadTree v -> (IntQuadTree v -> IntQuadTree v) -> IntQuadTree v
selectQuadrant pos@(V2 px py) node@(Node origin@(V2 ox oy) side_length node1 node2 node3 node4) fun
  | px >= ox && py >= oy && px < ox+side_length && py < oy+side_length =
    if x < side_length_half
      then (if y < side_length_half
              then Node origin side_length (fun node1) node2 node3 node4
              else Node origin side_length node1 node2 (fun node3) node4)
      else (if y < side_length_half
              then Node origin side_length node1 (fun node2) node3 node4
              else Node origin side_length node1 node2 node3 (fun node4))
  | otherwise = node
 where
  V2 x y = pos - origin
  side_length_half = side_length `div` 2
selectQuadrant _ node _ = node
{-# INLINE selectQuadrant #-}

collapse :: IntQuadTree v -> IntQuadTree v
collapse node@(Node _ _ q1 q2 q3 q4) =
  let num_empties = countEmpty q1 +
                    countEmpty q2 +
                    countEmpty q3 +
                    countEmpty q4

   in if | num_empties == 3
           -> (if | isNonEmpty q1 -> q1
                  | isNonEmpty q2 -> q2
                  | isNonEmpty q3 -> q3
                  | isNonEmpty q4 -> q4
                  | otherwise -> node)
         | num_empties == 4
           -> Empty
         | otherwise
           -> node
 where
  isNonEmpty Empty = False
  isNonEmpty _ = True
collapse x = x

leafify :: IntQuadTree v -> IntQuadTree v
leafify node@(Node _ _ q1 q2 q3 q4) =
  let num_empties = countEmpty q1 +
                    countEmpty q2 +
                    countEmpty q3 +
                    countEmpty q4


   in if num_empties == 3
        then (if | isLeaf q1 -> q1
                 | isLeaf q2 -> q2
                 | isLeaf q3 -> q3
                 | isLeaf q4 -> q4
                 | otherwise -> node)
        else node
 where
  isLeaf Leaf{} = True
  isLeaf _ = False
leafify node = node

countEmpty :: IntQuadTree v -> Int
countEmpty Empty = 1 :: Int
countEmpty _ = 0
{-# INLINE countEmpty #-}

delete :: V2 Int -> IntQuadTree v -> IntQuadTree v
delete _pos Empty = Empty
delete pos leaf@(Leaf leafpos _leafvalue)
  | pos == leafpos = Empty
  | otherwise      = leaf
delete pos node =
  let new_node = selectQuadrant pos node $ delete pos
   in collapse new_node
{-# INLINEABLE delete #-}

-- | Returns the keys in a quad tree. They are not guaranteed to be in any
-- particular order.
keys :: IntQuadTree v -> [V2 Int]
keys Empty = []
keys (Leaf leafpos _) = [leafpos]
keys (Node _ _ q1 q2 q3 q4) =
  keys q1 <> (keys q2 <> (keys q3 <> keys q4))
{-# INLINEABLE keys #-}

tests :: [Test]
tests =
  [testGroup "QuadTree"
    [testProperty "pegToPowerOfTwo makes sensible results" testPegToPowerOfTwo
        ,testProperty "insert 1, you'll get it back" testInsertOne
        ,testProperty "insert 2, you'll get them back" testInsertTwo
        ,testProperty "insert bazillion, you'll get them back" testBazillion
        ,testProperty "insert and then delete, you won't get anything" testBazillionDelete
        ,testProperty "inserting in different order doesn't change anything" testDifferentOrder
        ,testProperty "inserting in different order doesn't affect set of keys" testDifferentOrderKeys
        ,testProperty "delete delets at most 1 point" testDeleteDeletesOne
        ,testProperty "deleting and reinserting points works" testDeleteAndReinsert
        ,testProperty "inserting and moving all points one by one results in same set of values" testMoves
        ,testProperty "show + read is identity" testShowRead
        ,testProperty "fromList . toList is identity" testFromListToList
        ,testProperty "findClosestNeighbour is equivalent to naive version" testClosestNeighbourAgainstNaive
        ,testProperty "monoid makes sense" testMonoid
        ,testProperty "deleting most of the map shrinks the quadtree properly" testScarcity
        ,testProperty "delete works" testDelete
        ,testProperty "test Data.Binary serialization" testBinary
        ,testProperty "no empty nodes" testNoEmptyNodes]]

fromList :: [(V2 Int, v)] -> IntQuadTree v
fromList = foldr folder empty
 where
  folder (key, value) = insert key value
{-# INLINEABLE fromList #-}

toList :: IntQuadTree v -> [(V2 Int, v)]
toList Empty = []
toList (Leaf leafpos leafvalue) = [(leafpos, leafvalue)]
toList (Node _ _ q1 q2 q3 q4) =
  toList q1 <> (toList q2 <> (toList q3 <> toList q4))
{-# INLINEABLE toList #-}

testPegToPowerOfTwo :: Int -> Property
testPegToPowerOfTwo x = x >= 1 ==> pegToPowerOfTwo x >= x

testInsertOne :: (Int, Int) -> Bool
testInsertOne (x, y) =
  let pos = V2 x y
      map = insert pos () empty

   in lookup pos map == Just ()

testInsertTwo :: (Int, Int) -> (Int, Int) -> Property
testInsertTwo pos1@(x1, y1) pos2@(x2, y2) =
  pos1 /= pos2 ==>
    let pos1' = V2 x1 y1
        pos2' = V2 x2 y2

        map1 = insert pos1' 'a' empty
        map = insert pos2' 'b' map1

     in lookup pos1' map == Just 'a' &&
        lookup pos2' map == Just 'b'

testMoves :: [(Int, Int)] -> Bool
testMoves lst =
  let map = foldr folder empty (zip [0..] lst) :: IntQuadTree Int
      folder (idx, (x, y)) = insert (V2 x y) (idx :: Int)

      indexes = snd <$> toList map

      moved_map = go map (toList map)

      go map ((V2 lx ly, idx):rest) =
                    case lookup (V2 (lx+1) ly) map of
                      Nothing ->
                        go (insert (V2 (lx+1) ly) idx $
                            delete (V2 lx ly) map)
                        rest
                      Just{} -> go map rest
      go map [] = map

      new_indexes = snd <$> toList moved_map

   in S.fromList indexes == S.fromList new_indexes

insertBazillion :: [(Int, Int)] -> IntQuadTree Int
insertBazillion lst =
  let map = foldr folder empty (zip [0..] lst)
      folder (idx, (x, y)) = insert (V2 x y) (idx :: Int)

   in map

testScarcity :: [(Int, Int)] -> Int -> Int -> Property
testScarcity lst (abs -> x) (abs -> y) = withMaxSuccess 2000 $
  nub lst == lst && length lst > 2 && x < length lst && y < length lst && x /= y ==>

    let map = insertBazillion lst
        smap = foldr folder map (zip [0..] lst)

        folder (idx, (px, py)) map =
          if idx == x || idx == y
            then map
            else delete (V2 px py) map

     in depth smap == 1 && size smap == 2

testBazillion :: [(Int, Int)] -> Property
testBazillion lst =
  nub lst == lst ==>

    let map = insertBazillion lst

        folder_check _ False = False
        folder_check (idx, (x, y)) True =
          lookup (V2 x y) map == Just idx

     in foldr folder_check True (zip [0..] lst)

testBazillionDelete :: [(Int, Int)] -> Bool
testBazillionDelete lst =
  let map = insertBazillion lst
      dmap = foldr folder map lst

      folder (x, y) = delete (V2 x y)

   in dmap == Empty

testDeleteAndReinsert :: [(Int, Int)] -> Property
testDeleteAndReinsert lst = not (null lst) ==>
  let map = insertBazillion lst
      imap = foldr folder map (toList map)

      folder (loc, val) map =
        insert loc val $
        delete loc map

   in imap == map

testDeleteDeletesOne :: [(Int, Int)] -> Int -> Property
testDeleteDeletesOne lst idx' = not (null lst) ==>
  let map = insertBazillion lst
      lmap = toList map
      idx = abs idx' `mod` length lmap
      dcoords = fst $ lmap !! idx

   in size (delete dcoords map) == size map - 1

testDifferentOrder :: [(Int, Int)] -> Bool
testDifferentOrder lst1 =
  let lst2 = reverse lst1
      lst3 = sort lst2
      lst4 = reverse lst3

      map1 = void $ insertBazillion lst1
      map2 = void $ insertBazillion lst2
      map3 = void $ insertBazillion lst3
      map4 = void $ insertBazillion lst4

   in map1 == map2 &&
      map1 == map3 &&
      map1 == map4

testDifferentOrderKeys :: [(Int, Int)] -> Bool
testDifferentOrderKeys lst1 =
  let lst2 = reverse lst1
      lst3 = sort lst2
      lst4 = reverse lst3

      map1 = void $ insertBazillion lst1
      map2 = void $ insertBazillion lst2
      map3 = void $ insertBazillion lst3
      map4 = void $ insertBazillion lst4

      keys1 = sort $ keys map1
      keys2 = sort $ keys map2
      keys3 = sort $ keys map3
      keys4 = sort $ keys map4

   in keys1 == keys2 &&
      keys1 == keys3 &&
      keys1 == keys4

testShowRead :: [(Int, Int)] -> Bool
testShowRead lst1 =
  let map = insertBazillion lst1
      smap = show map
      rmap = read smap

   in rmap == map

testFromListToList :: [(Int, Int)] -> Bool
testFromListToList lst1 =
  let map = insertBazillion lst1

   in fromList (toList map) == map

testClosestNeighbourAgainstNaive :: [(Int, Int)] -> (Int, Int) -> Bool
testClosestNeighbourAgainstNaive lst (tx, ty) =
  let map = insertBazillion lst
      testpos = V2 tx ty

      testlst = toList map

      closest_map = sort $ findClosestNeighbour testpos map
      closest_map_lst = sort $ findClosestNeighbourLst testpos testlst

   in closest_map == closest_map_lst

testMonoid :: [(Int, Int)] -> [(Int, Int)] -> Bool
testMonoid lst1 lst2 =
  let map1 = void $ insertBazillion lst1
      map2 = void $ insertBazillion lst2

      map3 = map1 `mappend` map2

   in sort (keys map3) == fmap (uncurry V2) (sort (nub $ lst1 <> lst2))

saturatedMap :: IntQuadTree Int
saturatedMap = fromList [ (V2 x y, x * y) | x <- [-50..50], y <- [-50..50] ]
{-# NOINLINE saturatedMap #-}

testNoEmptyNodes :: [(Int, Int)] -> Property
testNoEmptyNodes lst = withMaxSuccess 2000 $
  let map = insertBazillion lst
   in not (hasEmptyNode map)

testDelete :: (Int, Int) -> Property
testDelete (x, y) =
  x >= (-50) && y >= (-50) && x <= 50 && y <= 50 ==>

  let pos = V2 x y
      deleted_map = delete pos saturatedMap

   in size deleted_map == size saturatedMap-1 &&
      isNothing (lookup pos deleted_map) &&
      lookup pos saturatedMap == Just (x*y)

testBinary :: [(Int, Int)] -> Bool
testBinary lst =
  let map = insertBazillion lst
      serialized = encode map
      unserialized = decode serialized

  in unserialized == map

randomMap :: MonadRandomSupply m => Int -> m (IntQuadTree Int)
randomMap = go empty
 where
  go map 0 = return map
  go map n = do
    pos <- randomV2In (V2 (-200) (-200), V2 200 200)
    case lookup pos map of
      Nothing -> go (insert pos n map) (n-1)
      Just{} -> go map n

setupBenchmarkEnv :: IO [(IntQuadTree Int, [(V2 Int, Int)])]
setupBenchmarkEnv =
  runWithRandomSupply 22345 $ do
    map1 <- randomMap 1
    map2 <- randomMap 5
    map3 <- randomMap 100
    map4 <- randomMap 1000
    map5 <- randomMap 10000
    map6 <- randomMap 100000
    return [(map1, toList map1)
           ,(map2, toList map2)
           ,(map3, toList map3)
           ,(map4, toList map4)
           ,(map5, toList map5)
           ,(map6, toList map6)]

benchmarks :: [Benchmark]
benchmarks =
  [env setupBenchmarkEnv $ \ ~[(env1, lst1),
                               (env5, lst5),
                               (env100, lst100),
                               (env1000, lst1000),
                               (env10000, lst10000),
                               (env100000, lst100000)] -> bgroup "QuadTree"
    [bgroup "IntQuadTree"
      [bench "findClosestNeighbourIn1Points" $ whnf (findClosestNeighbour (V2 0 0)) env1
      ,bench "findClosestNeighbourIn5Points" $ whnf (findClosestNeighbour (V2 0 0)) env5
      ,bench "findClosestNeighbourIn100Points" $ whnf (findClosestNeighbour (V2 0 0)) env100
      ,bench "findClosestNeighbourIn1000Points" $ whnf (findClosestNeighbour (V2 0 0)) env1000
      ,bench "findClosestNeighbourIn10000Points" $ whnf (findClosestNeighbour (V2 0 0)) env10000
      ,bench "findClosestNeighbourIn100000Points" $ whnf (findClosestNeighbour (V2 0 0)) env100000]
    ,bgroup "NaiveList"
      [bench "findClosestNeighbourLstIn1Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst1
      ,bench "findClosestNeighbourLstIn5Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst5
      ,bench "findClosestNeighbourLstIn100Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst100
      ,bench "findClosestNeighbourLstIn1000Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst1000
      ,bench "findClosestNeighbourLstIn10000Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst10000
      ,bench "findClosestNeighbourLstIn100000Points" $ whnf (findClosestNeighbourLst (V2 0 0)) lst100000]]]

