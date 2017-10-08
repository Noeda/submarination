module Submarination.Tension
  ( pullTension
  , tests )
  where

import Data.Maybe
import Linear.V2
import Protolude
import Test.Framework
import Test.Framework.Providers.QuickCheck2

isConnected :: V2 Int -> V2 Int -> Bool
isConnected (V2 x1 y1) (V2 x2 y2) =
  abs (x1-x2) <= 1 && abs (y1-y2) <= 1

moveTowardsUntil :: (V2 Int -> Bool) -> V2 Int -> (V2 Int -> V2 Int -> Bool) -> V2 Int -> V2 Int -> Bool -> Maybe (V2 Int)
moveTowardsUntil _ _ test subject target _ | test subject target = Just subject
moveTowardsUntil obstacles first_candidate test subject@(V2 x1 y1) target@(V2 x2 y2) straight_pull =
  if straight_pull
    then same_dir
    else diagonal
 where
  diagonal =
    -- Try diagonal moving
    let x3 = if | x1 < x2   -> x1+1
                | x1 > x2   -> x1-1
                | otherwise -> x1

        y3 = if | y1 < y2   -> y1+1
                | y1 > y2   -> y1-1
                | otherwise -> y1

     in if not (obstacles (V2 x3 y3))
          then moveTowardsUntil obstacles first_candidate test (V2 x3 y3) target straight_pull
          else xMove

  xMove =
    let x3 = if | x1 < x2   -> x1+1
                | x1 > x2   -> x1-1
                | otherwise -> x1

     in if x3 /= x1 && not (obstacles (V2 x3 y1))
          then moveTowardsUntil obstacles first_candidate test (V2 x3 y1) target straight_pull
          else yMove

  yMove =
    let y3 = if | y1 < y2   -> y1+1
                | y1 > y2   -> y1-1
                | otherwise -> y1

     in if y3 /= y1 && not (obstacles (V2 x1 y3))
          then moveTowardsUntil obstacles first_candidate test (V2 x1 y3) target straight_pull
          else (if straight_pull
                  then Nothing
                  else same_dir)

  same_dir
    | test first_candidate target && test subject first_candidate =
        Just first_candidate
    | straight_pull =
        diagonal
    | otherwise =
        Nothing

pullTension :: (V2 Int -> Bool) -> V2 Int -> [V2 Int] -> Bool -> Maybe [V2 Int]
pullTension _ _ [] _ = Just []
pullTension obstacles target (f:rest) pull_straight =
  (target:) <$> connect f target rest
 where
  connect _first _target [] = Just []
  connect _first target (x:rest) | isConnected target x = Just (x:rest)
  connect first target (x:rest) =
    moveTowardsUntil obstacles first isConnected x target pull_straight >>= \x' ->
      (x':) <$> connect x x' rest

tests :: [Test]
tests = [testProperty "tension pulling removes one item from positions" testTensionPullingListLength
        ,testProperty "tension pulling smoke test 1" testTensionPullingSmokeTest1
        ,testProperty "tension pulling smoke test 2" testTensionPullingSmokeTest2
        ,testProperty "tension pulling smoke test 3" testTensionPullingSmokeTest3
        ,testProperty "tension pulling smoke test 4" testTensionPullingSmokeTest4
        ,testProperty "tension pulling smoke test 5" testTensionPullingSmokeTest5]

testTensionPullingListLength :: (Int, Int) -> [(Int, Int)] -> Bool
testTensionPullingListLength (fx, fy) lst' =
  let lst = fmap (uncurry V2) lst'
      first_item = V2 fx fy
   in length (fromJust $ pullTension (const False) first_item lst False) == length lst

testTensionPullingSmokeTest1 :: Bool
testTensionPullingSmokeTest1 =
  pullTension (const False) (V2 0 0) [V2 1 0] False == Just [V2 0 0]

testTensionPullingSmokeTest2 :: Bool
testTensionPullingSmokeTest2 =
  pullTension (const False) (V2 0 0) [V2 1 0, V2 2 0] False == Just [V2 0 0, V2 1 0]

testTensionPullingSmokeTest3 :: Bool
testTensionPullingSmokeTest3 =
  pullTension (const False) (V2 0 0) [V2 1 0, V2 1 1] False == Just [V2 0 0, V2 1 1]

testTensionPullingSmokeTest4 :: Bool
testTensionPullingSmokeTest4 =
  pullTension (const False) (V2 0 0) [V2 1 0, V2 0 0] False == Just [V2 0 0, V2 0 0]

testTensionPullingSmokeTest5 :: Bool
testTensionPullingSmokeTest5 =
  pullTension (const False) (V2 0 0) [V2 1 0, V2 1 1, V2 2 2] False == Just [V2 0 0, V2 1 1, V2 2 2]

