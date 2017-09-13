module Main ( main ) where

import qualified Submarination.GameState as GameState
import qualified Submarination.Item as Item
import qualified Submarination.QuadTree as QuadTree
import qualified Submarination.Tension as Tension

import Test.Framework
import Protolude

tests = QuadTree.tests <> Item.tests <> GameState.tests <> Tension.tests

main :: IO ()
main = defaultMain tests

