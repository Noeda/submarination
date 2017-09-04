module Main ( main ) where

import qualified Submarination.GameState as GameState
import qualified Submarination.Item as Item
import Test.Framework
import Protolude

tests = Item.tests <> GameState.tests

main :: IO ()
main = defaultMain tests

