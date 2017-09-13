module Main ( main ) where

import Criterion.Main
import Protolude

import qualified Submarination.QuadTree as QuadTree

benchmarks = QuadTree.benchmarks

main :: IO ()
main = defaultMain benchmarks

