{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Creature
  ( Creature(..)
  , foodVendor
  , ammoVendor
  , toolVendor
  , materialVendor
  , isAnimal )
  where

import Data.Binary
import Data.Data
import Protolude

data Creature
  = FoodVendor
  | AmmoVendor
  | ToolVendor
  | MaterialVendor
  | Snoatfish
  | Biddy
  | Enneapus
  | Camobream
  | Gator
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

isAnimal :: Creature -> Bool
isAnimal Snoatfish = True
isAnimal Biddy = True
isAnimal Enneapus = True
isAnimal Camobream = True
isAnimal Gator = True
isAnimal _ = False

foodVendor :: Creature
foodVendor = FoodVendor

ammoVendor :: Creature
ammoVendor = AmmoVendor

toolVendor :: Creature
toolVendor = ToolVendor

materialVendor :: Creature
materialVendor = MaterialVendor

