{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Creature
  ( Creature(..)
  , foodVendor
  , ammoVendor
  , toolVendor
  , materialVendor )
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

foodVendor :: Creature
foodVendor = FoodVendor

ammoVendor :: Creature
ammoVendor = AmmoVendor

toolVendor :: Creature
toolVendor = ToolVendor

materialVendor :: Creature
materialVendor = MaterialVendor

