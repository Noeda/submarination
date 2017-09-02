{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Creature
  ( Creature(..)
  , foodVendor
  , ammoVendor
  , toolVendor
  , materialVendor
  , isAnimal
  , creatureName )
  where

import Data.Binary
import Data.Data
import Protolude

import Submarination.Plural

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

creatureName :: Creature -> Plural -> Text
creatureName FoodVendor Singular     = "a food vendor"
creatureName FoodVendor Many         = "food vendors"
creatureName AmmoVendor Singular     = "a munitions vendor"
creatureName AmmoVendor Many         = "munition vendors"
creatureName ToolVendor Singular     = "a hardware vendor"
creatureName ToolVendor Many         = "hardware vendors"
creatureName MaterialVendor Singular = "a material vendor"
creatureName MaterialVendor Many     = "material vendors"
creatureName Snoatfish Singular      = "a snoatfish"
creatureName Snoatfish Many          = "snoatfish"
creatureName Biddy Singular          = "a biddy"
creatureName Biddy Many              = "biddy"
creatureName Enneapus Singular       = "an enneapus"
creatureName Enneapus Many           = "enneapi"
creatureName Camobream Singular      = "a camobream"
creatureName Camobream Many          = "camobreams"
creatureName Gator Singular          = "a gator"
creatureName Gator Many              = "gators"

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

