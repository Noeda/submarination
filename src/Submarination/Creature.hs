{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Creature
  ( Creature(..)
  , CreatureType(..)
  , isAnimal
  , creatureFromType
  , setIndex
  , creatureName )
  where

import Data.Binary
import Data.Data
import Protolude

import Submarination.Index
import Submarination.Plural

data Creature = Creature
  { creatureIndex :: {-# UNPACK #-} !Index
  , creatureType  :: !CreatureType }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

-- | Creature must be reindexed before put into game. This set index to 0.
creatureFromType :: CreatureType -> Creature
creatureFromType ctype = Creature { creatureIndex = invalidIndex
                                  , creatureType = ctype }

data CreatureType
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

setIndex :: Index -> Creature -> Creature
setIndex idx cr = cr { creatureIndex = idx }
{-# INLINEABLE setIndex #-}

creatureName :: CreatureType -> Plural -> Text
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

isAnimal :: CreatureType -> Bool
isAnimal Snoatfish = True
isAnimal Biddy = True
isAnimal Enneapus = True
isAnimal Camobream = True
isAnimal Gator = True
isAnimal _ = False

