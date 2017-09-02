{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Item
  ( Item(..)
  , Plural(..)
  , isItemBulky
  , isEdible
  , itemNutrition
  , itemName
  , itemDescription
  , itemPrice
  , itemContents
  , itemStorageLimit
  , storageBoxContents
  , groupItems )
  where

import Control.Lens
import Data.Binary
import Data.Data
import qualified Data.Map.Strict as M
import Protolude

import Submarination.Plural

data Item
  = SardineTin
  | Poylent
  | Chicken
  | Potato
  | Freezer [Item]
  | Refrigerator [Item]
  | Microwave [Item]
  | Whiskey
  | StorageBox [Item]
  | WoundedCorpse
  | MutilatedCorpse
  | BloatedCorpse
  | PartiallyEatenCorpse
  | SkeletonCorpse
  | PlantPersonCorpse
  | Explosives
  | Taser
  | Harpoon
  | WinchAndCable
  | HullParts
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

groupItems :: [Item] -> M.Map Item Int
groupItems = foldr folder M.empty
 where
  folder item = at item %~ Just . \case
    Nothing -> 1
    Just v  -> v+1

itemName :: Item -> Plural -> Text
itemName SardineTin      Singular      = "a tin of sardines"
itemName SardineTin      Many          = "tins of sardines"
itemName Poylent         Singular      = "a bottle of poylent"
itemName Poylent         Many          = "bottles of poylent"
itemName Chicken         Singular      = "a chicken"
itemName Chicken         Many          = "chickens"
itemName Potato          Singular      = "a potato"
itemName Potato          Many          = "potatoes"
itemName Freezer{}       Singular      = "a freezer"
itemName Freezer{}       Many          = "freezers"
itemName Refrigerator{}  Singular      = "a refrigerator"
itemName Refrigerator{}  Many          = "refrigerators"
itemName Microwave{}     Singular      = "a microwave"
itemName Microwave{}     Many          = "microwaves"
itemName Whiskey         Singular      = "a bottle of whiskey"
itemName Whiskey         Many          = "bottles of whiskey"
itemName StorageBox{}    Singular      = "a storage box"
itemName StorageBox{}    Many          = "storage boxes"
itemName WoundedCorpse   Singular      = "a diver's wounded corpse"
itemName WoundedCorpse   Many          = "divers' wounded corpses"
itemName MutilatedCorpse Singular      = "a diver's mutilated corpse"
itemName MutilatedCorpse Many          = "divers' mutilated corpses"
itemName BloatedCorpse   Singular      = "a bloated corpse"
itemName BloatedCorpse   Many          = "bloated corpses"
itemName PartiallyEatenCorpse Singular = "a partially eaten corpse"
itemName PartiallyEatenCorpse Many     = "partially eaten corpses"
itemName SkeletonCorpse Singular       = "skeletal remains"
itemName SkeletonCorpse Many           = "skeletal remains"
itemName PlantPersonCorpse Singular    = "plantperson corpse"
itemName PlantPersonCorpse Many        = "plantperson corpses"
itemName WinchAndCable Singular        = "a winch and cable"
itemName WinchAndCable Many            = "winches and cables"
itemName Explosives Singular           = "explosives"
itemName Explosives Many               = "explosives"
itemName Taser Singular                = "a taser"
itemName Taser Many                    = "tasers"
itemName HullParts Singular            = "a hull part"
itemName HullParts Many                = "hull parts"
itemName Harpoon Singular              = "a harpoon"
itemName Harpoon Many                  = "harpoons"

itemDescription :: Item -> Text
itemDescription SardineTin =
  "Sardines in a tin. It's hard to ravish a tin of sardines but they'll fill you up."
itemDescription Poylent =
  "Bottle filled with goo that meets all nutritional requirements. It's latest and only product of Beet Labs. Non-refundable."
itemDescription Chicken =
  "A common avian delicacy. Chicken is rich in protein and actually tastes like something. However, chicken spoils quickly if it's not refrigerated or frozen and it must be warmed up before eaten."
itemDescription Potato =
  "How many potatoes does it take to kill an Irishman? Answer: None."
itemDescription Freezer{} =
  "As long as it is powered, freezer can stop the spoilage of any food. However, the nutritional quality of some foods can decrease if frozen."
itemDescription Refrigerator{} =
  "Refrigerator keeps foods in low temperature as long as it is powered."
itemDescription Microwave{} =
  "Put food in. Press button. Wait until you hear a beep. Take food out."
itemDescription Whiskey =
  "Love makes the world go around? Nonsense. Whiskey makes it go round twice as fast."
itemDescription (StorageBox inner_items) =
  "Storage boxes can store your items inside. They can keep their contents dry even if submerged in water." <>
  if null inner_items
    then ""
    else let num_items = length inner_items
          in if num_items > 1
               then " This box contains " <> show (length inner_items) <> " items."
               else " This box contains 1 item."
itemDescription WinchAndCable =
  "A winch and a cable. Tie it on yourself when you leave airlock and it can quickly pull you back in case of danger."
itemDescription Explosives =
  "Most personal problems can be solved with a healthy dose of volatile explosives."
itemDescription Taser =
  "Do you want to electrocute rare and exotic species for fun and profit? Then this is for you. Taser needs is powered and needs to be charged."
itemDescription HullParts = "What are submarines made of? Flavorless, dull colored hull parts disappointing to the last screw, that's what they are made of."
itemDescription Harpoon = "Use this harpoon to impale fish like kebab!!"

-- TODO: write...uh..."fun" descriptions for all the corpses
itemDescription WoundedCorpse = "A bloodied wounded corpse."
itemDescription MutilatedCorpse = "A mutilated corpse."
itemDescription BloatedCorpse = "A bloated corpse."
itemDescription PartiallyEatenCorpse = "This corpse has been partially consumed by local fauna."
itemDescription SkeletonCorpse = "The skeletal remains of an unfortunate diver."
itemDescription PlantPersonCorpse = "The tattered remains of a plantperson."

itemPrice :: Item -> Int
itemPrice SardineTin   = 10
itemPrice Poylent      = 100
itemPrice Chicken      = 50
itemPrice Potato       = 10
itemPrice Whiskey      = 100
itemPrice (StorageBox inner_items)   = 300 + sum (itemPrice <$> inner_items)
itemPrice (Freezer inner_items)      = 200 + sum (itemPrice <$> inner_items)
itemPrice (Refrigerator inner_items) = 150 + sum (itemPrice <$> inner_items)
itemPrice (Microwave inner_items)    = 150 + sum (itemPrice <$> inner_items)
itemPrice WoundedCorpse = 0
itemPrice MutilatedCorpse = 0
itemPrice BloatedCorpse = 0
itemPrice PartiallyEatenCorpse = 0
itemPrice SkeletonCorpse = 0
itemPrice PlantPersonCorpse = 0
itemPrice Taser = 400
itemPrice Explosives = 500
itemPrice WinchAndCable = 300
itemPrice HullParts = 150
itemPrice Harpoon = 500

itemStorageLimit :: Item -> Int
itemStorageLimit StorageBox{}   = 20
itemStorageLimit Freezer{}      = 10
itemStorageLimit Microwave{}    = 1
itemStorageLimit Refrigerator{} = 10
itemStorageLimit _ = 0

-- bulkiness = can I carry more than one and can there be more than one of
-- these per square?
isItemBulky :: Item -> Bool
isItemBulky StorageBox{} = True
isItemBulky Freezer{} = True
isItemBulky Refrigerator{} = True
isItemBulky Microwave{} = True
isItemBulky WoundedCorpse{} = True
isItemBulky MutilatedCorpse{} = True
isItemBulky BloatedCorpse{} = True
isItemBulky PartiallyEatenCorpse{} = True
isItemBulky SkeletonCorpse{} = True
isItemBulky PlantPersonCorpse{} = True
isItemBulky _ = False

itemContents :: Traversal' Item [Item]
itemContents fun (StorageBox items)   = StorageBox <$> fun items
itemContents fun (Freezer items)      = Freezer <$> fun items
itemContents fun (Refrigerator items) = Refrigerator <$> fun items
itemContents fun (Microwave items)    = Microwave <$> fun items
itemContents _fun x = pure x

storageBoxContents :: Prism' Item [Item]
storageBoxContents = prism' StorageBox $ \case
  StorageBox items -> Just items
  _ -> Nothing

isEdible :: Item -> Bool
isEdible = (> 0) . itemNutrition

itemNutrition :: Item -> Int
itemNutrition SardineTin = 50
itemNutrition Chicken    = 300
itemNutrition Potato     = 45
itemNutrition Poylent    = 300
itemNutrition Whiskey    = 10
itemNutrition _          = 0

