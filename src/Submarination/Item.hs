{-# LANGUAGE DeriveAnyClass #-}

module Submarination.Item
  ( Item()
  , ItemType(..)
  , itemType
  , itemFromType
  , Plural(..)
  , isItemBulky
  , isEdible
  , itemNutrition
  , itemName
  , itemDescription
  , itemPrice
  , itemContents
  , itemStorageLimit
  , groupItems )
  where

import Control.Lens
import Data.Binary
import Data.Data
import qualified Data.Map.Strict as M
import Protolude hiding ( (&) )

import Submarination.Plural
import Submarination.Turn

data Item = Item
  { _itemType           :: !ItemType
  , _creationTurn       :: !Turn
  , _refrigerated       :: !(Maybe Turn)
  , _unrefrigerated     :: !(Maybe Turn)
  , _frozen             :: !(Maybe Turn)
  , _unfrozen           :: !(Maybe Turn) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data ItemType
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

itemType :: Lens' Item ItemType
itemType = lens get_it set_it
 where
  get_it = _itemType
  set_it item new_it = item { _itemType = new_it }
{-# INLINEABLE itemType #-}

groupItems :: Ord a => [a] -> M.Map a Int
groupItems = foldr folder M.empty
 where
  folder = M.alter (\case
    Nothing -> Just 1
    Just x  -> Just (x+1))

itemName :: Item -> Plural -> Text
itemName item plural = go (item^.itemType) plural
 where
  go SardineTin      Singular      = "a tin of sardines"
  go SardineTin      Many          = "tins of sardines"
  go Poylent         Singular      = "a bottle of poylent"
  go Poylent         Many          = "bottles of poylent"
  go Chicken         Singular      = "a chicken"
  go Chicken         Many          = "chickens"
  go Potato          Singular      = "a potato"
  go Potato          Many          = "potatoes"
  go Freezer{}       Singular      = "a freezer"
  go Freezer{}       Many          = "freezers"
  go Refrigerator{}  Singular      = "a refrigerator"
  go Refrigerator{}  Many          = "refrigerators"
  go Microwave{}     Singular      = "a microwave"
  go Microwave{}     Many          = "microwaves"
  go Whiskey         Singular      = "a bottle of whiskey"
  go Whiskey         Many          = "bottles of whiskey"
  go StorageBox{}    Singular      = "a storage box"
  go StorageBox{}    Many          = "storage boxes"
  go WoundedCorpse   Singular      = "a diver's wounded corpse"
  go WoundedCorpse   Many          = "divers' wounded corpses"
  go MutilatedCorpse Singular      = "a diver's mutilated corpse"
  go MutilatedCorpse Many          = "divers' mutilated corpses"
  go BloatedCorpse   Singular      = "a bloated corpse"
  go BloatedCorpse   Many          = "bloated corpses"
  go PartiallyEatenCorpse Singular = "a partially eaten corpse"
  go PartiallyEatenCorpse Many     = "partially eaten corpses"
  go SkeletonCorpse Singular       = "skeletal remains"
  go SkeletonCorpse Many           = "skeletal remains"
  go PlantPersonCorpse Singular    = "plantperson corpse"
  go PlantPersonCorpse Many        = "plantperson corpses"
  go WinchAndCable Singular        = "a winch and cable"
  go WinchAndCable Many            = "winches and cables"
  go Explosives Singular           = "explosives"
  go Explosives Many               = "explosives"
  go Taser Singular                = "a taser"
  go Taser Many                    = "tasers"
  go HullParts Singular            = "a hull part"
  go HullParts Many                = "hull parts"
  go Harpoon Singular              = "a harpoon"
  go Harpoon Many                  = "harpoons"

itemDescription :: Item -> Text
itemDescription item = go $ item^.itemType
 where
  go SardineTin =
    "Sardines in a tin. It's hard to ravish a tin of sardines but they'll fill you up."
  go Poylent =
    "Bottle filled with goo that meets all nutritional requirements. It's latest and only product of Beet Labs. Non-refundable."
  go Chicken =
    "A common avian delicacy. Chicken is rich in protein and actually tastes like something. However, chicken spoils quickly if it's not refrigerated or frozen and it must be warmed up before eaten."
  go Potato =
    "How many potatoes does it take to kill an Irishman? Answer: None."
  go Freezer{} =
    "As long as it is powered, freezer can stop the spoilage of any food. However, the nutritional quality of some foods can decrease if frozen."
  go Refrigerator{} =
    "Refrigerator keeps foods in low temperature as long as it is powered."
  go Microwave{} =
    "Put food in. Press button. Wait until you hear a beep. Take food out."
  go Whiskey =
    "Love makes the world go around? Nonsense. Whiskey makes it go round twice as fast."
  go (StorageBox inner_items) =
    "Storage boxes can store your items inside. They can keep their contents dry even if submerged in water." <>
    if null inner_items
      then ""
      else let num_items = length inner_items
            in if num_items > 1
                 then " This box contains " <> show (length inner_items) <> " items."
                 else " This box contains 1 item."
  go WinchAndCable =
    "A winch and a cable. Tie it on yourself when you leave airlock and it can quickly pull you back in case of danger."
  go Explosives =
    "Most personal problems can be solved with a healthy dose of volatile explosives."
  go Taser =
    "Do you want to electrocute rare and exotic species for fun and profit? Then this is for you. Taser needs is powered and needs to be charged."
  go HullParts = "What are submarines made of? Flavorless, dull colored hull parts disappointing to the last screw, that's what they are made of."
  go Harpoon = "Use this harpoon to impale fish like kebab!!"

-- TODO: write...uh..."fun" descriptions for all the corpses
  go WoundedCorpse = "A bloodied wounded corpse."
  go MutilatedCorpse = "A mutilated corpse."
  go BloatedCorpse = "A bloated corpse."
  go PartiallyEatenCorpse = "This corpse has been partially consumed by local fauna."
  go SkeletonCorpse = "The skeletal remains of an unfortunate diver."
  go PlantPersonCorpse = "The tattered remains of a plantperson."

itemPrice :: Item -> Int
itemPrice item = go $ item^.itemType
 where
  go SardineTin   = 10
  go Poylent      = 100
  go Chicken      = 50
  go Potato       = 10
  go Whiskey      = 100
  go (StorageBox inner_items)   = 300 + sum (itemPrice <$> inner_items)
  go (Freezer inner_items)      = 200 + sum (itemPrice <$> inner_items)
  go (Refrigerator inner_items) = 150 + sum (itemPrice <$> inner_items)
  go (Microwave inner_items)    = 150 + sum (itemPrice <$> inner_items)
  go WoundedCorpse = 0
  go MutilatedCorpse = 0
  go BloatedCorpse = 0
  go PartiallyEatenCorpse = 0
  go SkeletonCorpse = 0
  go PlantPersonCorpse = 0
  go Taser = 400
  go Explosives = 500
  go WinchAndCable = 300
  go HullParts = 150
  go Harpoon = 500

itemStorageLimit :: Item -> Int
itemStorageLimit item = go $ item^.itemType
 where
  go StorageBox{}   = 20
  go Freezer{}      = 10
  go Microwave{}    = 1
  go Refrigerator{} = 10
  go _ = 0

-- bulkiness = can I carry more than one and can there be more than one of
-- these per square?
isItemBulky :: Item -> Bool
isItemBulky item = go $ item^.itemType
 where
  go StorageBox{} = True
  go Freezer{} = True
  go Refrigerator{} = True
  go Microwave{} = True
  go WoundedCorpse{} = True
  go MutilatedCorpse{} = True
  go BloatedCorpse{} = True
  go PartiallyEatenCorpse{} = True
  go SkeletonCorpse{} = True
  go PlantPersonCorpse{} = True
  go _ = False

itemContents :: Traversal' Item [Item]
itemContents fun item =
  (\new_itemtype -> item & itemType .~ new_itemtype) <$> go (item^.itemType)
 where
  go (StorageBox items)   = StorageBox <$> fun items
  go (Freezer items)      = Freezer <$> fun items
  go (Refrigerator items) = Refrigerator <$> fun items
  go (Microwave items)    = Microwave <$> fun items
  go x = pure x

isEdible :: Item -> Bool
isEdible = (> 0) . itemNutrition

itemNutrition :: Item -> Int
itemNutrition item = case item^.itemType of
  SardineTin -> 50
  Chicken    -> 300
  Potato     -> 45
  Poylent    -> 300
  Whiskey    -> 10
  _          -> 0

itemFromType :: Turn -> ItemType -> Item
itemFromType turn itype = Item
  { _itemType = itype
  , _creationTurn = turn
  , _refrigerated = Nothing
  , _unrefrigerated = Nothing
  , _frozen = Nothing
  , _unfrozen = Nothing }

