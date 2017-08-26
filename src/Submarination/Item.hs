module Submarination.Item
  ( Item(..)
  , Plural(..)
  , isItemBulky
  , itemName
  , itemDescription
  , itemPrice
  , itemContents
  , storageBoxContents
  , groupItems )
  where

import Control.Lens
import Data.Data
import qualified Data.Map.Strict as M
import Protolude

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
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Plural
  = Many
  | Singular
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

groupItems :: [Item] -> M.Map Item Int
groupItems = foldr folder M.empty
 where
  folder item = at item %~ Just . \case
    Nothing -> 1
    Just v  -> v+1

itemName :: Item -> Plural -> Text
itemName SardineTin     Singular     = "a tin of sardines"
itemName SardineTin     Many         = "tins of sardines"
itemName Poylent        Singular     = "a bottle of poylent"
itemName Poylent        Many         = "bottles of poylent"
itemName Chicken        Singular     = "a chicken"
itemName Chicken        Many         = "chickens"
itemName Potato         Singular     = "a potato"
itemName Potato         Many         = "potatoes"
itemName Freezer{}      Singular     = "a freezer"
itemName Freezer{}      Many         = "freezers"
itemName Refrigerator{} Singular     = "a refrigerator"
itemName Refrigerator{} Many         = "refrigerators"
itemName Microwave{}    Singular     = "a microwave"
itemName Microwave{}    Many         = "microwaves"
itemName Whiskey        Singular     = "a bottle of whiskey"
itemName Whiskey        Many         = "bottles of whiskey"
itemName StorageBox{}   Singular     = "a storage box"
itemName StorageBox{}   Many         = "storage boxes"

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

-- bulkiness = can I carry more than one and can there be more than one of
-- these per square?
isItemBulky :: Item -> Bool
isItemBulky StorageBox{} = True
isItemBulky Freezer{} = True
isItemBulky Refrigerator{} = True
isItemBulky Microwave{} = True
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


