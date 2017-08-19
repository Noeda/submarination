module Submarination.Item
  ( Item(..)
  , Plural(..)

  , itemName
  , itemDescription
  , itemPrice )
  where

import Data.Data
import Protolude

data Item
  = SardineTin
  | Poylent
  | Chicken
  | Potato
  | Freezer
  | Refrigerator
  | Microwave
  | Whiskey
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Plural
  = Many
  | Singular
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

itemName :: Item -> Plural -> Text
itemName SardineTin   Singular     = "a tin of sardines"
itemName SardineTin   Many         = "tins of sardines"
itemName Poylent      Singular     = "a bottle of poylent"
itemName Poylent      Many         = "bottles of poylent"
itemName Chicken      Singular     = "a chicken"
itemName Chicken      Many         = "chickens"
itemName Potato       Singular     = "a potato"
itemName Potato       Many         = "potatoes"
itemName Freezer      Singular     = "a freezer"
itemName Freezer      Many         = "freezers"
itemName Refrigerator Singular     = "a refrigerator"
itemName Refrigerator Many         = "refrigerators"
itemName Microwave    Singular     = "a microwave"
itemName Microwave    Many         = "microwaves"
itemName Whiskey      Singular     = "a bottle of whiskey"
itemName Whiskey      Many         = "bottles of whiskey"

itemDescription :: Item -> Text
itemDescription SardineTin =
  "Sardines in a tin. It's hard to ravish a tin of sardines but they'll fill you up."
itemDescription Poylent =
  "Bottle filled with goo that meets all nutritional requirements. It's latest and only product of Beet Labs. Non-refundable."
itemDescription Chicken =
  "A common avian delicacy. Chicken is rich in protein and actually tastes like something. However, chicken spoils quickly if it's not refrigerated or frozen and it must be warmed up before eaten."
itemDescription Potato =
  "How many potatoes does it take to kill an Irishman? Answer: None."
itemDescription Freezer =
  "As long as it is powered, freezer can stop the spoilage of any food. However, the nutritional quality of some foods can decrease if frozen."
itemDescription Refrigerator =
  "Refrigerator keeps foods in low temperature as long as it is powered."
itemDescription Microwave =
  "Put food in. Press button. Wait until you hear a beep. Take food out."
itemDescription Whiskey =
  "Love makes the world go around? Nonsense. Whiskey makes it go round twice as fast."

itemPrice :: Item -> Int
itemPrice SardineTin   = 10
itemPrice Poylent      = 100
itemPrice Chicken      = 50
itemPrice Potato       = 10
itemPrice Freezer      = 200
itemPrice Refrigerator = 150
itemPrice Microwave    = 150
itemPrice Whiskey      = 100

