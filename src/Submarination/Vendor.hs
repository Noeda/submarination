module Submarination.Vendor
  ( vendorDescription
  , vendorItems )
  where

import Protolude

import Submarination.Creature
import Submarination.Item

vendorDescription :: Creature -> Text
vendorDescription FoodVendor =
  "Welcome to Nigel's Seafood Catering Company! All our food is guaranteed lead-free!"
vendorDescription AmmoVendor =
  "I bid you welcome to Matilda's Munitions. We'll help you in your quest to violently destroy all your enemies."
vendorDescription ToolVendor =
  "Pag has tool. You buy tool? Pag sell tool. You wheel? You deal."
vendorDescription MaterialVendor =
  "YOU THERE! Buy our wondrous materials. We may not have a great variety but you can be sure all our materials are very wondrous."

vendorItems :: Creature -> [Item]
vendorItems FoodVendor =
  [SardineTin
  ,Poylent
  ,Chicken
  ,Potato
  ,Freezer []
  ,Refrigerator []
  ,Microwave []
  ,Whiskey]
vendorItems _ = []

