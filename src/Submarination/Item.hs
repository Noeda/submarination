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
  , groupItems
  , putItem
  , removeItem
  , isSpoiled

  , tests )
  where

import Control.Lens
import Data.Binary
import Data.Data
import qualified Data.IntMap as IM
import Data.List ( delete )
import qualified Data.Map.Strict as M
import Protolude hiding ( (&), to )

import Submarination.Plural
import Submarination.Turn
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

data Item = Item
  { _itemType            :: !ItemType
  , _creationTurn        :: !Turn
  , _frozenHistory       :: !(IM.IntMap QuotientState)
  , _refrigeratedHistory :: !(IM.IntMap QuotientState) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data QuotientState
  = NotFrozen
  | Frozen
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, Binary )

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
makeLenses ''Item

itemArbitrary :: Int -> Gen Item
itemArbitrary tree_division = do
  itype <- itemTypeArbitrary tree_division
  cturn <- arbitrary :: Gen Turn
  let cturn' = turnToInt cturn

  fhist <- IM.mapKeys ((+cturn') . abs) <$> arbitrary
  rhist <- IM.mapKeys ((+cturn') . abs) <$> arbitrary

  return Item
    { _itemType            = itype
    , _creationTurn        = cturn
    , _frozenHistory       = fhist
    , _refrigeratedHistory = rhist }

instance Arbitrary QuotientState where
  arbitrary = oneof $ pure <$> [Frozen, NotFrozen]

instance Arbitrary Item where
  arbitrary = itemArbitrary 1

itemTypeArbitrary :: Int -> Gen ItemType
itemTypeArbitrary tree_division =
  oneof [pure SardineTin
        ,pure Poylent
        ,pure Chicken
        ,pure Potato
        ,Freezer <$> itemsArbitrary
        ,Refrigerator <$> itemsArbitrary
        ,Microwave <$> itemsArbitrary
        ,pure Whiskey
        ,StorageBox <$> itemsArbitrary
        ,pure WoundedCorpse
        ,pure MutilatedCorpse
        ,pure BloatedCorpse
        ,pure PartiallyEatenCorpse
        ,pure SkeletonCorpse
        ,pure PlantPersonCorpse
        ,pure Explosives
        ,pure Taser
        ,pure Harpoon
        ,pure WinchAndCable
        ,pure HullParts]
 where
  itemsArbitrary = scale (`div` tree_division) $ listOf $ itemArbitrary (tree_division+1)

instance Arbitrary ItemType where
  arbitrary = itemTypeArbitrary 1

  shrink item | items <- item^.itemTypeContents, not (null items) =
    let nitems = length items
        sitems = fmap shrink items
     in [(item & itemTypeContents .~ [])
        ,(item & itemTypeContents .~ (take (nitems `div` 2) items))
        ,(item & itemTypeContents .~ (drop (max 1 $ nitems `div` 2) items))] <>
        fmap (\sitem_set -> item & itemTypeContents .~ sitem_set) sitems

  shrink _ = []

groupItems :: Ord a => [a] -> M.Map a Int
groupItems = foldr folder M.empty
 where
  folder = M.alter (\case
    Nothing -> Just 1
    Just x  -> Just (x+1))

ungroupItems :: Ord a => M.Map a Int -> [a]
ungroupItems = concatMap (\(key, value) -> replicate value key) . M.assocs

itemName :: Turn -> Item -> Plural -> Text
itemName turn item plural =
  go (item^.itemType) plural <>
  if isSpoiled turn item
    then " (spoiled)"
    else ""
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

itemTypeContents :: Traversal' ItemType [Item]
itemTypeContents fun = go
 where
  go (StorageBox items)   = StorageBox <$> fun items
  go (Freezer items)      = Freezer <$> fun items
  go (Refrigerator items) = Refrigerator <$> fun items
  go (Microwave items)    = Microwave <$> fun items
  go x = pure x

itemContents :: Traversal' Item [Item]
itemContents fun item =
  (\new_itemtype -> item & itemType .~ new_itemtype) <$>
  forOf itemTypeContents (item^.itemType) fun

isEdible :: Item -> Bool
isEdible = (> 0) . itemNutrition

itemNutrition :: Item -> Int
itemNutrition item = case item^.itemType of
  SardineTin -> 30
  Chicken    -> 300
  Potato     -> 80
  Poylent    -> 300
  Whiskey    -> 10
  _          -> 0

itemFromType :: Turn -> ItemType -> Item
itemFromType turn itype = Item
  { _itemType = itype
  , _creationTurn = turn
  , _refrigeratedHistory = IM.empty
  , _frozenHistory = IM.empty }

canonicalizeFrozenHistory :: Lens' Item (IM.IntMap QuotientState) -> Item -> Turn -> Item
canonicalizeFrozenHistory frozen item (turnToInt -> turn) =
  case current_state of
    (NotFrozen, latest_turn) | latest_turn <= turnToInt (item^.creationTurn) ->
      item & (frozen .~ IM.empty)
    (Frozen, latest_turn) | latest_turn <= turnToInt (item^.creationTurn) ->
      item & (frozen .~ IM.singleton (turnToInt $ item^.creationTurn) Frozen)
    (NotFrozen, latest_turn) ->
      let turns_frozen = (turn - turnToInt (item^.creationTurn)) - rt_quotient
       in item & (frozen .~ IM.fromList [(latest_turn-turns_frozen, Frozen)
                                        ,(latest_turn, NotFrozen)])
    (Frozen, _latest_turn) ->
      let turns_frozen = (turn - turnToInt (item^.creationTurn)) - rt_quotient
       in item & (frozen .~ IM.singleton (turn-turns_frozen) Frozen)
 where
  rt_quotient = getQuotientInRT (^.frozen) (intToTurn turn) item

  current_state = case IM.maxViewWithKey (item^.frozen) of
    Just ((turn, state), _) -> (state, turn)
    Nothing                 -> (NotFrozen, turnToInt $ item^.creationTurn)

getQuotientInRT :: (Item -> IM.IntMap QuotientState) -> Turn -> Item -> Int
getQuotientInRT getfrozen (turnToInt -> turn) item =
  go frozen NotFrozen (turnToInt $ item^.creationTurn) 0
 where
  frozen = getfrozen item

  go frozen state last_state_change turns_in_room_temperature =
    case IM.minViewWithKey frozen of
      Just ((turn, Frozen), rest) ->
        go rest Frozen turn (tir_increment turn)
      Just ((turn, NotFrozen), rest) ->
        go rest NotFrozen turn (tir_increment turn)
      Nothing -> tir_increment turn
   where
    tir_increment turn = case state of
      NotFrozen -> turns_in_room_temperature + (turn - last_state_change)
      Frozen    -> turns_in_room_temperature

unfrozenQuotient :: Turn -> Item -> Int
unfrozenQuotient = getQuotientInRT (^.frozenHistory)

unrefrigeratedQuotient :: Turn -> Item -> Int
unrefrigeratedQuotient = getQuotientInRT (^.refrigeratedHistory)

isSpoiled :: Turn -> Item -> Bool
isSpoiled turn item = case item^.itemType of
  Chicken ->
    age - frozen_quotient - (refrigerated_quotient - refrigerated_quotient `div` 10) > 500
  Potato ->
    age - frozen_quotient - (refrigerated_quotient - refrigerated_quotient `div` 10) > 3000
  _ -> False
 where
  age = turnToInt $ turn - item^.creationTurn
  frozen_quotient         = age-unfrozen_quotient
  unfrozen_quotient       = unfrozenQuotient turn item
  refrigerated_quotient   = age-unrefrigerated_quotient
  unrefrigerated_quotient = unrefrigeratedQuotient turn item

removeItem :: Turn -> Item -> Item -> Either Text (Item, Item)
removeItem turn item container =
  case firstOf itemContents container of
    Nothing -> Left "Not a container"
    Just old_items | item `elem` old_items -> Right
      (container & itemContents %~ delete item, remove_item)
    Just _ -> Left "Item not in the container."
 where
  remove_item = case container^.itemType of
    Freezer{}      -> canonicalizeFrozenHistory frozenHistory (unfreezeItem turn item) turn
    Refrigerator{} -> canonicalizeFrozenHistory refrigeratedHistory (unrefrigerateItem turn item) turn
    _ -> item

putItem :: Turn -> Item -> Item -> Either Text Item
putItem turn item container =
  case firstOf itemContents container of
    Nothing -> Left "Not a container"
    Just old_items | length old_items >= itemStorageLimit item
      -> Left "Container full"
    Just{} ->
      Right $ container & itemContents %~ \old_items -> insert_item:old_items
 where
  insert_item = case container^.itemType of
    Freezer{}      -> canonicalizeFrozenHistory frozenHistory (freezeItem turn item) turn
    Refrigerator{} -> canonicalizeFrozenHistory refrigeratedHistory (refrigerateItem turn item) turn

    _ -> item

freezeItem :: Turn -> Item -> Item
freezeItem turn =
  frozenHistory %~ IM.insert (turnToInt turn) Frozen

unfreezeItem :: Turn -> Item -> Item
unfreezeItem turn =
  frozenHistory %~ IM.insert (turnToInt turn) NotFrozen

refrigerateItem :: Turn -> Item -> Item
refrigerateItem turn =
  refrigeratedHistory %~ IM.insert (turnToInt turn) Frozen

unrefrigerateItem :: Turn -> Item -> Item
unrefrigerateItem turn =
  refrigeratedHistory %~ IM.insert (turnToInt turn) NotFrozen

tests :: [Test]
tests =
  [testProperty "groupItems is reversible" testGroupItemsReversible
  ,testProperty "canonicalized frozen history" testCanonicalizedFrozenHistory
  ,testProperty "canonicalized refrigerated history" testCanonicalizedRefrigeratedHistory
  ,testProperty "chicken spoilage" testChickenSpoilage]

testChickenSpoilage :: Bool
testChickenSpoilage =
  let chicken = itemFromType (intToTurn 100) Chicken
   in isSpoiled (intToTurn 100) chicken == False &&
      isSpoiled (intToTurn 200) chicken == False &&
      isSpoiled (intToTurn 10000) chicken == True

testGroupItemsReversible :: [Item] -> Bool
testGroupItemsReversible items =
  ungroupItems (groupItems items) == sort items

testCanonicalizedFrozenHistory :: Item -> Turn -> Bool
testCanonicalizedFrozenHistory item turn' =
  let max_turn = maximum $ (item^.creationTurn):(fmap intToTurn $ IM.keys $ item^.frozenHistory)
      turn = turn' + max_turn
   in unfrozenQuotient turn item == unfrozenQuotient turn (canonicalizeFrozenHistory frozenHistory item turn)

testCanonicalizedRefrigeratedHistory :: Item -> Turn -> Bool
testCanonicalizedRefrigeratedHistory item turn' =
  let max_turn = maximum $ (item^.creationTurn):(fmap intToTurn $ IM.keys $ item^.refrigeratedHistory)
      turn = turn' + max_turn
   in unfrozenQuotient turn item == unfrozenQuotient turn (canonicalizeFrozenHistory refrigeratedHistory item turn)

