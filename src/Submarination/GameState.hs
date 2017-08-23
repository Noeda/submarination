module Submarination.GameState where

import Control.Monad.Trans.Except
import Control.Lens hiding ( Level, levels )
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List ( (!!), delete )
import qualified Data.Set as S
import Linear.V2
import qualified Prelude as E
import Protolude hiding ( (&), to )

import Submarination.Creature
import Submarination.GameState.Types
import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Vendor

statusName :: Status -> Text
statusName Slow = "Slow"

data Direction
  = D4
  | D8
  | D2
  | D6
  | D9
  | D7
  | D1
  | D3
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

allNeighbours :: V2 Int -> [V2 Int]
allNeighbours (V2 x y) =
  [V2 (x-1) y
  ,V2 (x+1) y
  ,V2 x (y-1)
  ,V2 x (y+1)
  ,V2 (x-1) (y-1)
  ,V2 (x+1) (y+1)
  ,V2 (x-1) (y+1)
  ,V2 (x+1) (y-1)]

type GameMonad m = StateT GameState m
type GameMonadRo m = ReaderT GameState m

data SelectGroundItemResult
  = NoItems
  | PickedUp !Item
  | NeedItemMenu
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )

currentlySelectedGroundItem :: GameState -> SelectGroundItemResult
currentlySelectedGroundItem gs =
  case filter (not . isItemBulky) $ gs^.levelItemsAt (gs^.player.playerPosition) of
    [single_item] -> PickedUp single_item
    [] -> NoItems
    _many_items -> NeedItemMenu

startGameState :: GameState
startGameState = GameState
  { _player = Player { _playerPosition = V2 15 8
                     , _playerMaximumHealth = 100
                     , _playerHealth = 100
                     , _playerOxygen = 100
                     , _playerShells = 1000
                     , _playerInventory = []
                     , _playerDragging = Nothing }
  , _menuState = M.empty
  , _turn = 1
  , _levels = M.singleton 0 surfaceLevel
  , _sub = Sub { _subPosition = V2 23 (-2)
               , _topology = initial_sub_topo }
  , _depth  = 0 }
 where
  initial_sub_topo =
    placeStorageBox $
    removeNonAirLockDoors $
      composeVertically
        (composeHorizontally'
           [bridge
           ,standardRoom
           ,standardRoom])
        airLock

  placeStorageBox = subItemsP (V2 6 1) .~ [StorageBox []]

currentStatuses :: GameState -> S.Set Status
currentStatuses gs =
  if isSlow gs then S.singleton Slow else S.empty

isSlow :: GameState -> Bool
isSlow gs = isJust $ gs^.player.playerDragging

currentVendorMenuSelection :: Lens' GameState (Maybe Int)
currentVendorMenuSelection = lens get_it set_it
 where
  get_it gs = 
    M.lookup Vendor (gs^.menuState)
  set_it gs new_selection =
    gs & menuState.at Vendor .~ new_selection

currentMenuSelection :: MenuState -> Lens' GameState (Maybe Int)
currentMenuSelection ms = menuState.at ms

currentVendorItemSelection :: GameState -> Maybe Item
currentVendorItemSelection gs = do
  vendor <- getCurrentVendor gs
  current_selection' <- gs^.currentVendorMenuSelection

  let current_selection = max 0 $ min (length items-1) current_selection'
      items = vendorItems vendor

  guard (not $ null items)

  return $ items !! current_selection

gm :: Monad m => (GameState -> a) -> GameMonad m a
gm action = do
  gs <- get
  return $ action gs

gr :: (MonadReader r m, HasGameState r) => (GameState -> a) -> m a
gr action = do
  env <- ask
  return $ action $ env^.gameState
{-# INLINE gr #-}

directionToDelta :: Direction -> V2 Int
directionToDelta D2 = V2 0 1
directionToDelta D8 = V2 0 (-1)
directionToDelta D4 = V2 (-1) 0
directionToDelta D6 = V2 1 0
directionToDelta D7 = V2 (-1) (-1)
directionToDelta D9 = V2 1 (-1)
directionToDelta D3 = V2 1 1
directionToDelta D1 = V2 (-1) 1

advanceTurn :: forall m. Monad m => GameMonad m ()
advanceTurn = do
  turn += 1
  walkActiveMetadata
 where
  walkActiveMetadata :: GameMonad m ()
  walkActiveMetadata = do
    walkCurrentLevelMetadata
    walkSubLevelMetadata

  walkCurrentLevelMetadata :: GameMonad m ()
  walkCurrentLevelMetadata = do
    lvl <- use currentLevel
    new_level <- walkLevel (lvl, V2 0 0)
    currentLevel .= new_level

  walkSubLevelMetadata :: GameMonad m ()
  walkSubLevelMetadata = do
    topo <- use $ sub.topology
    new_sub_topo <- forOf subLevelsWithOffset topo walkLevel
    sub.topology .= new_sub_topo

  walkLevel :: (Level, V2 Int) -> GameMonad m Level
  walkLevel (lvl, offset) = do
    current_turn <- use turn
    playerpos <- use $ player.playerPosition

    walkLevelActiveMetadata lvl $ \coords cell metadata -> return $ case (cell, metadata) of
      (OpenHatch, HatchAutoClose close_turn) | close_turn <= current_turn && null (lvl^.itemsAt coords) && playerpos /= coords + offset ->
        (Hatch, Nothing)
      (OpenHatch, metadata@HatchAutoClose{}) ->
        (OpenHatch, Just metadata)
      (feature, _) -> (feature, Nothing)

moveDirection :: Monad m => Direction -> GameMonad m ()
moveDirection direction = do
  old_pos <- gm (^.player.playerPosition)
  let new_playerpos = old_pos + directionToDelta direction

  lcell <- gm (^.levelCellAt new_playerpos)

  -- Is it a hatch? (that I can open)
  if lcell == Hatch
    then openHatch new_playerpos
    else notHatch lcell new_playerpos
 where
  openHatch new_playerpos = do
    current_turn <- use turn

    levelCellAt new_playerpos .= OpenHatch
    levelActiveMetadataAt new_playerpos .= (Just $ HatchAutoClose (current_turn+5))
    advanceTurn


  notHatch lcell new_playerpos = do
    -- Can I walk there?
    let walkable = isWalkable lcell
    -- Is there some monster in there?
    occupied <- gm $ isOccupied new_playerpos
    -- Are we dragging something and there is something bulky in target?
    dragging <- gm (^.player.playerDragging.to isJust)
    bulky_at_target <- gm (^.levelBulkyItemAt new_playerpos.to isJust)

    when (walkable && not occupied && not (bulky_at_target && dragging)) $ do
      player.playerPosition .= new_playerpos

      -- Moving takes extra time if we are slow
      slow <- gm isSlow
      when slow advanceTurn

      advanceTurn

surfaceLevel :: Level
surfaceLevel = levelFromStringsPlacements SurfaceWater placements
  [".....,................................................."
  ,"...,...........,.................................,....."
  ,"...........................,..........................."
  ,".........,.............,............#,................."
  ,".........#,............................................"
  ,".........^,......================,,........,..........."
  ,"...,,...........==================,,..,...,............"
  ,"...........#,...==================,,.#,..^,............"
  ,"....,..#...^^,,====================,....##,......,....."
  ,"...#.......##,,########g=g#########,,.................."
  ,"......###########^^#^#^gg=#^^##################,......."
  ,"#######################ggg##^##########################"
  ,"######################1ggg2##^^^^######################"
  ,"####^^^^#######^^^####ggggg##^^########^^^#############"
  ,"###^^###########^^##^#ggggg############################"
  ,"################^^##^#3ggg4###############^############"
  ,"########^^^#####^^####ggggg###^#####^##################"
  ,"#################^#####ggg####^###^^############^######"
  ,"#################^##^######^^#^###^^###################"
  ,"####################^##########^#^^^###################"
  ,"################################^^^######^^^###########"
  ,"###################^^^^^^#^#####^^^####################"
  ,"#########^#^##^####^^^^^##^############################"
  ,"#########^####^####^^^^################################"
  ,"#########^####^####^###################################"
  ,"##################^^######^^^^#########################"
  ,"##################^^###################################"
  ,"##################^##########################^#^^######"
  ,"#######################################^###############"
  ,"#######################################################"
  ,"#######################################################"
  ,"#######################################################"]
 where
  placements =
    [(1, ('g', placeCreature foodVendor))
    ,(2, ('g', placeCreature ammoVendor))
    ,(3, ('g', placeCreature toolVendor))
    ,(4, ('g', placeCreature materialVendor))]

getMaximumOxygenLevel :: GameState -> Int
getMaximumOxygenLevel _ = 100


levelActiveMetadataAt :: V2 Int -> Lens' GameState (Maybe LevelActiveMetadata)
levelActiveMetadataAt =
  subOrLevelLens activeMetadataAt subActiveMetadataAt

levelBulkyItemAt :: V2 Int -> Lens' GameState (Maybe Item)
levelBulkyItemAt coords = lens get_it set_it
 where
  get_it gs =
    let items = gs^.levelItemsAt coords
     in find isItemBulky items

  -- some sanity checking
  set_it _ (Just bulky_item) | not (isItemBulky bulky_item) = E.error "levelBulkyItemAt: Item is not bulky."
  set_it gs (Just bulky_item) =
    -- To satisfy lens laws, we must remove any existing bulky item from target
    let items = gs^.levelItemsAt coords
     in case find isItemBulky items of
          Nothing -> gs & levelItemsAt coords .~ (bulky_item:items)
          Just existing_item ->
            let fitems = filter (/= existing_item) items
             in gs & levelItemsAt coords .~ (bulky_item:fitems)

  set_it gs Nothing = gs & levelItemsAt coords %~ filter (not . isItemBulky)
{-# INLINEABLE levelBulkyItemAt #-}

currentAreaName :: GameState -> Text
currentAreaName _ = "--- SURFACE ---"

isOccupied :: V2 Int -> GameState -> Bool
isOccupied coords gamestate =
  let cr = gamestate^.currentLevel
   in isJust $ cr^.creatures.at coords

isOnSurface :: GameState -> Bool
isOnSurface gs = gs^.depth == 0

inventoryLimit :: Int
inventoryLimit = 20

runMaybeExcept :: Except a () -> Maybe a
runMaybeExcept action =
  case runExcept action of
    Left value -> Just value
    _ -> Nothing

addItemInventory :: Item -> Prism' GameState GameState
addItemInventory item =
  prism' identity $ \gs ->
    if length (gs^.player.playerInventory) >= inventoryLimit
      then Nothing
      else Just $ gs & player.playerInventory %~ (item:)

removeOneItemFromGround :: Item -> Prism' GameState GameState
removeOneItemFromGround item =
  prism' identity $ \gs ->
    let items = gs^.levelItemsAt (gs^.player.playerPosition)
     in case delete item items of
          new_items | new_items /= items ->
            Just $ gs & levelItemsAt (gs^.player.playerPosition) .~ new_items
          _ -> Nothing

attemptPickUpItemOrGoToMenu :: Prism' GameState GameState
attemptPickUpItemOrGoToMenu =
  prism' identity $ \gs ->
    case currentlySelectedGroundItem gs of
      PickedUp single_item ->
        gs ^? removeOneItemFromGround single_item .
              addItemInventory single_item
      _ -> Nothing

attemptPurchase :: GameState -> GameState
attemptPurchase gs = fromMaybe gs $ do
  item_selection <- currentVendorItemSelection gs
  let playerpos = gs^.player.playerPosition
      shells = gs^.player.playerShells

  guard (shells >= itemPrice item_selection)

  if isItemBulky item_selection
    then do
      -- Cannot buy bulky item if dragging something
      guard (isNothing $ gs^.player.playerDragging)
      -- Cannot buy bulky item if bulky item already on floor
      guard (isNothing $ gs^.levelBulkyItemAt playerpos)

      -- Put bulky item for dragging
      return $ gs & (player.playerShells -~ itemPrice item_selection) .
                    (player.playerDragging .~ Just item_selection)

    else return $ case gs^?addItemInventory item_selection of
           Nothing -> gs
           Just new_gs -> new_gs & player.playerShells -~ itemPrice item_selection

getCurrentVendor :: GameState -> Maybe Creature
getCurrentVendor gs = runMaybeExcept $ do
  let playerpos = gs^.player.playerPosition
      lvl = gs^.currentLevel

  for_ (allNeighbours playerpos) $ \candidate_pos ->
    case lvl^.creatures.at candidate_pos of
      Just FoodVendor -> throwE FoodVendor
      Just AmmoVendor -> throwE AmmoVendor
      Just MaterialVendor -> throwE MaterialVendor
      Just ToolVendor -> throwE ToolVendor
      _ -> return ()

currentlySelectedMenuItemLens :: MenuState -> (Lens' GameState [Item]) -> Getter GameState (Maybe Item)
currentlySelectedMenuItemLens menu_state item_get = to $ \gs -> do
  selection <- gs^.menuState.at menu_state
  let items = (gs^.item_get.to groupItems.to M.assocs)
  guard (selection >= 0 && selection < length items)
  return $ fst $ items !! selection

currentSelectedInventoryItem :: Getter GameState (Maybe Item)
currentSelectedInventoryItem = currentlySelectedMenuItemLens Inventory (player.playerInventory)

getActiveMenuHandler :: GameState -> Maybe ItemMenuHandler
getActiveMenuHandler gs =
  ch Inventory <|> ch Pickup
 where
  ch ms = case gs^.menuState.at ms of
    Nothing -> Nothing
    Just _index -> itemHandler ms

currentlySelectedPickupItem :: Getter GameState (Maybe Item)
currentlySelectedPickupItem = to $ \gs ->
  let playerpos = gs^.player.playerPosition
   in gs^.currentlySelectedMenuItemLens Pickup (levelItemsAt playerpos)

getActiveMenu :: GameState -> Maybe MenuState
getActiveMenu gs =
  if | Inventory `M.member` menus -> Just Inventory
     | Pickup    `M.member` menus -> Just Pickup

     | otherwise -> Nothing
 where
  menus = gs^.menuState

currentlySelectedItem :: Getter GameState (Maybe Item)
currentlySelectedItem = to $ \gs -> do
  handler <- getActiveMenuHandler gs
  let items = filter (menuFilter handler) (gs^.itemLens handler)
      gitems = groupItems items

  guard (not $ null items)

  let menu_selection = fromMaybe 0 (gs^.selectionLens handler)
      max_selection = M.size gitems-1
      current_selection = max 0 $ min max_selection menu_selection

  return $ fst $ M.assocs gitems !! current_selection

pickUpItem :: Getter GameState (Maybe GameState)
pickUpItem = to $ \gs -> do
  guard (getActiveMenu gs == Just Pickup)
  handler <- getActiveMenuHandler gs
  item <- gs^.currentlySelectedItem

  new_gs <- gs^?addItemInventory item
  return $ new_gs & (itemLens handler %~ delete item)

dropInventoryItem :: Getter GameState (Maybe GameState)
dropInventoryItem = to $ \gs -> do
  selected_inventory_item <- gs^.currentSelectedInventoryItem
  return $ gs & (player.playerInventory %~ delete selected_inventory_item) .
                (levelItemsAt (gs^.player.playerPosition) %~ ((:) selected_inventory_item))

cycleGame :: Monad m => GameMonad m ()
cycleGame = do
  gs <- get

  -- Enter vendor menu if not currently next to vendor.
  case getCurrentVendor gs of
    Nothing -> currentVendorMenuSelection .= Nothing
    Just _vendor ->
      when (isNothing $ gs^.currentVendorMenuSelection) $
        currentVendorMenuSelection .= Just 0

  -- Inventory selection must be within range
  case gs^.menuState.at Inventory of
    Just _selection -> case gs^.currentSelectedInventoryItem of
      Nothing -> menuState.at Inventory .= Just 0
      _ -> return ()
    _ -> return ()

inActiveMenu :: GameState -> Bool
inActiveMenu = isJust . getActiveMenu

keyToMenuStateTrigger :: Char -> Maybe MenuState
keyToMenuStateTrigger ch = go menu_states
 where
  menu_states = enumFrom (toEnum 0)

  go [] = Nothing
  go (candidate:rest) = case itemHandler candidate of
    Just handler | ch `S.member` triggerKeys handler -> Just candidate
    _ -> go rest

itemHandler :: MenuState -> Maybe ItemMenuHandler
itemHandler Inventory = Just $ ItemMenuHandler
  { triggerKeys = S.fromList "i"
  , offKeys = S.fromList "i"
  , menuKeys = M.fromList [('d', ("Drop", \gs -> gs^.dropInventoryItem))]
  , prerequisites = not . inActiveMenu
  , selectionLens = menuState.at Inventory
  , menuFilter = const True
  , itemLens = player.playerInventory }
itemHandler Pickup = Just $ ItemMenuHandler
  { triggerKeys = S.fromList ","
  , offKeys = S.empty
  , menuKeys = M.fromList [(',', ("Pick up", \gs -> gs^.pickUpItem))]

  , prerequisites = \gs -> not (inActiveMenu gs) && not (null $ gs^.itemsAtPlayer)
  , selectionLens = menuState.at Pickup
  , menuFilter = not . isItemBulky
  , itemLens = itemsAtPlayer }
itemHandler _ = Nothing

