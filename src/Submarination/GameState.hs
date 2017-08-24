module Submarination.GameState
  ( 
  -- * Directions
    Direction(..)
  , allNeighbours
  , directionToDelta
  -- * Monad tools
  , GameMonad
  , GameMonadRo
  , gm
  , gr
  -- * Game state
  , initialGameState
  , GameState()
  , gsAdvanceTurn
  , gsCycleGame
  , gsTurn
  , gmMoveToDirection
  -- * Level manipulation
  , gsCurrentAreaName
  , gsCurrentLevel
  , gsIsOnSurface
  , gsIsOccupied
  , glActiveMetadataAt
  , glCellAt
  , glItemsAt
  -- ** Level items
  , gmAddItemInventory
  , glBulkyItemAt
  , inventoryLimit
  -- * Predefined levels
  , surfaceLevel
  -- * Statuses
  , gsCurrentStatuses
  , gsIsSlow
  -- ** Status utilities
  , statusName
  , Status(..)
  -- * Player properties
  , gsMaximumOxygenLevel
  , glPlayer
  , gllAtPlayer
  , playerDragging
  , playerHealth
  , playerInventory
  , playerMaximumHealth
  , playerOxygen
  , playerPosition
  , playerShells
  , Player()
  -- * Submarine propertries
  , glSub
  , subPosition
  , subTopology
  , Sub()
  -- * Menus
  , gsEnterMenu
  , gsInActiveMenu
  , gsIsMenuActive
  , gmActiveMenuHandler
  , gmActiveMenu
  , gmCurrentlySelectedItem
  , gmCurrentlySelectedInventoryItem
  , gmCloseMenu
  , glCurrentMenuSelection
  , MenuState(..)
  -- ** Menu utilities
  , menuKeyToMenuStateTrigger
  , menuItemHandler
  , ItemMenuHandler(..)
  -- ** Vendor stuff
  , gsCurrentVendorItemSelection
  , gsIsVendoring
  , gmAttemptPurchase
  , gmCurrentVendorCreature
  , glCurrentVendorMenuSelection )
  where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
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

directionToDelta :: Direction -> V2 Int
directionToDelta D2 = V2 0 1
directionToDelta D8 = V2 0 (-1)
directionToDelta D4 = V2 (-1) 0
directionToDelta D6 = V2 1 0
directionToDelta D7 = V2 (-1) (-1)
directionToDelta D9 = V2 1 (-1)
directionToDelta D3 = V2 1 1
directionToDelta D1 = V2 (-1) 1

type GameMonad m = StateT GameState m
type GameMonadRo m = ReaderT GameState m

initialGameState :: GameState
initialGameState = GameState
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
               , _subTopology = initial_sub_topo }
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

glSub :: Lens' GameState Sub
glSub = sub
{-# INLINE glSub #-}

glPlayer :: Lens' GameState Player
glPlayer = player
{-# INLINE glPlayer #-}

gllAtPlayer :: (V2 Int -> Lens' GameState a) -> Lens' GameState a
gllAtPlayer inner_lens = lens get_it set_it
 where
  get_it gs = gs^.inner_lens (gs^.player.playerPosition)

  set_it gs new_value =
    gs & inner_lens (gs^.player.playerPosition) .~ new_value
{-# INLINE gllAtPlayer #-}

gsCurrentStatuses :: GameState -> S.Set Status
gsCurrentStatuses gs =
  if gsIsSlow gs then S.singleton Slow else S.empty

gsIsSlow :: GameState -> Bool
gsIsSlow gs = isJust $ gs^.player.playerDragging

glCurrentMenuSelection :: MenuState -> Lens' GameState (Maybe Int)
glCurrentMenuSelection ms = menuState.at ms

glCurrentVendorMenuSelection :: Lens' GameState (Maybe Int)
glCurrentVendorMenuSelection = lens get_it set_it
 where
  get_it gs = 
    M.lookup Vendor (gs^.menuState)
  set_it gs new_selection =
    gs & menuState.at Vendor .~ new_selection

gsCurrentVendorItemSelection :: GameState -> Maybe Item
gsCurrentVendorItemSelection gs = do
  vendor <- gmCurrentVendorCreature gs
  current_selection' <- gs^.glCurrentVendorMenuSelection

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

gsTurn :: GameState -> Int
gsTurn gs = gs^.turn

gsAdvanceTurn :: GameState -> GameState
gsAdvanceTurn = execState $ do
  turn += 1
  walkActiveMetadata
 where
  walkActiveMetadata = do
    walkCurrentLevelMetadata
    walkSubLevelMetadata

  walkCurrentLevelMetadata = do
    lvl <- use glCurrentLevel
    new_level <- walkLevel (lvl, V2 0 0)
    glCurrentLevel .= new_level

  walkSubLevelMetadata = do
    topo <- use $ sub.subTopology
    new_sub_topo <- forOf subLevelsWithOffset topo walkLevel
    sub.subTopology .= new_sub_topo

  walkLevel (lvl, offset) = do
    current_turn <- use turn
    playerpos <- use $ player.playerPosition

    walkLevelActiveMetadata lvl $ \coords cell metadata -> return $ case (cell, metadata) of
      (OpenHatch, HatchAutoClose close_turn) | close_turn <= current_turn && null (lvl^.itemsAt coords) && playerpos /= coords + offset ->
        (Hatch, Nothing)
      (OpenHatch, metadata@HatchAutoClose{}) ->
        (OpenHatch, Just metadata)
      (feature, _) -> (feature, Nothing)

gmMoveToDirection :: Direction -> GameState -> Maybe GameState
gmMoveToDirection direction gs = flip evalState gs $ runMaybeT $ do
  old_pos <- use (player.playerPosition)
  let new_playerpos = old_pos + directionToDelta direction

  -- Is it a hatch? (that I can open)
  openHatch new_playerpos <|> notHatch new_playerpos
  get
 where
  openHatch new_playerpos = do
    lcell <- use (glCellAt new_playerpos)
    guard (lcell == Hatch)

    current_turn <- use turn

    glCellAt new_playerpos .= OpenHatch
    glActiveMetadataAt new_playerpos .= (Just $ HatchAutoClose (current_turn+5))
    identity %= gsAdvanceTurn

  notHatch new_playerpos = do
    lcell <- use (glCellAt new_playerpos)

    -- Can I walk there?
    let walkable = isWalkable lcell
    -- Is there some monster in there?
    occupied <- use $ to (gsIsOccupied new_playerpos)
    -- Are we dragging something and there is something bulky in target?
    dragging <- use $ player.playerDragging.to isJust
    bulky_at_target <- use (glBulkyItemAt new_playerpos.to isJust)

    when (walkable && not occupied && not (bulky_at_target && dragging)) $ do
      player.playerPosition .= new_playerpos

      -- Moving takes extra time if we are slow
      slow <- use $ to gsIsSlow
      when slow $ identity %= gsAdvanceTurn

      identity %= gsAdvanceTurn

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

gsMaximumOxygenLevel :: GameState -> Int
gsMaximumOxygenLevel _ = 100

glActiveMetadataAt :: V2 Int -> Lens' GameState (Maybe LevelActiveMetadata)
glActiveMetadataAt =
  subOrLevelLens activeMetadataAt subActiveMetadataAt

glBulkyItemAt :: V2 Int -> Lens' GameState (Maybe Item)
glBulkyItemAt coords = lens get_it set_it
 where
  get_it gs =
    let items = gs^.glItemsAt coords
     in find isItemBulky items

  -- some sanity checking
  set_it _ (Just bulky_item) | not (isItemBulky bulky_item) = E.error "levelBulkyItemAt: Item is not bulky."
  set_it gs (Just bulky_item) =
    -- To satisfy lens laws, we must remove any existing bulky item from target
    let items = gs^.glItemsAt coords
     in case find isItemBulky items of
          Nothing -> gs & glItemsAt coords .~ (bulky_item:items)
          Just existing_item ->
            let fitems = filter (/= existing_item) items
             in gs & glItemsAt coords .~ (bulky_item:fitems)

  set_it gs Nothing = gs & glItemsAt coords %~ filter (not . isItemBulky)

gsCurrentLevel :: GameState -> Level
gsCurrentLevel gs = gs^.glCurrentLevel

gsCurrentAreaName :: GameState -> Text
gsCurrentAreaName _ = "--- SURFACE ---"

gsIsOccupied :: V2 Int -> GameState -> Bool
gsIsOccupied coords gamestate =
  let cr = gamestate^.glCurrentLevel
   in isJust $ cr^.creatures.at coords

gsIsOnSurface :: GameState -> Bool
gsIsOnSurface gs = gs^.depth == 0

inventoryLimit :: Int
inventoryLimit = 20

runMaybeExcept :: Except a () -> Maybe a
runMaybeExcept action =
  case runExcept action of
    Left value -> Just value
    _ -> Nothing

gmAddItemInventory :: Item -> GameState -> Maybe GameState
gmAddItemInventory item gs =
  if length (gs^.player.playerInventory) >= inventoryLimit
    then Nothing
    else Just $ gs & player.playerInventory %~ (item:)

gmAttemptPurchase :: GameState -> Maybe GameState
gmAttemptPurchase gs = do
  item_selection <- gsCurrentVendorItemSelection gs
  let playerpos = gs^.player.playerPosition
      shells = gs^.player.playerShells

  guard (shells >= itemPrice item_selection)

  if isItemBulky item_selection
    then do
      -- Cannot buy bulky item if dragging something
      guard (isNothing $ gs^.player.playerDragging)
      -- Cannot buy bulky item if bulky item already on floor
      guard (isNothing $ gs^.glBulkyItemAt playerpos)

      -- Put bulky item for dragging
      return $ gs & (player.playerShells -~ itemPrice item_selection) .
                    (player.playerDragging .~ Just item_selection)

    else return $ case gs^.to (gmAddItemInventory item_selection) of
           Nothing -> gs
           Just new_gs -> new_gs & player.playerShells -~ itemPrice item_selection

gmCurrentVendorCreature :: GameState -> Maybe Creature
gmCurrentVendorCreature gs = runMaybeExcept $ do
  let playerpos = gs^.player.playerPosition
      lvl = gs^.glCurrentLevel

  for_ (allNeighbours playerpos) $ \candidate_pos ->
    case lvl^.creatures.at candidate_pos of
      Just FoodVendor -> throwE FoodVendor
      Just AmmoVendor -> throwE AmmoVendor
      Just MaterialVendor -> throwE MaterialVendor
      Just ToolVendor -> throwE ToolVendor
      _ -> return ()

gmActiveMenuHandler :: GameState -> Maybe ItemMenuHandler
gmActiveMenuHandler gs =
  ch Inventory <|> ch Pickup
 where
  ch ms = case gs^.menuState.at ms of
    Nothing -> Nothing
    Just _index -> menuItemHandler ms

gmActiveMenu :: GameState -> Maybe MenuState
gmActiveMenu gs =
  if | Inventory `M.member` menus -> Just Inventory
     | Pickup    `M.member` menus -> Just Pickup

     | otherwise -> Nothing
 where
  menus = gs^.menuState

gmCloseMenu :: GameState -> Maybe GameState
gmCloseMenu gs = do
  active_menu <- gmActiveMenu gs
  return $ gs & menuState.at active_menu .~ Nothing

gsEnterMenu :: MenuState -> GameState -> GameState
gsEnterMenu ms = menuState.at ms .~ Just 0

gsCycleGame :: GameState -> GameState
gsCycleGame = execState $ do
  gs <- get

  -- Enter vendor menu if not currently next to vendor.
  case gmCurrentVendorCreature gs of
    Nothing -> glCurrentVendorMenuSelection .= Nothing
    Just _vendor ->
      when (isNothing $ gs^.glCurrentVendorMenuSelection) $
        glCurrentVendorMenuSelection .= Just 0

  -- Inventory selection must be within range
  case gs^.menuState.at Inventory of
    Just _selection -> case gmCurrentlySelectedInventoryItem gs of
      Nothing -> menuState.at Inventory .= Just 0
      _ -> return ()
    _ -> return ()

gsInActiveMenu :: GameState -> Bool
gsInActiveMenu = isJust . gmActiveMenu

gsIsMenuActive :: MenuState -> GameState -> Bool
gsIsMenuActive ms gs = gs^.menuState.to (ms `M.member`)

menuKeyToMenuStateTrigger :: Char -> Maybe MenuState
menuKeyToMenuStateTrigger ch = go menu_states
 where
  menu_states = enumFrom (toEnum 0)

  go [] = Nothing
  go (candidate:rest) = case menuItemHandler candidate of
    Just handler | ch `S.member` triggerKeys handler -> Just candidate
    _ -> go rest

gmCurrentlySelectedItem :: GameState -> Maybe Item
gmCurrentlySelectedItem gs = do
  handler <- gmActiveMenuHandler gs
  let items = filter (menuFilter handler) (gs^.itemLens handler)
      gitems = groupItems items

  guard (not $ null items)

  let menu_selection = fromMaybe 0 (gs^.selectionLens handler)
      max_selection = M.size gitems-1
      current_selection = max 0 $ min max_selection menu_selection

  return $ fst $ M.assocs gitems !! current_selection

currentlySelectedMenuItemLens :: MenuState -> Lens' GameState [Item] -> Getter GameState (Maybe Item)
currentlySelectedMenuItemLens menu_state item_get = to $ \gs -> do
  selection <- gs^.menuState.at menu_state
  let items = gs^.item_get.to groupItems.to M.assocs
  guard (selection >= 0 && selection < length items)
  return $ fst $ items !! selection

gmCurrentlySelectedInventoryItem :: GameState -> Maybe Item
gmCurrentlySelectedInventoryItem gs =
  gs^.currentlySelectedMenuItemLens Inventory (player.playerInventory)

gmDropInventoryItem :: GameState -> Maybe GameState
gmDropInventoryItem gs = do
  selected_inventory_item <- gmCurrentlySelectedInventoryItem gs
  return $ gs & (player.playerInventory %~ delete selected_inventory_item) .
                (glItemsAt (gs^.player.playerPosition) %~ (:) selected_inventory_item)


gmPickUpItem :: GameState -> Maybe GameState
gmPickUpItem gs = do
  guard (gmActiveMenu gs == Just Pickup)
  handler <- gmActiveMenuHandler gs
  item <- gmCurrentlySelectedItem gs

  new_gs <- gs^.to (gmAddItemInventory item)
  return $ new_gs & (itemLens handler %~ delete item)

menuItemHandler :: MenuState -> Maybe ItemMenuHandler
menuItemHandler Inventory = Just ItemMenuHandler
  { triggerKeys = S.fromList "i"
  , offKeys = S.fromList "i"
  , menuKeys = M.fromList [('d', ("Drop", \gs -> gs^.to gmDropInventoryItem))]
  , prerequisites = not . gsInActiveMenu
  , selectionLens = menuState.at Inventory
  , menuFilter = const True
  , itemLens = player.playerInventory }
menuItemHandler Pickup = Just ItemMenuHandler
  { triggerKeys = S.fromList ","
  , offKeys = S.empty
  , menuKeys = M.fromList [(',', ("Pick up", \gs -> gs^.to gmPickUpItem))]

  , prerequisites = \gs -> not (gsInActiveMenu gs) && not (null $ gs^.itemsAtPlayer)
  , selectionLens = menuState.at Pickup
  , menuFilter = not . isItemBulky
  , itemLens = itemsAtPlayer }
menuItemHandler _ = Nothing

gsIsVendoring :: GameState -> Bool
gsIsVendoring gs = isJust $ gs^.menuState.at Vendor

