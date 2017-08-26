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
  , gsAdvanceInputTurn
  , gsRetractInputTurn
  , gsCycleGame
  , gsTurn
  , gmMoveToDirection
  -- * Messages
  , gsAddMessage
  , gsCurrentMessage
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
  , gsInActiveMenu
  , gsIsMenuActive
  , gmActiveMenuHandler
  , gmActiveMenu
  , gmCurrentSelectMode
  , gmCurrentlySelectedItems
  , gmCurrentlySelectedSingleItem
  , gmCurrentlySelectedSingleInventoryItem
  , gmCloseMenu
  , gmEnterMenu
  , gmSelectToggleCursorItem
  , ActiveMenuState(..)
  , SelectMode(..)
  , gmMenuCursor
  , gmMenuSelections
  , gmIncreaseMenuCursor
  , gmDecreaseMenuCursor
  -- ** Menu utilities
  , menuKeyToMenuStateTrigger
  , menuItemHandler
  , ItemMenuHandler(..)
  , singleSelection
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
  , _activeMenuState = M.empty
  , _activeMenuInventory = []
  , _vendorMenu      = Nothing
  , _turn = 1
  , _inputTurn = 1
  , _levels = M.singleton 0 surfaceLevel
  , _messages = M.empty
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

glCurrentVendorMenuSelection :: Lens' GameState (Maybe Int)
glCurrentVendorMenuSelection = vendorMenu

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

gsAdvanceInputTurn :: GameState -> GameState
gsAdvanceInputTurn = inputTurn +~ 1

gsRetractInputTurn :: GameState -> GameState
gsRetractInputTurn = inputTurn -~ 1

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

  let add_purchase_text item = gsAddMessage ("Purchased " <> itemName item Singular <> " for " <> show (itemPrice item) <> " shells.") :: GameState -> GameState

  if isItemBulky item_selection
    then do
      -- Cannot buy bulky item if dragging something
      guard (isNothing $ gs^.player.playerDragging)
      -- Cannot buy bulky item if bulky item already on floor
      guard (isNothing $ gs^.glBulkyItemAt playerpos)

      -- Put bulky item for dragging
      return $ (gs & ((player.playerShells -~ itemPrice item_selection) .
                      (player.playerDragging .~ Just item_selection)))
                   ^.to (add_purchase_text item_selection)

    else return $ case gs^.to (gmAddItemInventory item_selection) of
           Nothing -> gs
           Just new_gs ->
             (new_gs & (player.playerShells -~ itemPrice item_selection))
                     ^.to (add_purchase_text item_selection)

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
  ch Drop <|> ch Inventory <|> ch Pickup <|> ch ContainerPutIn <|> ch ContainerTakeOut
 where
  ch ms = case gs^.activeMenuState.at ms of
    Nothing -> Nothing
    Just _index -> Just $ menuItemHandler ms

gmActiveMenu :: GameState -> Maybe ActiveMenuState
gmActiveMenu gs =
  if | Inventory           `M.member` menus -> Just Inventory
     | Drop                `M.member` menus -> Just Drop
     | Pickup              `M.member` menus -> Just Pickup
     | ContainerPutIn      `M.member` menus -> Just ContainerPutIn
     | ContainerTakeOut    `M.member` menus -> Just ContainerTakeOut

     | otherwise -> Nothing
 where
  menus = gs^.activeMenuState

gmCloseMenu :: GameState -> Maybe GameState
gmCloseMenu gs = do
  active_menu <- gmActiveMenu gs
  return $ gs & activeMenuState.at active_menu .~ Nothing

gmEnterMenu :: ActiveMenuState -> GameState -> Maybe GameState
gmEnterMenu ms gs = do
  let handler = menuItemHandler ms
  guard (prerequisites handler gs)

  let initial_selection = case selectMode handler of
                            NotSelectable -> S.empty
                            SingleSelect -> S.singleton 0
                            MultiSelect -> S.empty

  return $ fromMaybe (gs & (activeMenuState.at ms .~ Just (initial_selection, 0)) .
                           (activeMenuState .~ M.empty) .
                           (activeMenuInventory .~ toActiveMenuInventory handler gs)) $
    quickEnterAction handler gs

gsCycleGame :: GameState -> GameState
gsCycleGame = execState $ do
  gs <- get

  -- Enter vendor menu if not currently next to vendor.
  case gmCurrentVendorCreature gs of
    Nothing -> glCurrentVendorMenuSelection .= Nothing
    Just _vendor ->
      when (isNothing $ gs^.glCurrentVendorMenuSelection) $
        glCurrentVendorMenuSelection .= Just 0

gsInActiveMenu :: GameState -> Bool
gsInActiveMenu = isJust . gmActiveMenu

gsIsMenuActive :: ActiveMenuState -> GameState -> Bool
gsIsMenuActive ms gs = gs^.activeMenuState.to (ms `M.member`)

menuKeyToMenuStateTrigger :: Char -> Maybe ActiveMenuState
menuKeyToMenuStateTrigger ch = go menu_states
 where
  menu_states = enumFrom (toEnum 0)

  go [] = Nothing
  go (candidate:rest) =
    if ch `S.member` triggerKeys (menuItemHandler candidate)
      then Just candidate
      else go rest

gmCurrentlySelectedItems :: GameState -> Maybe [Item]
gmCurrentlySelectedItems gs = do
  handler <- gmActiveMenuHandler gs
  case selectMode handler of
    SingleSelect  -> (:[]) <$> gmCurrentlySelectedSingleItem gs
    MultiSelect   -> do
      selected_set <- gs^?activeMenuState.at (menuStateKey handler)._Just._1
      let items = filter (menuFilter handler) (gs^.itemLens handler)
          gitems = M.assocs $ groupItems items

      return $ concatMap (\index ->
        let (item, count) = gitems !! index
         in replicate count item) (S.toList selected_set)

    NotSelectable -> Nothing

gmCurrentlySelectedSingleItem :: GameState -> Maybe Item
gmCurrentlySelectedSingleItem gs = do
  handler <- gmActiveMenuHandler gs
  let items = filter (menuFilter handler) (gs^.itemLens handler)
      gitems = groupItems items

  guard (not $ null items)

  let menu_selection = fromMaybe 0 (gs^?activeMenuState.at (menuStateKey handler)._Just._1.singleSelection)
      max_selection = M.size gitems-1
      current_selection = max 0 $ min max_selection menu_selection

  return $ fst $ M.assocs gitems !! current_selection

singleSelection :: Prism' (S.Set Int) Int
singleSelection = prism' S.singleton $ \sset ->
  if S.size sset == 1
    then Just $ S.findMin sset
    else Nothing

currentlySelectedSingleMenuItemLens :: ActiveMenuState -> Lens' GameState [Item] -> Getter GameState (Maybe Item)
currentlySelectedSingleMenuItemLens menu_state item_get = to $ \gs -> do
  selection <- gs^?activeMenuState.at menu_state._Just._2
  let items = gs^.item_get.to groupItems.to M.assocs
  guard (selection >= 0 && selection < length items)
  return $ fst $ items !! selection

gmCurrentlySelectedSingleInventoryItem :: GameState -> Maybe Item
gmCurrentlySelectedSingleInventoryItem gs =
  gs^.currentlySelectedSingleMenuItemLens Inventory (player.playerInventory)

gmDropItem :: Item -> GameState -> Maybe GameState
gmDropItem item gs = do
  guard (item `elem` (gs^.player.playerInventory))
  return $ gs & (player.playerInventory %~ delete item) .
                (glItemsAt (gs^.player.playerPosition) %~ (:) item)

gmPutInItemsByMenu :: GameState -> Maybe GameState
gmPutInItemsByMenu gs = do
  guard (gmActiveMenu gs == Just ContainerPutIn)
  guard (isJust $ gs^?gllAtPlayer glBulkyItemAt._Just.storageBoxContents)

  items <- gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Put in " <> itemName single_item Singular <> "."
    [] -> ""
    many_items -> "Put in " <> show (length many_items) <> " items.")
 where
  go [] gs = Just gs
  go (item:rest) gs =
    let new_gs = gs & (player.playerInventory %~ delete item) .
                       (gllAtPlayer glBulkyItemAt._Just.storageBoxContents %~ (:) item)

     in go rest new_gs

gmTakeOutItemsByMenu :: GameState -> Maybe GameState
gmTakeOutItemsByMenu gs = do
  guard (gmActiveMenu gs == Just ContainerTakeOut)

  items <- gmCurrentlySelectedItems gs
  
  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Took out " <> itemName single_item Singular <> "."
    [] -> ""
    many_items -> "Took out " <> show (length many_items) <> " items.")
 where
  go [] gs = Just gs
  go (item:rest) gs' = do
    gs <- gmAddItemInventory item gs'

    let old_storage = fromMaybe [] $ gs^?gllAtPlayer glBulkyItemAt._Just.storageBoxContents
        new_storage = delete item old_storage

    guard (new_storage /= old_storage)

    go rest $ gs & gllAtPlayer glBulkyItemAt._Just.storageBoxContents .~ new_storage

gmPickUpItem :: Item -> GameState -> Maybe GameState
gmPickUpItem item gs = do
  let items_on_floor = gs^.itemsAtPlayer
  guard (item `elem` items_on_floor)

  new_gs <- gs^.to (gmAddItemInventory item)
  return $ new_gs & (itemsAtPlayer %~ delete item)

gmPickUpItemByMenu :: GameState -> Maybe GameState
gmPickUpItemByMenu gs = do
  guard (gmActiveMenu gs == Just Pickup)
  items <- gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Picked up " <> itemName single_item Singular <> "."
    [] -> ""
    many_items -> "Picked up " <> show (length many_items) <> " items.")
 where
  go :: [Item] -> GameState -> Maybe GameState
  go [] gs = Just gs
  go (item:rest) gs = gmPickUpItem item gs >>= go rest

gmDropInventoryItemByMenu :: GameState -> Maybe GameState
gmDropInventoryItemByMenu gs = do
  guard (gmActiveMenu gs == Just Drop)
  items <- gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Dropped " <> itemName single_item Singular <> "."
    [] -> ""
    many_items -> "Dropped " <> show (length many_items) <> " items.")
 where
  go :: [Item] -> GameState -> Maybe GameState
  go [] gs = Just gs
  go (item:rest) gs = gmDropItem item gs >>= go rest

gmCurrentSelectMode :: GameState -> Maybe SelectMode
gmCurrentSelectMode gs =
  selectMode <$> gmActiveMenuHandler gs

defaultItemHandler :: ActiveMenuState -> Lens' GameState [Item] -> ItemMenuHandler
defaultItemHandler key item_lens = ItemMenuHandler
  { triggerKeys           = S.empty
  , offKeys               = S.empty
  , menuStateKey          = key
  , selectMode            = NotSelectable
  , menuKeys              = M.empty
  , prerequisites         = const True
  , menuFilter            = const True
  , quickEnterAction      = const Nothing
  , otherKeys             = M.empty
  , toActiveMenuInventory = const []
  , itemLens              = item_lens }

menuItemHandler :: ActiveMenuState -> ItemMenuHandler
menuItemHandler Inventory = (defaultItemHandler Inventory (player.playerInventory))
  { triggerKeys = S.fromList "i"
  , offKeys = S.fromList "qi "
  , selectMode = NotSelectable
  , menuKeys = M.empty
  , prerequisites = not . gsInActiveMenu
  , menuFilter = const True
  , otherKeys = M.fromList [('d', "Drop items")] }

menuItemHandler Drop = (defaultItemHandler Drop (player.playerInventory))
  { triggerKeys = S.fromList "d"
  , offKeys = S.fromList "q"
  , selectMode = MultiSelect
  , menuKeys = M.fromList [('d', ("Drop", (^.to gmDropInventoryItemByMenu)))]
  , prerequisites = \gs -> gmActiveMenu gs == Just Inventory &&
                           not (null $ gs^.player.playerInventory)
  , menuFilter = const True }

menuItemHandler Pickup = (defaultItemHandler Pickup itemsAtPlayer)
  { triggerKeys = S.fromList ","
  , offKeys = S.fromList "q"
  , selectMode = MultiSelect
  , menuKeys = M.fromList [(',', ("Pick up", \gs -> gs^.to gmPickUpItemByMenu))]

  , prerequisites = \gs -> not (gsInActiveMenu gs) && not (null $ gs^.itemsAtPlayer)
  , menuFilter = not . isItemBulky
  , quickEnterAction = \gs -> case filter (not . isItemBulky) $ gs^.itemsAtPlayer of
      [single_item] -> gs ^?
        to (gmPickUpItem single_item)._Just.
        to (gsAddMessage $ "Picked up " <> itemName single_item Singular <> ".")
      _ -> Nothing }

menuItemHandler ContainerTakeOut = (defaultItemHandler ContainerTakeOut activeMenuInventory)
  { triggerKeys = S.fromList "t"
  , offKeys     = S.fromList "q"
  , selectMode  = MultiSelect
  , menuKeys = M.fromList [('t', ("Take out", \gs -> gs^.to gmTakeOutItemsByMenu))]
  , prerequisites = isJust . bulkies
  , menuFilter = not . isItemBulky
  , toActiveMenuInventory = fromMaybe [] . bulkies
  }
 where
  bulkies = firstOf (gllAtPlayer glBulkyItemAt._Just.storageBoxContents)

menuItemHandler ContainerPutIn = (defaultItemHandler ContainerPutIn (player.playerInventory))
  { triggerKeys = S.fromList "p"
  , offKeys     = S.fromList "q"
  , selectMode  = MultiSelect
  , menuKeys = M.fromList [('p', ("Put in", \gs -> gs^.to gmPutInItemsByMenu))]
  , prerequisites = view (player.playerInventory.to (not . null))
  , menuFilter = not . isItemBulky }

gsIsVendoring :: GameState -> Bool
gsIsVendoring gs = isJust $ gs^.vendorMenu

getSelection :: ItemMenuHandler -> GameState -> (Int, Int)
getSelection handler gs =
  let items = filter (menuFilter handler) (gs^.itemLens handler)
      menu_selection = fromMaybe 0 $ gs^?activeMenuState.at (menuStateKey handler)._Just._2
      max_selection = M.size (groupItems items)-1

   in (max_selection, max 0 $ min max_selection menu_selection)

gmCursorModification :: (Int -> Int) -> GameState -> Maybe GameState
gmCursorModification action gs =
  activeMenuCursorModification <|> vendorCursorModification
 where
  activeMenuCursorModification = do
    handler <- gmActiveMenuHandler gs
    let (max_selection, current_selection) = getSelection handler gs
        new_selection = action current_selection

    guard (new_selection >= 0 && new_selection <= max_selection)

    return $ gs & activeMenuState.at (menuStateKey handler)._Just._2 .~ new_selection

  vendorCursorModification = do
    old_selection <- gs^.vendorMenu
    vendor <- gmCurrentVendorCreature gs

    let vitems = vendorItems vendor
        gitems = groupItems vitems
        max_selection = M.size gitems-1
        new_selection = action old_selection

    guard (new_selection >= 0 && new_selection <= max_selection)

    return $ gs & vendorMenu .~ Just new_selection

gmMenuSelections :: GameState -> Maybe (S.Set Int)
gmMenuSelections gs = do
  handler <- gmActiveMenuHandler gs
  guard (selectMode handler == MultiSelect)
  gs^?activeMenuState.at (menuStateKey handler)._Just._1

gmMenuCursor :: GameState -> Maybe Int
gmMenuCursor gs = gmActiveMenuHandler gs <&> snd . flip getSelection gs

gmSelectToggleCursorItem :: GameState -> Maybe GameState
gmSelectToggleCursorItem gs = do
  handler <- gmActiveMenuHandler gs
  cursor <- gmMenuCursor gs
  return $ gs & activeMenuState.at (menuStateKey handler)._Just._1 %~ \set ->
    if S.member cursor set
      then S.delete cursor set
      else S.insert cursor set

gmIncreaseMenuCursor :: GameState -> Maybe GameState
gmIncreaseMenuCursor = gmCursorModification (+1)

gmDecreaseMenuCursor :: GameState -> Maybe GameState
gmDecreaseMenuCursor = gmCursorModification (\x -> x - 1)

gsAddMessage :: Text -> GameState -> GameState
gsAddMessage message gs =
  gs & messages.at (gs^.inputTurn) %~ Just . (<> " " <> message) . fromMaybe ""

gsCurrentMessage :: GameState -> Text
gsCurrentMessage gs = fromMaybe "" $ gs^.messages.at (gs^.inputTurn)

