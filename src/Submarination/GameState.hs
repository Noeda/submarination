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
  , gsWait
  , gsDepth
  , gsIsDead
  , gsDeathReason
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
  , glCreatureAt
  , glItemsAt
  -- ** Level items
  , gmAddItemInventory
  , geAddItemInventory
  , glBulkyItemAt
  , inventoryLimit
  -- * Predefined levels
  , surfaceLevel
  -- * Statuses
  , gsCurrentStatuses
  , gsSanitizedCurrentStatuses
  , gsIsSlow
  , gsIsHungry
  , gsIsStarving
  , gsIsSatiated
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
  , subEnergy
  , subMaxEnergy
  , gsIsSubLocation
  , gsPlayerIsInsideSub
  , geStartDiving
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
  , gmCurrentlySelectedCount
  , gmCloseMenu
  , gmEnterMenu
  , gmInsertMenuDigit
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
  , glCurrentVendorMenuSelection

  -- ** Tests
  , tests )
  where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Lens hiding ( Level, levels, Index )
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List ( (!!), delete )
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Linear.V2
import qualified Prelude as E
import Protolude hiding ( (&), to )
import Test.Framework

import Submarination.Biome.IntertidalZone
import Submarination.Creature
import Submarination.Direction
import Submarination.GameState.Types
import Submarination.Index
import Submarination.Item hiding ( tests )
import Submarination.Level
import Submarination.Random
import Submarination.Sub
import Submarination.Turn
import Submarination.Vendor

type GameMonad m = StateT GameState m
type GameMonadRo m = ReaderT GameState m

initialGameState :: GameState
initialGameState = gsIndexUnindexedCreatures $ GameState
  { _player = Player { _playerPosition = V2 15 8
                     , _playerMaximumHealth = 100
                     , _playerHealth = 100
                     , _playerOxygen = 100
                     , _playerShells = 1000
                     , _playerHunger = initialHungerLevel
                     , _playerInventory = []
                     , _playerDragging = Nothing }
  , _activeMenuState = M.empty
  , _activeMenuInventory = []
  , _activeMenuCounter = Nothing
  , _dead            = False
  , _deathReason     = "Spontaneous death"
  , _vendorMenu      = Nothing
  , _turn = turn1
  , _inputTurn = turn1
  , _runningIndex = firstIndex
  , _godMode = False
  , _miscObjects = M.empty
  , _levels = M.fromList
                [(0, surfaceLevel)
                ,(50, rebase (V2 70 70) intertidalZone)]
  , _messages = M.empty
  , _sub = Sub { _subPosition = V2 23 (-2)
               , _subTopology = initial_sub_topo
               , _subDiving   = False
               , _subEnergy   = 100 }
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

  placeStorageBox = subItemsP (V2 6 1) .~ [itemFromType turn1 $ StorageBox []]

initialHungerLevel :: Int
initialHungerLevel = 800

subMaxEnergy :: Sub -> Int
subMaxEnergy _ = 100

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
gsCurrentStatuses gs = mconcat
  [check gsIsSlow     Slow
  ,check gsIsHungry   Hungry
  ,check gsIsStarving Starving
  ,check gsIsSatiated Satiated]
 where
  check fun stat = if fun gs then S.singleton stat else S.empty

-- | Same as `gsCurrentStatuses` but redundant Statuses will be removed. That
-- means you won't get Hungry and Starving together, only either Hungry or
-- Starving or neither of them.
gsSanitizedCurrentStatuses :: GameState -> S.Set Status
gsSanitizedCurrentStatuses gs =
  if Hungry `S.member` stats && Starving `S.member` stats
    then S.delete Hungry stats
    else stats
 where
  stats = gsCurrentStatuses gs

statusName :: Status -> Text
statusName Slow = "Slow"
statusName Hungry = "Hungry"
statusName Starving = "Starving"
statusName Satiated = "Satiated"

gsIsSlow :: GameState -> Bool
gsIsSlow gs =
  (isJust $ gs^.player.playerDragging) ||
  (gs^.player.playerHunger <= 100)

gsIsSatiated :: GameState -> Bool
gsIsSatiated gs = gs^.player.playerHunger > initialHungerLevel

gsIsHungry :: GameState -> Bool
gsIsHungry gs = gs^.player.playerHunger <= 500

gsIsStarving :: GameState -> Bool
gsIsStarving gs = gs^.player.playerHunger <= 150

glCurrentVendorMenuSelection :: Lens' GameState (Maybe Int)
glCurrentVendorMenuSelection = vendorMenu

gsCurrentVendorItemSelection :: GameState -> Maybe ItemType
gsCurrentVendorItemSelection gs = do
  vendor <- gmCurrentVendorCreature gs
  current_selection' <- gs^.glCurrentVendorMenuSelection

  let current_selection = max 0 $ min (length items-1) current_selection'
      items = vendorItems $ creatureType vendor

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

gsTurn :: GameState -> Turn
gsTurn gs = gs^.turn

gsAdvanceInputTurn :: GameState -> GameState
gsAdvanceInputTurn = inputTurn %~ nextTurn

gsRetractInputTurn :: GameState -> GameState
gsRetractInputTurn = inputTurn %~ previousTurn

gsIsSubLocation :: V2 Int -> GameState -> Bool
gsIsSubLocation pos gs =
  isJust $ firstOf (subCellP (pos - gs^.sub.subPosition)) (gs^.sub.subTopology)
{-# INLINEABLE gsIsSubLocation #-}

gsPlayerIsInsideSub :: GameState -> Bool
gsPlayerIsInsideSub gs =
  gsIsSubLocation (gs^.player.playerPosition) gs
{-# INLINEABLE gsPlayerIsInsideSub #-}

geStartDiving :: GameState -> Failing GameState
geStartDiving gs = do
  guardE (gsPlayerIsInsideSub gs) "You are not inside submarine."
  guardE (gs^.sub.subEnergy >= 15) "Not enough energy."
  return $ gs & (sub.subDiving .~ True) .
                (sub.subEnergy -~ 15)

gsDeathCheck :: Text -> GameState -> GameState
gsDeathCheck death_reason gs =
  if gs^.player.playerHealth <= 0
    then gs & (dead .~ True) .
              (deathReason .~ death_reason)
    else gs

gsAdvanceTurn :: GameState -> GameState
gsAdvanceTurn gs | gs^.dead = gs
gsAdvanceTurn gs =
  runST $ flip execStateT gs $ runWithRandomSupply (fromIntegral $ hash $ gs^.turn) go
 where
  go :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  go = do
    turn %= nextTurn

    godModeCheck

    -- Make submarine go deeper if it's diving
    is_diving <- use $ sub.subDiving
    when is_diving $ do
      depth += 1
      modify gsRemoveCreaturesUnderSub

    curdepth <- use depth
    when (curdepth >= 50) $
      sub.subDiving .= False

    -- Oxygen runs out if you are not on surface and outside submarine
    ((,) <$> use (to gsIsOnSurface) <*> use (to gsPlayerIsInsideSub)) >>= \case
      (False, False) -> player.playerOxygen -= 1
      _ -> do
        player.playerOxygen %= max (-1)
        player.playerOxygen += 1

    -- Clamp oxygen
    gs <- get
    player.playerOxygen %= max (-50) . min (gsMaximumOxygenLevel gs)

    -- Hunger tick
    unless (gsIsOnSurface gs) $
      player.playerHunger -= 1

    gs <- get
    when (gs^.player.playerHunger <= 0) $ do
      player.playerHealth .= 0
      modify $ gsDeathCheck "Starvation"

    oxygen <- use $ player.playerOxygen
    when (oxygen <= 0) $ do
      player.playerHealth -= 1
      modify $ gsDeathCheck "Asphyxia"

    walkCreatures
    walkActiveMetadata
    godModeCheck

  godModeCheck :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  godModeCheck = do
    god_mode <- use godMode
    when god_mode $ do
      gs <- get
      player.playerHealth .= (gs^.player.playerMaximumHealth)
      player.playerOxygen .= (gs^.to gsMaximumOxygenLevel)

  walkActiveMetadata :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkActiveMetadata = do
    walkCurrentLevelMetadata
    walkSubLevelMetadata

  walkCreatures :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkCreatures = do
    walkSubLevelCreatures
    walkCurrentLevelCreatures

  walkCurrentLevelCreatures :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkCurrentLevelCreatures = do
    lvl <- use glCurrentLevel
    void $ walkCreaturesLevel (lvl, V2 0 0)

  walkSubLevelCreatures :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkSubLevelCreatures = do
    topo <- use $ sub.subTopology
    subpos <- use $ sub.subPosition
    void $ forOf (subLevelsWithOffset subpos) topo walkCreaturesLevel

  walkCurrentLevelMetadata :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkCurrentLevelMetadata = do
    lvl <- use glCurrentLevel
    new_level <- walkLevel (lvl, V2 0 0)
    glCurrentLevel .= new_level

  walkSubLevelMetadata :: forall s. RandomSupplyT (StateT GameState (ST s)) ()
  walkSubLevelMetadata = do
    topo <- use $ sub.subTopology
    subpos <- use $ sub.subPosition
    new_sub_topo <- forOf (subLevelsWithOffset subpos) topo walkLevel
    sub.subTopology .= new_sub_topo

  walkLevel :: forall s. (Level, V2 Int) -> RandomSupplyT (StateT GameState (ST s)) Level
  walkLevel (lvl, offset) = do
    current_turn <- use turn
    playerpos <- use $ player.playerPosition

    walkLevelActiveMetadata lvl $ \coords cell metadata -> return $ case (cell, metadata) of
      (OpenHatch, HatchAutoClose close_turn) | close_turn <= current_turn && null (lvl^.itemsAt coords) && playerpos /= coords + offset ->
        (Hatch, Nothing)
      (OpenHatch, metadata@HatchAutoClose{}) ->
        (OpenHatch, Just metadata)
      (feature, _) -> (feature, Nothing)

  walkCreaturesLevel :: forall s. (Level, V2 Int) -> RandomSupplyT (StateT GameState (ST s)) Level
  walkCreaturesLevel (lvl, offset) = do
    flip evalStateT IS.empty $ ifor_ (lvl^.creatures) $ \pos' (creatureIndex -> index) -> do
      let pos = pos' + offset
      sset <- get
      unless (toInt index `IS.member` sset) $
        lift (use (glCreatureAt pos)) >>= \case
          Just creature | creatureIndex creature == index -> do
            modify $ IS.insert (toInt $ creatureIndex creature)
            lift $ cycleCreature pos creature
          _ -> return ()
    return lvl

cycleCreature :: (MonadRandomSupply m, MonadState s m, HasGameState s)
              => V2 Int
              -> Creature
              -> m ()
cycleCreature pos creature | isAnimal (creatureType creature) = go (4 :: Int)
 where
  go 0 = return ()
  go tries = do
    dir <- randomDirection
    gs <- get
    playerpos <- use $ gameState.player.playerPosition

    let new_pos = move1V2 dir pos
        is_okay = isNothing (gs^.gameState.glCreatureAt new_pos) &&
                  isWalkable (gs^.gameState.glCellAt new_pos) &&
                  new_pos /= playerpos

    if is_okay
      then do gameState.glCreatureAt pos .= Nothing
              gameState.glCreatureAt new_pos .= Just creature

      else go (tries-1)
cycleCreature _ _ = return ()
{-# INLINEABLE cycleCreature #-}

gsWait :: GameState -> GameState
gsWait = gsAdvanceTurn

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

    -- Do not open hatch if it's in airlock and sub is moving
    case isAirLock <$> getAtomTopologyAt (new_playerpos - gs^.sub.subPosition) (gs^.sub.subTopology) of
      Just True -> guard (not $ gs^.sub.subDiving)
      _ -> return ()

    current_turn <- use turn

    glCellAt new_playerpos .= OpenHatch
    glActiveMetadataAt new_playerpos .= (Just $ HatchAutoClose (nextNTurn 5 current_turn))
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
  ,".........^,.......=.===.=======..,,........,..........."
  ,"...,,............================.,,..,...,............"
  ,"...........#,...================.=,,.#,..^,............"
  ,"....,..#...^^,,====================,....##,......,....."
  ,"...#.......##,,########g=g#########,,.................."
  ,"......###########^^#^#^gg=#^^##################,......."
  ,"#######################=gg##^##########################"
  ,"#####################g1ggg2##^^^^######################"
  ,"####^^^^#######^^^###gggggg##^^########^^^#############"
  ,"###^^###########^^##^ggggggg###########################"
  ,"################^^##^gggggg4##############^############"
  ,"########^^^#####^^###g3gggg###^#####^##################"
  ,"#################^####^ggg####^###^^############^######"
  ,"#################^##^##gg##^^#^###^^###################"
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
    [(1, ('g', placeCreature $ creatureFromType FoodVendor))
    ,(2, ('g', placeCreature $ creatureFromType AmmoVendor))
    ,(3, ('g', placeCreature $ creatureFromType ToolVendor))
    ,(4, ('g', placeCreature $ creatureFromType MaterialVendor))]

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
gsCurrentAreaName gs =
  if gs^.depth == 0
    then "Surface"
    else "Depth " <> show (gs^.depth) <> "m"

gsIsOccupied :: V2 Int -> GameState -> Bool
gsIsOccupied coords gamestate = isJust $ gamestate^.glCreatureAt coords

gsIsOnSurface :: GameState -> Bool
gsIsOnSurface gs = gs^.depth == 0

inventoryLimit :: Int
inventoryLimit = 20

runMaybeExcept :: Except a () -> Maybe a
runMaybeExcept action =
  case runExcept action of
    Left value -> Just value
    _ -> Nothing

geAddItemInventory :: Item -> GameState -> Failing GameState
geAddItemInventory item gs =
  if length (gs^.player.playerInventory) >= inventoryLimit
    then Left "Inventory full"
    else Right $ gs & player.playerInventory %~ (item:)

gmAddItemInventory :: Item -> GameState -> Maybe GameState
gmAddItemInventory item gs = toMaybe $ geAddItemInventory item gs

gmAttemptPurchase :: GameState -> Maybe GameState
gmAttemptPurchase gs = do
  item_type_selection <- gsCurrentVendorItemSelection gs
  let curturn = gs^.turn
      item_selection = itemFromType curturn item_type_selection
      playerpos = gs^.player.playerPosition
      shells = gs^.player.playerShells

  guard (shells >= itemPrice item_selection)

  let add_purchase_text item = gsAddMessage ("Purchased " <> itemName (gs^.turn) item Singular <> " for " <> show (itemPrice item) <> " shells.") :: GameState -> GameState

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

  for_ (allNeighbours playerpos) $ \candidate_pos ->
    case gs^.glCreatureAt candidate_pos of
      Just creature | isVendor (creatureType creature) -> throwE creature
      _ -> return ()

gmActiveMenuHandler :: GameState -> Maybe ItemMenuHandler
gmActiveMenuHandler gs =
  ch Drop <|>
  ch Eat <|>
  ch Inventory <|>
  ch Pickup <|>
  ch ContainerPutIn <|>
  ch ContainerTakeOut <|>
  ch StartDive <|>
  ch MicrowaveMenu
 where
  ch ms = case gs^.activeMenuState.at ms of
    Nothing -> Nothing
    Just _index -> Just $ menuItemHandler ms

gmActiveMenu :: GameState -> Maybe ActiveMenuState
gmActiveMenu gs =
  if | Inventory           `M.member` menus -> Just Inventory
     | Drop                `M.member` menus -> Just Drop
     | Eat                 `M.member` menus -> Just Eat
     | Pickup              `M.member` menus -> Just Pickup
     | MicrowaveMenu       `M.member` menus -> Just MicrowaveMenu
     | ContainerPutIn      `M.member` menus -> Just ContainerPutIn
     | ContainerTakeOut    `M.member` menus -> Just ContainerTakeOut
     | StartDive           `M.member` menus -> Just StartDive

     | otherwise -> Nothing
 where
  menus = gs^.activeMenuState

gmCloseMenu :: GameState -> Maybe GameState
gmCloseMenu gs = do
  active_menu <- gmActiveMenu gs
  return $ gs & activeMenuState.at active_menu .~ Nothing

gmInsertMenuDigit :: Int -> GameState -> Maybe GameState
gmInsertMenuDigit digit gs = do
  ms <- gmActiveMenu gs
  let handler = menuItemHandler ms
  guard (selectMode handler == MultiSelect)

  return $ gs & activeMenuCounter %~ Just . \case
    Nothing -> digit
    Just old_digit ->
      let new_value = old_digit*10 + digit

       in if new_value < old_digit
            then maxBound
            else new_value

gmEnterMenu :: ActiveMenuState -> GameState -> Maybe GameState
gmEnterMenu ms gs = do
  let handler = menuItemHandler ms
  guard (prerequisites handler gs)

  let initial_selection = case selectMode handler of
                            NotSelectable -> M.empty
                            SingleSelect -> M.singleton 0 1
                            MultiSelect -> M.empty

  return $ fromMaybe (gs & (activeMenuState.at ms .~ Just (initial_selection, 0)) .
                           (activeMenuState .~ M.empty) .
                           (activeMenuInventory .~ toActiveMenuInventory handler gs) .
                           (activeMenuCounter .~ Nothing)) $
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

menuKeyToMenuStateTrigger :: GameState -> Char -> Maybe ActiveMenuState
menuKeyToMenuStateTrigger gs ch = go menu_states
 where
  menu_states = enumFrom (toEnum 0)

  go [] = Nothing
  go (candidate:rest) =
    if ch `S.member` triggerKeys (menuItemHandler candidate) &&
       prerequisites (menuItemHandler candidate) gs
      then Just candidate
      else go rest

gmCurrentlyAvailableItems :: GameState -> Maybe [Item]
gmCurrentlyAvailableItems gs = do
  handler <- gmActiveMenuHandler gs
  let items = filter (menuFilter handler gs) (gs^.itemLens handler)
  return items

gmCurrentlySelectedItems :: GameState -> Maybe [Item]
gmCurrentlySelectedItems gs = do
  handler <- gmActiveMenuHandler gs
  case selectMode handler of
    SingleSelect  -> (:[]) <$> gmCurrentlySelectedSingleItem gs
    MultiSelect   -> do
      selected_set' <- gs^?activeMenuState.at (menuStateKey handler)._Just._1
      let items = filter (menuFilter handler gs) (gs^.itemLens handler)
          gitems = M.assocs $ groupItems items
          selected_set = M.filterWithKey (\idx _ -> idx < length gitems) selected_set'

      return $ concatMap (\(index, num_selected) ->
        let (item, _) = gitems !! index
         in replicate num_selected item) (M.assocs selected_set)

    NotSelectable -> Nothing

gmCurrentlySelectedSingleItem :: GameState -> Maybe Item
gmCurrentlySelectedSingleItem gs = do
  handler <- gmActiveMenuHandler gs
  let items = filter (menuFilter handler gs) (gs^.itemLens handler)
      gitems = groupItems items

  guard (not $ null items)

  let menu_selection = fromMaybe 0 (gs^.to gmMenuCursor)
      max_selection = M.size gitems-1
      current_selection = max 0 $ min max_selection menu_selection

  guard (current_selection <= max_selection)

  return $ fst $ M.assocs gitems !! current_selection

singleSelection :: Prism' (M.Map Int Int) Int
singleSelection = prism' (`M.singleton` 1) $ \sset ->
  if M.size sset == 1
    then case M.findMin sset of
           (key, 1) -> Just key
           _ -> Nothing
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

geEatItem :: Item -> GameState -> Failing GameState
geEatItem item gs = do
  guardE (not $ gsIsSatiated gs) "Too satiated to eat."
  guardE (item `elem` (gs^.player.playerInventory)) "No item in inventory"
  guardE (gs^.player.playerHealth > 0) "Cannot eat if you are dead"

  return $ if isSpoiled (gsTurn gs) item
    then gs & (player.playerHealth %~ max 1) .
              (player.playerHealth -~ 20) .
              (player.playerInventory %~ delete item) .
              (player.playerHunger +~ (itemNutrition item `div` 3))
    else gs & (player.playerInventory %~ delete item) .
              (player.playerHunger +~ (itemNutrition item))

geDropItem :: Item -> GameState -> Failing GameState
geDropItem item gs = do
  guardE (item `elem` (gs^.player.playerInventory)) "No item in inventory"
  return $ gs & (player.playerInventory %~ delete item) .
                (glItemsAt (gs^.player.playerPosition) %~ (:) item)

gePutInItemsByMenu :: GameState -> Failing GameState
gePutInItemsByMenu gs = do
  guardE (gmActiveMenu gs == Just ContainerPutIn) ""
  guardE (isJust $ firstOf (gllAtPlayer glBulkyItemAt._Just.itemContents) gs) "There is no container here"
  items <- toFailing $ gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Put in " <> itemName (gs^.turn) single_item Singular <> "."
    [] -> ""
    many_items -> "Put in " <> show (length many_items) <> " items.")
 where
  go [] gs = pure gs
  go (item:rest) gs = do
    container <- toFailing $ gs^.gllAtPlayer glBulkyItemAt
    new_container <- putItem (gs^.turn) item container

    let new_gs = gs & (player.playerInventory %~ delete item) .
                      (gllAtPlayer glBulkyItemAt .~ Just new_container)
     in go rest new_gs

geTakeOutItemsByMenu :: GameState -> Failing GameState
geTakeOutItemsByMenu gs = do
  guardE (gmActiveMenu gs == Just ContainerTakeOut) ""
  items <- toFailing $ gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Took out " <> itemName (gs^.turn) single_item Singular <> "."
    [] -> ""
    many_items -> "Took out " <> show (length many_items) <> " items.")
 where
  go [] gs = pure gs
  go (item:rest) gs' = do
    container <- toFailing $ gs'^.gllAtPlayer glBulkyItemAt
    (new_container, removed_item) <- removeItem (gs'^.turn) item container
    gs <- geAddItemInventory removed_item gs'
    go rest $ gs & gllAtPlayer glBulkyItemAt .~ Just new_container

gmPickUpItem :: Item -> GameState -> Maybe GameState
gmPickUpItem item gs = toMaybe $ gePickUpItem item gs

gePickUpItem :: Item -> GameState -> Failing GameState
gePickUpItem item gs = do
  let items_on_floor = gs^.itemsAtPlayer
  guardE (item `elem` items_on_floor) "No such item here"

  new_gs <- gs^.to (geAddItemInventory item)
  return $ new_gs & (itemsAtPlayer %~ delete item)

gePickUpItemByMenu :: GameState -> Failing GameState
gePickUpItemByMenu gs = do
  guardE (gmActiveMenu gs == Just Pickup) ""
  items <- toFailing $ gmCurrentlySelectedItems gs

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Picked up " <> itemName (gs^.turn) single_item Singular <> "."
    [] -> ""
    many_items -> "Picked up " <> show (length many_items) <> " items.")
 where
  go :: [Item] -> GameState -> Failing GameState
  go [] gs = pure gs
  go (item:rest) gs = gePickUpItem item gs >>= go rest

geEatInventoryItemByMenu :: GameState -> Failing GameState
geEatInventoryItemByMenu gs = do
  guardE (gmActiveMenu gs == Just Eat) ""
  items' <- addFail (gmCurrentlySelectedItems gs) "No currently selected items."
  let edibles = filter isEdible items'
  guardE (not $ null edibles) "No currently selected edible items."
  guardE (length edibles == 1) "Cannot eat more than one edible at a time."

  let [edible] = edibles
  geEatItem edible gs <&> gsAddMessage ("Ate " <> itemName (gs^.turn) edible Singular <> ".")

geDropInventoryItemByMenu :: GameState -> Failing GameState
geDropInventoryItemByMenu gs = do
  guardE (gmActiveMenu gs == Just Drop) ""
  items <- addFail (gmCurrentlySelectedItems gs) "No currently selected items."

  go items gs <&> gsAddMessage (case items of
    [single_item] -> "Dropped " <> itemName (gs^.turn) single_item Singular <> "."
    [] -> ""
    many_items -> "Dropped " <> show (length many_items) <> " items.")
 where
  go :: [Item] -> GameState -> Failing GameState
  go [] gs = pure gs
  go (item:rest) gs = geDropItem item gs >>= go rest

gmCurrentSelectMode :: GameState -> Maybe SelectMode
gmCurrentSelectMode gs =
  selectMode <$> gmActiveMenuHandler gs

defaultItemHandler :: ActiveMenuState -> Lens' GameState [Item] -> ItemMenuHandler
defaultItemHandler key item_lens = ItemMenuHandler
  { triggerKeys           = S.empty
  , offKeys               = S.empty
  , menuName              = "<UNKNOWN>"
  , menuText              = ""
  , menuStateKey          = key
  , selectMode            = NotSelectable
  , menuKeys              = M.empty
  , prerequisites         = const True
  , menuFilter            = (\_ _ -> True)
  , quickEnterAction      = const Nothing
  , otherKeys             = const M.empty
  , toActiveMenuInventory = const []
  , itemLens              = item_lens }

anyItemMenuFilter :: GameState -> Item -> Bool
anyItemMenuFilter _ _ = True

menuTransition :: GameState -> Char -> Text -> ActiveMenuState -> [(Char, Text)]
menuTransition gs ch text ams =
  if prerequisites (menuItemHandler ams) gs
    then [(ch, text)]
    else []

menuItemHandler :: ActiveMenuState -> ItemMenuHandler
menuItemHandler Inventory = (defaultItemHandler Inventory (player.playerInventory))
  { triggerKeys = S.fromList "i"
  , menuName = "Inventory"
  , offKeys = S.fromList "qi "
  , selectMode = NotSelectable
  , menuKeys = M.empty
  , prerequisites = \gs -> not (gsInActiveMenu gs) &&
                           not (null $ gs^.player.playerInventory)
  , menuFilter = anyItemMenuFilter
  , otherKeys = \gs -> M.fromList $
                  menuTransition gs 'd' "Drop items" Drop <>
                  menuTransition gs 'e' "Eat" Eat
  }

menuItemHandler Eat = (defaultItemHandler Eat (player.playerInventory))
  { triggerKeys = S.fromList "e"
  , menuName = "Eat"
  , offKeys = S.fromList "q"
  , selectMode = SingleSelect
  , menuKeys = M.fromList [('e', ("Eat", (^.to geEatInventoryItemByMenu)))]
  , prerequisites = \gs -> gmActiveMenu gs == Just Inventory &&
                           not (null $ filter isEdible $ gs^.player.playerInventory)
  , menuFilter = \_ -> isEdible }

menuItemHandler Drop = (defaultItemHandler Drop (player.playerInventory))
  { triggerKeys = S.fromList "d"
  , menuName = "Drop items"
  , offKeys = S.fromList "q"
  , selectMode = MultiSelect
  , menuKeys = M.fromList [('d', ("Drop", (^.to geDropInventoryItemByMenu)))]
  , prerequisites = \gs -> gmActiveMenu gs == Just Inventory &&
                           not (null $ gs^.player.playerInventory)
  , menuFilter = anyItemMenuFilter }

menuItemHandler Pickup = (defaultItemHandler Pickup itemsAtPlayer)
  { triggerKeys = S.fromList ","
  , menuName = "Pick up"
  , offKeys = S.fromList "q"
  , selectMode = MultiSelect
  , menuKeys = M.fromList [(',', ("Pick up", \gs -> gs^.to gePickUpItemByMenu))]

  , prerequisites = \gs -> not (gsInActiveMenu gs) && not (null $ gs^.itemsAtPlayer)
  , menuFilter = \_ -> not . isItemBulky
  , quickEnterAction = \gs -> case filter (not . isItemBulky) $ gs^.itemsAtPlayer of
      [single_item] -> gs ^?
        to (gmPickUpItem single_item)._Just.
        to (gsAddMessage $ "Picked up " <> itemName (gs^.turn) single_item Singular <> ".")
      _ -> Nothing }

menuItemHandler ContainerTakeOut = (defaultItemHandler ContainerTakeOut activeMenuInventory)
  { triggerKeys = S.fromList "t"
  , menuName    = "Take out items"
  , offKeys     = S.fromList "q"
  , selectMode  = MultiSelect
  , menuKeys = M.fromList [('t', ("Take out", \gs -> gs^.to geTakeOutItemsByMenu))]
  , prerequisites = isJust . bulkies
  , menuFilter = \_ -> not . isItemBulky
  , toActiveMenuInventory = fromMaybe [] . bulkies
  }
 where
  bulkies :: GameState -> Maybe [Item]
  bulkies gs = do
    cont <- firstOf (gllAtPlayer glBulkyItemAt._Just.itemContents) gs
    guard (not $ null cont)
    pure cont

menuItemHandler ContainerPutIn = (defaultItemHandler ContainerPutIn (player.playerInventory))
  { triggerKeys = S.fromList "p"
  , menuName    = "Put in items"
  , offKeys     = S.fromList "q"
  , selectMode  = MultiSelect
  , menuKeys = M.fromList [('p', ("Put in", \gs -> gs^.to gePutInItemsByMenu))]
  , prerequisites = \gs -> view (player.playerInventory.to (not . null)) gs &&
                           isJust (gs^?gllAtPlayer glBulkyItemAt._Just.itemContents)
  , menuFilter = \_ -> not . isItemBulky }

menuItemHandler StartDive = (defaultItemHandler StartDive activeMenuInventory)
  { triggerKeys   = S.fromList "d"
  , menuName      = "Dive"
  , offKeys       = S.fromList "q "
  , selectMode    = NotSelectable
  , menuKeys      = M.fromList [('d', ("Dive", \gs -> fmap gsAdvanceTurn $ gs^.to geStartDiving))]
  , prerequisites = \gs -> isNothing (gmActiveMenu gs) &&
                           not (gs^.sub.subDiving) &&
                           (isBridge <$> getAtomTopologyAt (gs^.player.playerPosition - gs^.sub.subPosition) (gs^.sub.subTopology)) == Just True
  , menuFilter    = \_ _ -> False
  , menuText      = "There is no turning back after you dive. Make sure you've spent your money and stocked up properly."
  }

menuItemHandler MicrowaveMenu = (defaultItemHandler MicrowaveMenu activeMenuInventory)
  { triggerKeys   = S.fromList "f"
  , menuName      = "Microwave"
  , offKeys       = S.fromList "q "
  , selectMode    = NotSelectable
  , menuKeys      = M.fromList [('f', ("Microwave", \gs -> fmap gsAdvanceTurn $ gs^.to geMicrowave))]
  , prerequisites = \gs -> isNothing (gmActiveMenu gs) &&
                           gsPlayerIsInsideSub gs &&
                           (isMicrowave <$> (gs^?gllAtPlayer glBulkyItemAt._Just.itemType)) == Just True
  , menuFilter    = \gs item -> isJust (microwaveItem (gsTurn gs) item)
  , menuText      = "These are the items that would be microwaved:"
  , toActiveMenuInventory = \gs -> fromMaybe [] $ gs^?gllAtPlayer glBulkyItemAt._Just.itemContents
  }

geMicrowave :: GameState -> Failing GameState
geMicrowave gs = do
  guardE ((isMicrowave <$> (gs^?gllAtPlayer glBulkyItemAt._Just.itemType)) == Just True) "No microwave nearby."
  guardE (gs^.sub.subEnergy >= 5) "Submarine does not have enough energy."

  let microwaveables = fromMaybe [] $ gs^?gllAtPlayer glBulkyItemAt._Just.itemContents :: [Item]
  guardE (length microwaveables > 0) "There is nothing inside microwave."

  microwaved <- addFail (traverse (microwaveItem (gsTurn gs)) microwaveables) "There is nothing microwaveable inside microwave."

  return $ gs & (sub.subEnergy -~ 5) .
                (gllAtPlayer glBulkyItemAt._Just.itemContents .~ microwaved)

gsIsVendoring :: GameState -> Bool
gsIsVendoring gs = isJust $ gs^.vendorMenu

getSelection :: ItemMenuHandler -> GameState -> (Int, Int)
getSelection handler gs =
  let items = filter (menuFilter handler gs) (gs^.itemLens handler)
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

    let vitems = vendorItems (creatureType vendor)
        gitems = groupItems vitems
        max_selection = M.size gitems-1
        new_selection = action old_selection

    guard (new_selection >= 0 && new_selection <= max_selection)

    return $ gs & vendorMenu .~ Just new_selection

gmMenuSelections :: GameState -> Maybe (M.Map Int Int)
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
  gitems <- groupItems <$> gmCurrentlyAvailableItems gs

  guard (cursor < M.size gitems)
  let max_selection_number = snd $ M.assocs gitems !! cursor
      num_selections = fromMaybe max_selection_number $ min max_selection_number <$> gs^.activeMenuCounter

  return $ gs & (activeMenuCounter .~ Nothing) .
                (activeMenuState.at (menuStateKey handler)._Just._1 %~ \set ->
    if M.member cursor set
      then M.delete cursor set
      else M.insert cursor num_selections set)

gmIncreaseMenuCursor :: GameState -> Maybe GameState
gmIncreaseMenuCursor gs =
  gmCursorModification (+1) gs <&> activeMenuCounter .~ Nothing

gmDecreaseMenuCursor :: GameState -> Maybe GameState
gmDecreaseMenuCursor gs =
  gmCursorModification (\x -> x - 1) gs <&> activeMenuCounter .~ Nothing

gsAddMessage :: Text -> GameState -> GameState
gsAddMessage message gs =
  gs & messages.at (gs^.inputTurn) %~ Just . (<> " " <> message) . fromMaybe ""

gsCurrentMessage :: GameState -> Text
gsCurrentMessage gs = fromMaybe "" $ gs^.messages.at (gs^.inputTurn)

gmCurrentlySelectedCount :: GameState -> Maybe Int
gmCurrentlySelectedCount = (^.activeMenuCounter)

gsIsDead :: GameState -> Bool
gsIsDead = _dead

gsDeathReason :: GameState -> Text
gsDeathReason = (^.deathReason)

glCreatureAt :: V2 Int -> Lens' GameState (Maybe Creature)
glCreatureAt = subOrLevelLens creatureAt subCreatureP
{-# INLINE glCreatureAt #-}

gsRemoveCreaturesUnderSub :: GameState -> GameState
gsRemoveCreaturesUnderSub gs = gs & glCurrentLevel.creatures %~ removeThem
 where
  removeThem = M.filterWithKey (\loc _creature -> not $ gsIsSubLocation loc gs)

-- | Gets the next unique `Index` from `GameState`.
nextGSIndex :: (MonadState s m, HasGameState s) => m Index
nextGSIndex = do
  idx <- use $ gameState.runningIndex
  pure idx <* (gameState.runningIndex .= next idx)

-- | Traversal to EVERY creature in the game
allCreatures :: Traversal' GameState Creature
allCreatures fun gs =
  (\new_levels new_sub -> gs & (levels .~ new_levels) . (sub.subTopology .~ new_sub))
  <$> new_levels <*> new_sub
 where
  new_levels =
    for (gs^.levels) $ \lvl ->
      (\new_creatures -> lvl & creatures .~ new_creatures) <$>
        for (lvl^.creatures) fun

  new_sub =
    forOf subLevels (gs^.sub.subTopology) $ \lvl ->
      (\new_creatures -> lvl & creatures .~ new_creatures) <$>
        for (lvl^.creatures) fun

-- | This walks every creature everywhere in the game and indexes them if they
-- don't have a unique index.
gsIndexUnindexedCreatures :: GameState -> GameState
gsIndexUnindexedCreatures gs = flip execState gs $ do
  new_gs <- forOf allCreatures gs $ \creature ->
    if isInvalidIndex (creatureIndex creature)
      then nextGSIndex <&> flip setIndex creature
      else pure creature
  st_gs <- get
  put $ new_gs & runningIndex .~ (st_gs^.runningIndex)

gsDepth :: GameState -> Int
gsDepth = (^.depth)

-------------------
-- *** TESTS *** --

tests :: [Test]
tests = []

