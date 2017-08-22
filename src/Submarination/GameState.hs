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
import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal
import Submarination.Vendor

data MenuState
  = VendorMenuSelection
  | PickupMenuSelection
  | Inventory
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Sub = Sub
  { _topology :: !SubTopology
  , _subPosition :: !(V2 Int) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data GameState = GameState
  { _player    :: !Player
  , _sub       :: !Sub
  , _menuState :: !(M.Map MenuState Int)
  , _depth     :: !Int
  , _turn      :: !Int
  , _levels    :: !(M.Map Int Level) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Player = Player
  { _playerPosition      :: !(V2 Int)
  , _playerMaximumHealth :: !Int
  , _playerHealth        :: !Int
  , _playerOxygen        :: !Int
  , _playerShells        :: !Int
  , _playerInventory     :: ![Item]
  , _playerDragging      :: !(Maybe Item) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Status
  = Slow
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

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
makeLenses ''GameState
makeLenses ''Player
makeLenses ''Sub

class HasGameState a where
  gameState :: Lens' a GameState

instance HasGameState GameState where
  gameState = lens identity (\_ gs -> gs)
  {-# INLINE gameState #-}

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

instance MonadTerminalState m => MonadTerminalState (StateT GameState m) where
  getTerminal = lift getTerminal
  putTerminal = lift . putTerminal

instance MonadTerminalState m => MonadTerminalState (ReaderT GameState m) where
  getTerminal = lift getTerminal
  putTerminal = lift . putTerminal

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
    M.lookup VendorMenuSelection (gs^.menuState)
  set_it gs new_selection =
    gs & menuState.at VendorMenuSelection .~ new_selection

currentInventoryMenuSelection :: Lens' GameState (Maybe Int)
currentInventoryMenuSelection = lens get_it set_it
 where
  get_it gs =
    M.lookup Inventory (gs^.menuState)
  set_it gs new_selection =
    gs & menuState.at Inventory .~ new_selection

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

subOrLevelLens :: forall a.
                  (V2 Int -> Lens' Level a)
               -> (V2 Int -> Traversal' SubTopology a)
               -> (V2 Int -> Lens' GameState a)
subOrLevelLens level_lens sub_lens coords@(V2 x y) = lens get_it set_it
 where
  get_it :: GameState -> a
  get_it gamestate =
    -- If we are not near sub, look up level cell normally
    if x < sx || y < sy || x >= sx+sw || y >= sy+sh
      then level_cell
      -- If we *are* inside sub bounding box, attempt to look up sub level cell
      -- instead.
      else fromMaybe level_cell $ case gamestate^..sub.topology.sub_lens (V2 (x-sx) (y-sy)) of
             [target] -> Just target
             _ -> Nothing
   where
    level_cell = gamestate^.currentLevel.level_lens coords

    V2 sx sy = gamestate^.sub.subPosition
    V2 sw sh = subSize (gamestate^.sub.topology)

  set_it :: GameState -> a -> GameState
  set_it gamestate new_cell =
    gamestate & failing (sub.topology.sub_lens (V2 (x-sx) (y-sy))) (currentLevel.level_lens coords) .~ new_cell
   where
    V2 sx sy = gamestate^.sub.subPosition
{-# INLINE subOrLevelLens #-}

levelActiveMetadataAt :: V2 Int -> Lens' GameState (Maybe LevelActiveMetadata)
levelActiveMetadataAt =
  subOrLevelLens activeMetadataAt subActiveMetadataAt

levelCellAt :: V2 Int -> Lens' GameState LevelCell
levelCellAt = subOrLevelLens cellAt subCellP
{-# INLINE levelCellAt #-}

levelItemsAt :: V2 Int -> Lens' GameState [Item]
levelItemsAt = subOrLevelLens itemsAt subItemsP
{-# INLINE levelItemsAt #-}

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

currentLevel :: Lens' GameState Level
currentLevel = lens get_it set_it
 where
  get_it gs = fromJust $ M.lookup (gs^.depth) (gs^.levels)
  set_it gs new_level = gs & levels.at (gs^.depth) .~ Just new_level
{-# INLINE currentLevel #-}

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

cycleGame :: Monad m => GameMonad m ()
cycleGame = do
  gs <- get

  -- Enter vendor menu if not currently next to vendor.
  case getCurrentVendor gs of
    Nothing -> currentVendorMenuSelection .= Nothing
    Just _vendor ->
      when (isNothing $ gs^.currentVendorMenuSelection) $
        currentVendorMenuSelection .= Just 0

