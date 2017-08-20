module Submarination.GameState where

import Control.Monad.Trans.Except
import Control.Lens hiding ( Level, levels )
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List ( (!!) )
import qualified Data.Set as S
import Linear.V2
import qualified Prelude as E
import Protolude hiding ( (&) )

import Submarination.Creature
import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal
import Submarination.Vendor

data MenuState
  = NotInMenu
  | MenuSelection !Int
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Sub = Sub
  { _topology :: !SubTopology
  , _subPosition :: !(V2 Int) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data GameState = GameState
  { _player    :: !Player
  , _sub       :: !Sub
  , _menuState :: !MenuState
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

startGameState :: GameState
startGameState = GameState
  { _player = Player { _playerPosition = V2 15 8
                     , _playerMaximumHealth = 100
                     , _playerHealth = 100
                     , _playerOxygen = 100
                     , _playerShells = 1000
                     , _playerDragging = Nothing }
  , _menuState = NotInMenu
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

currentMenuSelection :: GameState -> Int
currentMenuSelection gs = case gs^.menuState of
  NotInMenu -> 0
  MenuSelection selection -> selection

currentVendorItemSelection :: GameState -> Maybe Item
currentVendorItemSelection gs = do
  vendor <- getCurrentVendor gs
  let current_selection' = currentMenuSelection gs
      current_selection = max 0 $ min (length items-1) current_selection'
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
    new_level <- walkLevel =<< use currentLevel
    currentLevel .= new_level

  walkSubLevelMetadata :: GameMonad m ()
  walkSubLevelMetadata = do
    topo <- use $ sub.topology
    new_sub_topo <- forOf subLevels topo walkLevel
    sub.topology .= new_sub_topo

  walkLevel :: Level -> GameMonad m Level
  walkLevel lvl = do
    current_turn <- use turn

    player_pos <- use $ player.playerPosition

    walkLevelActiveMetadata lvl $ \coords cell metadata -> return $ case (cell, metadata) of
      (OpenHatch, HatchAutoClose close_turn) | close_turn <= current_turn && coords /= player_pos ->
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
    when walkable $ do
      occupied <- gm $ isOccupied new_playerpos
      unless occupied $ do
        player.playerPosition .= new_playerpos

        -- Moving takes extra time if we are slow
        slow <- gm isSlow
        when slow $
          advanceTurn

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

runMaybeExcept :: Except a () -> Maybe a
runMaybeExcept action =
  case runExcept action of
    Left value -> Just value
    _ -> Nothing

attemptPurchase :: GameState -> GameState
attemptPurchase gs = fromMaybe gs $ do
  item_selection <- currentVendorItemSelection gs
  let shells = gs^.player.playerShells

  guard (shells >= itemPrice item_selection)

  return $ gs & player.playerShells -~ itemPrice item_selection

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

