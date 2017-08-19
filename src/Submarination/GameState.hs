module Submarination.GameState where

import Control.Monad.Trans.Except
import Control.Lens hiding ( Level )
import Data.Data
import Data.List ( (!!) )
import Linear.V2
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
  , _depth     :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Player = Player
  { _playerPosition      :: !(V2 Int)
  , _playerMaximumHealth :: !Int
  , _playerHealth        :: !Int
  , _playerOxygen        :: !Int
  , _playerShells        :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

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

type GameMonad m a = StateT GameState m a
type GameMonadRo m a = ReaderT GameState m a

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
                     , _playerShells = 1000 }
  , _menuState = NotInMenu
  , _sub = Sub { _subPosition = V2 18 (-2)
               , _topology = composeVertically (composeHorizontally bridge (composeHorizontally standardRoom standardRoom)) airLock }
  , _depth  = 0 }

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

gr :: Monad m => (GameState -> a) -> GameMonadRo m a
gr action = do
  gs <- ask
  return $ action gs

directionToDelta :: Direction -> V2 Int
directionToDelta D2 = V2 0 1
directionToDelta D8 = V2 0 (-1)
directionToDelta D4 = V2 (-1) 0
directionToDelta D6 = V2 1 0
directionToDelta D7 = V2 (-1) (-1)
directionToDelta D9 = V2 1 (-1)
directionToDelta D3 = V2 1 1
directionToDelta D1 = V2 (-1) 1

moveDirection :: Monad m => Direction -> GameMonad m ()
moveDirection direction = do
  old_pos <- gm (^.player.playerPosition)
  let new_playerpos = old_pos + directionToDelta direction

  -- Can I walk there?
  walkable <- isWalkable <$> gm (levelCellAt new_playerpos)

  -- Is there some monster in there?
  when walkable $ do
    occupied <- gm $ isOccupied new_playerpos
    unless occupied $
      player.playerPosition .= new_playerpos

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

levelCellAt :: V2 Int -> GameState -> LevelCell
levelCellAt coords@(V2 x y) gamestate =
  -- If we are not near sub, look up level cell normally
  if x < sx || y < sy || x >= sx+sw || y >= sy+sh
    then currentLevel gamestate^.cellAt coords
    -- If we *are* inside sub bounding box, attempt to look up sub level cell
    -- instead.
    else fromMaybe level_cell $ subCell (gamestate^.sub.topology) (V2 (x-sx) (y-sy))
 where
  level_cell = currentLevel gamestate^.cellAt coords

  V2 sx sy = gamestate^.sub.subPosition
  V2 sw sh = subSize (gamestate^.sub.topology)

currentLevel :: GameState -> Level
currentLevel _ = surfaceLevel

currentAreaName :: GameState -> Text
currentAreaName _ = "--- SURFACE ---"

isOccupied :: V2 Int -> GameState -> Bool
isOccupied coords gamestate =
  let cr = currentLevel gamestate
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
      lvl = currentLevel gs

  for_ (allNeighbours playerpos) $ \candidate_pos ->
    case lvl^.creatures.at candidate_pos of
      Just FoodVendor -> throwE FoodVendor
      Just AmmoVendor -> throwE AmmoVendor
      Just MaterialVendor -> throwE MaterialVendor
      Just ToolVendor -> throwE ToolVendor
      _ -> return ()

