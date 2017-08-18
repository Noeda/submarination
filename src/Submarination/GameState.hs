module Submarination.GameState where

import Control.Monad.Trans.Except
import Control.Lens hiding ( Level )
import Data.Data
import Linear.V2
import Protolude

import Submarination.Creature
import Submarination.Level
import Submarination.Terminal

data GameState = GameState
  { _player :: !Player
  , _depth  :: !Int }
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
  , _depth  = 0 }

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
  ,".........,.............,.............,................."
  ,"......................................................."
  ,".................================,,........,..........."
  ,"...,,...........==================,,..,................"
  ,"................==================,,..................."
  ,"....,..........====================,.............,....."
  ,"...............#########g##########,,.................."
  ,"......###########^^#^###g######################,......."
  ,"#######################ggg#############################"
  ,"######################1ggg2##^^^^######################"
  ,"####^^^^#######^^^####ggggg##^^########^^^#############"
  ,"###^^###########^^####ggggg############################"
  ,"################^^####3ggg4###############^############"
  ,"########^^^#####^^####ggggg###^#####^##################"
  ,"#################^#####ggg####^###^^###################"
  ,"#################^############^###^^###################"
  ,"###############################^#^^^###################"
  ,"################################^^^######^^^###########"
  ,"###################^^^^^^#^#####^^^####################"
  ,"#########^#^##^####^^^^^##^############################"
  ,"#########^####^####^^^^################################"
  ,"#########^####^####^###################################"
  ,"##################^^######^^^^#########################"
  ,"##################^^###################################"
  ,"##################^####################################"
  ,"#######################################################"
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
levelCellAt coords gamestate = currentLevel gamestate^.cellAt coords

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

