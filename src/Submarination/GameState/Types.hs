module Submarination.GameState.Types
  ( GameState(..)
  , player
  , sub
  , activeMenuState
  , activeMenuInventory
  , activeMenuCounter
  , vendorMenu
  , depth
  , dead
  , deathReason
  , turn
  , inputTurn
  , runningIndex
  , levels
  , godMode
  , Sub(..)
  , subTopology
  , subPosition
  , subDiving
  , subEnergy
  , Player(..)
  , playerPosition
  , playerMaximumHealth
  , playerHealth
  , playerOxygen
  , playerShells
  , playerInventory
  , playerDragging
  , playerHunger
  , Status(..)
  , ActiveMenuState(..)
  , ItemMenuHandler(..)
  , itemsAtPlayer
  , subOrLevelLens
  , glCellAt
  , glItemsAt
  , glCurrentLevel
  , messages
  , HasGameState(..)
  , SelectMode(..)
  , Failing
  , guardE
  , toMaybe
  , toFailing
  , addFail )
  where

import Control.Lens hiding ( Level, levels, Index )
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Linear.V2
import Protolude hiding ( (&) )

import Submarination.Index
import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal

data ActiveMenuState
  = Pickup
  | Inventory
  | Drop
  | Eat
  | ContainerTakeOut
  | ContainerPutIn
  | StartDive
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Sub = Sub
  { _subTopology :: !SubTopology
  , _subPosition :: !(V2 Int)
  , _subDiving   :: !Bool
  , _subEnergy   :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data GameState = GameState
  { _player              :: Player
  , _sub                 :: Sub
  , _activeMenuState     :: M.Map ActiveMenuState (M.Map Int Int, Int)
  , _activeMenuInventory :: [Item]
  , _activeMenuCounter   :: Maybe Int
  , _vendorMenu          :: Maybe Int
  , _dead                :: Bool
  , _deathReason         :: Text
  , _depth               :: Int
  , _turn                :: Int
  , _inputTurn           :: Int
  , _godMode             :: Bool
  , _runningIndex        :: Index
  , _levels              :: M.Map Int Level
  , _messages            :: M.Map Int Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Player = Player
  { _playerPosition      :: (V2 Int)
  , _playerMaximumHealth :: Int
  , _playerHealth        :: Int
  , _playerOxygen        :: Int
  , _playerShells        :: Int
  , _playerInventory     :: [Item]
  , _playerDragging      :: (Maybe Item)
  , _playerHunger        :: Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data Status
  = Slow
  | Hungry
  | Starving
  | Satiated
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
makeLenses ''GameState
makeLenses ''Player
makeLenses ''Sub

glCellAt :: V2 Int -> Lens' GameState LevelCell
glCellAt = subOrLevelLens cellAt subCellP
{-# INLINE glCellAt #-}

glItemsAt :: V2 Int -> Lens' GameState [Item]
glItemsAt = subOrLevelLens itemsAt subItemsP
{-# INLINE glItemsAt #-}

itemsAtPlayer :: Lens' GameState [Item]
itemsAtPlayer = lens get_it set_it
 where
  get_it gs = gs^.glItemsAt (gs^.player.playerPosition)
  set_it gs new_items = gs & glItemsAt (gs^.player.playerPosition) .~ new_items

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
      else fromMaybe level_cell $ case firstOf (sub.subTopology.sub_lens (V2 (x-sx) (y-sy))) gamestate of
             Just target -> Just target
             _ -> Nothing
   where
    level_cell = gamestate^.glCurrentLevel.level_lens coords

    V2 sx sy = gamestate^.sub.subPosition
    V2 sw sh = subSize (gamestate^.sub.subTopology)

  set_it :: GameState -> a -> GameState
  set_it gamestate new_cell =
    gamestate & failing (sub.subTopology.sub_lens (V2 (x-sx) (y-sy))) (glCurrentLevel.level_lens coords) .~ new_cell
   where
    V2 sx sy = gamestate^.sub.subPosition
{-# INLINE subOrLevelLens #-}

fullWaterLevel :: Level
fullWaterLevel = levelFromStrings Water []
{-# NOINLINE fullWaterLevel #-}

glCurrentLevel :: Lens' GameState Level
glCurrentLevel = lens get_it set_it
 where
  get_it gs = fromMaybe fullWaterLevel $ M.lookup (gs^.depth) (gs^.levels)
  set_it gs new_level =
    if new_level == fullWaterLevel
      then gs & levels.at (gs^.depth) .~ Nothing
      else gs & levels.at (gs^.depth) .~ Just new_level
{-# INLINE glCurrentLevel #-}

data ItemMenuHandler = ItemMenuHandler
  { triggerKeys           :: S.Set Char
  , menuName              :: Text
  , offKeys               :: S.Set Char
  , quickEnterAction      :: GameState -> Maybe GameState
  , menuStateKey          :: !ActiveMenuState
  , menuKeys              :: M.Map Char (Text, GameState -> Failing GameState)
  , prerequisites         :: GameState -> Bool
  , selectMode            :: SelectMode
  , menuFilter            :: Item -> Bool
  , menuText              :: !Text
  , toActiveMenuInventory :: GameState -> [Item]
  , otherKeys             :: GameState -> M.Map Char Text
  , itemLens              :: Lens' GameState [Item] }
  deriving ( Typeable )

data SelectMode
  = NotSelectable
  | SingleSelect
  | MultiSelect
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

class HasGameState a where
  gameState :: Lens' a GameState

instance HasGameState GameState where
  gameState = lens identity (\_ gs -> gs)
  {-# INLINE gameState #-}

instance MonadTerminalState m => MonadTerminalState (StateT GameState m) where
  getTerminal = lift getTerminal
  putTerminal = lift . putTerminal

instance MonadTerminalState m => MonadTerminalState (ReaderT GameState m) where
  getTerminal = lift getTerminal
  putTerminal = lift . putTerminal

type Failing a = Either Text a

guardE :: Bool -> Text -> Failing ()
guardE False txt = Left txt
guardE True _ = Right ()

toMaybe :: Failing a -> Maybe a
toMaybe Left{} = Nothing
toMaybe (Right v) = Just v

addFail :: Maybe a -> Text -> Failing a
addFail Nothing txt = Left txt
addFail (Just v) _ = Right v

toFailing :: Maybe a -> Failing a
toFailing Nothing = Left ""
toFailing (Just v) = Right v

