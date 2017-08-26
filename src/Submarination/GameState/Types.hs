module Submarination.GameState.Types
  ( GameState(..)
  , player
  , sub
  , activeMenuState
  , activeMenuInventory
  , vendorMenu
  , depth
  , turn
  , inputTurn
  , levels
  , Sub(..)
  , subTopology
  , subPosition
  , Player(..)
  , playerPosition
  , playerMaximumHealth
  , playerHealth
  , playerOxygen
  , playerShells
  , playerInventory
  , playerDragging
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
  , SelectMode(..) )
  where

import Control.Lens hiding ( Level, levels )
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Linear.V2
import Protolude hiding ( (&) )

import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal

data ActiveMenuState
  = Pickup
  | Inventory
  | Drop
  | ContainerTakeOut
  | ContainerPutIn
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data Sub = Sub
  { _subTopology :: !SubTopology
  , _subPosition :: !(V2 Int) }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data GameState = GameState
  { _player              :: Player
  , _sub                 :: Sub
  , _activeMenuState     :: M.Map ActiveMenuState (S.Set Int, Int)
  , _activeMenuInventory :: [Item]
  , _vendorMenu          :: Maybe Int
  , _depth               :: Int
  , _turn                :: Int
  , _inputTurn           :: Int
  , _levels              :: M.Map Int Level
  , _messages            :: M.Map Int Text }
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
      else fromMaybe level_cell $ case gamestate^..sub.subTopology.sub_lens (V2 (x-sx) (y-sy)) of
             [target] -> Just target
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

glCurrentLevel :: Lens' GameState Level
glCurrentLevel = lens get_it set_it
 where
  get_it gs = fromJust $ M.lookup (gs^.depth) (gs^.levels)
  set_it gs new_level = gs & levels.at (gs^.depth) .~ Just new_level
{-# INLINE glCurrentLevel #-}

data ItemMenuHandler = ItemMenuHandler
  { triggerKeys           :: S.Set Char
  , offKeys               :: S.Set Char
  , quickEnterAction      :: GameState -> Maybe GameState
  , menuStateKey          :: !ActiveMenuState
  , menuKeys              :: M.Map Char (Text, GameState -> Maybe GameState)
  , prerequisites         :: GameState -> Bool
  , selectMode            :: SelectMode
  , menuFilter            :: Item -> Bool
  , toActiveMenuInventory :: GameState -> [Item]
  , otherKeys             :: M.Map Char Text
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

