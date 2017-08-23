module Submarination.GameState.Types
  ( GameState(..)
  , player
  , sub
  , menuState
  , depth
  , turn
  , levels
  , Sub(..)
  , topology
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
  , MenuState(..)
  , ItemMenuHandler(..)
  , itemsAtPlayer
  , subOrLevelLens
  , levelCellAt
  , levelItemsAt
  , currentLevel
  , HasGameState(..) )
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

data MenuState
  = Vendor
  | Pickup
  | Inventory
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

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
makeLenses ''GameState
makeLenses ''Player
makeLenses ''Sub

levelCellAt :: V2 Int -> Lens' GameState LevelCell
levelCellAt = subOrLevelLens cellAt subCellP
{-# INLINE levelCellAt #-}

levelItemsAt :: V2 Int -> Lens' GameState [Item]
levelItemsAt = subOrLevelLens itemsAt subItemsP
{-# INLINE levelItemsAt #-}

itemsAtPlayer :: Lens' GameState [Item]
itemsAtPlayer = lens get_it set_it
 where
  get_it gs = gs^.levelItemsAt (gs^.player.playerPosition)
  set_it gs new_items = gs & levelItemsAt (gs^.player.playerPosition) .~ new_items

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

currentLevel :: Lens' GameState Level
currentLevel = lens get_it set_it
 where
  get_it gs = fromJust $ M.lookup (gs^.depth) (gs^.levels)
  set_it gs new_level = gs & levels.at (gs^.depth) .~ Just new_level
{-# INLINE currentLevel #-}

data ItemMenuHandler = ItemMenuHandler
  { triggerKeys   :: S.Set Char
  , offKeys       :: S.Set Char
  , menuKeys      :: M.Map Char (Text, GameState -> Maybe GameState)
  , selectionLens :: Lens' GameState (Maybe Int)
  , prerequisites :: GameState -> Bool
  , menuFilter    :: Item -> Bool
  , itemLens      :: Lens' GameState [Item] }
  deriving ( Typeable )

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

