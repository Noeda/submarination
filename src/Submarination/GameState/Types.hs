{-# LANGUAGE DeriveAnyClass #-}

module Submarination.GameState.Types
  ( GameState(..)
  , player
  , sub
  , activeMenuState
  , activeMenuInventory
  , activeMenuCounter
  , vendorMenu
  , cables
  , depth
  , dead
  , deathReason
  , turn
  , inputTurn
  , runningIndex
  , levels
  , godMode
  , Cable(..)
  , CableNext(..)
  , gsTopCableAt
  , glCableAt
  , glCablesAt
  , glCableIndex
  , cableNext
  , cablePrevious
  , cablePosition
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
  , playerTethered
  , Status(..)
  , Action(..)
  , ActionHandler(..)
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
import Data.Binary
import Data.Data
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Linear.V2
import Protolude hiding ( (&) )

import Submarination.Index
import Submarination.Item
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal
import Submarination.Turn

data Action
  = Pickup
  | Inventory
  | Drop
  | Eat
  | ContainerTakeOut
  | ContainerPutIn
  | StartDive
  | MicrowaveMenu
  | Anchor
  | UnAnchor
  | RetractTether
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, Binary )

data Sub = Sub
  { _subTopology :: !SubTopology
  , _subPosition :: !(V2 Int)
  , _subDiving   :: !Bool
  , _subEnergy   :: !Int }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

type ActionMenuSelections = (M.Map Int Int, Int)

data GameState = GameState
  { _player              :: Player
  , _sub                 :: Sub
  , _activeMenuState     :: M.Map Action ActionMenuSelections
  , _activeMenuInventory :: [Item]
  , _activeMenuCounter   :: Maybe Int
  , _vendorMenu          :: Maybe Int
  , _dead                :: Bool
  , _deathReason         :: Text
  , _depth               :: Int
  , _turn                :: Turn
  , _inputTurn           :: Turn
  , _godMode             :: Bool
  , _runningIndex        :: Index
  , _cables              :: M.Map Int (M.Map (V2 Int) (IM.IntMap (IM.IntMap Cable)))
  , _levels              :: M.Map Int Level
  , _messages            :: M.Map Turn Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data Player = Player
  { _playerPosition      :: V2 Int
  , _playerMaximumHealth :: Int
  , _playerHealth        :: Int
  , _playerOxygen        :: Int
  , _playerShells        :: Int
  , _playerInventory     :: [Item]
  , _playerDragging      :: Maybe Item
  , _playerHunger        :: Int
  , _playerTethered      :: Maybe Index }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data CableNext
  = End
  | CableAt (V2 Int)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data Cable = Cable
  { _cablePosition :: Int
  , _cableIndex    :: Index
  , _cableNext     :: CableNext
  , _cablePrevious :: CableNext }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

data Status
  = Slow
  | Hungry
  | Starving
  | Satiated
  | God
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum, Binary )
makeLenses ''GameState
makeLenses ''Player
makeLenses ''Sub
makeLenses ''Cable

glCableIndex :: Lens' Cable Index
glCableIndex = cableIndex

gsTopCableAt :: Index -> V2 Int -> GameState -> Maybe Cable
gsTopCableAt index pos gs =
  let im = gs^.glCableAt index pos
   in case IM.maxView im of
        Nothing             -> Nothing
        Just (cable, _rest) -> Just cable

glCableAt :: Index -> V2 Int -> Lens' GameState (IM.IntMap Cable)
glCableAt index pos = lens get_it set_it
 where
  get_it gs =
    fromMaybe IM.empty $ IM.lookup (toInt index) (gs^.glCablesAt pos)

  set_it :: GameState -> IM.IntMap Cable -> GameState
  set_it gs im | IM.null im = gs & glCablesAt pos %~ IM.delete (toInt index)
  set_it gs cable' =
    let cable = cable' & each.cableIndex .~ index
     in gs & glCablesAt pos %~ IM.insert (toInt index) cable

glCablesAt :: V2 Int -> Lens' GameState (IM.IntMap (IM.IntMap Cable))
glCablesAt pos = lens get_it set_it
 where
  get_it gs = fromMaybe IM.empty $ gs^?cables.at (gs^.depth)._Just.at pos._Just

  set_it gs im | IM.null im =
    case gs^.cables.at (gs^.depth) of
      Nothing -> gs
      Just old_misc_objects ->
        let new_misc_objects = old_misc_objects & at pos .~ Nothing
         in if M.null new_misc_objects
              then gs & cables.at (gs^.depth) .~ Nothing
              else gs & cables.at (gs^.depth) .~ Just new_misc_objects

  set_it gs im =
    case gs^.cables.at (gs^.depth) of
      Nothing -> gs & cables.at (gs^.depth) .~ Just (M.singleton pos im)
      Just old_misc_objects ->
        let new_misc_objects = old_misc_objects & at pos .~ Just im
         in gs & cables.at (gs^.depth) .~ Just new_misc_objects
{-# INLINEABLE glCablesAt #-}

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

data ActionHandler = ActionHandler
  { triggerKeys           :: S.Set Char
  , menuName              :: GameState -> Text
  , menuStateKey          :: !Action
  , menuKeys              :: M.Map Char (Text, GameState -> Failing GameState)
  , menuFilter            :: GameState -> Item -> Bool
  , menuText              :: Text
  , offKeys               :: S.Set Char
  , quickEnterAction      :: GameState -> Maybe GameState
  , prerequisites         :: GameState -> Bool
  , selectMode            :: SelectMode
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

