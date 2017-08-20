{-# LANGUAGE CPP #-}

module Submarination.Render
  ( terminalRenderer
  , UpdateRequestState()
  , UpdateRequestStateKnob
  , newUpdateRequestKnob
  , showSplashScreen
  , showGameState )
  where

import Control.Arrow ( (***) )
import Control.Lens hiding ( Level )
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Data
import qualified Data.Text as T
import Linear.V2
import Protolude hiding ( to )
import System.Timeout

import Submarination.Creature
import Submarination.GameState
import Submarination.Item
import Submarination.MonotonicClock
import Submarination.Level
import Submarination.Sub
import Submarination.Terminal
import Submarination.Vendor

data Rendering
  = RenderGameState !GameState
  | RenderSplashScreen
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

data UpdateRequestState
  = PleaseRender !Rendering
  | NoUpdates
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

type UpdateRequestStateKnob = TVar UpdateRequestState

losDistance :: Int
losDistance = 11

newUpdateRequestKnob :: MonadIO m => m UpdateRequestStateKnob
newUpdateRequestKnob = liftIO $ newTVarIO NoUpdates

showSplashScreen :: MonadIO m => UpdateRequestStateKnob -> m ()
showSplashScreen update_request_state =
  liftIO $ atomically $ writeTVar update_request_state $
    PleaseRender RenderSplashScreen

showGameState :: MonadIO m => UpdateRequestStateKnob -> GameState -> m ()
showGameState update_request_state game_state =
  liftIO $ atomically $ writeTVar update_request_state $
    PleaseRender $ RenderGameState game_state

terminalRenderer :: forall m. MonadTerminalState m => UpdateRequestStateKnob -> m ()
terminalRenderer update_request_state = do
  (w, h) <- getTerminalSize
  go w h Nothing
 where
  go :: Int -> Int -> Maybe Rendering -> m ()
  go _previous_w _previous_h previous_rendering = do
    -- Wait here until new updates come in.
    -- We also tick every 250ms to check if terminal size changed, so that's
    -- what `timeout` is used here for.
    maybe_rendering <- liftIO $ timeout 250000 $ atomically $ readTVar update_request_state >>= \case
      NoUpdates -> retry
      PleaseRender rendering -> do
        writeTVar update_request_state NoUpdates
        return rendering

    (w, h) <- getTerminalSize
    -- Take the current monotonic clock. Used for animation
    monotonic_time_ns <- getMonotonicTime

    case maybe_rendering of
      Just rendering -> do
        case rendering of
          RenderGameState game_state -> renderGameState game_state monotonic_time_ns
          RenderSplashScreen         -> renderSplashScreen

        go w h (Just rendering)

      -- Tick
      Nothing ->
        -- If terminal size is different from last tick and there are no
        -- pending updates, re-render with new size.
        case previous_rendering of
          Just previous_rendering' -> do
            pr <- liftIO $ atomically $ readTVar update_request_state >>= \case
              NoUpdates -> do
                writeTVar update_request_state (PleaseRender previous_rendering')
                return previous_rendering'
              PleaseRender updates -> return updates

            go w h (Just pr)

          _ -> go w h previous_rendering

renderSplashScreen :: MonadTerminalState m => m ()
renderSplashScreen = mutateTerminalStateM $ do
  clear
  setText 0 1 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 2 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 3 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 4 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 5 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 6 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 7 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 8 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 10 4 Vivid White Dull Black "  Submarination  "
  setText 17 6 Dull White Dull Black "^"
  setText 12 7 Dull White Dull Black "<#######)"

  setText 3 10 Dull White Dull Black "Make your choice:"
  setText 3 12  Vivid Cyan Dull Black "space)"
  setText 10 12 Vivid White Dull Black "Start a new game"

#ifndef GHCJS_BROWSER
  setText 3 13  Vivid Cyan Dull Black "q)"
  setText 10 13 Vivid White Dull Black "Quit"
#endif

renderGameState :: MonadTerminalState m => GameState -> Integer -> m ()
renderGameState game_state monotonic_time_ns = runReaderT (renderGameState' monotonic_time_ns) game_state

renderGameState' :: MonadTerminalState m => Integer -> GameMonadRo m ()
renderGameState' monotonic_time_ns = do
  game_state <- gr identity
  lift $ mutateTerminalStateM $ do
    clear

    let rendering = do renderCurrentLevel monotonic_time_ns
                       renderSub monotonic_time_ns
                       renderCreatures
                       renderPlayer
                       renderHud monotonic_time_ns

    runReaderT rendering game_state

type GameMonadRoTerminal s = GameMonadRo (MutateTerminal s)

creatureToAppearance :: Creature -> Cell
creatureToAppearance creature = case creature of
  FoodVendor     -> Cell Vivid White Dull Yellow '@'
  AmmoVendor     -> Cell Vivid White Dull Yellow '@'
  MaterialVendor -> Cell Vivid White Dull Yellow '@'
  ToolVendor     -> Cell Vivid White Dull Yellow 'O'

itemToAppearance :: Item -> Cell
itemToAppearance SardineTin      = Cell Dull Cyan Dull Black '%'
itemToAppearance Poylent         = Cell Vivid Cyan Dull Black '%'
itemToAppearance Chicken         = Cell Vivid Red Dull Black '%'
itemToAppearance Potato          = Cell Vivid Yellow Dull Black '%'
itemToAppearance Whiskey         = Cell Dull Yellow Dull Black '!'
itemToAppearance Freezer         = Cell Vivid Cyan Dull Black '■'
itemToAppearance Refrigerator    = Cell Dull Cyan Dull Black '■'
itemToAppearance Microwave       = Cell Vivid White Dull Black '■'
itemToAppearance (StorageBox []) = Cell Dull White Dull Black '±'
itemToAppearance StorageBox{}    = Cell Dull White Dull Black '≡'

reverseAppearance :: Cell -> Cell
reverseAppearance (Cell fintensity fcolor bintensity bcolor ch) =
  Cell bintensity bcolor fintensity fcolor ch

renderCreatures :: GameMonadRoTerminal s ()
renderCreatures = do
  level <- gr (^.currentLevel)
  V2 px py <- view $ player.playerPosition

  lift $ ifor_ (level^.creatures) $ \(V2 cx cy) creature -> do
    -- relative position
    let (rx, ry) = (cx-px, cy-py)

    -- Only render creature if it's in renderable area
    when (rx >= -losDistance && rx <= losDistance && ry >= -losDistance && ry <= losDistance) $ do
      -- position in terminal
      let (tx, ty) = ((+) rx *** (+) ry) mapMiddleOnTerminal

      setCell' tx ty (creatureToAppearance creature)

renderCurrentLevel :: Integer -> GameMonadRoTerminal s ()
renderCurrentLevel monotonic_time_ns = do
  lvl <- gr (^.currentLevel)
  playerpos <- gr (^.player.playerPosition)
  renderLevel monotonic_time_ns lvl playerpos

levelFeatureToAppearance :: LevelCell -> Double -> Int -> Int -> Cell
levelFeatureToAppearance lcell monotonic_time x y = case lcell of
  Water         -> Cell Vivid Blue Dull Black '~'
  WoodenBoards  -> Cell Vivid Yellow Dull Black '.'
  Rock          -> Cell Vivid Black Vivid Black ' '
  MountainRock  -> Cell Vivid White Vivid Black '^'
  Grass         -> Cell Vivid Green Dull Black '.'
  Hull          -> Cell Dull Yellow Dull Black '█'
  InteriorFloor -> Cell Dull Yellow Dull Black '.'
  Hatch         -> Cell Vivid Yellow Dull Black '+'
  OpenHatch     -> Cell Vivid Yellow Dull Black '-'
  Window        -> Cell Vivid Cyan Dull Black '◘'

  cell | cell == SurfaceWater || cell == SurfaceWaterSplashing ->
    let theta = sin $ sectime*0.1+dx*0.5+dy*0.25
     in if | theta > 0.5
             -> Cell Dull Blue Dull Black '≈'
           | theta < -0.7 && cell == SurfaceWaterSplashing
             -> Cell Vivid White Dull Blue '▒'
           | theta < -0.5
             -> Cell Vivid Blue Dull Blue '≈'
           | otherwise
             -> Cell Vivid Blue Dull Black '≈'

  _ -> Cell Vivid Red Vivid White '?'
 where
  sectime = monotonic_time / 1000.0
  dx = fromIntegral x :: Double
  dy = fromIntegral y :: Double

mapMiddleOnTerminal :: (Int, Int)
mapMiddleOnTerminal = (40, 12)

renderSub :: Integer -> GameMonadRoTerminal s ()
renderSub monotonic_time_ns = do
  V2 sx sy <- gr (^.sub.subPosition)
  V2 sw sh <- gr (^.sub.topology.to subSize)
  V2 px py <- view $ player.playerPosition

  topo <- gr (^.sub.topology)

  lift $ for_ [V2 x y | x <- [px-losDistance..px+losDistance], y <- [py-losDistance..py+losDistance]] $ \(V2 lx ly) -> do
    let terminalcoord = ((+) (lx - px) *** (+) (ly - py)) mapMiddleOnTerminal
    when (lx >= sx && ly >= sy && lx < sx+sw && ly < sy+sh) $ do
      let subcoords = V2 (lx-sx) (ly-sy)
      case subCell topo subcoords of
        Nothing -> return ()
        Just subcell -> do
          let render_at = uncurry setCell' terminalcoord
          render_at $ levelFeatureToAppearance subcell monotonic_time sx sy

          for_ (subItems topo subcoords) $ \items ->
            renderItemPile render_at items
 where
  monotonic_time = fromIntegral (monotonic_time_ns `div` 1000000) :: Double

renderItemPile :: Monad m => (Cell -> m ()) -> [Item] -> m ()
renderItemPile _action [] = return ()
renderItemPile action [single_item] =
  action $ itemToAppearance single_item
renderItemPile action (first_item:_rest) =
  action $ reverseAppearance $ itemToAppearance first_item

renderLevel :: Integer -> Level -> V2 Int -> GameMonadRoTerminal s ()
renderLevel monotonic_time_ns level (V2 ox oy) =
  lift $ for_ [V2 x y | x <- [ox-losDistance..ox+losDistance], y <- [oy-losDistance..oy+losDistance]] $ \levelcoord@(V2 lx ly) -> do
    let terminalcoord = ((+) (lx - ox) *** (+) (ly - oy)) mapMiddleOnTerminal
    -- What is the cell we should render?
        level_feature = level^.cellAt levelcoord

    -- Now...actually instruct the terminal to render a symbol for this level
    -- feature.

    let render_at = uncurry setCell' terminalcoord

    render_at $ levelFeatureToAppearance level_feature monotonic_time lx ly
    renderItemPile render_at (level^.itemsAt levelcoord)
 where
  monotonic_time = fromIntegral (monotonic_time_ns `div` 1000000) :: Double

renderPlayer :: GameMonadRoTerminal s ()
renderPlayer = lift $
  uncurry setCell' mapMiddleOnTerminal (Cell Dull Black Dull White '@')

blocksText :: Double -> Text
blocksText len =
  let wholes = floor len :: Int
   in T.replicate wholes "█" <> partialText (len - fromIntegral wholes)
 where
  partialText len | len < 0.25 = ""
  partialText len | len > 0.75 = "█"
  partialText _ = "▌"

renderBar :: Text -> Int -> Int -> Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> GameMonadRoTerminal s ()
renderBar label value max_value x y bar_intensity bar_color unbar_intensity unbar_color = do
  let portion = fromIntegral value / fromIntegral max_value :: Double
      bar_len = 13 :: Int
      bar_txt_head = blocksText $ fromIntegral bar_len * portion
      bar_txt_tail = T.replicate (bar_len - T.length bar_txt_head) " "

  lift $ do
    setText (x+12) y Vivid White Dull Black "├"
    setText (x+26) y Vivid White Dull Black "┤"
    setText (x+13) y bar_intensity bar_color unbar_intensity unbar_color (bar_txt_head <> bar_txt_tail)
    setText x y Vivid White Dull Black $ label <> show value <> "/" <> show max_value

-- | Type that keeps track of Y position while we render stuff on hud. This
-- pushes text below if there are messages or whatever. Monad transformer.
newtype VerticalBoxRender m a = VerticalBoxRender (StateT Int m a)
  deriving ( Functor, Applicative, Monad )

instance MonadTrans VerticalBoxRender where
  lift action = VerticalBoxRender $ lift action

instance MonadReader r m => MonadReader r (VerticalBoxRender m) where
  ask = VerticalBoxRender $ lift ask
  local mod (VerticalBoxRender action) = VerticalBoxRender $ do
    st <- get
    (result, new_st) <- lift $ local mod (runStateT action st)
    put new_st
    return result

runVerticalBoxRender :: Monad m => Int -> VerticalBoxRender m a -> m a
runVerticalBoxRender y_start (VerticalBoxRender stateful) =
  evalStateT stateful y_start

appendText :: Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Text -> VerticalBoxRender (GameMonadRoTerminal s) ()
appendText x skip fintensity fcolor bintensity bcolor txt = VerticalBoxRender $ do
  y <- get
  put (y+skip)

  lift $ lift $ setText x y fintensity fcolor bintensity bcolor txt

renderHud :: Integer -> GameMonadRoTerminal s ()
renderHud _monotonic_time_ns = do

  -- The name of the section of submarine you are in
  player_pos <- gr (^.player.playerPosition)
  sub_pos <- gr (^.sub.subPosition)
  sub_topo <- gr (^.sub.topology)

  case getLocationNameInSub (player_pos - sub_pos) sub_topo of
    Just sub_section_name -> do
      let sub_title = "+++ " <> sub_section_name <> " +++"
      let sub_title_w = textWidth sub_title
      lift $ setText (40-(sub_title_w `div` 2)) 0 Vivid White Dull Black sub_title
    Nothing -> do
      title <- gr currentAreaName
      let title_w = textWidth title
      lift $ setText (40-(title_w `div` 2)) 0 Vivid White Dull Black title

  -- Health/Oxygen
  hp     <- gr (^.player.playerHealth)
  max_hp <- gr (^.player.playerMaximumHealth)

  oxygen     <- gr (^.player.playerOxygen)
  max_oxygen <- gr getMaximumOxygenLevel

  renderBar "HP: " hp max_hp 53 2 Vivid Red Dull Blue
  renderBar "O₂: " oxygen max_oxygen 53 4 Vivid Cyan Dull Blue

  runVerticalBoxRender 6 $ do
    on_surface <- gr isOnSurface
    when on_surface renderSurfaceHud

renderItemPileHud :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderItemPileHud = do
  player_pos <- gr (^.player.playerPosition)
  items <- gr (^.levelItemsAt player_pos)
  case items of
    [] -> return ()
    [_single_item] ->
      appendText 53 2 Dull White Dull Black "There is one item here:"
    lst ->
      appendText 53 2 Dull White Dull Black $ "There are " <> show (length lst) <> " items here:"

  let grouped = groupItems items

  ifor_ grouped $ \item count ->
    if count > 1
      then appendText 53 1 Dull White Dull Black $ show count <> " " <> itemName item Many
      else appendText 53 1 Dull White Dull Black $ itemName item Singular

  appendText 53 1 Dull White Dull Black ""

  unless (null items) $ do
    appendText 53 0 Dull White Dull Black "[ ] Pick up [ ] Drag"
    appendText 54 0 Vivid Green Dull Black ","
    appendText 66 2 Vivid Green Dull Black "g"

statusAppearance :: Status -> (ColorIntensity, Color, ColorIntensity, Color)
statusAppearance Slow = (Dull, Red, Dull, Black)

renderStatuses :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderStatuses = do
  statuses <- gr currentStatuses
  unless (null statuses) $ do
    flip evalStateT 53 $ for_ statuses $ \status -> do
      let status_name = statusName status
          (fintensity, fcolor, bintensity, bcolor) = statusAppearance status
      x <- get

      let status_width = textWidth status_name

      lift $ appendText x 0 fintensity fcolor bintensity bcolor status_name
      put (x+status_width+1)

renderSurfaceHud :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderSurfaceHud = do
  shells <- gr (^.player.playerShells)

  appendText 53 0 Vivid Yellow Dull Black "$"
  appendText 54 2 Vivid White Dull Black $ ": " <> show shells

  renderStatuses

  renderItemPileHud

  menu_selection <- gr currentMenuSelection

  gr getCurrentVendor >>= \case
    Just vendor -> do
      let desc = vendorDescription vendor
      lift2 $ setWrappedText 3 3 25 Dull White Dull Black desc

      let items = zip [8..] (vendorItems vendor)
          last_y = length items + 8

      lift2 $ for_ items $ \(y, item) -> do
        let selection_num = y-8
        if selection_num == menu_selection
          then do setText 2 y Vivid Green Dull Black "➔"
                  setText 4 y Vivid Blue Dull Black (itemName item Singular)
                  setText 53 8 Vivid White Dull Black (itemName item Singular)
                  setWrappedText 53 10 25 Dull White Dull Black (itemDescription item)
          else setText 4 y Dull Blue Dull Black (itemName item Singular)

        setText 24 y Vivid Yellow Dull Black "$"
        setText 25 y Vivid White Dull Black $ show $ itemPrice item

      lift2 $ do
        setText 2 (last_y+2) Dull White Dull Black "[ ] ↑ [ ] ↓"
        setText 2 (last_y+3) Dull White Dull Black "[     ] Purchase"
        setText 3 (last_y+2) Vivid Green Dull Black "A"
        setText 9 (last_y+2) Vivid Green Dull Black "Z"
        setText 3 (last_y+3) Vivid Green Dull Black "SPACE"

    _ -> return ()
 where
  lift2 = lift . lift

