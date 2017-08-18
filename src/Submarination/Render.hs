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
import Control.Lens
import Control.Concurrent.STM
import Data.Data
import qualified Data.Text as T
import Linear.V2
import Protolude hiding ( to )
import System.Timeout

import Submarination.Creature
import Submarination.GameState
import Submarination.MonotonicClock
import Submarination.Level
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

    let rendering = do renderLevel monotonic_time_ns
                       renderCreatures
                       renderPlayer
                       renderHud monotonic_time_ns

    runReaderT rendering game_state

type GameMonadRoTerminal s a = GameMonadRo (MutateTerminal s) a

creatureToAppearance :: Creature -> Cell
creatureToAppearance creature = case creature of
  FoodVendor     -> Cell Vivid White Dull Yellow '@'
  AmmoVendor     -> Cell Vivid White Dull Yellow '@'
  MaterialVendor -> Cell Vivid White Dull Yellow '@'
  ToolVendor     -> Cell Vivid White Dull Yellow 'O'

renderCreatures :: GameMonadRoTerminal s ()
renderCreatures = do
  level <- gr currentLevel
  V2 px py <- view $ player.playerPosition

  lift $ ifor_ (level^.creatures) $ \(V2 cx cy) creature -> do
    -- relative position
    let (rx, ry) = (cx-px, cy-py)

    -- Only render creature if it's in renderable area
    when (rx >= -11 && rx <= 11 && ry >= -11 && ry <= 11) $ do
      -- position in terminal
      let (tx, ty) = ((+) rx *** (+) ry) mapMiddleOnTerminal

      setCell' tx ty (creatureToAppearance creature)

renderLevel :: Integer -> GameMonadRoTerminal s ()
renderLevel monotonic_time_ns = gr isOnSurface >>= \case
  True -> renderSurface monotonic_time_ns
  False -> renderAbyss

levelFeatureToAppearance :: LevelCell -> Double -> Int -> Int -> Cell
levelFeatureToAppearance lcell monotonic_time x y = case lcell of
  Water        -> Cell Vivid Blue Dull Black '~'
  WoodenBoards -> Cell Vivid Yellow Dull Black '.'
  Rock         -> Cell Vivid Black Vivid Black ' '
  MountainRock -> Cell Vivid White Vivid Black '^'
  Grass        -> Cell Vivid Green Dull Black '.'

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

renderSurface :: Integer -> GameMonadRoTerminal s ()
renderSurface monotonic_time_ns = do
  level <- gr currentLevel

  -- Render surface relative to player
  V2 px py <- view $ player.playerPosition

  lift $ for_ [V2 x y | x <- [px-11..px+11], y <- [py-11..py+11]] $ \levelcoord@(V2 lx ly) -> do
    let terminalcoord = ((+) (lx - px) *** (+) (ly - py)) mapMiddleOnTerminal
    -- What is the cell we should render?
        level_feature = level^.cellAt levelcoord

    -- Now...actually instruct the terminal to render a symbol for this level
    -- feature.

    uncurry setCell' terminalcoord (levelFeatureToAppearance level_feature monotonic_time lx ly)
 where
  monotonic_time = fromIntegral (monotonic_time_ns `div` 1000000) :: Double

renderAbyss :: Monad m => GameMonadRo m ()
renderAbyss = return ()

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

renderHud :: Integer -> GameMonadRoTerminal s ()
renderHud _monotonic_time_ns = do
  -- Title
  title <- gr currentAreaName
  let title_w = textWidth title
  lift $ setText (40-(title_w `div` 2)) 0 Vivid White Dull Black title

  -- Health
  hp     <- gr (^.player.playerHealth)
  max_hp <- gr (^.player.playerMaximumHealth)

  oxygen     <- gr (^.player.playerOxygen)
  max_oxygen <- gr getMaximumOxygenLevel

  renderBar "HP: " hp max_hp 53 2 Vivid Red Dull Blue
  renderBar "O₂: " oxygen max_oxygen 53 4 Vivid Cyan Dull Blue

  on_surface <- gr isOnSurface
  when on_surface renderSurfaceHud

renderSurfaceHud :: GameMonadRoTerminal s ()
renderSurfaceHud = do
  shells <- gr (^.player.playerShells)

  lift $ do
    setText 53 6 Vivid Yellow Dull Black "$"
    setText 54 6 Vivid White Dull Black $ ": " <> show shells

  gr getCurrentVendor >>= \case
    Just vendor -> do
      let desc = vendorDescription vendor
      lift $ setWrappedText 3 3 25 Dull White Dull Black desc
    _ -> return ()

