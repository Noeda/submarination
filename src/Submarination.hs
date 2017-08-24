{-# LANGUAGE CPP #-}

module Submarination
  ( runSubmarination )
  where

import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude ( String )
import Protolude hiding ( to, drop )

import Submarination.GameState
import Submarination.Render
import Submarination.Terminal
import Submarination.Vendor

runSubmarination :: IO ()
runSubmarination = withKeyboardTerminal $ do
  -- We run the rendering in one thread and input/logic in another. We use
  -- `UpdateRequestState` above in a STM TVar to communicate when we want to
  -- re-render.
  render_tvar <- newUpdateRequestKnob

  race_ (runTerminalStateT (terminalRenderer render_tvar))
        (splashScreen render_tvar)

splashScreen :: UpdateRequestStateKnob -> IO ()
splashScreen knob = do
  showSplashScreen knob
  ch <- getInputChar
  case ch of
#ifndef GHCJS_BROWSER
    'm' -> return ()
#endif
    ' ' -> evalStateT (startGame knob) initialGameState
    _ -> splashScreen knob

startGame :: MonadIO m => UpdateRequestStateKnob -> GameMonad m ()
startGame knob = do
  modify gsCycleGame
  game_state <- gm identity
  showGameState knob game_state

  in_active_menu <- gm gsInActiveMenu
  in_vendor <- gm gsIsVendoring
  gs <- gm identity

  ch <- getInputChar
  case ch of
#ifndef GHCJS_BROWSER
    'm' -> return ()
#endif
    ch -> do
      case ch of
        dir_ch | dir_ch `elem` ("hjklyubn123456789" :: String) && not in_active_menu ->
          modifyConditional $ move dir_ch

        -- Item menus
        ch | gs^?to gmActiveMenuHandler._Just.to offKeys.to (ch `S.member`) == Just True ->
          itemMenuOff
        ch | Just ms <- menuKeyToMenuStateTrigger ch,
             Just ms /= gmActiveMenu gs ->
          itemTrigger ms
        ' ' | Just MultiSelect <- gmCurrentSelectMode gs ->
          itemSelectAction
        ch | gs^?to gmActiveMenuHandler._Just.to menuKeys.to (ch `M.member`) == Just True ->
          itemMenuAction ch
        ch | ch `elem` ("az" :: String) && in_active_menu ->
          navigateCursor ch

        -- Vendor menu
        ch | ch `elem` ("az " :: String) && in_vendor ->
          vendorMenu ch

        -- Dragging
        'g' | not in_active_menu ->
          drag

        _ -> return ()

      startGame knob

itemSelectAction :: Monad m => GameMonad m ()
itemSelectAction = modifyConditional gmSelectToggleCursorItem

itemMenuAction :: Monad m => Char -> GameMonad m ()
itemMenuAction ch = use (to gmActiveMenuHandler) >>= \case
  Just handler | Just (_, action) <- menuKeys handler^.at ch -> do
    gs <- gm identity
    case action gs of
      Nothing -> return ()
      Just new_gs -> do
        put new_gs
        itemMenuOff

  _ -> return ()

drag :: Monad m => GameMonad m ()
drag = use (glPlayer.playerDragging) >>= \case
  Nothing -> startDragging
  Just bulky_item -> stopDragging bulky_item
 where
  stopDragging bulky_item =
    -- Can't stop if there is already a bulky item here
    use (gllAtPlayer glBulkyItemAt) >>= \case
      Nothing -> do
        gllAtPlayer glBulkyItemAt .= Just bulky_item
        glPlayer.playerDragging .= Nothing

        modify gsAdvanceTurn

      Just _existing_bulky_item -> return ()

  startDragging =
    use (gllAtPlayer glBulkyItemAt) >>= \case
      Nothing -> return ()
      Just bulky_item -> do
        gllAtPlayer glBulkyItemAt .= Nothing
        glPlayer.playerDragging .= Just bulky_item

        modify gsAdvanceTurn

itemTrigger :: Monad m => ActiveMenuState -> GameMonad m ()
itemTrigger ams = do
  modifyConditional $ gsEnterMenu ams

itemMenuOff :: Monad m => GameMonad m ()
itemMenuOff = modifyConditional gmCloseMenu

navigateCursor :: Monad m => Char -> GameMonad m ()
navigateCursor ch = case ch of
  'a' -> modifyConditional gmDecreaseMenuCursor
  'z' -> modifyConditional gmIncreaseMenuCursor
  _ -> return ()

vendorMenu :: Monad m => Char -> GameMonad m ()
vendorMenu ch = ((,) <$> gm gmCurrentVendorCreature <*> gm (^.glCurrentVendorMenuSelection)) >>= \case
  (Just vendor, Just current_selection') -> do
    let items = vendorItems vendor
        current_selection = max 0 $ min (length items-1) current_selection'
    case ch of
      'a' | current_selection > 0 -> glCurrentVendorMenuSelection .= (Just $ current_selection-1)
      'z' | current_selection < length items-1 -> glCurrentVendorMenuSelection .= (Just $ current_selection+1)
      ' ' -> modifyConditional gmAttemptPurchase
      _ -> return ()

  _ -> return ()

modifyConditional :: MonadState s m => (s -> Maybe s) -> m ()
modifyConditional modifier = do
  old <- get
  case modifier old of
    Nothing -> return ()
    Just new -> put new

move :: Char -> (GameState -> Maybe GameState)
move 'j' = gmMoveToDirection D2
move 'k' = gmMoveToDirection D8
move 'h' = gmMoveToDirection D4
move 'l' = gmMoveToDirection D6
move 'y' = gmMoveToDirection D7
move 'u' = gmMoveToDirection D9
move 'b' = gmMoveToDirection D1
move 'n' = gmMoveToDirection D3
move '2' = gmMoveToDirection D2
move '8' = gmMoveToDirection D8
move '4' = gmMoveToDirection D4
move '6' = gmMoveToDirection D6
move '7' = gmMoveToDirection D7
move '9' = gmMoveToDirection D9
move '1' = gmMoveToDirection D1
move '3' = gmMoveToDirection D3
move _ = const Nothing

