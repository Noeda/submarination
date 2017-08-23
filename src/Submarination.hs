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
import Submarination.GameState.Types
import Submarination.Item
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
    'q' -> return ()
#endif
    ' ' -> evalStateT (startGame knob) startGameState
    _ -> splashScreen knob

startGame :: MonadIO m => UpdateRequestStateKnob -> GameMonad m ()
startGame knob = do
  cycleGame
  game_state <- gm identity
  showGameState knob game_state

  in_active_menu <- gm inActiveMenu
  in_vendor <- gm (^.menuState.at Vendor . to isJust)
  gs <- gm identity

  ch <- getInputChar
  case ch of
#ifndef GHCJS_BROWSER
    'q' -> return ()
#endif
    ch -> do
      case ch of
        dir_ch | dir_ch `elem` ("hjklyubn123456789" :: String) && not in_active_menu -> do
          move dir_ch

        -- Item menus
        ch | gs^?to getActiveMenuHandler._Just.to offKeys.to (ch `S.member`) == Just True ->
          itemMenuOff
        ch | gs^?to getActiveMenuHandler._Just.to menuKeys.to (ch `M.member`) == Just True ->
          itemMenuAction ch
        ch | Just ms <- keyToMenuStateTrigger ch ->
          itemTrigger ms
        ' ' | in_active_menu ->
          itemMenuOff
        ch | ch `elem` ("az" :: String) && in_active_menu ->
          itemMenu ch

        -- Vendor menu
        ch | ch `elem` ("az " :: String) && in_vendor ->
          vendorMenu ch

        -- Dragging
        'g' | not in_active_menu ->
          drag

        _ -> return ()

      startGame knob

itemMenuAction :: Monad m => Char -> GameMonad m ()
itemMenuAction ch = use (to getActiveMenuHandler) >>= \case
  Just handler | Just (_, action) <- menuKeys handler^.at ch -> do
    gs <- gm identity
    case action gs of
      Nothing -> return ()
      Just new_gs -> do
        put new_gs
        itemMenuOff

  _ -> return ()

drag :: Monad m => GameMonad m ()
drag = use (player.playerDragging) >>= \case
  Nothing -> startDragging
  Just bulky_item -> stopDragging bulky_item
 where
  stopDragging bulky_item = do
    -- Can't stop if there is already a bulky item here
    player_pos <- gm (^.player.playerPosition)
    use (levelBulkyItemAt player_pos) >>= \case
      Nothing -> do
        levelBulkyItemAt player_pos .= Just bulky_item
        player.playerDragging .= Nothing

        advanceTurn

      Just _existing_bulky_item -> return ()

  startDragging = do
    player_pos <- gm (^.player.playerPosition)
    use (levelBulkyItemAt player_pos) >>= \case
      Nothing -> return ()
      Just bulky_item -> do
        levelBulkyItemAt player_pos .= Nothing
        player.playerDragging .= Just bulky_item

        advanceTurn

itemTrigger :: Monad m => MenuState -> GameMonad m ()
itemTrigger ms = do
  old_state <- use (menuState.at ms)
  case old_state of
    Nothing -> menuState.at ms .= Just 0
    Just{}  -> menuState.at ms .= Nothing

itemMenuOff :: Monad m => GameMonad m ()
itemMenuOff = gm getActiveMenu >>= \case
  Just ms -> menuState. at ms .= Nothing
  _ -> return ()

itemMenu :: Monad m => Char -> GameMonad m ()
itemMenu ch = gm getActiveMenuHandler >>= \case
  Nothing -> return ()
  Just handler -> do
    items <- filter (menuFilter handler) <$> gm (^.itemLens handler)
    menu_selection <- fromMaybe 0 <$> gm (^.selectionLens handler)
    let max_selection = M.size (groupItems items)-1
        current_selection = max 0 $ min max_selection menu_selection

    case ch of
      'a' | current_selection > 0 ->
        selectionLens handler .= (Just $ current_selection-1)
      'z' | current_selection < max_selection ->
        selectionLens handler .= (Just $ current_selection+1)
      _ -> return ()

vendorMenu :: Monad m => Char -> GameMonad m ()
vendorMenu ch = ((,) <$> gm getCurrentVendor <*> gm (^.currentVendorMenuSelection)) >>= \case
  (Just vendor, Just current_selection') -> do
    let items = vendorItems vendor
        current_selection = max 0 $ min (length items-1) current_selection'
    case ch of
      'a' | current_selection > 0 -> currentVendorMenuSelection .= (Just $ current_selection-1)
      'z' | current_selection < length items-1 -> currentVendorMenuSelection .= (Just $ current_selection+1)
      ' ' -> modify attemptPurchase
      _ -> return ()

  _ -> return ()

move :: Monad m => Char -> GameMonad m ()
move 'j' = moveDirection D2
move 'k' = moveDirection D8
move 'h' = moveDirection D4
move 'l' = moveDirection D6
move 'y' = moveDirection D7
move 'u' = moveDirection D9
move 'b' = moveDirection D1
move 'n' = moveDirection D3
move '2' = moveDirection D2
move '8' = moveDirection D8
move '4' = moveDirection D4
move '6' = moveDirection D6
move '7' = moveDirection D7
move '9' = moveDirection D9
move '1' = moveDirection D1
move '3' = moveDirection D3
move _ = return ()

