{-# LANGUAGE CPP #-}

module Submarination
  ( runSubmarination )
  where

import Control.Lens
import qualified Data.Map.Strict as M
import Prelude ( String )
import Protolude hiding ( to )

import Submarination.GameState
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

  in_inventory <- gm (^.menuState.to (Inventory `M.member`))

  ch <- getInputChar
  case ch of
#ifndef GHCJS_BROWSER
    'q' -> return ()
#endif
    dir_ch | dir_ch `elem` ("hjklyubn123456789" :: String) && not in_inventory -> do
      move dir_ch
      startGame knob
    menu_ch | menu_ch `elem` ("az " :: String) && not in_inventory -> do
      vendorMenu menu_ch
      startGame knob
    menu_ch | menu_ch `elem` ("az" :: String) && in_inventory -> do
      inventoryMenu menu_ch
      startGame knob
    'g' | not in_inventory -> do
      drag
      startGame knob
    'i' -> do
      inventory
      startGame knob
    ' ' | in_inventory -> do
      inventory
      startGame knob
    _ -> startGame knob

inventory :: Monad m => GameMonad m ()
inventory = menuState %= \menus ->
  if Inventory `M.member` menus
    then M.delete Inventory menus
    else M.insert Inventory 0 menus

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

inventoryMenu :: Monad m => Char -> GameMonad m ()
inventoryMenu ch = gm (^.currentInventoryMenuSelection) >>= \case
  Just menu_selection -> do
    inventory <- gm (^.player.playerInventory)
    let max_selection = M.size (groupItems inventory)-1
        current_selection = max 0 $ min max_selection menu_selection
    case ch of
      'a' | current_selection > 0 -> currentInventoryMenuSelection .= (Just $ current_selection-1)
      'z' | current_selection < max_selection -> currentInventoryMenuSelection .= (Just $ current_selection+1)
      _ -> return ()

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

