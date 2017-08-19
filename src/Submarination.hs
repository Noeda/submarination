{-# LANGUAGE CPP #-}

module Submarination
  ( runSubmarination )
  where

import Control.Lens
import Prelude ( String )
import Protolude

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
    'q' -> return ()
#endif
    ' ' -> evalStateT (startGame knob) startGameState
    _ -> splashScreen knob

startGame :: MonadIO m => UpdateRequestStateKnob -> GameMonad m ()
startGame knob = do
  game_state <- gm identity
  showGameState knob game_state

  ch <- getInputChar
  case ch of
#ifndef GHCJS_BROWSER
    'q' -> return ()
#endif
    dir_ch | dir_ch `elem` ("hjklyubn123456789" :: String) -> do
      move dir_ch
      startGame knob
    menu_ch | menu_ch `elem` ("az " :: String) -> do
      menu menu_ch
      startGame knob
    _ -> startGame knob

menu :: Monad m => Char -> GameMonad m ()
menu ch = do
  gm getCurrentVendor >>= \case
    Nothing -> return ()
    Just vendor -> do
      current_selection' <- gm currentMenuSelection
      let items = vendorItems vendor
          current_selection = max 0 $ min (length items-1) current_selection'
      case ch of
        'a' | current_selection > 0 -> menuState .= MenuSelection (current_selection-1)
        'z' | current_selection < length items-1 -> menuState .= MenuSelection (current_selection+1)
        ' ' -> modify attemptPurchase
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

