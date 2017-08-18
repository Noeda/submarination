module Submarination
  ( runSubmarination )
  where

import Prelude ( String )
import Protolude

import Submarination.GameState
import Submarination.Render
import Submarination.Terminal

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
    'q' -> return ()
    ' ' -> evalStateT (startGame knob) startGameState
    _ -> splashScreen knob

startGame :: MonadIO m => UpdateRequestStateKnob -> GameMonad m ()
startGame knob = do
  game_state <- gm identity
  showGameState knob game_state

  ch <- getInputChar
  case ch of
    'q' -> return ()
    dir_ch | dir_ch `elem` ("hjklyubn" :: String) -> do
      move dir_ch
      startGame knob
    _ -> startGame knob

move :: Monad m => Char -> GameMonad m ()
move 'j' = moveDirection D2
move 'k' = moveDirection D8
move 'h' = moveDirection D4
move 'l' = moveDirection D6
move 'y' = moveDirection D7
move 'u' = moveDirection D9
move 'b' = moveDirection D1
move 'n' = moveDirection D3
move _ = return ()

