{-# LANGUAGE CPP #-}

module Submarination
  ( runSubmarination )
  where

import Control.Lens
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude ( String )
import Protolude hiding ( to, drop )

import Submarination.GameState
import Submarination.Key
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
    CharKey 'm' -> return ()
#endif
    CharKey ' ' -> evalStateT (startGame knob) initialGameState
    _ -> splashScreen knob

startGame :: MonadIO m => UpdateRequestStateKnob -> GameMonad m ()
startGame knob = do
  modify gsCycleGame
  game_state <- gm identity
  showGameState knob game_state

  in_active_menu <- gm gsInActiveMenu
  in_vendor <- gm gsIsVendoring
  gs_pre_input <- gm identity

  ch <- getInputChar
  let gs = gsAdvanceInputTurn gs_pre_input
  put gs

  case ch of
#ifndef GHCJS_BROWSER
    CharKey 'm' -> return ()
#endif
    ch | not (gsIsDead gs) -> do
      case ch of
        CharKey '.' | not in_active_menu ->
          modify gsWait

        CharKey dir_ch | dir_ch `elem` ("hjklyubn123456789" :: String) && not in_active_menu ->
          modifyConditional $ move (CharKey dir_ch)

        dir_ch | dir_ch == KeyUp || dir_ch == KeyLeft || dir_ch == KeyRight || dir_ch == KeyDown ->
          modifyConditional $ move dir_ch

        -- Item menus
        CharKey digit | isDigit digit && in_active_menu ->
          itemDigitSelect (ord digit - ord '0')
        CharKey ch | gs^?to gmActiveMenuHandler._Just.to offKeys.to (ch `S.member`) == Just True ->
          itemMenuOff
        CharKey ch | Just ms <- menuKeyToMenuStateTrigger gs ch,
                     Just ms /= gmActiveMenu gs ->
          itemTrigger ms
        CharKey ch | Just MultiSelect <- gmCurrentSelectMode gs,
                     ch == ' ' || ch == '\n' || ch == '\r' ->
          itemSelectAction
        CharKey ch | gs^?to gmActiveMenuHandler._Just.to menuKeys.to (ch `M.member`) == Just True ->
          itemMenuAction ch
        CharKey ch | ch `elem` ("az" :: String) && in_active_menu ->
          navigateCursor ch

        -- Vendor menu
        CharKey ch | ch `elem` ("az " :: String) && in_vendor ->
          vendorMenu ch

        -- Dragging
        CharKey 'g' | not in_active_menu ->
          drag

        _ -> modify gsRetractInputTurn

      startGame knob
    _ -> startGame knob

itemDigitSelect :: Monad m => Int -> GameMonad m ()
itemDigitSelect = modifyConditional . gmInsertMenuDigit

itemSelectAction :: Monad m => GameMonad m ()
itemSelectAction = modifyConditional gmSelectToggleCursorItem

itemMenuAction :: Monad m => Char -> GameMonad m ()
itemMenuAction ch = use (to gmActiveMenuHandler) >>= \case
  Just handler | Just (_, action) <- menuKeys handler^.at ch -> do
    gs <- gm identity
    case action gs of
      Left{} -> return ()
      Right new_gs -> do
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
itemTrigger = modifyConditional . gmEnterMenu

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
  for_ (modifier old) put

move :: Key -> (GameState -> Maybe GameState)
move (CharKey 'j') = gmMoveToDirection D2
move (CharKey 'k') = gmMoveToDirection D8
move (CharKey 'h') = gmMoveToDirection D4
move (CharKey 'l') = gmMoveToDirection D6
move (CharKey 'y') = gmMoveToDirection D7
move (CharKey 'u') = gmMoveToDirection D9
move (CharKey 'b') = gmMoveToDirection D1
move (CharKey 'n') = gmMoveToDirection D3
move (CharKey '2') = gmMoveToDirection D2
move (CharKey '8') = gmMoveToDirection D8
move (CharKey '4') = gmMoveToDirection D4
move (CharKey '6') = gmMoveToDirection D6
move (CharKey '7') = gmMoveToDirection D7
move (CharKey '9') = gmMoveToDirection D9
move (CharKey '1') = gmMoveToDirection D1
move (CharKey '3') = gmMoveToDirection D3
move KeyUp         = gmMoveToDirection D8
move KeyLeft       = gmMoveToDirection D4
move KeyRight      = gmMoveToDirection D6
move KeyDown       = gmMoveToDirection D2
move _ = const Nothing

