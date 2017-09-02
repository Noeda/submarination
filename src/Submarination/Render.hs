{-# LANGUAGE CPP #-}


module Submarination.Render
  ( terminalRenderer
  , UpdateRequestState()
  , UpdateRequestStateKnob
  , newUpdateRequestKnob
  , showSplashScreen
  , showGameState
  -- * Debugging
  , renderLevelHuge )
  where

import Control.Arrow ( (***) )
import Control.Lens hiding ( Level )
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Char
import Data.Data
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Development.GitRev
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

gitRev :: Text
gitRev = $(gitBranch) <> "@" <> $(gitHash)

gitDate :: Text
gitDate = $(gitCommitDate)

renderSplashScreen :: MonadTerminalState m => m ()
renderSplashScreen = mutateTerminalStateM $ do
  clear
  unless (gitDate == "UNKNOWN") $ do
    setText 0 0 Dull White Dull Black gitRev
    setText 0 1 Dull White Dull Black gitDate

  setText 0 3 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 4 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 5 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 6 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 7 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 8 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 9 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 0 10 Vivid Blue Dull Black "≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈"
  setText 10 6 Vivid White Dull Black "  Submarination  "
  setText 17 7 Dull White Dull Black "^"
  setText 12 8 Dull White Dull Black "<#######)"

  setText 3 12 Dull White Dull Black "Make your choice:"
  setText 3 14  Vivid Cyan Dull Black "space)"
  setText 10 14 Vivid White Dull Black "Start a new game"

#ifndef GHCJS_BROWSER
  setText 3 15  Vivid Cyan Dull Black "q)"
  setText 10 15 Vivid White Dull Black "Quit"
#else
  setText 3 20 Dull White Dull Black "(c) 2017 Mikko Juola"
  setText 3 21 Dull White Dull Black "https://github.com/Noeda/submarination"
#endif

renderGameState :: MonadTerminalState m => GameState -> Integer -> m ()
renderGameState game_state monotonic_time_ns = runReaderT (renderGameState' monotonic_time_ns) game_state

renderGameState' :: MonadTerminalState m => Integer -> GameMonadRo m ()
renderGameState' monotonic_time_ns = do
  game_state <- gr identity
  in_active_menu <- gr $ isJust . gmActiveMenu
  is_dead <- gr gsIsDead

  lift $ mutateTerminalStateM $ do
    clear

    let rendering = do creature_count <- if in_active_menu
                         then return M.empty
                         else do renderCurrentLevel monotonic_time_ns
                                 renderSub monotonic_time_ns
                                 creatures <- renderCreatures
                                 renderPlayer
                                 return creatures
                       renderHud monotonic_time_ns creature_count

    if is_dead
      then runReaderT renderDeadScreen game_state
      else runReaderT rendering game_state

type GameMonadRoTerminal s = GameMonadRo (MutateTerminal s)

creatureToAppearance :: Creature -> Cell -> Cell
creatureToAppearance creature (Cell fintensity fcolor bintensity bcolor _) = case creature of
  FoodVendor     -> Cell Vivid White Dull Yellow '@'
  AmmoVendor     -> Cell Vivid White Dull Yellow '@'
  MaterialVendor -> Cell Vivid White Dull Yellow '@'
  ToolVendor     -> Cell Vivid White Dull Yellow 'O'
  Snoatfish      -> Cell Vivid Red   Dull Black 's'
  Biddy          -> Cell Vivid Blue  Dull Black 'b'
  Enneapus       -> Cell Dull Yellow Dull Black  'o'
  Camobream      -> Cell fintensity fcolor bintensity bcolor 'c'
  Gator          -> Cell Dull Green  Dull Black  'G'

itemToAppearance :: Item -> Cell
itemToAppearance SardineTin           = Cell Dull Cyan Dull Black '%'
itemToAppearance Poylent              = Cell Vivid Cyan Dull Black '%'
itemToAppearance Chicken              = Cell Vivid Red Dull Black '%'
itemToAppearance Potato               = Cell Vivid Yellow Dull Black '%'
itemToAppearance Whiskey              = Cell Dull Yellow Dull Black '!'
itemToAppearance Freezer{}            = Cell Vivid Cyan Dull Black '■'
itemToAppearance Refrigerator{}       = Cell Dull Cyan Dull Black '■'
itemToAppearance Microwave{}          = Cell Vivid White Dull Black '■'
itemToAppearance (StorageBox [])      = Cell Dull White Dull Black '±'
itemToAppearance StorageBox{}         = Cell Dull White Dull Black '≡'
itemToAppearance WoundedCorpse        = Cell Vivid White Dull Red '@'
itemToAppearance MutilatedCorpse      = Cell Vivid White Dull Red '@'
itemToAppearance BloatedCorpse        = Cell Vivid Green Dull Red '@'
itemToAppearance PartiallyEatenCorpse = Cell Vivid Green Dull Red '%'
itemToAppearance SkeletonCorpse       = Cell Dull White Vivid Black '@'
itemToAppearance PlantPersonCorpse    = Cell Dull Green Dull Red 'P'
itemToAppearance HullParts            = Cell Dull Yellow Dull Black '['
itemToAppearance Explosives           = Cell Vivid Red Dull Black '≡'
itemToAppearance Taser                = Cell Vivid Blue Dull Black '('
itemToAppearance Harpoon              = Cell Dull White Dull Black '('
itemToAppearance WinchAndCable        = Cell Vivid Yellow Dull Black ']'

reverseAppearance :: Cell -> Cell
reverseAppearance (Cell fintensity fcolor bintensity bcolor ch) =
  Cell bintensity bcolor fintensity fcolor ch

renderCreatures :: GameMonadRoTerminal s (M.Map Creature Int)
renderCreatures = do
  V2 px py <- view $ glPlayer.playerPosition
  gs <- gr identity

  lift $ flip execStateT M.empty $ for_ [V2 x y | x <- [negate losDistance..losDistance], y <- [negate losDistance..losDistance]] $ \(V2 rx ry) -> do
    -- relative position
    let (cx, cy) = (rx+px, ry+py)
        (tx, ty) = ((+) rx *** (+) ry) mapMiddleOnTerminal

    case gs^.glCreatureAt (V2 cx cy) of
      Nothing -> return ()
      Just creature -> do
        lift $ do
          old_cell <- getCell' tx ty
          setCell' tx ty (creatureToAppearance creature old_cell)

        unless (isVendor creature) $
          at creature %= \case Nothing -> Just 1; Just x -> Just (x+1)

renderCurrentLevel :: Integer -> GameMonadRoTerminal s ()
renderCurrentLevel monotonic_time_ns = do
  lvl <- gr gsCurrentLevel
  playerpos <- gr (^.glPlayer.playerPosition)
  renderLevel monotonic_time_ns lvl playerpos

levelFeatureToAppearance :: LevelCell -> Double -> Int -> Int -> Cell
levelFeatureToAppearance lcell monotonic_time x y = case lcell of
  WoodenBoards      -> Cell Vivid Yellow Dull Black '.'
  Rock              -> Cell Vivid Black Dull White ' '
  MountainRock      -> Cell Vivid White Dull White '^'
  DeepRock          -> Cell Vivid Black Vivid Black ' '
  DeepMountainRock  -> Cell Vivid White Vivid Black '^'
  Grass             -> Cell Vivid Green Dull Black '.'
  Hull              -> Cell Dull Yellow Dull Black '█'
  InteriorFloor     -> Cell Dull Cyan Dull Black '.'
  Hatch             -> Cell Vivid Yellow Dull Black '+'
  OpenHatch         -> Cell Vivid Yellow Dull Black '-'
  Window            -> Cell Vivid Cyan Dull Black '◘'
  Soil              -> Cell Dull Yellow Dull Black '.'
  Blood             -> Cell Vivid Red Dull Black '≈'
  BloodCoagulated   -> Cell Dull Red Dull Black '≈'

  cell | cell == Water ->
    let theta = sin (dx*9338.3) + cos (sin (dy*3.3)*119) - sin (sin dx+cos dy*3331 + sin sectime)

     in if theta < 0.9
          then Cell Vivid Blue Dull Black '.'
          else Cell Dull Blue Dull Black '.'

  cell | cell == HappyCoral ->
    let theta = sin (dx*9238.3) + cos (sin (dy*3.3)*117) - sin (sin dx+cos dy*333) + sin (sectime*0.1)
        ch = if sin (dx*37.728) + cos (sin (dy*2.71)*622) - sin (cos dx+sin (dy*1.3)*17) > 0.2
               then '☼'
               else '○'
     in if | theta > 0.5
             -> Cell Vivid White Dull Black ch
           | theta > 0.2
             -> Cell Vivid Blue Dull Black ch
           | theta > -0.2
             -> Cell Vivid Green Dull Black ch
           | theta > -0.4
             -> Cell Vivid Yellow Dull Black ch
           | theta > -0.95
             -> Cell Vivid White Dull Black ch
           | otherwise
             -> Cell Vivid Red Dull Black ch

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
  V2 sx sy <- gr (^.glSub.subPosition)
  V2 sw sh <- gr (^.glSub.subTopology.to subSize)
  V2 px py <- view $ glPlayer.playerPosition

  topo <- gr (^.glSub.subTopology)
  oxygen <- gr (^.glPlayer.playerOxygen)

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

          when (oxygen < 0) $ do
            Cell _ _ _ _ ch <- uncurry getCell' terminalcoord
            render_at (Cell Dull Red Dull Black ch)
 where
  monotonic_time = fromIntegral (monotonic_time_ns `div` 1000000) :: Double

renderItemPile :: Monad m => (Cell -> m ()) -> [Item] -> m ()
renderItemPile _action [] = return ()
renderItemPile action [single_item] =
  action $ itemToAppearance single_item
renderItemPile action (first_item:_rest) =
  action $ reverseAppearance $ itemToAppearance first_item

renderLevel :: Integer -> Level -> V2 Int -> GameMonadRoTerminal s ()
renderLevel monotonic_time_ns level (V2 ox oy) = do
  oxygen <- gr (^.glPlayer.playerOxygen)
  lift $ for_ [V2 x y | x <- [ox-losDistance..ox+losDistance], y <- [oy-losDistance..oy+losDistance]] $ \levelcoord@(V2 lx ly) -> do
    let terminalcoord = ((+) (lx - ox) *** (+) (ly - oy)) mapMiddleOnTerminal
    -- What is the cell we should render?
        level_feature = level^.cellAt levelcoord

    -- Now...actually instruct the terminal to render a symbol for this level
    -- feature.

    let render_at = uncurry setCell' terminalcoord

    render_at $ levelFeatureToAppearance level_feature monotonic_time lx ly
    renderItemPile render_at (level^.itemsAt levelcoord)

    when (oxygen < 0) $ do
      Cell _ _ _ _ ch <- uncurry getCell' terminalcoord
      render_at (Cell Dull Red Dull Black ch)
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

renderBar :: Text -> Int -> Int -> Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Integer -> Text -> GameMonadRoTerminal s ()
renderBar label value max_value x y bar_intensity bar_color unbar_intensity unbar_color monotonic_time_ns extratext = do
  let portion = fromIntegral value / fromIntegral max_value :: Double
      bar_len = 10 :: Int
      bar_txt_head = blocksText $ fromIntegral bar_len * portion
      bar_txt_tail = T.replicate (bar_len - T.length bar_txt_head) " "

  lift $ do
    setText (x+12) y Vivid White Dull Black "├"
    setText (x+23) y Vivid White Dull Black "┤"
    setText (x+13) y bar_intensity bar_color unbar_intensity unbar_color (bar_txt_head <> bar_txt_tail)
    setText x y Vivid White Dull Black $ label <> show value <> "/" <> show max_value

    when ((monotonic_time_ns `div` 1000000000) `mod` 2 == 0) $
      setText (x+25) y Vivid Yellow Vivid Red extratext

data ActiveSide
  = LeftSide
  | RightSide
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | Type that keeps track of Y position while we render stuff on hud. This
-- pushes text below if there are messages or whatever. Monad transformer.
newtype VerticalBoxRender m a = VerticalBoxRender (StateT (Int, Int, ActiveSide) m a)
  deriving ( Functor, Applicative, Monad )

instance MonadTrans VerticalBoxRender where
  lift = VerticalBoxRender . lift

instance MonadReader r m => MonadReader r (VerticalBoxRender m) where
  ask = VerticalBoxRender $ lift ask
  local mod (VerticalBoxRender action) = VerticalBoxRender $ do
    st <- get
    (result, new_st) <- lift $ local mod (runStateT action st)
    put new_st
    return result

withSide :: Monad m => ActiveSide -> VerticalBoxRender m a -> VerticalBoxRender m a
withSide side (VerticalBoxRender st) = VerticalBoxRender $
  use _3 >>= \old_side ->
    (_3 .= side) *> st <* (_3 .= old_side)

getSideY :: Monad m => VerticalBoxRender m Int
getSideY = VerticalBoxRender $
  use _3 >>= \case
    LeftSide -> use _1
    RightSide -> use _2

putSideY :: Monad m => Int -> VerticalBoxRender m ()
putSideY y = VerticalBoxRender $ use _3 >>= \case
  LeftSide  -> _1 .= y
  RightSide -> _2 .= y

runVerticalBoxRender :: Monad m => Int -> Int -> VerticalBoxRender m a -> m a
runVerticalBoxRender y_left_start y_right_start (VerticalBoxRender stateful) =
  evalStateT stateful (y_left_start, y_right_start, RightSide)

appendText :: Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Text -> VerticalBoxRender (GameMonadRoTerminal s) ()
appendText x skip fintensity fcolor bintensity bcolor txt = do
  y <- getSideY
  putSideY (y+skip)

  lift $ lift $ setText x y fintensity fcolor bintensity bcolor txt

appendWrappedText :: Int -> Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Text -> VerticalBoxRender (GameMonadRoTerminal s) ()
appendWrappedText x skip max_width fintensity fcolor bintensity bcolor txt = do
  y <- getSideY
  height <- lift $ lift $ setWrappedText x y max_width fintensity fcolor bintensity bcolor txt

  putSideY (y+height+skip)

renderHud :: Integer -> M.Map Creature Int -> GameMonadRoTerminal s ()
renderHud monotonic_time_ns creature_count = do

  -- The name of the section of submarine you are in
  player_pos <- gr (^.glPlayer.playerPosition)
  sub_pos <- gr (^.glSub.subPosition)
  sub_topo <- gr (^.glSub.subTopology)

  case getLocationNameInSub (player_pos - sub_pos) sub_topo of
    Just sub_section_name -> do
      current_area_name <- gr gsCurrentAreaName
      let sub_title = "+++ " <> sub_section_name <> ", " <> current_area_name <> " +++"
      let sub_title_w = textWidth sub_title
      lift $ setText (40-(sub_title_w `div` 2)) 0 Vivid White Dull Black sub_title
    Nothing -> do
      title' <- gr gsCurrentAreaName
      let title = "--- " <> title' <> " ---"
          title_w = textWidth title
      lift $ setText (40-(title_w `div` 2)) 0 Vivid White Dull Black title

  -- Health/Oxygen
  hp'    <- gr (^.glPlayer.playerHealth)
  max_hp <- gr (^.glPlayer.playerMaximumHealth)

  oxygen'     <- gr (^.glPlayer.playerOxygen)
  max_oxygen  <- gr gsMaximumOxygenLevel

  let oxygen = max 0 $ min max_oxygen oxygen'
      hp = max 0 $ min max_hp hp'

  renderBar "HP: " hp max_hp 53 2 Vivid Red Dull Blue monotonic_time_ns ""

  if oxygen >= 15
    then renderBar "O₂: " oxygen max_oxygen 53 4 Vivid Cyan Dull Blue monotonic_time_ns ""
    else renderBar "O₂: " oxygen max_oxygen 53 4 Vivid Cyan Dull Blue monotonic_time_ns "!"

  renderTurn

  maybe_item_menu_handler <- gr gmActiveMenuHandler

  case maybe_item_menu_handler of
    Just item_menu_handler ->
      runVerticalBoxRender 1 2 (renderItemMenu item_menu_handler)
    _ -> runVerticalBoxRender 1 6 (renderAdditionalHud creature_count)

renderItemMenu :: ItemMenuHandler -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderItemMenu item_handler = do
  unless (T.null $ menuText item_handler) $
    appendWrappedText 1 2 40 Dull White Dull Black (menuText item_handler)

  items <- filter (menuFilter item_handler) <$> gr (^.itemLens item_handler)
  let gitems = groupItems items

  (case selectMode item_handler of
     SingleSelect -> singleOrNotSelectableSelectRender True
     MultiSelect -> multiSelectRender
     NotSelectable -> singleOrNotSelectableSelectRender False) gitems


  appendText 2 1 Dull Yellow Dull Black ""

  gr gmCurrentlySelectedCount >>= \case
    Nothing -> return ()
    Just counter -> appendText 2 2 Dull White Dull Black $ "Select #: " <> show counter <> "_"
 where
  actionInstructions gs = M.assocs (menuKeys item_handler) <&> \(ch, (text, action)) ->
    case action gs of
      Left err -> ([T.singleton $ toUpper ch], text <> " !" <> err <> "!", Warning)
      Right _  -> ([T.singleton $ toUpper ch], text, Okay)

  otherInstructions gs = M.assocs (otherKeys item_handler gs) <&> \(ch, text) ->
    ([T.singleton $ toUpper ch], text, Okay)

  renderArrowedItem item = do
    appendText 2 0 Vivid Green Dull Black "➔"
    lift $ lift $ setText 53 6 Vivid White Dull Black $ itemName item Singular
    void $ lift $ lift $ setWrappedText 53 8 25 Dull White Dull Black $ itemDescription item

  multiSelectRender gitems = do
    cursor <- gr gmMenuCursor
    selections <- fromMaybe M.empty <$> gr gmMenuSelections

    for_ (zip [0..] (M.assocs gitems)) $ \(index, (item, count)) -> do
      let fintensity = if Just index == cursor then Vivid else Dull

      if | Just num_selected <- M.lookup index selections,
           num_selected >= count
           -> appendText 4 0 fintensity Yellow Dull Black "[✓]"
         | Just num_selected <- M.lookup index selections
           -> appendText 4 0 fintensity Yellow Dull Black $ "[" <> show num_selected <> "]"
         | otherwise -> appendText 4 0 fintensity Yellow Dull Black "[ ]"

      when (Just index == cursor) $
        renderArrowedItem item

      if count == 1
        then appendText 8 1 fintensity Yellow Dull Black $ itemName item Singular
        else appendText 8 1 fintensity Yellow Dull Black $ show count <> " " <> itemName item Many

    gs <- gr identity

    appendText 4 1 Dull White Dull White ""
    renderKeyInstructions ([(T.singleton <$> (toUpper <$> S.toList (offKeys item_handler)), "Cancel", Okay), (["SPACE"], "Select", Okay)] <> actionInstructions gs <> otherInstructions gs) 2

  singleOrNotSelectableSelectRender is_single gitems = do
    selection <- gr gmMenuCursor

    for_ (zip [0..] (M.assocs gitems)) $ \(index, (item, count)) -> do
      let fintensity = if Just index == selection && is_single
                         then Vivid
                         else Dull

      when (Just index == selection && is_single) $
        renderArrowedItem item

      if count == 1
        then appendText 4 1 fintensity Yellow Dull Black $ itemName item Singular
        else appendText 4 1 fintensity Yellow Dull Black $ show count <> " " <> itemName item Many

    gs <- gr identity

    if is_single
      then do appendText 4 1 Dull White Dull White ""
              renderKeyInstructions (actionInstructions gs <> otherInstructions gs) 4
      else do appendText 4 1 Dull White Dull White ""
              renderKeyInstructions ([(["SPACE"], "Close", Okay)] <> actionInstructions gs <> otherInstructions gs) 4

data Usability
  = Warning
  | Okay
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

renderKeyInstructions :: [([Text], Text, Usability)] -> Int -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderKeyInstructions insts original_x = go insts original_x
 where
  go [] _ = return ()
  go ((key, _action, _):rest) x | null key = go rest x
  go ((key, action, usability):rest) x = do
    appendText x 0 fintensity fcolor Dull Black "["
    go2 key (x+1)
   where
    (fintensity, fcolor) = case usability of
      Okay    -> (Dull, White)
      Warning -> (Vivid, Red)

    go2 [] x = do
      appendText x 1 fintensity fcolor Dull Black $ "] " <> action
      go rest original_x

    go2 (key1:key2:restkeys) x = do
      appendText x 0 Vivid Green Dull Black key1
      appendText (x+textWidth key1) 0 Dull White Dull Black " or "
      go2 (key2:restkeys) (x+textWidth key1+4)

    go2 [last_key] x = do
      appendText x 0 Vivid Green Dull Black last_key
      go2 [] (x+textWidth last_key)

renderCreatureListing :: M.Map Creature Int -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderCreatureListing creatures =
  renderList (M.assocs creatures) $ \(creature, count) y -> do
    lift $ lift $ setCell' 2 y (creatureToAppearance creature $ Cell Dull White Dull Black ' ')

    if count == 1
      then appendText 4 1 Dull White Dull Black $ creatureName creature Singular
      else appendText 4 1 Dull White Dull Black $ show count <> " " <> creatureName creature Many

renderList :: [a] -> (a -> Int -> VerticalBoxRender (GameMonadRoTerminal s) ()) -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderList lst fun = go lst (0 :: Int)
 where
  go [] _ = appendText 2 1 Dull White Dull Black ""
  go (x:rest) num_items | num_items < 7 = do
    y <- getSideY
    fun x y
    go rest (num_items+1)

  go _ _ =
    appendText 2 2 Dull White Dull Black "...and more"

renderItemPileHud :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderItemPileHud = do
  player_pos <- gr (^.glPlayer.playerPosition)
  items <- gr (^.glItemsAt player_pos)
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

  unless (not $ any isItemBulky items) $ do
    appendText 53 0 Dull White Dull Black "[ ] Drag"
    appendText 54 2 Vivid Green Dull Black "G"

statusAppearance :: Status -> (ColorIntensity, Color, ColorIntensity, Color)
statusAppearance Slow     = (Dull, Red, Dull, Black)
statusAppearance Hungry   = (Dull, Red, Dull, Black)
statusAppearance Starving = (Vivid, Red, Dull, Black)
statusAppearance Satiated = (Vivid, Green, Dull, Black)

renderStatuses :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderStatuses = do
  statuses <- gr gsSanitizedCurrentStatuses
  unless (null statuses) $ do
    flip evalStateT 53 $ for_ statuses $ \status -> do
      let status_name = statusName status
          (fintensity, fcolor, bintensity, bcolor) = statusAppearance status
      x <- get

      let status_width = textWidth status_name

      lift $ appendText x 0 fintensity fcolor bintensity bcolor status_name
      put (x+status_width+1)

    appendText 0 2 Vivid White Dull Black ""

renderDragging :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderDragging = gr (^.glPlayer.playerDragging) >>= \case
  Nothing -> return ()

  Just bulky_item -> do
    appendText 53 2 Vivid White Dull Black $ "Dragging: " <> itemName bulky_item Singular
    appendText 53 0 Dull White Dull Black "[ ] Stop"
    appendText 54 2 Vivid Green Dull Black "G"

renderMessages :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderMessages =
  T.strip <$> gr gsCurrentMessage >>= \msg ->
    unless (T.null msg) $
      appendWrappedText 2 2 25 Vivid Cyan Dull Black msg

renderTurn :: GameMonadRoTerminal s ()
renderTurn = do
  current_turn <- gr gsTurn

  let turn_text = "T: " <> show current_turn
  lift $ setText (80-textWidth turn_text) 0 Vivid White Dull Black turn_text

renderPossibleTriggerKeys :: VerticalBoxRender (GameMonadRoTerminal s) ()
renderPossibleTriggerKeys = do
  gs <- gr identity
  renderKeyInstructions (catMaybes (toInstructions gs <$> handlers)) 53
 where
  handlers = menuItemHandler <$> enumFrom (toEnum 0)

  toInstructions :: GameState -> ItemMenuHandler -> Maybe ([Text], Text, Usability)
  toInstructions gs handler
    | prerequisites handler gs = Just (T.singleton . toUpper <$> S.toList (triggerKeys handler), menuName handler, Okay)
    | otherwise = Nothing

renderInventorySummary :: Int -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderInventorySummary x = do
  inventory <- gr (^.glPlayer.playerInventory)
  let num_items = length inventory
  if | num_items == 0
       -> appendText x 2 Vivid White Dull Black "Not carrying anything."
     | num_items == 1
       -> appendText x 2 Vivid White Dull Black "Carrying 1 item."
     | otherwise
       -> appendText x 2 Vivid White Dull Black $ "Carrying " <> show num_items <> " items."

renderDeadScreen :: GameMonadRoTerminal s ()
renderDeadScreen = do
  turn <- gr gsTurn
  death_reason <- gr gsDeathReason
  lift $ do
    void $ setWrappedText 2 2 60 Dull White Dull Black "You are DEAD, R.I.P."
    void $ setWrappedText 2 4 60 Dull White Dull Black $ "Cause of death: " <> death_reason
    void $ setWrappedText 2 6 60 Dull White Dull Black $ "You survived for " <> show turn <> " turns."

renderAdditionalHud :: M.Map Creature Int -> VerticalBoxRender (GameMonadRoTerminal s) ()
renderAdditionalHud creature_count = do
  -- Money only has purpose on surface at the moment
  on_surface <- gr gsIsOnSurface

  when on_surface $ do
    shells <- gr (^.glPlayer.playerShells)

    appendText 53 0 Vivid Yellow Dull Black "$"
    appendText 54 2 Vivid White Dull Black $ ": " <> show shells

  renderStatuses
  renderDragging
  renderItemPileHud

  when on_surface $ withSide LeftSide $ ((,) <$> gr gmCurrentVendorCreature <*> gr (^.glCurrentVendorMenuSelection)) >>= \case
    (Just vendor, Just menu_selection) -> do
      let desc = vendorDescription vendor
      _ <- appendWrappedText 3 2 25 Dull White Dull Black desc

      for_ (zip (vendorItems vendor) [0..]) $ \(item, selection_num) -> do
        if selection_num == menu_selection
          then do appendText 2 0 Vivid Green Dull Black "➔"
                  appendText 4 0 Vivid Blue Dull Black (itemName item Singular)
                  withSide RightSide $ do
                    appendText 53 2 Vivid White Dull Black (itemName item Singular)
                    appendWrappedText 53 2 25 Dull White Dull Black (itemDescription item)
          else appendText 4 0 Dull Blue Dull Black (itemName item Singular)

        appendText 24 0 Vivid Yellow Dull Black "$"
        appendText 25 1 Vivid White Dull Black $ show $ itemPrice item

      appendText 2 1 Dull White Dull Black ""
      appendText 2 0 Dull White Dull Black "[ ] ↑ [ ] ↓"
      appendText 3 0 Vivid Green Dull Black "A"
      appendText 9 1 Vivid Green Dull Black "Z"
      appendText 2 0 Dull White Dull Black "[     ] Purchase"
      appendText 3 2 Vivid Green Dull Black "SPACE"

    _ -> return ()

  withSide LeftSide renderMessages

  renderInventorySummary 53
  withSide LeftSide $ renderCreatureListing creature_count
  renderPossibleTriggerKeys

-- | This is used for debugging level generation. It prints the level to the
-- terminal as big as it can.
renderLevelHuge :: MonadIO m => Level -> m ()
renderLevelHuge lvl = liftIO $ withKeyboardTerminal $
  race_ (void getInputChar) $ forever $ runTerminalStateT $ do
    (w, h) <- getTerminalSize
    monotonic_time_ns <- getMonotonicTime
    let monotonic_time = fromIntegral (monotonic_time_ns `div` 1000000) :: Double

    mutateTerminalStateM $ do
      clear
      for_ [0..w-1] $ \x ->
        for_ [0..h-1] $ \y -> do
          let cell       = lvl^.cellAt (V2 x y)
              appearance = levelFeatureToAppearance cell monotonic_time x y

          setCell' x y appearance

    liftIO $ threadDelay 100000

