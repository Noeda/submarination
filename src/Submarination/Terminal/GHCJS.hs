module Submarination.Terminal.GHCJS
  ( module System.Console.ANSI
  , withKeyboardTerminal
  , paintTerminalM
  , paintTerminal
  , getTerminalSize
  , getInputChar
  , mutateTerminalStateM )
  where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Data.Array ( (!) )
import Data.String ( fromString )
import Data.JSString
import qualified Data.Map.Strict as M
import Data.Maybe
import GHCJS.Foreign.Callback
import GHCJS.Types
import Protolude
import System.Console.ANSI
import System.IO.Unsafe ( unsafePerformIO )

import Submarination.Terminal.Common

foreign import javascript "$r = document.createElement('span');" make_span_element :: IO JSVal
foreign import javascript "$r = document.createElement('br');" make_br_element :: IO JSVal
foreign import javascript "$r = document.createElement('div');" make_div_element :: IO JSVal
foreign import javascript "$1.appendChild($2)" append_child :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.textContent = $2" set_text_content :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1.style = $2" set_style :: JSVal -> JSString -> IO ()
foreign import javascript "$r = document.getElementById('content')" get_content_div :: IO JSVal
foreign import javascript "$1.addEventListener('keydown', $2);" attach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript "$1.removeEventListener('keydown', $2);" detach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript "$r = document.getElementsByTagName('body')[0];" get_body :: IO JSVal
foreign import javascript unsafe "$r = $1.key || String.fromCharCode($1.keyCode);" get_event_char :: JSVal -> IO JSString

globalSpans :: MVar (M.Map (Int, Int) JSVal)
globalSpans = unsafePerformIO $ newMVar M.empty
{-# NOINLINE globalSpans #-}

inputKey :: TVar (Maybe Char)
inputKey = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE inputKey #-}

withKeyboardTerminal :: IO a -> IO a
withKeyboardTerminal action = mask $ \restore -> do
  -- Setup DOM stuff

  -- This div holds everything
  term_div <- make_div_element
  set_style term_div "font-family: monospace;"

  spanner term_div (restore action)

spanner :: JSVal -> IO a -> IO a
spanner term_div action = do
  -- Spans that show each symbol
  spans <- fmap M.fromList $ for [ (x, y) | x <- [0..79], y <- [0..23] ] $ \(x, y) -> do
    span <- make_span_element
    set_text_content span "\x00a0"
    return ((x, y), span)

  let _ = spans :: M.Map (Int, Int) JSVal

  -- Attach the spans (and <br />s) to the div
  for_ [0..23] $ \y -> do
    for_ [0..79] $ \x ->
      append_child term_div (fromJust $ M.lookup (x, y) spans)
    append_child term_div =<< make_br_element

  content_div <- get_content_div
  append_child content_div term_div

  modifyMVar_ globalSpans $ \_ -> return spans

  callback <- asyncCallback1 $ \val -> do
    js_str <- get_event_char val
    let ch_str = unpack js_str
    for_ ch_str $ \ch ->
      atomically $ readTVar inputKey >>= \case
        Nothing -> writeTVar inputKey (Just ch)
        Just{} -> retry

  body <- get_body
  set_style body "background: #000;"
  attach_keydown_handler body callback

  finally action $ do
    detach_keydown_handler body callback
    releaseCallback callback

paintTerminalM :: MonadTerminalState m => TerminalState -> m ()
paintTerminalM term = do
  old_term <- getTerminal
  paintTerminal old_term term
  putTerminal term

paintCell :: Cell -> JSVal -> IO ()
paintCell cell span = do
  let ch' = cell^.char
      ch = if ch' == ' ' then '\x00a0' else ch'
      style = cellToStyle cell

  set_text_content span $ fromString [ch]
  set_style span style
{-# INLINE paintCell #-}

paintTerminal :: MonadIO m => Maybe TerminalState -> TerminalState -> m ()
paintTerminal Nothing term = liftIO $ paintAllTerminal term
paintTerminal (Just old_term) term = liftIO $ do
  spans <- readMVar globalSpans
  go 0 0 spans
 where
  go x y spans | x >= 80 = go 0 (y+1) spans
  go _ y _ | y >= 24 = return ()

  go x y spans | cells old_term ! (x, y) /= cells term ! (x, y) = do
    paintCell (cells term ! (x, y))
              (fromJust $ M.lookup (x, y) spans)
    go (x+1) y spans

  go x y spans = go (x+1) y spans

cellToStyle :: Cell -> JSString
cellToStyle cell =
  foregroundToStyle cell `mappend` backgroundToStyle cell `mappend` "margin: -0.1px; padding: -0.1px;"

foregroundToStyle :: Cell -> JSString
foregroundToStyle (Cell Vivid White _ _ _) = "color: #fff;"
foregroundToStyle (Cell Vivid Red _ _ _) = "color: #f00;"
foregroundToStyle (Cell Vivid Green _ _ _) = "color: #0f0;"
foregroundToStyle (Cell Vivid Blue _ _ _) = "color: #00f;"
foregroundToStyle (Cell Vivid Cyan _ _ _) = "color: #0ff;"
foregroundToStyle (Cell Vivid Magenta _ _ _) = "color: #f0f;"
foregroundToStyle (Cell Vivid Yellow _ _ _) = "color: #ff0;"
foregroundToStyle (Cell Vivid Black _ _ _) = "color: #444;"
foregroundToStyle (Cell Dull White _ _ _) = "color: #888;"
foregroundToStyle (Cell Dull Red _ _ _) = "color: #800;"
foregroundToStyle (Cell Dull Green _ _ _) = "color: #080;"
foregroundToStyle (Cell Dull Blue _ _ _) = "color: #008;"
foregroundToStyle (Cell Dull Cyan _ _ _) = "color: #088;"
foregroundToStyle (Cell Dull Magenta _ _ _) = "color: #808;"
foregroundToStyle (Cell Dull Yellow _ _ _) = "color: #880;"
foregroundToStyle (Cell Dull Black _ _ _) = "color: #000;"

backgroundToStyle :: Cell -> JSString
backgroundToStyle (Cell _ _ Vivid White _) = "background: #fff;"
backgroundToStyle (Cell _ _ Vivid Red _) = "background: #f00;"
backgroundToStyle (Cell _ _ Vivid Green _) = "background: #0f0;"
backgroundToStyle (Cell _ _ Vivid Blue _) = "background: #00f;"
backgroundToStyle (Cell _ _ Vivid Cyan _) = "background: #0ff;"
backgroundToStyle (Cell _ _ Vivid Magenta _) = "background: #f0f;"
backgroundToStyle (Cell _ _ Vivid Yellow _) = "background: #ff0;"
backgroundToStyle (Cell _ _ Vivid Black _) = "background: #444;"
backgroundToStyle (Cell _ _ Dull White _) = "background: #888;"
backgroundToStyle (Cell _ _ Dull Red _) = "background: #800;"
backgroundToStyle (Cell _ _ Dull Green _) = "background: #080;"
backgroundToStyle (Cell _ _ Dull Blue _) = "background: #008;"
backgroundToStyle (Cell _ _ Dull Cyan _) = "background: #088;"
backgroundToStyle (Cell _ _ Dull Magenta _) = "background: #808;"
backgroundToStyle (Cell _ _ Dull Yellow _) = "background: #880;"
backgroundToStyle (Cell _ _ Dull Black _) = "background: #000;"

paintAllTerminal :: MonadIO m => TerminalState -> m ()
paintAllTerminal term = liftIO $ do
  spans <- readMVar globalSpans
  go 0 0 spans
 where

  go x y spans | x >= 80 = go 0 (y+1) spans
  go _ y _ | y >= 24 = return ()

  go x y spans = do
    paintCell (cells term ! (x, y))
              (fromJust $ M.lookup (x, y) spans)
    go (x+1) y spans

getTerminalSize :: MonadIO m => m (Int, Int)
getTerminalSize = return (80, 24)

mutateTerminalStateM :: forall m a. MonadTerminalState m
                     => (forall s. MutateTerminal s a)
                     -> m a
mutateTerminalStateM action = mutateTerminalStateMRaw action getTerminalSize paintTerminalM

getInputChar :: MonadIO m => m Char
getInputChar = liftIO $ atomically $ readTVar inputKey >>= \case
  Nothing -> retry
  Just ch -> do
    writeTVar inputKey Nothing
    return ch

