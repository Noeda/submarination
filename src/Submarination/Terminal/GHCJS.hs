{-# LANGUAGE CPP #-}

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
import qualified Data.Map.Strict as M
import Data.Maybe
#ifdef GHCJS
import GHCJS.Foreign.Callback
import GHCJS.Types
import Data.JSString
#else
import GHC.Wasm.Prim
#endif
import Protolude
import System.Console.ANSI hiding ( getTerminalSize )
import System.IO.Unsafe ( unsafePerformIO )

import Submarination.Key
import Submarination.Terminal.Common

-- GHCJS: you assign to $r
-- WASM: you return the value

#ifdef GHCJS
foreign import javascript "$r = document.createElement('span');" make_span_element :: IO JSVal
foreign import javascript "$r = document.createElement('br');" make_br_element :: IO JSVal
foreign import javascript "$r = document.createElement('div');" make_div_element :: IO JSVal
foreign import javascript "$r = document.getElementById('content')" get_content_div :: IO JSVal
foreign import javascript "$r = document.getElementsByTagName('body')[0];" get_body :: IO JSVal
foreign import javascript unsafe "$r = $1.key || String.fromCharCode($1.keyCode);" get_event_char :: JSVal -> IO JSString
#else
foreign import javascript unsafe "return document.createElement('span');" make_span_element :: IO JSVal
foreign import javascript unsafe "return document.createElement('br');" make_br_element :: IO JSVal
foreign import javascript unsafe "return document.createElement('div');" make_div_element :: IO JSVal
foreign import javascript unsafe "return document.getElementById('content')" get_content_div :: IO JSVal
foreign import javascript unsafe "return document.getElementsByTagName('body')[0];" get_body :: IO JSVal
foreign import javascript unsafe "return ($1.key || String.fromCharCode($1.keyCode));" get_event_char :: JSVal -> IO JSString
#endif
foreign import javascript unsafe "$1.appendChild($2)" append_child :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.textContent = $2" set_text_content :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1.style.cssText = $2" set_style :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "console.log($1);" console_log :: JSString -> IO ()

#ifdef GHCJS
foreign import javascript "$1.addEventListener('keydown', $2);" attach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript "$1.removeEventListener('keydown', $2);" detach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
#else
foreign import javascript "wrapper" wrapJSFuncToHS :: (JSVal -> IO ()) -> IO JSVal

foreign import javascript unsafe "$1.addEventListener('keydown', $2);" attach_keydown_handler :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.removeEventListener('keydown', $2);" detach_keydown_handler :: JSVal -> JSVal -> IO ()
#endif

globalSpans :: MVar (M.Map (Int, Int) JSVal)
globalSpans = unsafePerformIO $ newMVar M.empty
{-# NOINLINE globalSpans #-}

inputKey :: TVar (Maybe Key)
inputKey = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE inputKey #-}

withKeyboardTerminal :: IO a -> IO a
withKeyboardTerminal action = mask $ \restore -> do
  -- Setup DOM stuff

  -- This div holds everything
  term_div <- make_div_element
  set_style term_div (stringToJSString "font-family: 'Subbie', monospace; font-size: 1.3em;")

  spanner term_div (restore action)

#ifndef GHCJS
asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal
asyncCallback1 action = wrapJSFuncToHS action

releaseCallback :: JSVal -> IO ()
releaseCallback wrapped = freeJSVal wrapped

jsstringToString :: JSString -> [Char]
jsstringToString jsstr = fromJSString jsstr

stringToJSString :: [Char] -> JSString
stringToJSString str = toJSString str
#else
-- GHCJS codepath
-- Never tested if these compile on ghcjs, since adding wasm backend. - 2025-07-17
jsstringToString :: JSString -> [Char]
jsstringToString jsstr = unpack jsstr

stringToJSString :: [Char] -> JSString
stringToJSString = fromString
#endif

spanner :: JSVal -> IO a -> IO a
spanner term_div action = do
  -- Spans that show each symbol
  spans <- fmap M.fromList $ for [ (x, y) | x <- [0..79], y <- [0..23] ] $ \(x, y) -> do
    span <- make_span_element
    set_text_content span (stringToJSString "\x00a0")
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
    js_str <- get_event_char val :: IO JSString
    case jsstringToString js_str of
      [ch] ->
        atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just $ CharKey ch)
          Just{} -> retry
      "ArrowLeft" ->
        atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just KeyLeft)
          Just{} -> retry
      "ArrowUp" ->
        atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just KeyUp)
          Just{} -> retry
      "ArrowRight" ->
        atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just KeyRight)
          Just{} -> retry
      "ArrowDown" ->
        atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just KeyDown)
          Just{} -> retry
      _ -> return ()

  body <- get_body
  set_style body $ stringToJSString "background: #000;"

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

  set_text_content span $ stringToJSString [ch]
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
  -- Used to be:
  -- foregroundToStyle cell `mappend` backgroundToStyle cell
  -- 
  -- But in wasm, JSString is not a monoid. Solution: convert to regular
  -- string, append, convert back.
  let str1 = jsstringToString (foregroundToStyle cell)
      str2 = jsstringToString (backgroundToStyle cell)
   in stringToJSString $ str1 `mappend` str2

foregroundToStyle :: Cell -> JSString
foregroundToStyle cell = stringToJSString $ foregroundToStyleJSStr cell

backgroundToStyle :: Cell -> JSString
backgroundToStyle cell = stringToJSString $ backgroundToStyleJSStr cell

foregroundToStyleJSStr :: Cell -> [Char]
foregroundToStyleJSStr (Cell Vivid White _ _ _) = "color: #fff;"
foregroundToStyleJSStr (Cell Vivid Red _ _ _) = "color: #f00;"
foregroundToStyleJSStr (Cell Vivid Green _ _ _) = "color: #0f0;"
foregroundToStyleJSStr (Cell Vivid Blue _ _ _) = "color: #00f;"
foregroundToStyleJSStr (Cell Vivid Cyan _ _ _) = "color: #0ff;"
foregroundToStyleJSStr (Cell Vivid Magenta _ _ _) = "color: #f0f;"
foregroundToStyleJSStr (Cell Vivid Yellow _ _ _) = "color: #ff0;"
foregroundToStyleJSStr (Cell Vivid Black _ _ _) = "color: #444;"
foregroundToStyleJSStr (Cell Dull White _ _ _) = "color: #888;"
foregroundToStyleJSStr (Cell Dull Red _ _ _) = "color: #800;"
foregroundToStyleJSStr (Cell Dull Green _ _ _) = "color: #080;"
foregroundToStyleJSStr (Cell Dull Blue _ _ _) = "color: #008;"
foregroundToStyleJSStr (Cell Dull Cyan _ _ _) = "color: #088;"
foregroundToStyleJSStr (Cell Dull Magenta _ _ _) = "color: #808;"
foregroundToStyleJSStr (Cell Dull Yellow _ _ _) = "color: #880;"
foregroundToStyleJSStr (Cell Dull Black _ _ _) = "color: #000;"

backgroundToStyleJSStr :: Cell -> [Char]
backgroundToStyleJSStr (Cell _ _ Vivid White _) = "background: #fff;"
backgroundToStyleJSStr (Cell _ _ Vivid Red _) = "background: #f00;"
backgroundToStyleJSStr (Cell _ _ Vivid Green _) = "background: #0f0;"
backgroundToStyleJSStr (Cell _ _ Vivid Blue _) = "background: #00f;"
backgroundToStyleJSStr (Cell _ _ Vivid Cyan _) = "background: #0ff;"
backgroundToStyleJSStr (Cell _ _ Vivid Magenta _) = "background: #f0f;"
backgroundToStyleJSStr (Cell _ _ Vivid Yellow _) = "background: #ff0;"
backgroundToStyleJSStr (Cell _ _ Vivid Black _) = "background: #444;"
backgroundToStyleJSStr (Cell _ _ Dull White _) = "background: #888;"
backgroundToStyleJSStr (Cell _ _ Dull Red _) = "background: #800;"
backgroundToStyleJSStr (Cell _ _ Dull Green _) = "background: #080;"
backgroundToStyleJSStr (Cell _ _ Dull Blue _) = "background: #008;"
backgroundToStyleJSStr (Cell _ _ Dull Cyan _) = "background: #088;"
backgroundToStyleJSStr (Cell _ _ Dull Magenta _) = "background: #808;"
backgroundToStyleJSStr (Cell _ _ Dull Yellow _) = "background: #880;"
backgroundToStyleJSStr (Cell _ _ Dull Black _) = "background-rgba:rgba(0,0,0,0);"

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

getInputChar :: MonadIO m => m Key
getInputChar = liftIO $ atomically $ readTVar inputKey >>= \case
  Nothing -> retry
  Just ch -> do
    writeTVar inputKey Nothing
    return ch

