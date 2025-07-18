module Submarination.Terminal.Unix
  ( withKeyboardTerminal
  , paintTerminalM
  , paintTerminal
  , getTerminalSize
  , mutateTerminalStateM
  , getInputChar )
  where

import Control.Lens
import Control.Monad.Reader
import Data.Array ( (!) )
import qualified Data.Array as A
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Protolude hiding ( StateT, evalStateT )
import System.Console.ANSI hiding ( getTerminalSize )
import System.IO

import Submarination.Key
import Submarination.Terminal.Common

foreign import ccall setup_terminal :: Ptr TermiosStruct -> IO ()
foreign import ccall restore_terminal :: Ptr TermiosStruct -> IO ()
foreign import ccall sizeof_termios :: CSize
foreign import ccall get_terminal_size :: Ptr CInt -> Ptr CInt -> IO ()

data TermiosStruct

-- | Sets up terminal so that keyboard is in raw mode (we get one char at a
-- time) and restores the terminal when the function is over.
withKeyboardTerminal :: IO a -> IO a
withKeyboardTerminal action = mask $ \restore ->
  allocaBytes (fromIntegral sizeof_termios) $ \termios_settings -> do
    setup_terminal termios_settings

    let rollback_terminal = do setSGR [Reset]
                               clearScreen
                               restore_terminal termios_settings

    finally (restore action) rollback_terminal

paintTerminalM :: MonadTerminalState m => TerminalState -> m ()
paintTerminalM term = do
  old_term <- getTerminal
  paintTerminal old_term term
  putTerminal term

-- Given a terminal and optionally another terminal that represents current
-- state of the display, paints the new terminal.
--
-- This function will only send output for cells that have changed.
paintTerminal :: MonadIO m => Maybe TerminalState -> TerminalState -> m ()

-- Paint everything if we either don't have old state or size has changed
paintTerminal Nothing term = liftIO $ paintAllTerminal term
paintTerminal (Just old_term) term
  | A.bounds (cells old_term) /= A.bounds (cells term) = liftIO $ paintAllTerminal term

-- Paint deltas if we have old terminal and it's the same size
paintTerminal (Just old_term) term = liftIO $ paintDeltaTerminal old_term term

paintAllTerminal :: TerminalState -> IO ()
paintAllTerminal term = do
  hideCursor
  go 0 0
  hFlush stdout
  showCursor
 where
  ((_, _), (max_x, max_y)) = A.bounds $ cells term

  go x y  | x > max_x = go 0 (y+1)
  go _x y | y > max_y = return ()

  go x y = do
    let cell' = cells term ! (x, y)
    setCursorPosition y x
    setSGR [SetColor Foreground (cell'^.foreground._1) (cell'^.foreground._2)
           ,SetColor Background (cell'^.background._1) (cell'^.background._2)]
    putChar (cell'^.char)
    go (x+1) y

paintDeltaTerminal :: TerminalState -> TerminalState -> IO ()
paintDeltaTerminal old_term term = do
  hideCursor
  go 0 0 (-1) (-1)
  hFlush stdout
  showCursor
 where
  ((_, _), (max_x, max_y)) = A.bounds $ cells term

  -- x and y are where we are walking the terminal
  -- cx and cy are cursor position, we use it to check if we need to move it
  go x y cx cy    | x > max_x = go 0 (y+1) cx cy
  go _x y _cx _cy | y > max_y = return ()

  -- Has the cell changed?
  go x y cx cy | cells old_term ! (x, y) /= cells term ! (x, y) = do
    -- Do we need to update the cursor?
    when (cx /= x || cy /= y) $
      setCursorPosition y x

    let cell' = cells term ! (x, y)

    -- TODO: we always set attributes
    -- maybe we could not to do that if they were already set correctly.
    setSGR [SetColor Foreground (cell'^.foreground._1) (cell'^.foreground._2)
           ,SetColor Background (cell'^.background._1) (cell'^.background._2)]
    putChar (cell'^.char)

    -- TODO: we know absolutely nothing about wide unicode characters here
    -- so if there's any of them we mess up everything.
    go (x+1) y (cx+1) cy

  -- The cell hasn't changed.
  go x y cx cy = go (x+1) y cx cy

mutateTerminalStateM :: forall m a. MonadTerminalState m
                     => (forall s. MutateTerminal s a)
                     -> m a
mutateTerminalStateM action =
  mutateTerminalStateMRaw action getTerminalSize paintTerminalM

getTerminalSize :: MonadIO m => m (Int, Int)
getTerminalSize = liftIO $
  alloca $ \w_ptr -> alloca $ \h_ptr -> do
    get_terminal_size w_ptr h_ptr
    w <- peek w_ptr
    h <- peek h_ptr
    return (fromIntegral w, fromIntegral h)

getInputChar :: MonadIO m => m Key
getInputChar = CharKey <$> liftIO getChar

