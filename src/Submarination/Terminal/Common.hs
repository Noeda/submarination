module Submarination.Terminal.Common
  ( TerminalState(..)
  , HasTerminalState(..)
  , TerminalStateT()
  , MonadTerminalState(..)
  , runTerminalStateT
  , Cell(..)
  , emptyTerminalState
  , mutateTerminalStateMRaw
  , generateTerminalState
  , mutateTerminalSize
  , MutateTerminal()
  , clear
  , setCell
  , setCell'
  , textWidth
  , setText
  , setWrappedText
  , foreground
  , background
  , char
  , module System.Console.ANSI )
  where

import Control.Lens
import qualified Data.Array as A
import qualified Data.Array.ST as AM
import qualified Data.Text as T
import Protolude
import System.Console.ANSI ( Color(..), ColorIntensity(..) )

-- Stores state of terminal.
--
-- The state is needed for rendering; we only write changes from last frame. To
-- do that we need to keep track of what's already on screen and this is the
-- structure where they are stored.
newtype TerminalState = TerminalState
  { cells :: A.Array (Int, Int) Cell }
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

newtype TerminalStateMut s = TerminalStateMut
  { cellsMut :: AM.STArray s (Int, Int) Cell }
  deriving ( Typeable, Generic )

class HasTerminalState st where
  terminalState :: Lens' st TerminalState

instance HasTerminalState TerminalState where
  terminalState = lens identity (\_ new -> new)

-- This monad is designed to hide away state management when you want to only
-- paint deltas. Use with runTerminalStateT and paintTerminalM
newtype TerminalStateT m a = TerminalStateT (StateT (Maybe TerminalState) m a)
  deriving ( Functor, Applicative, Monad, Typeable, Generic, MonadIO )

newtype MutateTerminal s a = MutateTerminal { unwrapMutateTerminal :: ReaderT (TerminalStateMut s, Int, Int) (ST s) a }
  deriving ( Functor, Applicative, Monad, Typeable, Generic )

class MonadIO m => MonadTerminalState m where
  getTerminal :: m (Maybe TerminalState)
  putTerminal :: TerminalState -> m ()

instance MonadIO m => MonadTerminalState (TerminalStateT m) where
  getTerminal = TerminalStateT get
  putTerminal term = TerminalStateT $ put (Just term)

instance MonadTerminalState m => MonadTerminalState (StateT s m) where
  getTerminal = lift getTerminal
  putTerminal = lift . putTerminal

data Cell = Cell !ColorIntensity !Color !ColorIntensity !Color !Char
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

mutateTerminalState :: TerminalState
                    -> (forall s. TerminalStateMut s -> ST s a)
                    -> (a, TerminalState)
mutateTerminalState term action = runST $ do
  cells_mut <- AM.thaw (cells term)
  result <- action (TerminalStateMut cells_mut)
  frozen <- AM.freeze cells_mut
  return (result, TerminalState frozen)

runTerminalStateT :: Monad m => TerminalStateT m a -> m a
runTerminalStateT (TerminalStateT state_monad) =
  evalStateT state_monad Nothing

setCell :: Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Char -> MutateTerminal s ()
setCell x y fintensity fcolor bintensity bcolor ch = MutateTerminal $ do
  (mut, w, h) <- ask
  when (x < w && y < h && x >= 0 && y >= 0) $ lift $
    AM.writeArray (cellsMut mut) (x, y) (Cell fintensity fcolor bintensity bcolor ch)
{-# INLINE setCell #-}

setCell' :: Int -> Int -> Cell -> MutateTerminal s ()
setCell' x y cell = MutateTerminal $ do
  (mut, w, h) <- ask
  when (x < w && y < h && x >= 0 && y >= 0) $ lift $
    AM.writeArray (cellsMut mut) (x, y) cell
{-# INLINE setCell' #-}

mutateTerminalSize :: MutateTerminal s (Int, Int)
mutateTerminalSize = MutateTerminal $ do
  (_, w, h) <- ask
  return (w, h)
{-# INLINE mutateTerminalSize #-}

clear :: MutateTerminal s ()
clear = MutateTerminal $ do
  (mut, w, h) <- ask
  let cells' = cellsMut mut
  for_ [0..w-1] $ \x -> for_ [0..h-1] $ \y ->
    lift $ AM.writeArray cells' (x, y) (Cell Dull White Dull Black ' ')
{-# INLINE clear #-}

setWrappedText :: forall s. Int -> Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Text -> MutateTerminal s Int
setWrappedText ox oy w fintensity fcolor bintensity bcolor txt =
  go ox oy txt_words 0
 where
  txt_words = T.words txt

  go :: Int -> Int -> [Text] -> Int -> MutateTerminal s Int
  go _x _y [] height = return height

  go x y (word:rest) height =
    if | x+T.length word > ox+w && T.length word <= w
         -> go ox (y+1) (word:rest) (height+1)
       | T.length word > w && x == ox
         -> do setText x y fintensity fcolor bintensity bcolor word
               go ox (y+1) rest (height+1)
       | otherwise
         -> do setText x y fintensity fcolor bintensity bcolor word
               go (x+T.length word+1) y rest height

setText :: Int -> Int -> ColorIntensity -> Color -> ColorIntensity -> Color -> Text -> MutateTerminal s ()
setText x y fintensity fcolor bintensity bcolor txt = go 0
 where
  go idx | idx < T.length txt = do
    setCell (x+idx) y fintensity fcolor bintensity bcolor (T.index txt idx)
    go (idx+1)

  go _ = return ()
{-# INLINE setText #-}

mutateTerminalStateMRaw :: forall m a. MonadTerminalState m
                        => (forall s. MutateTerminal s a)
                        -> m (Int, Int)
                        -> (TerminalState -> m ())
                        -> m a
mutateTerminalStateMRaw action getTerminalSize paintTerminalM = do
  maybe_old_term <- getTerminal
  (w, h) <- getTerminalSize

  let mut_term = case maybe_old_term of
                   Just old_term | A.bounds (cells old_term) == ((0, 0), (w-1, h-1)) -> old_term
                   _ -> emptyTerminalState w h

  case mut mut_term (unwrapMutateTerminal action) of
    (result, new_term) -> do
       paintTerminalM new_term
       return result
 where
  mut :: TerminalState -> (forall s. ReaderT (TerminalStateMut s, Int, Int) (ST s) a) -> (a, TerminalState)
  mut mut_term action =
    let ((_, _), (w', h')) = A.bounds (cells mut_term)
     in mutateTerminalState mut_term (\mut -> runReaderT action (mut, w'+1, h'+1))

emptyTerminalState :: Int -> Int -> TerminalState
emptyTerminalState width height = TerminalState
  { cells = A.listArray ((0, 0), (width-1, height-1)) (repeat emptyCell) }

generateTerminalState :: Int -> Int -> (Int -> Int -> Cell) -> TerminalState
generateTerminalState width height generator = TerminalState
  { cells = A.array ((0, 0), (width-1, height-1))
                    [((x, y), generator x y) | x <- [0..width-1], y <- [0..height-1]] }

emptyCell :: Cell
emptyCell = Cell Dull White Dull Black ' '

foreground :: Lens' Cell (ColorIntensity, Color)
foreground = lens get_it set_it
 where
  get_it (Cell fintensity fcolor _ _ _) = (fintensity, fcolor)
  set_it (Cell _ _ bintensity bcolor char) (fintensity, fcolor) =
    Cell fintensity fcolor bintensity bcolor char

background :: Lens' Cell (ColorIntensity, Color)
background = lens get_it set_it
 where
  get_it (Cell _ _ bintensity bcolor _) = (bintensity, bcolor)
  set_it (Cell fintensity fcolor _ _ char) (bintensity, bcolor) =
    Cell fintensity fcolor bintensity bcolor char

char :: Lens' Cell Char
char = lens get_it set_it
 where
  get_it (Cell _ _ _ _ ch) = ch
  set_it (Cell fintensity fcolor bintensity bcolor _) =
    Cell fintensity fcolor bintensity bcolor

-- TODO: use wcwidth()
textWidth :: Text -> Int
textWidth = T.length

