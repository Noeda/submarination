{-# LANGUAGE CPP #-}

module Submarination.Terminal
  ( withKeyboardTerminal
  , TerminalState()
  , terminalSize
  , terminalCellAt
  , HasTerminalState(..)
  , TerminalStateT()
  , MonadTerminalState(..)
  , runTerminalStateT
  , paintTerminalM
  , Cell(..)
  , emptyTerminalState
  , mutateTerminalState
  , mutateTerminalStateM
  , mutateTerminalStateMOffscreen
  , generateTerminalState
  , paintTerminal
  , getTerminalSize
  , mutateTerminalSize
  , MutateTerminal()
  , clear
  , setCell
  , setCell'
  , getCell'
  , textWidth
  , setText
  , setWrappedText
  , getInputChar
  , module System.Console.ANSI )
  where

import Submarination.Terminal.Common

#ifdef GHCJS_BROWSER
import Submarination.Terminal.GHCJS
#else
import Submarination.Terminal.Unix
#endif

import System.Console.ANSI

