{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef WASM_BROWSER
module Main ( main, submarinationReactorInit ) where

import Prelude
import Submarination

foreign export javascript "submarination_reactor_init" submarinationReactorInit :: IO ()

submarinationReactorInit :: IO ()
submarinationReactorInit = runSubmarination

-- main does nothing. entrypoint is the exported function instead
main :: IO ()
main = return ()
#else
module Main ( main ) where

import Protolude

import Submarination

main :: IO ()
main = runSubmarination
#endif

