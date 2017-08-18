{-# LANGUAGE CPP #-}

module Submarination.MonotonicClock
  ( getMonotonicTime )
  where

import Protolude
#ifdef GHCJS_BROWSER
foreign import javascript "if ( typeof performance !== 'undefined' ) { $r = performance.now(); } else { $r = Date.now(); }" get_performance_now :: IO Double

getMonotonicTime :: MonadIO m => m Integer
getMonotonicTime = liftIO $ do
  msecs <- get_performance_now
  return $ floor $ msecs * 1000000
#else
import System.Clock
getMonotonicTime :: MonadIO m => m Integer
getMonotonicTime = liftIO $ toNanoSecs <$> getTime Monotonic
#endif

