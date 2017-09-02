module Submarination.Random.Native
  ( RandomSupplyT()
  , runWithRandomSupply )
  where

import Control.Monad.Trans.Class
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import Protolude
import System.Random.MWC

import Submarination.Random.Common

newtype RandomSupplyT m a = RandomSupplyT { unwrapRandomSupplyT :: ReaderT (Gen (PrimState m)) m a }
  deriving ( Functor, Applicative, Monad, Typeable, Generic )

instance MonadIO m => MonadIO (RandomSupplyT m) where
  liftIO = RandomSupplyT . liftIO

instance MonadTrans RandomSupplyT where
  lift = RandomSupplyT . lift
  {-# INLINE lift #-}

instance PrimMonad m => MonadRandomSupply (RandomSupplyT m) where
  randomInt = RandomSupplyT . (ask >>=) . uniformR
  {-# INLINE randomInt #-}

  randomDouble = RandomSupplyT . (ask >>=) . uniformR
  {-# INLINE randomDouble #-}

instance MonadState s m => MonadState s (RandomSupplyT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

runWithRandomSupply :: PrimMonad m => Word32 -> RandomSupplyT m a -> m a
runWithRandomSupply seed =
  (initialize (V.singleton seed) >>=) . runReaderT . unwrapRandomSupplyT

