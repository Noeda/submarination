module Submarination.Turn
  ( Turn()
  , turn1
  , nextTurn
  , nextNTurn
  , previousTurn )
  where

import Data.Binary
import Data.Data
import Data.Hashable
import Protolude

newtype Turn = Turn Int
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary )

instance Hashable Turn where
  hashWithSalt salt (Turn int) = hashWithSalt salt int
  {-# INLINE hashWithSalt #-}

turn1 :: Turn
turn1 = Turn 1

nextTurn :: Turn -> Turn
nextTurn (Turn x) = Turn $ x+1

nextNTurn :: Int -> Turn -> Turn
nextNTurn n (Turn x) = Turn $ x+n

previousTurn :: Turn -> Turn
previousTurn (Turn x) = Turn $ x-1

