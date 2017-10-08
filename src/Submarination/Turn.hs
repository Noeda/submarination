module Submarination.Turn
  ( Turn()
  , turn1
  , turnToInt
  , intToTurn
  , nextTurn
  , nextNTurn
  , previousTurn )
  where

import Data.Binary
import Data.Data
import Data.Hashable
import Protolude
import Test.QuickCheck

newtype Turn = Turn Int
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary, Num, Integral, Real, Enum )

instance Arbitrary Turn where
  arbitrary = Turn . abs <$> arbitrary

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

turnToInt :: Turn -> Int
turnToInt (Turn x) = x

intToTurn :: Int -> Turn
intToTurn = Turn

