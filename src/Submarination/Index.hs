module Submarination.Index
  ( Index()
  , toInt
  , firstIndex
  , invalidIndex
  , isInvalidIndex
  , next )
  where

import Data.Binary
import Data.Data
import Protolude

newtype Index = Index Int
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Binary, Num )

toInt :: Index -> Int
toInt (Index x) = x
{-# INLINE toInt #-}

firstIndex :: Index
firstIndex = Index 1

isInvalidIndex :: Index -> Bool
isInvalidIndex = (== invalidIndex)

invalidIndex :: Index
invalidIndex = Index 0

next :: Index -> Index
next (Index x) = Index (x+1)
{-# INLINE next #-}

