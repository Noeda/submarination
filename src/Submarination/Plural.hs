module Submarination.Plural
  ( Plural(..) )
  where

import Data.Data
import Protolude

data Plural
  = Many
  | Singular
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )
