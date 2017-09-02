module Submarination.Key
  ( Key(..) )
  where

import Data.Data
import Protolude

data Key
  = CharKey !Char
  | KeyUp
  | KeyLeft
  | KeyRight
  | KeyDown
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

