{-# LANGUAGE TypeFamilies #-}
-- |

module Advance.TypeFamily3 where

import Data.Vector
import Data.Sequence hiding (replicate)
import Data.Vector.Mutable (new)
import Prelude hiding (replicate)

data ArrayS a = MakeArrayInt (Vector Int) | MakeArrayChar (Seq Char)

data family Array a
data instance Array Int = MkArrayInt (Vector Int)
data instance Array Char = MkArrayChar (Seq Char)

type family Array' a :: *
type instance Array' Int = Vector Int
type instance Array' Char = Seq Char


arrInt :: Array Int
arrInt = MkArrayInt (replicate 5 4)
