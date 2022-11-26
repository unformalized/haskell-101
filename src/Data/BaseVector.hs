-- |
module Data.BaseVector where

import Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

va :: V.Vector Int
va = V.fromList [10, 20, 30, 40]
