module Advance.Typeable2 where

import Data.Rank1Dynamic
import Data.Rank1Typeable
import Data.Constraint

typeA :: TypeRep
typeA = typeOf (undefined :: ANY -> ANY1)
typeB :: TypeRep
typeB = typeOf (undefined :: Int -> Bool)
typeC :: TypeRep
typeC = typeOf (undefined :: ANY)

dId :: Dynamic
dId = toDynamic (id :: ANY1 -> ANY1)

d5 :: Dynamic
d5 = toDynamic (5 :: Int)

