{-# LANGUAGE TypeFamilies, OverloadedLists #-}

module Primary.PloyList where

import qualified Data.Foldable as DF (toList)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import GHC.Exts

list1 :: S.Set Integer
list1 = S.fromList [1,2,3,2,1]
list2 :: [Integer]
list2 = S.toList list1

class IsListS l where
  type ItemS l
  fromListS :: [ItemS l] -> l

  fromListNS :: Int -> [ItemS l] -> l
  fromListNS _ = fromListS

  toListS :: l -> [ItemS l]




