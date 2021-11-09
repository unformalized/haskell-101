-- |
{-# LANGUAGE ImplicitParams #-}

module Advance.ImplicitParams where

import Data.List

sortBy' :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortBy' f = sortBy cmp
  where
    cmp x y = if f x y then LT else GT

sort' :: (?cmp :: a -> a -> Bool) => Ord a => [a] -> [a]
sort' = sortBy' ?cmp

least xs = head (sort' xs)

-- 使用 let 绑定上下文中的 ?cmp
maxnum =
  let ?cmp = ((>) :: Int -> Int -> Bool)
  in least
