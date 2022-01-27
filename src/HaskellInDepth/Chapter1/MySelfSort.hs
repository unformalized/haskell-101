{-# LANGUAGE BangPatterns #-}

-- |
module HaskellInDepth.Chapter1.MySelfSort where

import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)

sortBySelf :: (a -> a -> Ordering) -> [a] -> [a]
sortBySelf cmp = mergeAll . sequences
  where
    -- 将数组分为排好序的数组片段
    sequences (a : b : xs)
      | a `cmp` b == GT = descending b [a] xs
      | otherwise = ascending b (a :) xs
    sequences xs = [xs]

    -- 将当前累计列表按升序排序，记录当前最小值，若是遇到更小值则更新当前排序列表，以及最小值，该方法更新列表时，是将最小值放在最前方
    -- a 当前最小值
    -- as 已排好的升序列表
    -- 当遇到更小值 b 时，继续进行 descending
    descending a as (b : bs)
      | a `cmp` b == GT = descending b (a : as) bs
    -- 当遇到 b >= a 时，则划分出当前排好序的片段，继续剩下元素的排序划分
    descending a as bs = (a : as) : sequences bs

    -- 将当前累计列表按升序排序，记录当前最大值，若是遇到更大值则更新当前排序列表，以及最大值，该方法更新列表时，是将最大值放在最后，所以传入一个函数。
    -- a 当前最大值
    -- as 为函数，接受一个最大值列表，将闭包中的存储的最小值放在最前方
    ascending a as (b : bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a : ys)) bs
    -- 同样遇到较小值时，则划分出当前已经排序的列表，继续剩下元素的划分
    ascending a as bs =
      let !x = as [a]
       in x : sequences bs

    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)

    mergePairs (a : b : xs) =
      let !x = merge a b
       in x : mergePairs xs
    mergePairs xs = xs

    merge as@(a : as') bs@(b : bs')
      | a `cmp` b == GT = b : merge as bs'
      | otherwise = a : merge as' bs
    merge [] bs = bs
    merge as [] = as

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = lesser ++ [x] ++ greater
  where
    lesser = quickSort $ filter (< x) xs
    greater = quickSort $ filter (>= x) xs

testData :: [Integer]
testData = [1, 6, 9, 11, 1, 8, 12, 4, 12, 20, 12, 40, 123, 56, 23, 654, 234, 65, 123, 34, 123, 4234, 65, 12, 33, 48, 1, 5234, 54, 12, 5312, 2432, 12, 56765, 3]

testLazySort :: IO ()
testLazySort = do
  print (quickSort testData)
  mapM_ (\n -> print (take n $ quickSort testData)) [1, 2, 4, 8, 16, 32]
