{-# LANGUAGE ParallelListComp, TransformListComp #-}

module Primary.List where

import GHC.Exts


map' :: (t -> a) -> [t] -> [a]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]


serise :: Int -> [Double]
serise n = [1/ (2 * fromIntegral k + 1) * (-1) ^ k | k <- [0..n]]


serise2 :: Int -> [Double]
serise2 n = [ ((1/2) ^ (2 * fromIntegral k + 1)) * (1 / (2 * fromIntegral k + 1)) * (-1) ^ k + ((1/3) ^ (2 * fromIntegral k + 1)) * (1 / (2 * fromIntegral k + 1)) * (-1) ^ k  | k <- [0..n]]

-- 并列的列表内包与一般化的列表内包

-- 并列列表

list0 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

list1 = [(x,y) | x <- [1,2,3] | y <- [4,5,6]]

-- 这里是 x,y 生成笛卡尔积，再与 z 进行 zip 合并
list2 = [(x,y,z) | x <- [1,2,3], y <- [4,5,6] | z <- [7,8,9]]

list3 = [(x,y,z) | x <- [1,2,3], y <- [4,5,6] | z <- cycle [7,8,9]]

-- 一般化列表 generalised list comprehension

table =
    [ ("hangzhou", "MP4", 243)
    , ("Hangzhou", "CD", 925)
    , ("Beijing", "MP4", 157)
    , ("Beijing", "CD", 536)
    , ("Shanghai", "MP4", 784)
    , ("Shanghai", "CD", 766)
    ]

analysis = [
    (the product, sum cost) | (city, product, cost) <- table,
    then group by product using groupWith,
    then sortWith by sum cost]

