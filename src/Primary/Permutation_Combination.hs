module Primary.Permutation_Combination (
    permutation,
    combinations,
    choose,
    position,
    queue,
    queue',
    queue'',
    queue'''
) where

import Data.List (delete, tails, subsequences)

insert :: a -> [a] -> [[a]]
insert n [] = [[n]]
insert n (n':ns) = (n : n' : ns) : [n' : ns' | ns' <- insert n ns]

permutation :: [a] -> [[a]]
permutation [] = [[]]
permutation (x:xs) = concat [insert x permuxs | permuxs <- permutation xs]

permutation' :: Eq a => [a] -> [[a]]
permutation' [] = [[]]
permutation' xs = [y : ys | y <- xs, ys <- permutation' (delete y xs)]


powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [ x : r | r <- powerSet xs ] ++ powerSet xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n xs = [y:ys | y:xs' <- tails xs, ys <- choose (n-1) (y:xs')]

-- 八皇后

position :: Int -> Int -> [[Int]]
position 0 _ = [[]]
position k n = [ x:xs | x <- [1..n], xs <- position (k-1) n ]

noSameRow :: Eq a => [a] -> Bool
noSameRow [] = True
noSameRow (x:xs) = notElem x xs && noSameRow xs

noSameDiag :: (Eq a, Num a, Enum a) => [a] -> Bool
noSameDiag [] = True
noSameDiag xs@(x:xs') = and [abs (i1 - i) /= abs (p1 - p) | (i, p) <- ip] && noSameDiag xs'
    where
        (i1, p1):ip = zip [1..] xs


-- 暴力枚举
queue :: Int -> [[Int]]
queue n = [xs | xs <- position n n, noSameDiag xs, noSameRow xs]

-- 插入法

insert' :: Int -> Int -> [[Int]]
insert' 0 n = [[]]
insert' k n = [ x:xs | xs <- insert' (k - 1) n, x <- [1..n], isSafe x xs ]

isSameDiag :: (Eq a, Num a, Enum a) => a -> [a] -> Bool
isSameDiag x xs = any (\(dist, p) -> abs (p - x) == dist) $ zip [1..] xs


queue' :: Int -> [[Int]]
queue' n = insert' n n

-- 全排列插入法，全排列自身为 noSameRow

queue'' :: Int -> [[Int]]
queue'' n = [xs | xs <- permutation [1..n], noSameDiag xs]

-- 回溯, 生成 position' 时进行筛选

position' :: (Num t, Num a, Enum a, Eq t, Eq a) => t -> a -> [[a]]
position' 0 n = [[]]
position' k n =
    [ p:ps | ps <- position' (k - 1) n, p <- [1..n], isSafe p ps]

isSafe :: (Eq a, Num a, Enum a) => a -> [a] -> Bool
isSafe p ps = not (elem p ps || sameDiag p ps)
    where
        sameDiag p ps = any (\(dist, q) -> abs (p - q) == dist) $ zip [1..] ps

queue''' :: (Num a, Enum a, Eq a) => a -> [[a]]
queue''' n = position' n n