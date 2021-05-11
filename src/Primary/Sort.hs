module Primary.Sort where

import Data.List (elem)

insert :: Ord a => a -> [a] -> [a]
insert x []              = [x]
insert x (y:ys) | x < y  = x:y:ys
                | otherwise = y:insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []


swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
  | x1 > x2 = x2 : swaps (x1 : xs)
  | otherwise = x1 : swaps (x2 : xs)

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = fix swaps

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' xs | swaps xs == xs = xs
               | otherwise = bubbleSort' $ swaps xs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l:ls) | x == l = ls
                | otherwise = l:delete x ls

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
  where
    mini = minimum xs
    xs'  = delete mini xs


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort mini ++ [x] ++ quickSort maxi
  where
    mini = filter (<x) xs
    maxi = filter (>=x) xs


quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x:xs) = quickSort' l ++ [x] ++ quickSort' r
  where
    (l,r) = filterSplit x xs

filterSplit :: Ord a => a -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit a (x:xs)
  | x >= a = (mini, x:maxi)
  | otherwise = (x:mini, maxi)
  where
    (mini, maxi) = filterSplit a xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x > y = y:merge (x:xs) ys
  | otherwise = x:merge xs (y:ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge front end
  where
    len   = div (length xs) 2
    front = mergeSort (take len xs)
    end   = mergeSort (drop len xs)


-- fix 不动点函数

fix' :: (a -> a) -> a
fix' f = f (fix' f)

factorial' :: Int -> Int
factorial' = fix' (\f n -> if n == 0 then 1 else n * f (n - 1))


fix'' :: Eq a => (a -> a) -> a -> a
fix'' f x | x == f x  = x
          | otherwise = fix'' f (f x)

-- 
squareroot :: Int -> Double -> Double
squareroot 0 x = x
squareroot n x = (squareroot (n - 1) x + x / squareroot (n-1) x ) / 2


squareFix :: (t -> t -> Bool) -> (t -> t) -> t -> t
squareFix p f x | p x (f x) = x
                | otherwise = squareFix p f (f x)

newton :: Fractional a => a -> a -> a
newton c t = (c/t + t) / 2

sqrt :: Double -> Double
sqrt c = squareFix (\a b -> a - b < 0.00001) (newton c) c


ham :: [Int]
ham = 1:mergeUnique (map (*2) ham) (mergeUnique (map (*3) ham) (map (*5) ham))

mergeUnique :: Ord a => [a] -> [a] -> [a]
mergeUnique xs [] = xs
mergeUnique [] ys = ys
mergeUnique (x:xs) (y:ys)
  | x == y = y:mergeUnique xs ys
  | x > y  = y:mergeUnique (x:xs) ys
  | otherwise  = x:mergeUnique xs (y:ys)

insertUnqiue :: Ord a => a -> [a] -> [a]
insertUnqiue x [] = [x]
insertUnqiue x (y:ys)
  | x < y = x:y:ys
  | x == y = y:ys
  | otherwise = y:insertUnqiue x ys

insertList :: [Int] -> [Int] -> [Int]
insertList [] xs = xs
insertList (x:xs) ys = insertList xs (insertUnqiue x ys)

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise = x:unique xs

