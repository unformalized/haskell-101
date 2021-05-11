{-# LANGUAGE BangPatterns #-}

module Primary.Recursive where

import Data.List

power :: Int -> Int -> Int
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n - 1)


-- fastly
power' :: Int -> Int -> Int
power' 0 0 = 1
power' _ 0 = 1
power' x n | odd n = let p = power' x ((n - 1) `div` 2)
                     in x * p * p
           | otherwise = let p = power' x (n `div` 2)
                         in p * p

deleteAll' :: (Ord a) => a -> [a] -> [a]
deleteAll' a [] = []
deleteAll' a (x:xs)
  | x == a = deleteAll' a xs
  | otherwise = x : deleteAll' a xs


total :: [Int] -> Int
total [] = 0
total xs = foldr (+) 0 xs


total' :: [Int] -> Int
total' xs =
  iter xs 0
  where
    iter :: [Int] -> Int -> Int
    iter [] n = n
    -- iter (x:xs) n = iter xs $! (x + n)
    iter (x:xs) !n = iter xs (x+n)


mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: (Enum a, Num a, Eq a) => a -> [a]
fibs n = map fib [1..n]

fibStep :: Num a => (a, a) -> (a, a)
fibStep (u,v) = (v, u + v)

fibPair :: (Eq a, Num a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n - 1))

fastFib :: (Eq b, Num b) => b -> b
fastFib n = fst (fibPair n)

fibs' :: (Enum a, Num a, Eq a) => a -> [a]
fibs' n = map fastFib [1..n]

fibs'' :: Int  -> [Int]
fibs'' n = take n (map fst (iterate fibStep (0,1)))

golden :: Fractional a => Int -> [a]
golden n = take n (map (uncurry (/)) (iterate fibStep (0, 1)))

-- fibonacci Fn-1*Fn+1 - Fn^2 = (-1)^2

combine :: [(a,a)] -> [(a,a,a)]
combine ((f1,f2):(f3,f4):fs) = (f1,f2,f4) : combine ((f3,f4):fs)
combine _ = []

fibPairs :: Int -> [(Int,Int)]
fibPairs n = map fibPair [1..n]

difference :: Int -> [Int]
difference n = map (\(f1,f2,f3) -> f1*f3 - f2*f2) (combine $ fibPairs n)


-- convertDecimalToRome
romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAmount romeNotation

subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a, _) -> a > n) pair)

convert :: Int -> String
convert 0 = ""
convert n = let (rome, m) = subtrahend n
            in m ++ convert (n - rome)

convertRomeToInt :: String -> Int
convertRomeToInt "" = 0
convertRomeToInt [h1] =
  let x = find (\(_, a) -> a == [h1]) pair
  in case x of
     Nothing -> 0
     Just (a, _) -> a
convertRomeToInt (h1:h2:tail) =
  let x = find (\(_, a) -> a == [h1, h2]) pair
  in case x of
     Nothing -> convertRomeToInt [h1] + convertRomeToInt (h2:tail)
     Just (a, _) -> a + convertRomeToInt tail

convertDecimalToBinary :: Int -> String
convertDecimalToBinary 0 = "0"
convertDecimalToBinary 1 = "1"
convertDecimalToBinary n =
  let x = div n 2
  in convertDecimalToBinary x ++ show (n - x * 2)

--- 二分搜索

-- 练习 5.8.1

search :: (Ord a) => a -> [a] -> [a]
search _ [] = []
search a xs
  | m >  a = search a front
  | m <  a = search a behind
  | m == a = search a front ++ [m] ++ search a behind
  where
    (front, m:behind) = splitAt (div (length xs) 2) xs


-- 汉诺塔

move :: (Int, Int, Int, Int) -> [(Int, Int)]
move (1, from, to, via) = [(from, to)]
move (n, from, to, via) = move (n - 1, from, via, to)
                          ++ [(from, to)] ++
                          move (n - 1, via, to, from)

hanoi n = move (n, 1, 2, 3)




