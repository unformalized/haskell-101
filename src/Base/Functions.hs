module Base.Functions where

import Data.Number.Dif

f :: Dif Int -> Dif Int
f x = x ^ 2

f' :: Int -> Int
f' = deriv f

map1 :: [Integer]
map1 = map (\x -> 2 * x + 10) [1..10]

tranangleS :: Double -> Double -> Double -> Double
tranangleS a b c =
    let p = (a + b + c) / 2
    in sqrt (p * (p - a) * (p - b) * (p - c))

-- case of

month :: Int -> Int
month n = case n of
    1  -> 31
    2  -> 28
    9  -> 30
    10 -> 31
    11 -> 30
    _  -> error "invalid month"

-- guarded expression

abs' :: (Num a, Ord a) => a -> a
abs' n
    | n > 0 = n
    | otherwise = -n

-- 定义

infixr 5 <+>, <->

(<->), (<+>) :: Int -> Int -> Int
(<->) x y = x - y
(<+>) x y = x + y
