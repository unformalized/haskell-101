module Primary.Matrix where

-- 矩阵乘法

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) =
    (x: [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

infixl 5 |*|

(|*|) :: Num a => [[a]] -> [[a]] -> [[a]]
(|*|) a b = [[ sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

-- 利用矩阵乘法计算斐波那契数列

fibUnit :: [[Integer]]
fibUnit = [[1,1], [1, 0]]

fib :: Integral a => a -> [[Integer]]
fib 1 = fibUnit
fib n | even n = let m = fib (div n 2) in m |*| m
      | otherwise = let m = fib (div (n - 1) 2) in m |*| fibUnit |*| m


