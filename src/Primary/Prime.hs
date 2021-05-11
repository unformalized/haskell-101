module Primary.Prime where

import Data.List (group, genericLength)

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

primes :: [Integer]
primes = [x | x <- [0..], isPrime x]

isPrime' :: Integral a => a -> Bool
isPrime' 2 = True
isPrime' p =
    odd p && all (\n -> p `mod` n /= 0) (takeWhile (\n -> n * n <= p) [3,5..])

nextPrime :: Integer -> Integer
nextPrime a | odd a = if isPrime a then a else nextPrime (a+2)
            | otherwise = nextPrime (a+1)

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes' :: [Integer]
primes' = sieve [2..]

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors 1 = [(1,1)]
primeFactors n
    | isPrime n = [(1,n)]
    | otherwise =
        let facs = group (primeFactor n)
        in [(genericLength factor, head factor) | factor <- facs]


primeFactor :: Integer -> [Integer]
primeFactor 1 = []
primeFactor n
    | isPrime n = [n]
    | otherwise = x1:primeFactor rest
        where
            x1 = head [x | x <- [2..n], mod n x == 0]
            rest = n `div` x1

