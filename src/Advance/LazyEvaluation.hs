{-# LANGUAGE BangPatterns #-}

module Advance.LazyEvaluation () where

-- data 与 newtype 在 bottom 值上的区别

newtype N = N Int
data D = D Int

n (N i) = 42
d (D i) = 42

d2 :: Int -> Int
d2 i = 42

-- seq 返回第二个参数，但是对第一个参数进行严格求值, seq 会将一个表达式计算为弱首范式
-- seq :: a -> b -> b

-- ($!) 于 $ 类似，但是会将参数 a 进行严格求值后再传递
-- ($!) :: (a -> b) -> a -> b


{-| difference in newtype and data with bottom value

>>> n undefined
42

>>> d undefined
Prelude.undefined

>>> d2 undefined
42

>>> seq undefined 0
Prelude.undefined

>>> const id undefined 0
0

>>> (const id $! undefined) 0
WAS No instance for (Show (a0 -> a0)) arising from a use of ‘evalPrint’
WAS   (maybe you haven't applied a function to enough arguments?)
NOW Prelude.undefined

-}

head' :: [a] -> a
head' (x:xs) = x

length' :: Num p => [a] -> p
length' [] = 0
length' (x:xs) = 1 + length' xs


-- deepseq 和 $!! 会将表达式计算为 NF

f :: p -> Bool
f x = True

data Point = Point !Int !Int !Int

fx :: p -> Bool
fx !x = True

{-|

>>> fx 1
True

>>> fx undefined
Prelude.undefined

-}

lazyf (x,y) = 5

lazyf' ~(x,y) = 5

{-|

>>> lazyf undefined
Prelude.undefined

>>> lazyf' undefined
5

-}


