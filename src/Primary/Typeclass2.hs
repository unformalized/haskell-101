{-# LANGUAGE MultiParamTypeClasses #-}

-- MultiParamTypeClasses 可以在声明类型类时使用多个类型参数
-- {-# MINIMAL fn1, fn2, fn3 | fn4 #-} 编译器参数，表示实现该类型类实例，至少需要实现该编译器参数声明的函数
-- 上述例子中就表示只需要实现 fn1 fn2, fn3 | fn4，可以用 | 表示或只需要实现一个

module Primary.Typeclass2 where

import Data.Bifunctor (bimap)
import Control.Applicative

data Tree a = Leaf a | Branch (Tree (a, a)) deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch x) = Branch (fmap (bimap f f) x)

-- Functor laws
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g

-- Applicative laws
-- 1. pure id <*> v = v
-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3. pure f <*> pure x = pure (f x)
-- 4. u <*> pure y = pure ($ y) <*> u
-- ($ x) = (\f -> f x)

class MultiFunctor f where
  fmap0 :: a -> f a
  fmap1 :: (a -> b) -> f a -> f b
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c
  fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  fmap3 f a b c =
    let x = fmap2 f a b
    in fmap2 ($) x c

l1 = [1] <|> [2,3]

some' :: Alternative f => f a -> f [a]
some' v = some_v
  where
    many_v = some_v <|> pure []
    some_v = liftA2 (:) v many_v


many' :: Alternative f => f a -> f [a]
many' v = many_v
  where
    many_v = some_v <|> pure []
    some_v = liftA2 (:) v many_v

lessThan10 a = if a < 10 then Just (10 -a) else Nothing




