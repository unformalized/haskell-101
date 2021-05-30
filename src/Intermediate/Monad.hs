{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Monad where

import Control.Monad
import Control.Applicative

-- monad 基础
-- Monad 定律
-- 1. 左单位元 return x >>= f = f x
-- 2. 右单位元 m >>= return = m
-- 3. 结合律   (m >>= f) >>= g = m >>= (\x -> f x >>= g)


-- monadPlus 与 Alternative 类似，计算失败时返回 mzero，没有与 Alternative 合并有历史原因
-- 若两元素有一个成功，就算计算成功

class Monad m => MonadPlusS m where
  mzeroS :: m a
  mplusS :: m a -> m a -> m a

instance MonadPlusS m => Semigroup (m a) where
  (<>) = mplusS

instance MonadPlusS m => Monoid (m a) where
  mempty = mzeroS
  mappend = mplusS

instance MonadPlusS [] where
  mzeroS = []
  mplusS = (++)

instance MonadPlusS Maybe where
  mzeroS = Nothing
  Nothing `mplusS` x = x
  x `mplusS`  _      = x

-- MonadPlus 定律

-- 1. mzero >>= f = mzero
-- 2. v >>= (\x -> mzero) = mzero
-- 3. v >> mzero = mzero
-- 4. mplus a b >>= k = mplus (a >>= k) (b >>= k)

-- Monad 相关函数

-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

testm1 :: Maybe [Int]
testm1 = filterM (Just . even) [1, 2]

powerSetM :: [Int] -> [[Int]]
powerSetM = filterM (const [True, False])


powerSetM' :: [b] -> [[b]]
powerSetM' = choice (const [True, False])

choice :: (a -> [Bool]) -> [a] -> [[a]]
choice _ [] = [[]]
choice f (x:xs) = [ if choose then x:ys else ys | choose <- f x, ys <- choice f xs ]

-- mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a

mfilter' :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter' p ma = do
  a <- ma
  if p a then return a else mzero

-- foldM  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
-- foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()

safeDiv :: Double -> Double -> Maybe Double
safeDiv a 0 = Nothing
safeDiv a b = Just (a / b)

safeContinueDiv :: Double -> [Double] -> Maybe Double
safeContinueDiv = foldM safeDiv

foldM' :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM' f x = foldl (\mb a -> mb >>= (`f` a)) (pure x)

-- replicateM :: Monad m => Int -> m a -> m [a]

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n ma = liftA2 (:) ma (replicateM' (n - 1) ma)


-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
composeM :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
composeM fbc fab a = fab a >>= fbc

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
pipeM :: Monad m => m a -> (a -> m b) -> m b
pipeM = flip (=<<)

-- Monad 定义
-- join :: m (m a) -> m a
-- join 函数满足以下定律
-- 1. join . return == id == join . fmap return
-- 2. join . join == join . fmap join

-- Monad 和 Applicative 差异
-- 使用 Applicative 进行计算时，每个参数不能依赖与其他参数，而使用 Monad 计算时，后续参数可以依赖于前置参数

listN :: [Integer]
listN = [1..3] >>= \x -> [1..x] >>= \y -> return (x + y)
-- 这里 y 就依赖于 x 的值

-- (<*>) :: f (a -> b) -> f a -> f b
-- (>>=) :: m a -> (a -> m b) -> m b
-- 其实 Monad (>>=) 第二个参数为一个函数，构造出一种连续的计算，函数的上下文中包含着上一个 Monad 传递的信息

-- 因为 Functor => Applicative => Monad 的关系
-- 在大多数选择函数情况下，尽量选择 Functor 或 Applicative 的函数，即使在 Monad 的类型类的实例上
-- 因为这样的类型类限定更为一般，遵循这一原则会让代码的扩展性更好

-- 一些重复函数
-- fmap  :: Functor f => (a -> b) -> f a -> f b
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Monad m => (a -> r) -> m a -> m r

-- forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- 在 ghc 7.10 后 Alternative 与 MonadPlus 合并
-- 尽量使用 Alternative 类型类


-- 扩展 ApplicativeDo 在 Functor 和 Applicative 类型类上使用 do 关键字

