{-# LANGUAGE InstanceSigs #-}
module Primary.FoldableTree (tree, flatten) where

import Data.Foldable ()
import Data.Monoid
    ( First(First, getFirst), Last(Last, getLast), All(All, getAll), Any(Any, getAny) )

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x) = f x
  foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a) = f a b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

tree :: Tree Int
tree = Node (Leaf 1) 2 (Leaf 3)

flatten :: Tree a -> [a]
flatten = foldMap (: [])

-- 使用 First 半群
find' :: Foldable t => (a -> Bool) -> t a -> Maybe a
find' p = getFirst . foldMap (\x -> First (if p x then Just x else Nothing))

findLast' :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast' p = getLast . foldMap (\x -> Last (if p x then Just x else Nothing))

-- 使用 All 半群
all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' p = getAll . foldMap (All . p)

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' p = getAny . foldMap (Any . p)

-- Fold 的二元性定理
-- 1. 若是 (M, (+), u) 为单位半群，则 foldr (+) u xs == foldl (+) u xs, xs为有限元素列表
-- 2. 若存在两个二元运算符 (+) (x) 和值 e , 满足 x (+) (y (x) z) = (x (+) y) (x) z, 且 x (+) e = e (x) x, 则 foldr (+) e xs = foldl (x) e xs, 且 xs 为有限元素的列表
-- 3. 对于所有有限元素列表 xs, 都有  foldr f u xs = foldl (flip f) e (reverse xs), u 与 e 分别为 f 与 flip f 的单位元

copyList :: [a] -> [a]
copyList = foldr (:) []

-- copyTree :: Tree a -> Tree a
-- copyTree = foldr _todo1 _todo2