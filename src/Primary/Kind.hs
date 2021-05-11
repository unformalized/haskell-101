{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances #-}

-- KindSignatures 扩展可以让我们在声明定义类型时使用 kind
-- FlexibleInstances 扩展可以在声明类型类实现时使用具体类型

module Primary.Kind where

data Triple a b c = Triple a b

data T :: * -> * where
  NIL :: T a
  CONS :: a -> T a -> T a

data AbsTree k a = AbsLeaf a | AbsNode (k (AbsTree k a))

data Tree :: (* -> *) -> * -> * where
  L :: a -> Tree k a
  N :: k (Tree k a) -> Tree k a

-- List 的 kind 为 * -> *
type RoseTree a = Tree [] a

instance Show a => Show (RoseTree a) where
  show (L a) = show a
  show (N tree) = show tree

test :: RoseTree Int
test =  N [N [L 5, L 8]]

