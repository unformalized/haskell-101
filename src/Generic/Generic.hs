{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Generic.Generic where

-- 类型类自动导出实现

data T a b = Q | N a b

data Tree a = Leaf a | Node a (Tree a) (Tree a)

data Choice = I Int
            | C Char
            | B Choice Bool
            | S Choice deriving Show

-- Eq 类型类实例实现

instance (Eq a, Eq b) => Eq (T a b) where
  Q == Q = True
  N a1 b1 == N a2 b2 = (a1 == a2) && (b1 == b2)
  _ == _ = False

instance (Eq a) => Eq (Tree a) where
  Leaf a1 == Leaf a2 = a1 == a2
  Node a1 left1 right1 == Node a2 left2 right2 = (a1 == a2) && (left1 == left2) && (right1 == right2)
  _ == _ = False


-- 基本代数类型

data U = U deriving (Show, Eq)
data a :*: b = a :*: b deriving (Show, Eq)
data a :+: b = L a | R b deriving (Show, Eq)

type MyBool = U :+: U

data Three = One | Two | Three
type AlgThree = (U :+: U) :+: U

class Generic a where
  type Rep a :: *
  from :: a -> Rep a
  to   :: Rep a -> a

instance Generic Bool where
  type Rep Bool = U :+: U
  from False = L U
  from True  = R U
  to   (L U) = False
  to   (R U) = True

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Generic (List a) where
  type Rep (List a) = U :+: (a :*: List a)
  from Nil = L U
  from (Cons x xs) = R (x :*: xs)
  to (L U) = Nil
  to (R (x :*: xs)) = Cons x xs


class GEq a where
  geq :: a -> a -> Bool
  default geq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  geq = geqDefault


geqDefault :: (Generic a, GEq (Rep a)) => a -> a -> Bool
geqDefault x y = geq (from x) (from y)


-- 实现同构类型的实例

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L a1) (L a2) = geq a1 a2
  geq (R b1) (R b2) = geq b1 b2
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq U where
  geq U U = True

instance GEq Bool

instance ((GEq a, Generic a)) => GEq (List a)


