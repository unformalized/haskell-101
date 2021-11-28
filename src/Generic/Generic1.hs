{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Generic.Generic1 where

data U1 p = U1 deriving (Show, Eq)
data (:*:) a b p = a p :*: b p deriving (Show, Eq)
data (:+:) a b p = L (a p) | R (b p) deriving (Show, Eq)

class Generic (a :: *) where
  type family Rep a :: *
  from :: a -> Rep a
  to   :: Rep a -> a

class Generic1 (f :: * -> *) where
  type Rep1 f :: * -> *
  from1 :: f p -> Rep1 f p
  to1   :: Rep1 f p -> f p

newtype Par p = Par { unPar :: p } deriving Show
newtype Rec a p = Rec { unRec :: a p } deriving Show


class GFunctor (f :: * -> *) where
  gfmap :: (a -> b) -> (f a -> f  b)
  default gfmap :: (Generic1 f, GFunctor (Rep1 f)) => (a -> b) -> (f a -> f b)
  gfmap = defaultfmap


defaultfmap :: (Generic1 f, GFunctor (Rep1 f)) => (a -> b) -> (f a -> f b)
defaultfmap f x = to1 (gfmap f (from1 x))

instance GFunctor U1 where
  gfmap f U1 = U1

instance (GFunctor a, GFunctor b) => GFunctor (a :*: b) where
  gfmap f (a :*: b) = gfmap f a :*: gfmap f b


instance (GFunctor a, GFunctor b) => GFunctor (a :+: b) where
  gfmap f (L a) = L (gfmap f a)
  gfmap f (R a) = R (gfmap f a)

instance GFunctor Par where
  gfmap f (Par p) = Par (f p)

instance (GFunctor a) => GFunctor (Rec a) where
  gfmap f (Rec a) = Rec (gfmap f a)

data List a = Nil | Cons a (List a) deriving Show
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

instance Generic1 List where
  type Rep1 List = U1 :+: (Par :*: (Rec List))
  from1 Nil = L U1
  from1 (Cons x xs) = R (Par x :*: Rec xs)
  to1 (L U1) = Nil
  to1 (R (Par x :*: Rec xs)) = Cons x xs

instance Generic1 Tree where
  type Rep1 Tree = U1 :+: (Par :*: (Rec Tree) :*: (Rec Tree))
  from1 Leaf = L U1
  from1 (Node a l r) = R (Par a :*: Rec l :*: Rec r)
  to1 (L U1) = Leaf
  to1 (R (Par a :*: Rec l :*: Rec r)) = Node a l r

instance GFunctor List
instance GFunctor Tree






