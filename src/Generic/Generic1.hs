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
