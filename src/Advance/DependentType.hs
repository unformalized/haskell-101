{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Advance.DependentType where

data Nat = Z | S Nat deriving (Eq, Show)

type family (a :: Nat) + (b :: Nat) :: Nat where
  Z   + m = m
  S n + m = n + S m

data Vec a (n :: Nat) where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

vHead :: Vec a (S n) -> a
vHead (Cons a n) = a

vTail :: Vec a (S n) -> Vec a n
vTail (Cons a n) = n

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

fromList :: SNat n -> [a] -> Vec a n
fromList SZ [] = Nil
fromList (SS n) (x:xs) = Cons x (fromList n xs)
fromList _ _ = error "size don't matched"

replicate' :: SNat n -> a -> Vec a n
replicate'  SZ _    = Nil
replicate' (SS n) a = Cons a (replicate' n a)
