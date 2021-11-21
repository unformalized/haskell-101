{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Advance.TypeComputation2 where

data Nat = Z | S Nat deriving (Eq, Show)

type family (a :: Nat) + (b :: Nat) :: Nat where
  Z   + m = m
  S n + m = n + S m

type family (n :: Nat) * (m :: Nat) :: Nat where
  Z   * m = Z
  S n * m = (m * n) + m
