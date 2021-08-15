{-# LANGUAGE PolyKinds, MultiParamTypeClasses #-}

-- | Kind 多态

module Advance.KindPloy () where

import Prelude hiding (Maybe)

newtype T f a = MKT (f a) -- T :: (* -> *) -> * -> *
data Maybe a = Nothing | Just a -- Maybe :: * -> *

type T1 = T Maybe Int

newtype F f = MKF (f Int) -- F :: (* -> *) -> *
type T2 = T F Maybe  -- kind 不匹配

data Ts f g a b = MKTs (f a) (g b)

newtype T' (f :: k -> *) (a :: k) = MkT' (f a)

class Foo (f :: k -> *) (a :: k) where
  foo :: f a -> Int
