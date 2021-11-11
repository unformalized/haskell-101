-- |
{-# LANGUAGE TypeFamilies #-}

module Advance.TypeFamily where

class (Eq ce) => Collections ce where
  type Element ce :: * -- 定义一个类型函数，
  empty :: ce
  insert :: Element ce -> ce -> ce
  member :: Element ce -> ce -> Bool

instance (Eq a) => Collections [a] where
  type Element [a] = a -- 实现上述的类型函数
  empty = []
  insert x xs = x:xs
  member x xs = x `elem` xs
