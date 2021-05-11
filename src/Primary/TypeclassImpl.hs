{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Primary.TypeclassImpl where

import Control.Applicative ()
import Data.Monoid ()
import Data.Char (ord)

-- 类型类实例的实现

-- 1. 使用 deriving 关键字自动实现，有限制（Functor、Foldable 等类型类需要使用语言扩展）
-- 2. 使用 instance 关键字

data Shape = Circle Double | Square Double | Rectangle Double Double

instance Eq Shape where
  Circle a == Circle b = a == b
  Square a == Square b = a == b
  Rectangle a1 b1 == Rectangle a2 b2 = a1 == a2 && b1 == b2
  _ == _ = False

-- newtype 定义类型的类型类实例导出
-- newtype 本质上是对已有类型进行一层包装，那么可以利用语言扩展对 newtype 实现其内部类型的类型类实例

newtype A a b = A (a,b) deriving (Show, Functor, Applicative)

class ToInt a where
  toInt :: a -> Int
  toInt _ = 0

instance ToInt Char where
  toInt = ord

newtype C = C Char deriving (Show, ToInt)

newtype Foo a = Foo a

