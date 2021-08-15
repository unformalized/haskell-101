-- | 可类型化
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Advance.Typeable where

import Data.Typeable
import Data.Proxy

data Person2 = Person2 String Bool deriving (Show, Typeable)

equalTypes :: (Typeable a, Typeable b) => a -> b -> Bool
equalTypes a b = typeOf a == typeOf b

-- typeable 的实现

type TypeRepSelf = String

class TypeableSelf (a :: *) where
  typeof :: a -> TypeRepSelf

instance TypeableSelf Int where
  typeof _ = "Int"

-- instance TypeableSelf a => TypeableSelf (Maybe a) where
--  typeof _  = "Maybe " ++ typeof (undefined :: a)


-- 我们可以看到 typeof 返回值与参数无关，与参数的类型相关，并且我们可以用 undefined 代表类型为 a 的值，对于每个构造器类型都要声明 Typeable 类型类实例比较重复

-- instance (TypeableSelf f, TypeableSelf a) => TypeableSelf (f a) where
--  typeof _ = typeof (undefined :: f) ++ typeof (undefined :: a)

-- 1. 首先 f 的 kind 为 * -> *, Typeable 默认接受的类型的 Kind 为 *
-- 2. 不存在 undefined :: f 类型的值，只有 Kind 为 * 的类型才有值

-- 以前为解决第一个问题，采用一下方式

class TypeableSelf1 t where
  typeof1 :: t a -> TypeRepSelf

class TypeableSelf2 t where
  typeof2 :: t a b -> TypeRepSelf

-- 使用 Proxy 代理类型

class TypeableS (t :: k) where
  typeofS :: Proxy t -> TypeRepSelf
  
-- instance (TypeableS f, TypeableS a) => TypeableS (f a) where
--  typeofS _ = typeofS (Proxy :: Proxy f) ++ typeofS (Proxy :: Proxy a)



















