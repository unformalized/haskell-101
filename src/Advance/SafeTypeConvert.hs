{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Advance.SafeTypeConvert where

import Data.Coerce

newtype Dollar = Dollar Int deriving (Show, Num)
newtype Pound  = Pound  Int deriving (Show, Num)

type family F a

type instance F Int = Int
type instance F Dollar = Char

class Bad a where
  bad :: a -> F a

instance Bad Int where
  bad = (+1)

-- Bad 类型类不能使用 coerce，因为使用类型上的计算
-- deriving instance Bad Dollar

-- 另一种情况为 data 定义时使用了类型的计算
data Tricky a = Tricky (F a)

maybe :: Maybe Dollar -> Maybe Int
maybe md = coerce md

-- tricky :: Tricky Dollar -> Tricky Int
-- tricky td = coerce td

data T a b c = T1 (F a) b c | T2 a b

type role Foo nominal representational
data Foo a b = Foo a Int


