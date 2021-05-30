-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, TypeFamilies #-}

module Primary.TypeDependencies (
  Nat2(
    Zero2,
    Succ2
  ),
  equals,
  Func(
    fun
  ),
  plus,
  multi
) where

import GHC.Float

class GEq a b where
  equals :: a -> b -> Bool

data Nat2 = Zero2 | Succ2 Nat2

instance GEq Nat2 [a] where
  equals a b = eq a b
    where
      eq Zero2 [] = True
      eq (Succ2 n) (_:xs) = eq n xs
      eq _ _ = False

-- 当多参数类型类中类型参数作为声明函数返回结果时，可能会产生歧义

class Func a b where
  fun :: a -> b

instance Func Int Nat2 where
  fun a = Zero2

instance Func Int Int where
  fun _ = 0

-- fun (5::Int) 则会产生歧义
-- 但是在 ghc8-6-5  ghci 中使用 :t fun (5::Int) 不会报错， 而是展示 fun (5::Int) :: Func Int b => b
-- 但是执行 fun 5 会报错

-- 使用 FunctionalDependencies 扩展，定义类型类 FuncFD 中 a b 的函数依赖

class FuncFD a b | a -> b where
  func :: a -> b

instance FuncFD Int Nat2 where
  func 0 = Zero2
  func n = Succ2 (func (n - 1))

class (Num a, Num b, Num c) => GPlus a b c | a b -> c where
  plus :: a -> b -> c

instance GPlus Int Float Float where
  plus a b = fromIntegral a + b

-- instance GPlus Int Float Double where
--   plus a b = fromIntegral a + float2Double b

instance GPlus Float Float Float where
  plus a b = a + b


class Multi a b c | a b -> c where
  multi :: a -> b -> c

data Vector = Vector Int Int Int deriving (Eq, Show)


instance Multi Vector Vector Int where
  multi (Vector x1 x2 x3) (Vector y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3

class Collection e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool

instance Eq a => Collection a [a] where
  empty = []
  insert x xs = x:xs
  member = elem

-- 关联类型

class (Num a, Num b) => GPlus2 a b where
  type SumType a b :: *
  plus2 :: a -> b -> SumType a b

instance GPlus2 Int Int where
  type SumType Int Int = Int
  plus2 a b = a + b



