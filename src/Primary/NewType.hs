{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Primary.NewType () where

-- newtype -- 单一构造器并且构造器单一参数

-- newtype T = One | Two
-- newtype T a b = NewType a b
-- newtype 与 data 的区别
-- data 定义类型会在编译系统类型检查与运行中产生额外的运算负担
-- newtype 只是对已有类型进行包装表示区分，在运行时不会使用构造器对值进行包装减少了运行时的开销

newtype T a b = Pair (a, b)
newtype Stream a = Cons (a, Stream a)

newtype Velocity = Velocity Int deriving (Num, Eq)
newtype Weight = Weight Int deriving (Num, Eq)
newtype Second = Second Int deriving (Num, Eq)

v15 :: Velocity
v15 = Velocity 5 + Velocity 10

instance Show Velocity where
  show (Velocity n) = show n ++ "m/s"

instance Show Weight where
  show (Weight x) = show x ++ "kg"

instance Show Second where
  show (Second n) | n == 0 = "0 second"
                  | n == 1 = "1 second"
                  | otherwise = show n ++ " seconds"

