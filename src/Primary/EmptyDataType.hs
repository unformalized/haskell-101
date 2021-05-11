{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

-- DataKinds 可以让定义在类型中的值当做类型使用

module Primary.EmptyDataType where

import Data.Void (Void, absurd)

testFoo :: Either Void a -> a
testFoo (Left a) = absurd a
testFoo (Right a) = a

data KEmpty = Empty | NoEmpty


-- data List a b where 的第二个参数没有进行限制可以为任意类型
data SafeList :: * -> KEmpty -> * where
  Nil :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NoEmpty

safeHead :: SafeList a NoEmpty -> a
safeHead (Cons x _) = x


