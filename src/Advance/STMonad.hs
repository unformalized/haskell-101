-- | ST Monad

module Advance.STMonad where

import Data.STRef
import Control.Monad.ST

-- 使用可变变量进行操作时运行会更快，计算过程不需要其他副作用

-- data ST s a
-- data STRef s a
-- newSTRef :: a -> ST s (STRef s a)  声明一个变量
-- readSTRef :: STRef s a -> ST s a   读取变量值
-- writeSTRef :: STRef s a -> a -> ST s () 写入变量值

-- runST :: forall s. (forall s. ST s a) -> a

-- 利用 ST Monad 实现阶乘

factorailST :: Int -> STRef s Int -> ST s Int
factorailST n accRef = do
  numRef <- newSTRef n
  num <- readSTRef numRef
  if num < 1
    then readSTRef accRef
    else do
    acc <- readSTRef accRef
    writeSTRef accRef (acc * n)
    writeSTRef numRef (num - 1)
    factorailST (num - 1) accRef

fact :: Int -> Int
fact n =
  runST $ do
  accRef <- newSTRef 1
  factorailST n accRef
