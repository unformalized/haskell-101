{-# LANGUAGE GADTs, KindSignatures #-}

module Intermediate.FreeMonad where

import Control.Monad
import Control.Monad.State


-- 这里 Interaction 就是一个 DSL
data Interaction :: * -> * where
  Say :: String -> Interaction ()
  Ask :: Interaction String
  Return :: a -> Interaction a
  Bind :: Interaction a -> (a -> Interaction b) -> Interaction b

instance Functor Interaction where

instance Applicative Interaction where
  pure = return
  (<*>) = ap

instance Monad Interaction where
  return = Return
  (>>=) = Bind

say :: String -> Interaction ()
say = Say

ask :: Interaction String
ask = Ask

test1Free :: Interaction ()
test1Free = do
  say "who are you"
  a <- ask
  say $ "hello" ++ a

-- test1Free 是没有具体含义的
-- 使用 IO Monad 进行解释

runIOFree1 :: Interaction a -> IO a
runIOFree1 (Say msg) = putStrLn msg
runIOFree1 Ask = getLine
runIOFree1 (Return x) = return x
runIOFree1 (Bind m f) = do
  x <- runIOFree1 m
  runIOFree1 (f x)

-- 使用 State Monad 进行解释

type Output = [String]
type Input = [String]

runStateFree2 :: Interaction a -> State (Input, Output) a
runStateFree2 (Say msg) = state $ \(input, write) -> ((), (input, write ++ [msg]))
runStateFree2 Ask = state $ \(i:is, write) -> (i, (is, write))
runStateFree2 (Return x) = return x
runStateFree2 (Bind m f) = do
  x <- runStateFree2 m
  runStateFree2 (f x)

testStateF1 :: ((), (Input, Output))
testStateF1 = (runState $ runStateFree2 test1Free) (["liubin"], [])

-- 这里 runIOFree1 和 runStateFree2 是含有重复代码的，对 Return 和 Bind 的定义，仅仅是类型不同
-- 上面的 Interaction DSL 定义有问题，不能实现 Functor 类型类实例

-- test1Free 去掉 do, 将 >>= 改为 Bind

test1Free2 :: Interaction ()
test1Free2 = say "who are you" `Bind` (\_ -> ask `Bind` (\a -> say $ "hello " ++ a))

-- 其实我们可以不需要 Bind，将 Bind 的内容交给 Say 和 Ask，因为 Bind 的作用时连接 Ask 和 Say

-- Say :: String -> (() -> Interaction a) -> Interaction a
-- Ask :: (String -> Interaction a) -> Interaction a

