{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Intermediate.FreeMonad4 where

import Control.Applicative
import Control.Monad

-- 对于前面的 Interaction 若是想要实现表达任意 DSL 行为，则可以将 InteractionOp 也作为参数传递

data Interaction :: (* -> *) -> * -> * where
  Return :: a -> Interaction f a
  Wrap :: f (Interaction f a) -> Interaction f a

-- 现在我们定义 Free Monad

data Free f a = Pure a | Poll (f (Free f a)) deriving Functor

deriving instance (Show (f (Free f a)),   Show a) => Show (Free f a)
deriving instance (Eq (f (Free f a)), Eq a) => Eq (Free f a)

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = Pure
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure x >>= f = f x
  Poll k >>= f = Poll (fmap (>>= f) k)

data InteractionOp :: * -> * where
  Say :: String -> (() -> f) -> InteractionOp f
  Ask :: (String -> r) -> InteractionOp r

deriving instance Functor InteractionOp

type Interaction' = Free InteractionOp

say :: String -> Interaction' ()
say msg = Poll (Say msg Pure)

ask :: Interaction' String
ask = Poll (Ask Pure)


testFree3 :: Interaction' ()
testFree3 = do
  say "who are you"
  a <- ask
  say $ "Hello " ++ a

runTest3 :: InteractionOp a -> IO a
runTest3 (Say msg k) = putStrLn msg >>= const( return (k ()))
runTest3 (Ask fs) = do
  fs <$> getLine










