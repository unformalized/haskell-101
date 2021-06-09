{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Intermediate.FreeMonad3 where

data Interaction :: * -> * where
  Return :: a -> Interaction a
  Wrap :: InteractionOp a -> Interaction a

-- 我们将 Ask, Say 行为放在 InteractionOp 类型中

data InteractionOp :: * -> * where
  Say :: String -> (() -> Interaction a) -> InteractionOp a
  Ask :: (String -> Interaction a) -> InteractionOp a


-- 但是 Interaction 和 InteractionOp 在定义中相互调用，就导致根本没有分离出来，将 InteractionOp 中的 Interaction 作为参数传递

data Interaction' :: * -> * where
  Return' :: a -> Interaction' a
  Wrap' :: InteractionOp' (Interaction' a) -> Interaction' a

data InteractionOp' :: * -> * where
  Say' :: String -> (() -> a) -> InteractionOp' a
  Ask' :: (String -> a) -> InteractionOp' a


