{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Intermediate.State where

import Control.Monad ( ap )
import Intermediate.Reader ( Reader(..) )
import qualified Control.Monad.State as S
import Data.Monoid
import qualified Control.Monad.Reader as R
import Intermediate.Stack ( Stack )

-- State 就是 oldState -> newState, 但是 State 只是一个状态变化的过程，可以使用 Monad 来累积这种过程，但是最终想要得到一个新的 state 需要使用 runState (old -> new) old
newtype State s a = State
  { runState :: s -> (a, s)
  } deriving Functor

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State sf >>= f = State $
    \s -> let (a, s') = sf s
          in runState (f a) s'

evalState :: State s a -> s -> a
evalState state s =
  fst (runState state s)

evaluate :: State s a -> Reader s a
evaluate s = Reader $ \e -> evalState s e

readOnly :: Reader s a -> State s a
readOnly r = State $ \s -> (runReader r s, s)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst $ ntAux t 0

ntAux :: Tree a -> Int -> (Tree (a, Int), Int)
ntAux (Leaf a) n = (Leaf (a, n), n + 1)
ntAux (Node l a r) n =
  let (nn, n') = ((a, n), n + 1)
  in
    let (ln, n'') = ntAux l n'
    in
      let (rn, n''') = ntAux r n''
      in (Node ln nn rn, n''')

-- 使用 State 完成树的标记

increase :: S.State Int Int
increase = S.state $ \i -> (i, i+1)

ntAuxS :: Tree a -> S.State Int (Tree (a, Int))
ntAuxS (Leaf a) = do
  nl <- increase
  return (Leaf (a, nl))
ntAuxS (Node l n r) = do
  nl <- increase
  lt <- ntAuxS l
  rt <- ntAuxS r
  return (Node lt (n, nl) rt)

ntAuxS' :: Tree a -> S.State Int (Tree (a, Int))
ntAuxS' (Leaf a) = increase >>= (\nl -> return (Leaf (a, nl)))
ntAuxS' (Node l n r) =
  increase >>= \nl ->
    ntAuxS' l >>= \lt ->
      ntAuxS' r >>= \rt -> return (Node lt (n, nl) rt)

labelTree' :: Tree a -> Tree (a, Int)
labelTree' t = S.evalState (ntAuxS t) 0


-- Reader Monad, FunApp, State Monad 关系
-- State 可以理解为由 FunApp 单位半群和 Reader Monad 组合成的一个 Monad

(|>) :: Monoid a => a -> a -> a
(|>) = mappend

newtype FunApp a = FunApp
  { appFunApp :: a -> a
  }

instance Semigroup (FunApp a) where
  (<>) (FunApp f) (FunApp g) = FunApp (g . f)

instance Monoid (FunApp a) where
  mempty = FunApp id
  mappend = (<>)

push :: Int -> FunApp Stack
push i = FunApp $ \xs -> i:xs

pop :: FunApp Stack
pop =  FunApp $ \(x:xs) -> xs


testM :: FunApp Stack
testM =
  push 3 |> push 1 |> pop

-- FunApp 单位半群可以顺序地改变上下文信息
-- Reader 则从上下文环境中读取信息

class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put s = State $ const ((), s)


