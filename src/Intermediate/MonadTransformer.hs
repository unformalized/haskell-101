{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Intermediate.MonadTransformer where

import Control.Monad ( ap )
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Data.Functor.Identity as Id
import qualified Control.Monad.Trans.Maybe as TM
import qualified Control.Monad.Base as B

import Intermediate.State ( MonadState )
import Intermediate.Reader ( MonadReader )
import Intermediate.Writer ( MonadWriter )

newtype Identity a = Identity { runIdentity :: a } deriving Functor
newtype IdentityT m a = IdentityT { runIdentityT :: m a } deriving Functor

instance Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (<*>) (IdentityT f) (IdentityT a) =
    IdentityT (f <*> a)

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) (IdentityT ma) f =
    IdentityT (ma >>= (\a -> let IdentityT mb = f a in mb))

type IState s a = IdentityT (S.State s) a

push :: Int -> IState [Int] ()
push x = IdentityT $ S.state $ \xs -> ((), x:xs)

pop :: IState [Int] Int
pop = IdentityT $ S.state $ \(x:xs) -> (x, xs)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  } deriving Functor

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap


instance Monad m => Monad (MaybeT m) where
  return a = MaybeT (return $ Just a)
  (>>=) (MaybeT mma) f =
    MaybeT (mma >>=
      \case
        Nothing -> pure Nothing
        Just a -> runMaybeT $ f a
    )

safeHeadT :: [a] -> MaybeT Identity a
safeHeadT [] = MaybeT $ Identity Nothing
safeHeadT (x:xs) = MaybeT $ Identity (Just x)

-- StateT

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) } deriving Functor

instance Monad m => Monad (StateT s m) where
  return a = StateT (\s -> pure (a, s))
  (>>=) msa f =
    StateT (\s ->
      let ms = runStateT msa s
      in ms >>= (\(a, s) ->
        let mb = f a
        in runStateT mb s)
    )

instance Monad m => Applicative (StateT s m) where
  pure = return
  (<*>) = ap

pushST :: Int -> StateT [Int] Identity ()
pushST x =
  StateT $ \xs -> Identity ((), x:xs)

popST :: StateT [Int] Identity Int
popST =
  StateT $ \(x:xs) -> Identity (x, xs)

-- State Maybe

pushSM :: Int -> S.StateT [Int] Maybe ()
pushSM x =
  S.StateT $ \xs -> Just ((), x:xs)

popSM :: S.StateT [Int] Maybe Int
popSM =
  S.StateT (\case
    [] -> Nothing
    (x:xs) -> Just (x, xs)
  )

pushMS :: Int -> TM.MaybeT (S.State [Int]) ()
pushMS x =
  TM.MaybeT $ S.state $ \xs -> (Just (), x:xs)


popMS :: TM.MaybeT (S.State [Int]) Int
popMS =
  TM.MaybeT $ S.state (\case
    [] -> (Nothing, [])
    (x:xs) -> (Just x, xs)
  )

-- Monad 转换器组合与复合 Monad 的区别


push' :: Int -> S.State [Int] ()
push' x = S.state $ \xs -> ((), x:xs)

-- pop 返回 Maybe 来达到以上效果

pop' :: S.State [Int] (Maybe Int)
pop' = S.state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

-- 但是在组合 state 时会需要额外的分支判断

testStack :: S.State [Int] ()
testStack = do
  -- x 为 Maybe Int
  x <- pop'
  push' 1

testStack' :: S.StateT [Int] Maybe ()
testStack' = do
  -- x 为 Int
  x <- popSM
  pushSM x


-- 而 Monad 转换器则是组合多个 Monad 效应并且 >>= 运算符会将组合的 Monad 效应一次处理好
-- Monad 转换器不仅仅简单地复合两个 Monad，在 Monad 转换器定义时，会严格地限定它们的组合方式。

type St s a = s -> (a, s)
type Wt w a = (a, w)

type WtT w m a = m (a, w)
type StT s m a = s -> m (a, s)

-- WriterT 组合 State 变成了 Writer w (State s) a => s -> ((a,w), s)，与 St s (Wt w a) 相同
-- StateT 组合 Writer 变成了 StateT s (Writer w) a => s -> ((a, s), w) 与 Wt w (St s a) = (s -> (a, s), w) 不同

type StateWM s w a = S.StateT s (W.WriterT w Maybe) a

stateWM :: S.StateT [Int] (W.WriterT String Maybe) Int
stateWM = S.StateT $ \(x:xs) -> W.WriterT $ Just ((x,xs), "test")

-- 对于 Monad 转换器组合 m1Tm2Tm3T...mn, m1T 虽然在 Monad 转换器的栈顶端，但是副作用在最内侧

-- 13.2.1 Monad转换器的组合顺序

-- 组合 StateT 和 Maybe ，MaybeT 和 State

type StM s a = S.StateT s Maybe a
type MTS s a = TM.MaybeT (S.State s) a

popSTM :: StM [Int] Int
popSTM = S.StateT $ \case
  -- StateT 组合 Maybe 时，当遇到 Nothing 时，整个 State Monad 效应会消失
  [] -> Nothing
  (x:xs) -> Just (x,xs)

popMTS :: MTS [Int] Int
popMTS = TM.MaybeT (S.state $
  -- MaybeT 组合 State 时，则不会影响 State 效应，因为 Maybe 的效应的作用域最小
  \case
    [] -> (Nothing, [])
    (x:xs) -> (Just x, xs))


-- 现在使用 WriterT String 和 State [Int] 组合来记录栈操作的过程
-- 使用 StateT 组合 Writer 作用没有差别
pushWS :: Int -> W.WriterT String (S.State [Int]) ()
pushWS x = W.WriterT (S.state $ \xs -> (((), "push: " ++ show x), x:xs))

popWS :: W.WriterT String (S.State [Int]) Int
popWS = W.WriterT (S.state $ \(x:xs) -> ((x, "pop: " ++ show x ), xs))

-- WriterT 和 State 与 StateT 和 Writer 是可以交换的
-- 而 MaybeT 和 State 不可以交换，因为 MaybeT 是多构造器定义类型，Nothing 这种
-- 零元构造器会使组合它的 Monad 转换器的行为消失

































