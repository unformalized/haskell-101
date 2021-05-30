{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
module Intermediate.Reader where

import Control.Monad
import qualified Control.Monad.Reader as R


-- Reader 非常简洁明了，一种从环境取值的过程
newtype Reader r a = Reader { runReader :: r -> a } deriving Functor

instance Applicative (Reader r) where
  pure = return
  (<*>) = ap

-- Reader 含义就是在 r 类型的环境中读取 a 类型的数据
-- Reader 通过 Monad 组合的意思就是代表一个 Reader 只能从环境取出一种值，而现在 Monad 
-- 组合多个 Reader 从环境中取出多个值并且进行整合，但是环境始终是那个环境

instance Monad (Reader r) where
  return a = Reader $ const a
  m >>= k = Reader $
    \r -> runReader (k (runReader m r)) r

class (Monad m) => MonadReader r m | m -> r where
  -- 得到一个环境
  ask :: m r
  -- 从一个环境变为一个新的环境，所以取的值从旧值变为新值
  local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
  ask = Reader id
  local f m = Reader $ runReader m . f

testReader :: Reader [Int] [Int]
testReader = do
  xs <- local (map (+1)) ask
  ask

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

-- 例子

data Exp = Val Int
         | Var String
         | Add Exp Exp
         | Decl Bind Exp deriving (Show, Eq)

type Bind = (String, Int)
type Env = [Bind]

updateEnv :: Bind -> Env -> Env
updateEnv = (:)

resolve :: Exp -> R.Reader Env (Maybe Exp)
resolve (Val i) = return (Just (Val i))
resolve (Var s) = do
  env <- R.ask
  case lookup s env of
    Nothing -> return Nothing
    Just v -> return (Just (Val v))
resolve (Add e1 e2) = do
  re1 <- resolve e1
  case re1 of
    Nothing -> return Nothing
    Just a -> do
      re2 <- resolve e2
      case re2 of
        Nothing -> return Nothing
        Just b -> return (Just (Add a b))
resolve (Decl b e) = R.local (updateEnv b) (resolve e)

test1Exp :: Exp
test1Exp = Decl ("x", 3) (Decl ("y", 5) (Add (Var "x") (Add (Var "y") (Val 6))))

test2Exp :: Exp
test2Exp = Add (Decl ("x", 5) (Var "x")) (Decl ("x", 3) (Var "x"))

runExp1 = R.runReader (resolve test1Exp) []
runExp = R.runReader (resolve test2Exp) []








