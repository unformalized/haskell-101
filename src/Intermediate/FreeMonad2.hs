{-# LANGUAGE GADTs, KindSignatures #-}

module Intermediate.FreeMonad2 where

import Control.Monad
import Control.Monad.State

data Interaction :: * -> * where
  Say :: String -> (() -> Interaction a) -> Interaction a
  Ask :: (String -> Interaction a) -> Interaction a
  Return :: a -> Interaction a

instance Functor Interaction where
  fmap f (Return a) = Return (f a)
  fmap f (Ask k) = Ask (fmap f . k)
  fmap f (Say msg k) = Say msg (fmap f . k)


instance Applicative Interaction where
  pure = return
  (<*>) = ap

instance Monad Interaction where
  return = Return
  Return x >>= f = f x
  (Ask k) >>= f = Ask (f <=< k)
  (Say msg k) >>= f = Say msg (f <=< k)

say :: String -> Interaction ()
say msg = Say msg Return


ask :: Interaction String
ask = Ask Return

testFree2 = do
  say "who are you"
  a <- ask
  say $ "hello" ++ a

testFree2WithoutDo =
  say "who are you" >>= (\_ -> ask >>= (\a -> say $ "hello" ++ a))

-- say msg = Say msg Return
--

runInterWithIO :: Interaction a -> IO a
runInterWithIO (Return x) = return x
runInterWithIO (Ask k) = getLine >>= runInterWithIO . k
runInterWithIO (Say msg k) = putStrLn msg >>= runInterWithIO . k

runInterList :: Interaction a -> [String] -> [String]
runInterList (Return _) _ = []
runInterList (Ask k) (i:ins) = runInterList (k i) ins
runInterList (Say msg k) ins = msg : runInterList (k ()) ins

-- 接下来我们将 Return 也去掉








