{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Intermediate.Writer where

import Control.Monad ( ap )
import qualified Control.Monad.Writer as W
import Primary.Sort (merge)


-- writer 的主要功能是记录信息, 也就是类型中的 w, 类型参数 a 则指计算结果，记录其实是指得到这个值，发生了什么
newtype Writer w a = Writer
  { runWriter :: (a , w)
  } deriving (Functor)


instance Monoid w => Applicative (Writer w) where
  pure  = return
  (<*>) = ap

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f =
    let (Writer (y, v')) = f x
    in Writer (y, v `mappend` v')
  
left', right' :: Int -> Writer String Int
left' x  = Writer (x-1, "Move left\n")
right' x = Writer (x+1, "Move Right\n")

move' :: Int -> Writer String Int
move' i = do
  x <- left' i
  left' x

-- MonadWriter 类型类是对所有 Writer Monad 行为的一种总结

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  -- 记录一条信息，不产生计算
  tell   :: w -> m ()
  -- 将计算结果不带信息变为计算结果中携带记录的信息
  listen :: m a -> m (a, w)
  -- 对计算结果的携带的信息进行 map
  pass   :: m (a, w -> w) -> m a

instance MonadWriter String (Writer String) where
  tell s = Writer ((), s)
  listen m =
    let (a, w) = runWriter m
    in Writer ((a, w), w)
  pass m =
    let ((a, f), s) = runWriter m
    in Writer (a, f s)


move'' :: Int -> Writer String Int
move'' i = left' i >>=
  \x -> tell "move left once!\n gonna move again\n" >> left' x
  
move''' :: Int -> Writer String (Int, String)
move''' i = do
  x <- left' i
  listen (right' x)


-- 记录归并排序过程

indent :: Int -> ShowS
indent n = showString (replicate (4 * n) ' ')

nl :: ShowS
nl = showChar '\n'

mergeSortW :: Int -> [Int] -> W.Writer String [Int]
mergeSortW l [] = return []
mergeSortW l s@[x] = return [x]
mergeSortW l s@xs = do
  W.tell $ (indent l . showString "mergeSort: " . shows s . nl) ""
  let (a1, a2) = splitAt (length s `div` 2) xs
  W.tell $ (indent (l + 1) . showString "mergeSort: " . shows a1 . shows a2 . nl) ""
  W.liftM2 merge (mergeSortW (l+2) a1) (mergeSortW (l + 2) a2)

mergeResult :: String
mergeResult = W.execWriter (mergeSortW 0 [5,4,3,6])

