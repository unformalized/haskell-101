module HaskellInDepth.Chapter5.STMonadTest where

import Control.Monad.Extra (when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42

comp2 :: STRef s Int -> ST s Int
comp2 = readSTRef

result :: Int
result = runST (comp1 >>= comp2)

-- runST :: forall a. (forall s. ST s a) -> a

-- runST comp1 会报错，因为返回值是 STRef s Int 而 s 变量是限定于 runST 的第一个参数中，所以 runST 的返回值只能是包含变量 a

sumST :: [Int] -> Int
sumST xs = runST $ do
  ref <- newSTRef 0
  mapM_ (\x -> modifySTRef' ref (x +)) xs
  readSTRef ref

countZeroST :: [Int] -> Int
countZeroST xs = runST $ do
  ref <- newSTRef 0
  traverse_ (\x -> when (x == 0) (modifySTRef' ref (+ 1))) xs
  readSTRef ref
