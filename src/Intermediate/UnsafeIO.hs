module Intermediate.UnsafeIO where

-- 不安全的IO 11.7 小节

import System.IO.Unsafe
import Data.IORef


hRef :: IO (IORef [Char])
hRef = newIORef "hello"

helloIO :: IO [Char]
helloIO = hRef >>= readIORef

helloText :: [Char]
helloText = unsafePerformIO helloIO


writeHell :: IO ()
writeHell = hRef >>= (`writeIORef` "hell")


-- unsafeInterleaveIO 延迟IO操作，等当IO容器中的值被需要时IO操作才进行


ref2 :: IORef Int
ref2 = unsafePerformIO $ newIORef 0

ref3 :: IORef Int
ref3 = unsafePerformIO $ newIORef 0


refPlus' :: IO ()
refPlus' = do
  x1 <- unsafeInterleaveIO $ readIORef ref2
  y1 <- unsafeInterleaveIO $ writeIORef ref2 1 >> return 100
  print (y1 + x1) 

refPlus'' :: IO ()
refPlus'' = do
  x2 <- unsafeInterleaveIO $ readIORef ref3
  y2 <- unsafeInterleaveIO $ writeIORef ref3 1 >> return 100
  print (x2 + y2) 



infixr 3 |+|

(|+|) :: Int -> Int -> Int
(|+|) = (+)

infixr 4 |*|

(|*|) :: Int -> Int -> Int
(|*|) = (*)

testInfix = (|*|) 3 4 |+| 4


infix2 = 2 |+| 3 |*| 4

