module Intermediate.Continuation3 where

import Control.Monad.Cont
import Control.Monad


print4 :: ContT r IO ()
print4 = do
  (goto, n) <-
    callCC $ \k ->
      let f x = k (f, x)
      in return (f, 0)
  if n < 4
    then do
      lift $ putStrLn "hello"
      goto (n + 1)
    else return ()


