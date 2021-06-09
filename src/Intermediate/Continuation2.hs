module Intermediate.Continuation2 where

import Control.Monad.Cont

fibCps3 :: Int -> ContT r IO Int
fibCps3 0 = return 1
fibCps3 1 = return 1
fibCps3 n = do
  n2 <- fibCps3 (n - 2)
  liftIO $ putStrLn $ "fibCps " ++ show (n - 2) ++ "=" ++ show n2
  n1 <- fibCps3 (n - 1)
  liftIO $ putStrLn $ "fibCps " ++ show (n - 1) ++ "=" ++ show n1
  return (n1 + n2)

testFibCps :: IO ()
testFibCps = runContT (fibCps3 4) print


