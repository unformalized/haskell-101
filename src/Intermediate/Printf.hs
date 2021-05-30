{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Intermediate.Printf where

import Text.Printf (printf)

helloP :: IO ()
helloP = do
  a <- getLine
  b <- getLine
  printf "Hello %s, I am %s.\n" a b

-- printf 函数实现

class PrintfSelf t where
  printf' :: String -> t

instance PrintfSelf (IO ()) where
  printf' cs = putStrLn cs

format :: Show t => String -> t -> String 
format ('%' : 's' : cs) cs' = show cs' ++ cs
format (c : cs) cs' = c : format cs cs'
format "" cs' = ""

instance Show t => PrintfSelf (t -> IO ()) where
  printf' cs x = putStrLn (format cs x)

instance (Show u, PrintfSelf t) => PrintfSelf (u -> t) where
  printf' cs = printf' . format cs

