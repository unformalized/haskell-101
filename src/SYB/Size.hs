{-# LANGUAGE DefaultSignatures #-}
module SYB.Size where

import Data.Data

class Size a where
  size :: a -> Int
  default size :: Data a => a -> Int
  size = gsize

gsize :: Data a => a -> Int
gsize t = 1 + sum (gmapQ gsize t)
