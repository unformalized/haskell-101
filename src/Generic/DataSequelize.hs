{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE DeriveAnyClass #-}
#endif

module Generic.DataSequelize where

import Data.Binary
import Numeric (showHex)
import qualified Data.ByteString.Lazy as DB
import GHC.Generics


data Exp = IntE Int | OpE String Exp Exp deriving Show

instance Binary Exp where
  put (IntE i) = do
    put (0 :: Word8)
    put i
  put (OpE s e1 e2) = do
    put (1 :: Word8)
    put s
    put e1
    put e2
  get = do
    t <- get :: Get Word8
    case t of
      0 -> do
        i <- get
        return (IntE i)
      1 -> do
        s <- get
        e1 <- get
        e2 <- get
        return (OpE s e1 e2)

fooExp :: Exp
fooExp = OpE "*" (IntE 1) (IntE 2)


testEncodeFooExp :: [Word8]
testEncodeFooExp = DB.unpack (encode fooExp)
--  1 第二个构造器 - String 列表长度 1 (64 bit) - 列表元素 42 - 0 第一个构造器 - 1 - 0 第一个构造器  - 2
-- [1,0,0,0,0,0,0,0,1,42,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,2]

testDecodeFooExp :: Exp
testDecodeFooExp = decode (encode fooExp)
-- OpE "*" (IntE 1) (IntE 2)

data Exp' = IntE' Int | OpE' String Exp' Exp'
#if __GLASGOW_HASKELL__ >= 710
  deriving (Show, Generic, Binary)
#else
  deriving (Show, Generic)
instance Binary Exp'
#endif


barExp :: Exp'
barExp = OpE' "*" (IntE' 1) (IntE' 2)

testEncodeBarExp' :: [Word8]
testEncodeBarExp' = DB.unpack (encode barExp)

