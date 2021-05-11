{-# LANGUAGE BinaryLiterals #-}

module Base.BaseType where

import Data.Word

-- Bool
true = True
false = False

-- Char
a = 'a'
ni = '\20320'

-- Int 有符号整数
i2 :: Int
i2 = 2^32

-- Word 无符号整数

w1 :: Word
w1 = 1

-- Integer 任意精度整数
int2 :: Integer
int2 = 2^64

b1 = 0b1110001

-- 小数，有理数
fpi :: Float
fpi = pi

dpi :: Double
dpi = pi

r1 :: Rational
r1 = 4.12322

-- String
s1 :: String
s1 = "Hello world"

-- tuple

t1 = (10, 40)


cu = curry

uc = uncurry

type RGB = (Word8, Word8, Word8)
type Picture = [[RGB]]


