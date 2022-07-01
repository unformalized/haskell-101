-- | 基础的整数类型操作
module Data.BaseInt where

import Data.Bits (Bits (shiftL, shiftR, zeroBits), (.|.))
import Data.Char (chr)
import Data.Int (Int, Int16, Int32, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- 不同 Word 类型间的转换, 默认为小端机器, 即高位在高位地址

-- word8 -> word16, word32 等，若是位数不够则补 0

zeros :: [Word8]
zeros = repeat 0

fromWord8 :: (Bits a, Integral a) => [Word8] -> a
fromWord8 = foldr accum 0
  where
    accum w8 a = (a `shiftL` 8) .|. fromIntegral w8

w8tow16 :: [Word8] -> Word16
w8tow16 ws = fromWord8 (take 2 (ws ++ zeros))

w8to16' :: Word8 -> Word16
w8to16' w = w8tow16 [w]

w8tow32 :: [Word8] -> Word32
w8tow32 ws = fromWord8 (take 4 (ws ++ zeros))

w8tow32' :: Word8 -> Word32
w8tow32' w = w8tow32 [w]

w8tow64 :: [Word8] -> Word64
w8tow64 ws = fromWord8 (take 8 (ws ++ zeros))

w8to64' :: Word8 -> Word64
w8to64' w = w8tow64 [w]

toWord8 :: (Integral a, Bits a) => Int -> a -> [Word8]
toWord8 n a = map (\i -> fromIntegral (a `shiftR` i)) shiftN
  where
    shiftN = take n [0, 8 ..]

w64tow8 :: Word -> [Word8]
w64tow8 = toWord8 8

w32tow8 :: Word32 -> [Word8]
w32tow8 = toWord8 4

w16tow8 :: Word16 -> [Word8]
w16tow8 = toWord8 2

-- Word 与 Int 间的互转

w2i :: (Enum a, Enum b) => a -> b
w2i = toEnum . fromEnum

i2w :: (Enum a, Enum b) => a -> b
i2w = toEnum . fromEnum

-- Word8 -> Char

w2c :: Word8 -> Char
w2c = chr . fromEnum

c2w :: Char -> Word8
c2w = i2w . fromEnum

safec2w :: Char -> Word8
safec2w = head . w64tow8 . i2w . fromEnum

-- others

w8toi :: [Word8] -> Int
w8toi ws = w2i (w8tow32 ws)

itow8 :: Int -> [Word8]
itow8 = w32tow8 . i2w
