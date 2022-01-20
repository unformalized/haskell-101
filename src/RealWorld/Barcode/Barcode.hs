module RealWorld.Barcode.Barcode where

{- https://zhuanlan.zhihu.com/p/82023189 条形码解析 -}

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (digitToInt)
import Data.Ix (Ix (..))
import Data.List (foldl', group, sort, sortBy, tails)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import GHC.Arr (Array (..), bounds, elems, indices, ixmap, listArray, (!))
import RealWorld.ParsePgm.Parse (Parse (Parse), assert, identity, parseByte, parseNat, parseTimes, parseWhileWith, skipSpaces, word2char)
import System.Environment (getArgs)

leftOddList :: [[Char]]
leftOddList =
  [ "0001101",
    "0011001",
    "0010011",
    "0111101",
    "0100011",
    "0110001",
    "0101111",
    "0111011",
    "0110111",
    "0001011"
  ]

rightList :: [[Char]]
rightList = map complement <$> leftOddList
  where
    complement '0' = '1'
    complement '1' = '0'

leftEvenList :: [[Char]]
leftEvenList = map reverse rightList

-- 前置符的奇偶性推断，根据左侧6个字符的奇偶性推断出来
parityList :: [[Char]]
parityList =
  [ "111111",
    "110100",
    "110010",
    "110001",
    "101100",
    "100110",
    "100011",
    "101010",
    "101001",
    "100101"
  ]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- strict left fold Array
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
  where
    go s (j : js) =
      let s' = f s (a ! j)
       in s' `seq` go s' js
    go s _ = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
  where
    products = mapEveryOther (* 3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first : rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where
    (left, right) = splitAt 6 rest
    lefties = zipWith leftEncode (parityCodes ! first) left
    righties = map rightEncode (right ++ [checkDigit s])
encodeDigits [] = []

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard :: [Char]
outerGuard = "101"

centerGuard :: [Char]
centerGuard = "01010"

type Pixel = Word8

type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
  parseWhileWith word2char (/= '\n')
    >>= \header ->
      skipSpaces
        >> assert (header == "p6") "invalid raw header"
        >> parseNat
          >>= \width ->
            skipSpaces >> parseNat
              >>= \height ->
                skipSpaces >> parseNat
                  >>= \maxValue ->
                    assert (maxValue == 255) "max value out of spec"
                      >> parseByte
                      >> parseTimes (width * height) parseRGB
                      >>= \pxs -> identity (listArray ((0, 0), (width - 1, height - 1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte >>= \r -> parseByte >>= \g -> parseByte >>= \b -> identity (r, g, b)

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r, g, b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b

type Greymap = Array (Int, Int) Pixel

pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

data Bit = Zero | One deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
  where
    binary i
      | i < pivot = Zero
      | otherwise = One
    pivot = round $ least + (greatest - least) * n
    least = fromIntegral $ choose (<) a
    greatest = fromIntegral $ choose (>) a
    choose f = foldA1 $ \x y -> if f x y then x else y

type Run = Int

type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
  where
    rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength
