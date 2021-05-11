{-# LANGUAGE NumDecimals #-}

module Base.Typeclass where

import Data.Complex
import Data.Ratio

import Numeric.Natural

import Data.Text
import Numeric.IEEE
import Data.Number.CReal

import Text.Show.Functions

-- Eq
-- Ord
-- Enum

enuma1 = [1..100]

enuma2 = 1
enuma3 = succ enuma2



-- Bounded

bound1 :: Bool
bound1 = maxBound

bound2 :: Int
bound2 = maxBound

boundMaxChar :: Char
boundMaxChar = maxBound


-- 实用的数类型函数

complex1 :: Complex Double
complex1 = 5

complex2 :: Ratio Int
complex2 = 4

toInteger' :: Integral  a => a -> Integer
toInteger' = toInteger

infi = isInfinite (1 / 0)

aa1 :: Double
aa1 = nan

aa2 :: Double
aa2 = epsilon

piString :: String
piString = showCReal 50 pi

e1 = 3.1456e8

-- Show 类型


