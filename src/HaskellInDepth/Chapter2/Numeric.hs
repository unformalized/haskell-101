module HaskellInDepth.Chapter2.Numeric where

import Data.Fixed (Fixed, HasResolution (resolution))

circleArea :: Double -> Double
circleArea r = pi * r * r

-- more generic

circleArea2 :: (Real a, Floating b) => a -> b
circleArea2 r = pi * realToFrac (r * r)

-- HasResolution 固定精度类型类
data E4

instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4
