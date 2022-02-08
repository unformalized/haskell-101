{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellInDepth.Chapter2.Radar where

import Fmt (Buildable (build), fmt, fmtLn, nameF, unlinesF, unwordsF, (+||), (||+))

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d
  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show)

instance Semigroup Turn where
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TAround = TLeft
  TRight <> TRight = TAround
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty = TNone
  mappend = (<>)

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

deriving instance Read Direction

deriving instance Read Turn

deriving instance Ord Turn

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

-- 优势：当 rotate 计算量较大时，可以明显减少计算量
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany ds@(_ : _ : _) = zipWith orient ds (tail ds)
orientMany _ = []

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir filename = do
  f <- readFile filename
  let turns = map read $ lines f
      finalDir = rotateMany' dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "FinalDirection: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile filename = do
  f <- readFile filename
  let dirs = map read $ lines f
      turns = orientMany dirs
  fmt $ nameF "All Turns" (unwordsF turns)
