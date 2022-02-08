{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellInDepth.Chapter2.Random where

import Control.Monad (replicateM, unless)
import Control.Monad.Cont (when)
import Data.List (nub, sort)
import HaskellInDepth.Chapter2.Radar (Direction, Turn, every, orient, orientMany, rotateMany, rotateMany', rotateManySteps)
import System.Exit (exitFailure)
import System.Random (Random, Uniform, uniform)
import System.Random.Stateful (Uniform (uniformM), UniformRange (uniformRM), getStdRandom)

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

deriving instance Random Direction

instance UniformRange Direction where
  uniformRM (low, high) range = do
    res <- uniformRM (fromEnum low :: Int, fromEnum high :: Int) range
    pure $ toEnum res

instance Uniform Direction where
  uniformM gen = uniformRM (minBound, maxBound) gen

deriving instance Random Turn

instance UniformRange Turn where
  uniformRM (low, high) range = do
    res <- uniformRM (fromEnum low :: Int, fromEnum high :: Int) range
    pure $ toEnum res

instance Uniform Turn where
  uniformM gen = uniformRM (minBound, maxBound) gen

-- random lists of directions and turns

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns = uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

writeRandomFile :: (Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n gen filename = do
  xs <- gen n
  writeFile filename $ unlines $ map show xs

writeTurns :: Int -> IO ()
writeTurns n = writeRandomFile n randomTurns "data/turns.txt"

writeDirections :: Int -> IO ()
writeDirections n = writeRandomFile n randomDirections "data/dirs.txt"

test_allTurnsInUse :: Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts = and [rotateMany d ts == rotateMany' d ts | d <- every]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d : _) = ds == rotateManySteps d (orientMany ds)

test_all :: IO ()
test_all = do
  ts <- randomTurns 1000
  ds <- randomDirections 1000
  unless (test_allTurnsInUse && test_orientRotateAgree ds && test_rotationsMonoidAgree ts) exitFailure
