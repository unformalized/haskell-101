{-# LANGUAGE ScopedTypeVariables #-}

module Intermediate.Random where

import System.Random
    ( newStdGen, mkStdGen, Random(randoms, randomRs, random) )
import Control.Monad.ST (ST)
import System.Random.MWC (GenST,  withSystemRandom, uniformVector, uniform, asGenST )
import Control.DeepSeq ( force )
import qualified Data.Vector.Unboxed as U
import System.Random.Internal

ran1 :: Int
ran1 = fst $ random (mkStdGen 2021)

rollDice :: Int -> IO ()
rollDice n = do
  gen <- newStdGen
  print $ take n (randomRs ((1, 6) :: (Int, Int)) gen)

random' :: Int -> IO (U.Vector Double)
random' n = do
  U.fromList . take n . randoms <$> newStdGen


random'' :: (Uniform a, U.Unbox a) => Int -> IO (U.Vector a)
random'' n = do
  vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen n
  return $ U.force vs


getPiv :: IO Double
getPiv = do
  xs <- random' 6000
  ys <- random' 6000
  let isIn = U.zipWith (\x y -> if (x - 0.5)^2 + (y - 0.5)^2 <= 0.25 then 1.0 else 0) xs ys
  return $ 4.0 * force (U.sum isIn) / 6000

testPI :: IO ()
testPI = do
  ps <- U.forM (U.fromList [1..2000 :: Int]) (const getPiv)
  print $ force $ U.sum ps / 2000
