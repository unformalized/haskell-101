{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

-- | cat talk
module CatTalks.ProbabilityMonad where

import Data.List
import System.Random (randomIO)

newtype Dist a = Dist [(a, Float)]

instance Show a => Show (Dist a) where
  show (Dist xs) = "Dist " ++ show xs

instance Functor Dist where
  fmap :: (a -> b) -> Dist a -> Dist b
  fmap f (Dist xs) = Dist $ map (\(a, p) -> (f a, p)) xs

instance Applicative Dist where
  pure a = certainly a

  (<*>) :: Dist (a -> b) -> Dist a -> Dist b
  (<*>) (Dist fs) (Dist as) = Dist bs
    where
      bs = concatMap (\(f, fp) -> map (\(a, ap) -> (f a, ap * fp)) as) fs

instance Monad Dist where
  (>>=) :: Dist a -> (a -> Dist b) -> Dist b
  (Dist as) >>= f = Dist $ concatMap (\(a, p) -> mult p $ f a) as
    where
      mult p (Dist bs) = map (\(b, q) -> (b, p * q)) bs

certainly :: a -> Dist a
certainly x = Dist [(x, 1.0)]

uniform :: [a] -> Dist a
uniform xs =
  let p = 1.0 / fromIntegral (length xs)
   in Dist (map (,p) xs)

dice :: Dist Int
dice = uniform [1, 2, 3, 4, 5, 6]

coin :: Dist Bool
coin = fmap (<= 3) dice

-- normalize ensure the probobility sum up to 1.0
normalize :: Dist a -> Dist a
normalize (Dist xs) = Dist $ normalizedProb xs
  where
    normalizedProb xs =
      let totalProb = sum $ map snd xs
       in map (\(a, p) -> (a, p / totalProb)) xs

-- combine the same outcome that appeared multiple times
combine :: Eq a => Dist a -> Dist a
combine (Dist xs) = Dist $ combineProb xs
  where
    combineProb :: Eq a => [(a, Float)] -> [(a, Float)]
    combineProb [] = []
    combineProb xs@((a, _) : _) =
      (a, sum $ map snd $ filter (\(a', _p) -> a == a') xs) : combineProb (filter (\(a', _p) -> a /= a') xs)

-- consume
select :: Float -> Dist a -> a
select r (Dist []) = error "can't select empty distribution"
select r (Dist ((a, p) : xs))
  | r <= p = a
  | otherwise = select (r - p) (Dist xs)

sample :: Eq a => Dist a -> IO a
sample dist = do
  p <- (randomIO :: IO Float)
  return (select p $ normalize dist)

dice2 :: Dist Int
dice2 = combine $ normalize $ fmap (+) dice <*> dice

data Outcome = Win | Lose
  deriving (Eq, Show)

firstChoice :: Dist Outcome
firstChoice = uniform [Win, Lose, Lose]

switchDoor :: Outcome -> Dist Outcome
switchDoor Win = certainly Lose
switchDoor Lose = certainly Win

choiceAndNotSwitch :: Dist Outcome
choiceAndNotSwitch = combine $ normalize firstChoice

choiceAndSwitch :: Dist Outcome
choiceAndSwitch = combine $ normalize $ (firstChoice >>= switchDoor)

data Door = A | B | C deriving (Show, Eq)

data State = State
  { prize :: Door,
    chosen :: Door,
    opened :: Door
  }
  deriving (Show, Eq)

start :: Dist State
start = certainly $ State {prize = undefined, chosen = undefined, opened = undefined}

hide :: State -> Dist State
hide s = uniform [s {prize = d} | d <- [A, B, C]]

choose :: State -> Dist State
choose s = uniform [s {chosen = d} | d <- [A, B, C]]

open :: State -> Dist State
open s = uniform [s {opened = d} | d <- [A, B, C] \\ [chosen s, prize s]]

switch :: State -> Dist State
switch s = uniform [s {chosen = d} | d <- [A, B, C] \\ [chosen s, opened s]]

eval :: State -> Dist Outcome
eval s
  | chosen s == prize s = certainly Win
  | otherwise = certainly Lose

evalSwitch :: Dist Outcome
evalSwitch = combine $ normalize $ (start >>= hide >>= choose >>= open >>= switch >>= eval)

evalNotSwitch :: Dist Outcome
evalNotSwitch = combine $ normalize $ (start >>= hide >>= choose >>= open >>= eval)
