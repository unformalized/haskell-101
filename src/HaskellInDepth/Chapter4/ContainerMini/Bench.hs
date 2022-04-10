module HaskellInDepth.Chapter4.ContainerMini.Bench where

import Data.List (unfoldr)
import HaskellInDepth.Chapter4.ContainerMini.Data.Deque as D
  ( empty,
    front,
    pop_front,
    push_front,
  )
import HaskellInDepth.Chapter4.ContainerMini.Data.Stack as S
  ( empty,
    pop,
    push,
    top,
  )
import System.TimeIt (timeItNamed)

fill :: (Num a, Enum a) => a -> (a -> b -> b) -> b -> b
fill n insert s = foldl (flip insert) s [1 .. n]

sumAll :: Num a => t -> (t -> Maybe a) -> (t -> t) -> a
sumAll s view remove = sum $ unfoldr iter s
  where
    iter s = view s >>= \x -> Just (x, remove s)

simple_stack_bench :: Int -> Int
simple_stack_bench n = sumAll (fill n push S.empty) top pop

simple_deque_bench :: Int -> Int
simple_deque_bench n = sumAll (fill n push_front D.empty) front pop_front

benchmark_test :: IO ()
benchmark_test = do
  let n = 10 ^ 6
  timeItNamed "Stack" $ print $ simple_stack_bench n
  timeItNamed "Deque" $ print $ simple_deque_bench n
