-- |
module HaskellInDepth.Chapter6.EvalRPN
  ( evalRPN,
  )
where

import Control.Monad.State (MonadState (get, put, state), State, evalState, modify)
import Data.Foldable (traverse_)

type Stack = [Integer]

type EvalM = State Stack

push :: Integer -> EvalM ()
push x = modify (x :)

pop :: EvalM Integer
pop = state (\(x : xs) -> (x, xs))

evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
  where
    processTops :: (Integer -> Integer -> Integer) -> EvalM ()
    processTops op = flip op <$> pop <*> pop >>= push
    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step t = push (read t)
    evalRPN' = traverse_ step (words expr) >> pop

isEmpty :: EvalM Bool
isEmpty = null <$> get

notEmpty :: EvalM Bool
notEmpty = not <$> isEmpty

oneElementOnStack :: EvalM Bool
oneElementOnStack = (\xs -> length xs == 1) <$> get
