module Intermediate.Stack where

import Control.Monad.State ( MonadState(state), State )

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

peek :: State Stack Int
peek = state $ \xs@(x:_) -> (x, xs)

push :: Int -> State Stack ()
push i = state $ \xs -> ((), i:xs)

testStack :: State Stack ()
testStack =
  push 5 >>
    pop >>= \a ->
      push (a + 5)



