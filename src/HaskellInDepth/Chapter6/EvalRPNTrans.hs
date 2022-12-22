module HaskellInDepth.Chapter6.EvalRPNTrans where

import Control.Monad.State (StateT)

type Stack = [Integer]

type EvalM = StateT Stack Maybe
