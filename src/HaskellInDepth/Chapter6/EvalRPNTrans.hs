module HaskellInDepth.Chapter6.EvalRPNTrans (evalPRNTrans) where

import Control.Applicative (Alternative, empty)
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    guard,
    modify,
    when,
  )
import Data.Foldable (traverse_)
import Text.Read (readMaybe)

type Stack = [Integer]

type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x :)

pop :: EvalM Integer
pop = do
  xs <- get
  -- (x:xs) <- get 使用模式匹配也可以检测到错误，当在 do 语句中时，若是发生模式匹配错误，则会调用 MonadFail.fail
  when (null xs) (lift Nothing)
  -- guard (not $ null xs) 或者使用 guard 保证 xs 不为空
  put (tail xs)
  pure (head xs)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  len <- length <$> get
  guard (len == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str = maybe empty pure $ readMaybe str

evalPRNTrans :: String -> Maybe Integer
evalPRNTrans str = evalStateT evalRPN' []
  where
    processTops op = flip op <$> pop <*> pop >>= push
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step t = readSafe t >>= push
    evalRPN' = traverse_ step (words str) >> oneElementOnStack >> pop
