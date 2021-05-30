module Intermediate.MonadTrans where

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.Maybe as TM
import qualified Control.Monad.IO.Class as IOC
import Data.Char


class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (S.StateT s) where
  lift m = S.StateT $ \s -> do
    a <- m
    return (a, s)

push :: Int -> S.State [Int] ()
push x = S.state $ \xs -> ((), x:xs)

pushMS :: Int -> TM.MaybeT (S.State [Int]) ()
pushMS x = T.lift $ push x

-- lift 定律
-- lift . return = return
-- return :: Monad m => a -> m a ，return 是将 a 提升到 m a，
-- lift (m >>= f) = lift m >>= (lift . f)

-- MonadIO 与 liftIO 
-- IO Monad 没有对应的转换器，不能组合其他 Monad，定义多个 Monad 组合时，IO 一定在组合 Monad 类型的最右侧

class (Monad m) => MoandIO m where
  liftIO :: IO a -> m a

isPasswordValid :: String -> Bool
isPasswordValid s = length s >= 8 && check s
  where
    check :: String -> Bool
    check s = and [f s | f <- map any [isUpper, isLower, isNumber]]

setPassword :: TM.MaybeT (W.WriterT [String] IO) ()
setPassword = do
  IOC.liftIO $ putStrLn "Please set a Password"
  pass <- IOC.liftIO getLine
  R.guard (isPasswordValid pass)
  T.lift $ W.tell [pass]

testPassword :: IO (Maybe (), [String])
testPassword = W.runWriterT $ TM.runMaybeT setPassword

-- MonadBase 和 liftBase




