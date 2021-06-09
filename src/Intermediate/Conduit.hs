
module Intermediate.Conduit where

import Data.Void (Void)
import Data.Conduit
import qualified Data.Conduit.List as CL

-- data ConduitM i o m r
-- type Source m o = ConduitM () o m ()
-- type Conduit i m o = ConduitM i o m ()
-- type Sink i m r = ConduitM i Void m r

-- runConduit :: Monad m => ConduitM () Void m r -> m r
-- ($$) :: Monad m => Source m a -> Sink a m b -> m b

l :: Monad m => Source m Int
l = CL.sourceList [1..10]

i :: Monad m => Source m Int
i = CL.sourceList [1..]

alternative :: Monad m => Conduit a m a
alternative = do
  p <- await
  case p of
    Just a -> do
      CL.drop 1
      yield a
      alternative
    Nothing -> return ()

sink1 :: Sink Int IO ()
sink1 = CL.mapM_ print



