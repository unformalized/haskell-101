{-# LANGUAGE FunctionalDependencies #-}

module HaskellInDepth.Chapter5.ReaderMonad where

class Monad m => MonadReadr r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
