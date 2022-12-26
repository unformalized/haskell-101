{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module HaskellInDepth.Chapter6.MyMaybeT where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f mta = MaybeT (fmap f <$> runMaybeT mta)

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure a = MaybeT (pure (Just a))
  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- <*> :: f (a -> b) -> (f a -> f b)
  -- 要将 m (Maybe (a -> b)) 变为 m (Maybe a -> Maybe b)
  (<*>) (MaybeT mf) (MaybeT ma) = MaybeT ((<*>) <$> mf <*> ma)
    where
      maybeF :: Maybe (a -> b) -> Maybe a -> Maybe b
      maybeF mf ma = case mf of
        Nothing -> Nothing
        Just f -> case ma of
          Nothing -> Nothing
          Just a -> Just (f a)

instance Monad m => Monad (MaybeT m) where
  return :: Monad m => a -> MaybeT m a
  return = pure
  (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (>>=) (MaybeT ma) f =
    MaybeT
      ( ma >>= \case
          Nothing -> return Nothing
          Just a -> runMaybeT $ f a
      )

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift ma = MaybeT (fmap Just ma)

instance Monad m => MonadFail (MaybeT m) where
  fail :: String -> MaybeT m a
  fail _ = MaybeT (pure Nothing)

instance Applicative m => Alternative (MaybeT m) where
  empty :: MaybeT m a
  empty = MaybeT (pure Nothing)

  (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  (MaybeT ma) <|> (MaybeT mb) = MaybeT ((<|>) <$> ma <*> mb)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO
