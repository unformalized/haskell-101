{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module HaskellInDepth.Chapter6.Du.AppRTWTST where

import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT))
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig, AppEnv, initialEnv)

newtype MyApp logEntry state a = MyApp
  { runApp :: ReaderT AppEnv (WriterT [logEntry] (StateT state IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppEnv,
      MonadWriter [logEntry],
      MonadState state
    )

runMyApp :: MyApp logEntry state a -> AppConfig -> state -> IO (a, [logEntry])
runMyApp app config st =
  evalStateT (runWriterT (runReaderT (runApp app) (initialEnv config))) st
