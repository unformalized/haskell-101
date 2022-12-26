{-# LANGUAGE RecordWildCards #-}

module HaskellInDepth.Chapter6.Du.AppTypes where

import System.PosixCompat (FileStatus, getFileStatus, getSymbolicLinkStatus)

data AppConfig = AppConfig
  { basePath :: FilePath,
    maxDepth :: Int,
    extension :: Maybe String,
    followSymlinks :: Bool
  }

data AppEnv = AppEnv
  { cfg :: AppConfig,
    path :: FilePath,
    depth :: Int,
    fileStatus :: FilePath -> IO FileStatus
  }

initialEnv :: AppConfig -> AppEnv
initialEnv config@AppConfig {..} =
  AppEnv
    { cfg = config,
      path = basePath,
      depth = 0,
      fileStatus = if followSymlinks then getFileStatus else getSymbolicLinkStatus
    }
