{-# LANGUAGE RecordWildCards #-}

module HaskellInDepth.Chapter6.Du.FileCount where

import Control.Monad (when)
import Control.Monad.RWS (MonadIO (liftIO), ask, tell)
import HaskellInDepth.Chapter6.Du.AppRWST (MyApp)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig (..), AppEnv (..))
import HaskellInDepth.Chapter6.Du.Utils (checkExtension, currentPathStatus, traverseDirectoryWith)
import System.Directory.Extra (listFiles)
import System.PosixCompat (isDirectory)

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    traverseDirectoryWith fileCount
    files <- liftIO $ listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]
