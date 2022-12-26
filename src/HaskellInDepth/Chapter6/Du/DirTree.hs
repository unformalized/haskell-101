{-# LANGUAGE RecordWildCards #-}

module HaskellInDepth.Chapter6.Du.DirTree where

import Control.Monad (when)
import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell))
import HaskellInDepth.Chapter6.Du.AppRWST (MyApp)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig (maxDepth), AppEnv (..))
import HaskellInDepth.Chapter6.Du.Utils (currentPathStatus, traverseDirectoryWith)
import System.FilePath (takeBaseName)
import System.PosixCompat (isDirectory)
import TextShow (Builder, fromString)

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    tell [(takeBaseName path, depth)]
    traverseDirectoryWith dirTree

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2 * n) ' '
