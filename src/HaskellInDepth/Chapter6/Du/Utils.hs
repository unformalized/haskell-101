{-# LANGUAGE NamedFieldPuns #-}

module HaskellInDepth.Chapter6.Du.Utils where

import Control.Monad.RWS (MonadIO (liftIO), MonadReader (ask, local), asks)
import Data.Foldable (traverse_)
import HaskellInDepth.Chapter6.Du.AppRWST (MyApp)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig (..), AppEnv (..))
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import System.PosixCompat (FileStatus)

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
  AppEnv {fileStatus, path} <- ask
  liftIO $ fileStatus path

traverseDirectoryWith :: MyApp log s () -> MyApp log s ()
traverseDirectoryWith app = do
  curPath <- asks path
  content <- liftIO $ listDirectory curPath
  traverse_ go content
  where
    go name = flip local app $
      \env ->
        env
          { path = path env </> name,
            depth = depth env + 1
          }

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension config filepath = maybe True (`isExtensionOf` filepath) (extension config)
