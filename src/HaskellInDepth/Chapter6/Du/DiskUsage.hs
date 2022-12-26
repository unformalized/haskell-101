{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HaskellInDepth.Chapter6.Du.DiskUsage where

import Control.Monad (liftM2, when)
import Control.Monad.RWS (ask, get, modify, tell)
import HaskellInDepth.Chapter6.Du.AppRWST (MyApp)
import HaskellInDepth.Chapter6.Du.AppTypes (AppConfig (..), AppEnv (..))
import HaskellInDepth.Chapter6.Du.Utils (checkExtension, currentPathStatus, traverseDirectoryWith)
import System.PosixCompat (FileOffset, FileStatus, fileSize, isDirectory, isRegularFile)

data DiskUsageEntryAction
  = TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide :: AppEnv -> FileStatus -> DiskUsageEntryAction
    decide AppEnv {..} fs
      | isDirectory fs = TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path = RecordFileSize (fileSize fs)
      | otherwise = None

    processEntry :: DiskUsageEntryAction -> MyApp (FilePath, FileOffset) FileOffset ()
    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+ fsize)
    processEntry _ = pure ()