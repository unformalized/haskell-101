-- |
module HaskellInDepth.Chapter5.FileCounter (startFileCount) where

-- word: ubiquitous 无处不在的

import Control.Monad.Extra (ifM, whenM, zipWithM_)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.Directory.Extra (doesDirectoryExist, listContents, listFilesRecursive)
import System.Environment (getArgs)

fileCount :: FilePath -> IO Int
fileCount filepath = do
  fcRef <- newIORef 0
  whenM (doesDirectoryExist filepath) $ go fcRef filepath
  readIORef fcRef
  where
    inc :: IORef Int -> IO ()
    inc ref = modifyIORef' ref (+ 1)
    go ref filepath = listContents filepath >>= traverse_ (processEntry ref)
    processEntry ref filepath = ifM (doesDirectoryExist filepath) (go ref filepath) (inc ref)

fileCount' :: FilePath -> IO Int
fileCount' fp = length <$> listFilesRecursive fp

startFileCount :: IO ()
startFileCount = do
  args <- getArgs
  xs <- traverse fileCount args
  zipWithM_ printEntry args xs
  where
    printEntry filepath n = putStrLn (show n ++ "\t" ++ filepath)
