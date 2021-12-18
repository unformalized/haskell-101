-- |

module RealWorld.SearchSystem.RecursiveContents where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then getRecursiveContents path
      else return [path]
  return (concat paths)
