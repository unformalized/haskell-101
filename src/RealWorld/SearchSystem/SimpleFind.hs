-- |

module RealWorld.SearchSystem.SimpleFind where

import RealWorld.SearchSystem.RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  paths <- getRecursiveContents path
  return (filter p paths)
