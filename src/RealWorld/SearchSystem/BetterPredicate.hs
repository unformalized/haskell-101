{-# LANGUAGE ScopedTypeVariables #-}
-- |
module RealWorld.SearchSystem.BetterPredicate where

import Control.Exception (bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime(..))
import RealWorld.SearchSystem.RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import GHC.IO.Exception (IOException(IOError))

type Predicate =
  FilePath ->
  Permissions ->
  Maybe Integer ->
  UTCTime ->
  Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize = undefined

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      prems <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name prems size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(_ :: IOError) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
