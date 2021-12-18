{-# LANGUAGE ScopedTypeVariables #-}

-- |
module RealWorld.SearchSystem.BetterPredicate where

import Control.Exception (bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime (..))
import GHC.IO.Exception (IOException (IOError))
import RealWorld.SearchSystem.RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile)

type InfoP a =
  FilePath ->
  Permissions ->
  Maybe Integer ->
  UTCTime ->
  a

type Predicate = InfoP Bool

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
-- 存在危险，当 openFile 后续操作失败时，并不会释放句柄资源，只有当Haskell运行垃圾回收时，文件句柄才能自动回收
saferFileSize path = handle (\(_ :: IOError) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

-- 使用 bracket 函数，
-- bracket :: (IO a) (a -> IO b) (a -> IO c)
-- 三个参数：请求资源 - 释放资源 - 使用资源

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: IOError) -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

-- 只需要 size 还是要写大量的 _
myTest :: Predicate
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072

pathP :: InfoP FilePath
pathP path _ _ _ = path

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

-- 只需要 size 判断时

findBySize :: Integer -> FilePath -> IO [FilePath]
findBySize s = betterFind (equalP sizeP s)

-- lift

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op f k w x y z = f w x y z `op` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
