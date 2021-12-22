{-# LANGUAGE ScopedTypeVariables #-}

module RealWorld.SearchSystem.ControlVisit where

import Control.Exception (bracket, handle)
import Control.Monad (filterM, forM, liftM)
import Data.List (sort, sortBy)
import Data.Time.Clock (UTCTime (..))
import GHC.IO.Exception (IOException (IOError))
import RealWorld.SearchSystem.RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getDirectoryContents, getModificationTime, getPermissions)
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import Prelude hiding (traverse)

data Info = Info
  { infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $
    forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

traversePostOrder :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traversePostOrder order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (map (path </>) names ++ [path])
  liftM concat $
    forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_ :: IOError) -> return Nothing) (Just `fmap` act)

traverseByRevAlpha :: FilePath -> IO [FilePath]
traverseByRevAlpha path = do
  infos <- traverse trans path
  return (map infoPath infos)
  where
    trans :: [Info] -> [Info]
    trans = sortBy (\info1 info2 -> compare (infoPath info2) (infoPath info1))

-- Info combinator
type InfoP a = Info -> Maybe a

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op f b info = case f info of
  Nothing -> Nothing
  Just a -> Just (op a b)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 op f g info = case f info of
  Just a -> case g info of
    Just b -> Just (op a b)
    _ -> Nothing
  _ -> Nothing

sizeP :: InfoP Integer
sizeP = infoSize

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = liftP (==)

constP :: a -> InfoP a
constP a _ = Just a

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
