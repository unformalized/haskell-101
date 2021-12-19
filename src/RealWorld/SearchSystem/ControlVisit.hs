module RealWorld.SearchSystem.ControlVisit where

import Control.Exception (bracket, handle)
import Control.Monad (filterM, forM, liftM)
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
getInfo = undefined

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`elem` [".", ".."]) names)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $
    forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
