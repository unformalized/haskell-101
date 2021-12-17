module RealWorld.Reg.Glob
  ( namesMatching,
  )
where

import Control.Exception (handle)
import Control.Monad (forM, liftM2)
import RealWorld.Reg.GlobRegex (matchesGlob)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesFileExist pat
    return ([pat | exists])
  | otherwise = do
    case splitFileName pat of
      ("", basename) -> do
        curDir <- getCurrentDirectory
        listMatches curDir basename
      (dirName, basename) -> do
        dirs <-
          if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
        let listDir =
              if isPattern basename
                then listMatches
                else listPlain
        pathNames <- forM dirs $ \dir -> do
          baseNames <- listDir dir basename
          return (map (dir </>) baseNames)
        return (concat pathNames)

doesNameExist :: String -> IO Bool
doesNameExist = liftM2 (liftM2 (&&)) doesFileExist doesDirectoryExist

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  handle matchesDef $ do
    names <- getDirectoryContents dirName'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')

matchesDef :: IOError -> IO [String]
matchesDef = const (return [])

isHidden :: String -> Bool
isHidden ('.' : _) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null dirName
      then doesDirectoryExist dirName
      else doesFileExist (dirName </> baseName)
  return ([baseName | exists])
