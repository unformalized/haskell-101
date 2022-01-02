module RealWorld.SearchSystem.FindCommand where

import Control.Monad (filterM)
import Data.Foldable (and)
import RealWorld.Reg.GlobRegex (matchesGlob)
import RealWorld.SearchSystem.ControlVisit (Info (Info, infoPath), isDirectory)
import RealWorld.SearchSystem.FoldDir (Iterate (Continue), Iterator, foldTree)

data OptionsType = Dir | File

data FindOptions
  = Name String -- glob 形式的匹配符
  | IgnoreName String -- 排除符合的文件或
  | FindType OptionsType -- 类型

find :: FilePath -> [FindOptions] -> IO [Info]
find path options = foldTree (convertOptions2Iterator options) [] path

convertOptions2Iterator :: [FindOptions] -> Iterator [Info]
convertOptions2Iterator options res info =
  if isPass then Continue (info : res) else Continue res
  where
    isPass = all (`options2Predicate` info) options

options2Predicate :: FindOptions -> Info -> Bool
options2Predicate option = case option of
  Name name -> \info -> matchesGlob (infoPath info) name
  IgnoreName name -> \info -> not (matchesGlob (infoPath info) name)
  FindType type' -> \info -> case type' of
    Dir -> isDirectory info
    File -> not (isDirectory info)

testFind :: FilePath -> IO [FilePath]
testFind path = do
  infos <- find path options
  return (map infoPath infos)
  where
    options =
      [ FindType File,
        Name "./src/**/Reg/*.hs"
        -- IgnoreName "./src/**/Glob.hs"
      ]
