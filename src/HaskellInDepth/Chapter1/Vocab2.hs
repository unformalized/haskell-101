module HaskellInDepth.Chapter1.Vocab2 where

import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

printHamletVocabulary2 :: IO ()
printHamletVocabulary2 = do
  [filename] <- getArgs
  text <- TIO.readFile filename
  let ws = map head $ group $ sort $ map T.toCaseFold $ filter (not . T.null) $ map (T.dropAround $ not . isLetter) $ T.words text
  TIO.putStrLn $ T.unwords ws
  print $ length ws
