module HaskellInDepth.Chapter1.Vocab1 where

import Data.Char (toLower)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort (x : xs) = smaller ++ [x] ++ bigger
  where
    smaller = filter (< x) xs
    bigger = filter (>= x) xs

filepath :: String
filepath = "data/texts/hamlet.txt"

printHamletVocabulary :: IO ()
printHamletVocabulary = do
  text <- readFile filepath
  let wordGroup = sortByCount $ map buildEntry  $ group $ sort $ (=<<) words $ lines $ map toLower text
  print (take 10 wordGroup)
  putStrLn ("total: " ++ show (sum (map snd wordGroup)))
  where
    buildEntry xs@(x:_) = (x, length xs)
    sortByCount = sortBy (comparing $ Down . snd)
