module HaskellInDepth.Chapter1.Vocab1 where

import Data.Char (toLower)
import Data.List (group, sortBy)

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
  -- 程序逻辑不正确
  let wordGroup = map head $ sortByCount $ group $ quickSort $ words $ map toLower text
  print (take 7 wordGroup)
  where
    sortByCount :: [[a]] -> [[a]]
    sortByCount = sortBy (\as bs -> if length bs > length as then GT else LT)
