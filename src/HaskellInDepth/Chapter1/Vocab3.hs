module HaskellInDepth.Chapter1.Vocab3 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isLetter)
import Data.List (group, sort)

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x:_) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

printWordCount :: Vocabulary -> IO ()
printWordCount vocab = do
  putStrLn ("vocabulary count: " ++ length vocab)

processTextFile :: FilePath -> IO ()
processTextFile filename = do
  text <- TIO.readFile filename
  let vocab = extractVocab text
  printAllWords vocab

printFrequentWords :: Vocabulary -> Int -> IO ()
printFrequentWords = undefined

printHamletVocabulary3 :: IO ()
printHamletVocabulary3 = processTextFile "data/texts/hamlet.txt"
