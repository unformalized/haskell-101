{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.Chapter1.Vocab4 where

import Control.Monad (when)
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt (blockListF, blockListF', fmt, nameF, unlinesF, (+|), (|+))
import HaskellInDepth.Chapter1.Vocab3 (Vocabulary, extractVocab)

allWordsReport :: Vocabulary -> Text
allWordsReport vocab = fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab =
  fmt $ "Total number of words: " +| total |+ "\nNumber of unique words: " +| unique |+ "\n"
  where
    (total, unique) = wordsCount vocab

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num = fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile filename withAllWords n = do
  text <- TIO.readFile filename
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

printHamletVocabulary4 :: Int -> IO ()
printHamletVocabulary4 = processTextFile "data/texts/hamlet.txt" False

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (sum (map snd vocab), length vocab)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)
