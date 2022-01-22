module HaskellInDepth.Chapter1.Vocab4 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Data.Ord ( comparing, Down )

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab = undefined

allWordsReport :: Vocabulary -> Text
allWordsReport = undefined

wordsCountReport :: Vocabulary -> Text
wordsCountReport = undefined

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport = undefined

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile = undefined

printHamletVocabulary4 :: IO ()
printHamletVocabulary4 = undefined

allWords :: Vocabulary -> [Text]
allWords = undefined

wordsCount :: Vocabulary -> (Int, Int)
wordsCount = undefined

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)



