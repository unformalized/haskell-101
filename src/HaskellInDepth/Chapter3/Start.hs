{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module HaskellInDepth.Chapter3.Start where

import Control.Applicative
import Control.Monad (unless, when)
import Data.ByteString ()
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord, Header, decodeByName)
import Data.Text (unpack)
import Data.Vector (Vector)
import HaskellInDepth.Chapter3.Charts (plotChart)
import HaskellInDepth.Chapter3.HtmlReport (htmlReport)
import HaskellInDepth.Chapter3.Params (Params (..), cmdLineParser)
import HaskellInDepth.Chapter3.QuoteData (QuoteData (QuoteData))
import HaskellInDepth.Chapter3.StatReport (statInfo, textReport)
import System.FilePath (dropDrive, isAbsolute, isRelative, joinPath, makeRelative, splitDirectories)
import System.Path.NameManip (absolute_path)

start :: IO ()
start = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReport params quotes

generateReport :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReport Params {..} quotes = do
  unless silent $ putStrLn textRpt
  when chart $ plotChart title quotes chartFName
  case htmlFile of
    Nothing -> pure ()
    (Just f) -> do
      relativeChartPath <- relativeTo f chartFName
      BL.writeFile f (htmlRpt relativeChartPath)
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt chartPath = htmlReport title quotes statInfo' [chartPath | chart]

    withCompany prefix = maybe mempty (prefix <>) company
    chartFName = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "

relativeTo :: FilePath -> FilePath -> IO FilePath
relativeTo path1 path2 = do
  absolutePath1 <- absolute_path path1
  absolutePath2 <- absolute_path path2
  return (makeRelative absolutePath1 absolutePath2)
