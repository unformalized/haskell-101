{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module HaskellInDepth.Chapter3.Start where

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
  saveHtml htmlFile htmlRpt
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt = htmlReport title quotes statInfo' [chartFName | chart]

    withCompany prefix = maybe mempty (prefix <>) company
    chartFName = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html
