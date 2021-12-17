module RealWorld.FileIO.HighestClose where

import qualified Data.ByteString.Lazy.Char8 as L

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!! 4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str =
  case L.readInt str of
    Nothing -> Nothing
    Just (dollars, rest) ->
      case L.readInt (L.tail rest) of
        Just (cents, more) ->
          Just (dollars * 100 + cents)
        Nothing -> Nothing

highClose :: L.ByteString -> Maybe Int
highClose = maximum . (Nothing :) . map closing . L.lines

highestCloseFrom :: IO ()
highestCloseFrom = do
  contents <- L.readFile "./temp/prices.csv"
  print (highClose contents)
