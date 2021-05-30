{-# LANGUAGE OverloadedStrings #-}

module Intermediate.StarDict (searchDict) where

import Data.Bits (shiftL)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC (putStrLn)
import Data.List (foldl')
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encode (decodeUtf8)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO
    ( openFile,
      IOMode(ReadMode),
      utf8,
      hClose,
      hSeek,
      hSetEncoding,
      SeekMode(AbsoluteSeek) )
import Text.Printf ( printf )

data WordIdx = WordIdx
  { word :: T.Text
  , offset :: Int
  , expLen :: Int
  } deriving (Show)

searchWord :: T.Text -> [WordIdx] -> Maybe WordIdx
searchWord str [] = Nothing
searchWord str xs
  | wrd < str = searchWord str behind
  | wrd > str = searchWord str front
  | otherwise = Just b
  where
    (front, b:behind) = splitAt (length xs `div` 2) xs
    wrd = T.toLower (word b)

getIndexList :: DB.ByteString -> [WordIdx]
getIndexList "" = []
getIndexList str = WordIdx w (byteToInt o) (byteToInt e) : getIndexList left
  where
    w = Encode.decodeUtf8 $ DB.takeWhile (/= 0) str
    o = DB.unpack $ DB.take 4 (DB.drop 1 off)
    e = DB.unpack $ DB.take 4 (DB.drop 5 off)
    off = DB.dropWhile (/= 0) str
    left = DB.drop 9 off

byteToInt :: [Word8] -> Int
byteToInt = foldl' (\x y -> shiftL (fromIntegral x) 8 + fromIntegral y) 0

searchDict :: IO ()
searchDict = do
  args <- getArgs
  case args of
    (wordName:idxFile:dictFile:_) -> do
      idctIdx <- DB.readFile dictFile
      let is = getIndexList idctIdx
      let result = searchWord (fromString wordName) is
      case result of
        Nothing -> printf "Word \"%s\"e not found" wordName
        Just wrd -> do
          inh <- openFile dictFile ReadMode
          hSeek inh AbsoluteSeek (toInteger $ offset wrd)
          hSetEncoding inh utf8
          explanation <- DB.hGet inh (expLen wrd)
          hClose inh
          DBC.putStrLn explanation
    [_] -> print "Usage: Dict <word> <idxFile> <dictFile>"
