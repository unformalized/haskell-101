module Intermediate.SystemTime where

import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import System.Locale (  )

printTime :: IO String
printTime = do
  curTime <- getCurrentTime
  print curTime
  let fmtT = formatTime defaultTimeLocale "%Y-%m-%d" curTime
  return fmtT

