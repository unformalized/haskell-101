module RealWorld.FileIO.FileIO1 where

import Data.Char (toUpper)
import System.Directory ()
import System.IO
  ( IOMode (ReadMode, WriteMode),
    hClose,
    hGetLine,
    hIsEOF,
    hPutStrLn,
    openFile,
  )

type Filepath = String

copyFileAndToUpper :: IO ()
copyFileAndToUpper = do
  inh <- openFile "temp/input.txt" ReadMode
  outh <- openFile "temp/output.txt" WriteMode
  loop inh outh
  hClose inh
  hClose outh
  where
    loop inh outh = do
      ineof <- hIsEOF inh
      if ineof
        then return ()
        else do
          inStr <- hGetLine inh
          hPutStrLn outh (map toUpper inStr)
          loop inh outh

-- Seek and Tell
