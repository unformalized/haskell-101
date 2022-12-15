module HaskellInDepth.Chapter5.IORef where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
  nRef <- newIORef 0
  go nRef
  where
    readNumber :: IO (Maybe Int)
    readNumber = do
      putStr "Put integer number (not a number to finish)"
      readMaybe <$> getLine
    go ref = readNumber >>= processNumber ref

    processNumber :: IORef Int -> Maybe Int -> IO Int
    processNumber ref Nothing = readIORef ref
    processNumber ref (Just n) = modifyIORef' ref (+ n) >> go ref

startIORefCounter :: IO ()
startIORefCounter = do
  n <- sumNumbers
  putStr "Your sum is: "
  print n
