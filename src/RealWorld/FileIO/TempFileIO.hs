module RealWorld.FileIO.TempFileIO where

import Control.Exception
import System.Directory
import System.IO
import System.IO.Error

doMyAction :: IO ()
doMyAction = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = do
  putStrLn "Welcome to tempfile.hs"
  putStrLn $ "I have a temporary file at " ++ tempname

  pos <- hTell temph
  putStrLn $ "My initial position is " ++ show pos

  let tempData = show [1 .. 10]
  putStrLn $
    "Writing one line containing"
      ++ show (length tempData)
      ++ " bytes: "
      ++ tempData
  hPutStrLn temph tempData

  pos <- hTell temph
  putStrLn $ "After writing, my new position is " ++ show pos

  putStrLn "The file content is: "

  hSeek temph AbsoluteSeek 0

  c <- hGetContents temph
  putStrLn c
  putStrLn "Which cound be expressed as the haskell literal:"
  print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
  tempdir <- catchIOError getTemporaryDirectory (\_ -> return "./temp/")
  (tempfile, temph) <- openTempFile tempdir pattern
  finally
    (func tempfile temph)
    ( do
        hClose temph
        removeFile tempfile
    )
