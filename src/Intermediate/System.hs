module Intermediate.System where

-- 系统编程
-- 文件目录操作相关API
import System.Directory
  ( createDirectory,
    findFile,
    renameFile,
    removeFile,
    copyFile
  )

-- 系统进程的相关API 对应书籍 11.6.2
import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import GHC.IO.Handle
import System.IO


cammand1 :: IO ()
cammand1 = do
  (Nothing, Nothing, Nothing, d) <- createProcess (proc "ls" [])
  return ()

cammand2 :: IO ()
cammand2 = do
  (Nothing, Just b, Nothing, d) <- createProcess (proc "ls" []) { std_out = CreatePipe }
  l <- hGetContents b
  print (lines l)
  
