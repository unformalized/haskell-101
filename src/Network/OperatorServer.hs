-- |
module Network.OperatorServer where

import Data.BaseInt (itow8, w2c, w2i, w8toi)
import qualified Data.Binary as B
import qualified Data.ByteString as BL
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.C (CInt)
import GHC.IO.Handle (hClose)
import GHC.IO.Handle.FD (stdout)
import GHC.IO.IOMode (IOMode (..))
import Network.Socket
  ( Family (..),
    HostAddress,
    PortNumber,
    ProtocolNumber,
    SockAddr (..),
    SocketType (..),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    socket,
    socketToHandle,
    tupleToHostAddress,
    withFdSocket,
  )

-- 127.0.0.1
hostAddr :: HostAddress
hostAddr = tupleToHostAddress (0x7f, 0, 0, 1)

opCountSize :: Int
opCountSize = 1

opValueSize :: Int
opValueSize = 4

opSize :: Int
opSize = 1

operateServer :: IO ()
operateServer = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 9190 hostAddr)
  listen sock 5
  (client_sock, _) <- accept sock
  putStrLn "Connectted client"
  handle <- socketToHandle client_sock ReadWriteMode
  opCountBS <- BL.hGet handle opCountSize
  print (BL.unpack opCountBS)
  let opCount = bsToInt opCountBS
  opValuesBS <- BL.hGet handle (opValueSize * opCount)
  print (BL.unpack opValuesBS)
  let opValues = map w8toi $ chunk opValueSize $ BL.unpack opValuesBS
  opBS <- BL.hGet handle opSize
  print (BL.unpack opBS)
  let op = w2c $ BL.head opBS
  let result = calc op opValues
  print result
  BL.hPutStr handle (BL.pack $ itow8 result)
  hClose handle
  close client_sock
  close sock
  return ()

chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs < n = [xs]
  | otherwise = take n xs : chunk n (drop n xs)

bsToInt :: BL.ByteString -> Int
bsToInt = w8toi . BL.unpack

calc :: Char -> [Int] -> Int
calc '*' = product
calc '+' = sum
calc '-' = foldl1 (-)
calc _ = const 0
