module RealWorld.Network.TcpServer where

import Data.Bits (Bits (..))
import Data.List (foldl')
import GHC.Word (Word32, Word8)
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    defaultProtocol,
    listen,
    socket,
    socketToHandle,
  )
import System.IO (IOMode (ReadWriteMode), hClose, hGetLine, hPutStrLn)

fromOctets :: [Word8] -> Word32
fromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

w2i :: [Word8] -> Int
w2i = undefined

startTCPServer :: IO ()
startTCPServer = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4000 0)
  listen sock 2
  putStrLn "Listening on port 4000..."
  lookForever sock

lookForever :: Socket -> IO ()
lookForever sock = do
  (conn, _) <- accept sock
  handleSock <- socketToHandle conn ReadWriteMode

  line <- hGetLine handleSock
  putStrLn $ "Request received: " ++ line

  hPutStrLn handleSock "Hey, client"
  hClose handleSock
  lookForever sock
