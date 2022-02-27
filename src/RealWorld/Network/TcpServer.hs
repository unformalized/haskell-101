module RealWorld.Network.TcpServer where

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
