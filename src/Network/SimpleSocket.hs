-- |
module Network.SimpleSocket where

import Data.BaseInt (w2i, w8toi, w8tow32)
import qualified Data.Binary as B
import qualified Data.Binary as Bin
import Data.Bits (Bits (..))
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl', foldr)
import Data.Word (Word8)
import Foreign.C (CInt)
import GHC.IO.Handle (hClose)
import GHC.IO.Handle.FD (stdout)
import GHC.IO.IOMode (IOMode (..))
import GHC.Word (Word32)
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
import System.IO (hGetContents)

-- 127.0.0.1
hostAddr :: HostAddress
hostAddr = tupleToHostAddress (0x7f, 0, 0, 1)

simpleSock :: IO ()
simpleSock = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 9190 hostAddr)
  listen sock 5
  (client_sock, _) <- accept sock
  putStrLn "Connectted client"
  handle <- socketToHandle client_sock ReadWriteMode
  bs <- BL.hGetContents handle
  print (BL.foldr (:) [] bs)
  print $ w8toi (BL.unpack bs)
  hClose handle
  close client_sock
  close sock
  return ()
