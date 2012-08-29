module Network.FTP.Socket
  ( startPasvServer
  , toPortString
  , fromPortString
  , module Network.Socket
  , module Network.BSD
  ) where

{-|
 - Network Utils.
 -}

import Control.Exception (bracketOnError)
import Network.Socket
import Network.BSD (getProtocolNumber)
import Network.FTP.Client.Parser (toPortString, fromPortString)

startPasvServer :: SockAddr -> IO Socket
startPasvServer (SockAddrInet _ host) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
      (socket AF_INET Stream proto)
      sClose
      (\sock -> do
          setSocketOption sock ReuseAddr 0
          bindSocket sock (SockAddrInet aNY_PORT host)
          listen sock 1
          return sock
      )
startPasvServer _ = fail "Require IPv4 sockets"
