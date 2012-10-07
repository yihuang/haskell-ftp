{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.FTP.Server
  ( ftpServer
  ) where

{-| Run ftp server to backend monad.
 -}

import qualified Prelude as P
import BasicPrelude

import Data.Conduit ( ($=), ($$+) )
import qualified Data.Conduit.Binary as C
import Data.Conduit.Network (Application, appSource, appSink, appSockAddr, appLocalAddr)

import Network.FTP.Monad (runFTP, reply, defaultFTPState)
import Network.FTP.Backend (FTPBackend)
import Network.FTP.Commands (commandLoop)

{-| Main ftp server application, should be runned by `runTCPServer`.
 -}
ftpServer :: FTPBackend m => Application m
ftpServer ad = do
    let local = fromMaybe (error "Need to runTCPServer with serverNeedLocalAddr=True.") (appLocalAddr ad)
    (src', _) <- appSource ad $= C.lines $$+ return ()
    void $ runFTP (defaultFTPState src' (appSink ad) (appSockAddr ad) local) $ do
        reply "220" "Welcome to Haskell Ftp Server."
        commandLoop
