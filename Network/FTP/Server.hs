{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.FTP.Server
  ( ftpServer
  , runTCPServer
  , module Data.Conduit.Network
  ) where

import qualified Prelude as P
import BasicPrelude

import Control.Exception
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Concurrent

import Data.Conduit
import qualified Data.Conduit.Binary as C
import Data.Conduit.Network hiding (runTCPServer)

import qualified Network.Socket as NS
import Network.FTP.Monad
import Network.FTP.Backend (FTPBackend)
import Network.FTP.Commands (commandLoop)

{- main ftp server application
 -}
ftpServer :: FTPBackend m => NS.SockAddr -> NS.SockAddr -> Application m
ftpServer remote local src snk = do
    (src', _) <- src $= C.lines $$+ return ()
    _ <- runFTP (defaultFTPState src' snk remote local) $ do
        reply "220" "Welcome to Haskell Ftp Server."
        commandLoop
    return ()

{- modified version of the one in network-conduit, pass remote and local addresses to application.
 -}
runTCPServer :: (MonadIO m, MonadBaseControl IO m) => ServerSettings -> (NS.SockAddr -> NS.SockAddr -> Application m) -> m ()
runTCPServer (ServerSettings port host) app = control $ \run -> bracket
    (liftIO $ bindPort port host)
    (liftIO . NS.sClose)
    (run . forever . serve)
  where
    serve lsocket = do
        (socket, remote) <- liftIO $ NS.accept lsocket
        local <- liftIO $ NS.getSocketName socket
        let src = sourceSocket socket
            sink = sinkSocket socket
            app' run = run (app remote local src sink) >> return ()
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

