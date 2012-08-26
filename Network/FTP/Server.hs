{-# LANGUAGE OverloadedStrings #-}
module Network.FTP.Server where

import qualified Prelude as P
import BasicPrelude

import Control.Monad.Trans.State

import Data.Conduit
import qualified Data.Conduit.Binary as C
import Data.Conduit.Network (Application)

import Network.FTP.Monad
import Network.FTP.Commands

--data FtpBackendState = FtpBackendState
--  { authed :: Bool
--  , dir    :: ByteString
--  }
--
--newtype FileSystemBackend a = FileSystemBackend { unFileSystemBackend :: StateT FtpBackendState IO a }
--
--instance FtpBackend FileSystemBackend where
--    ftplog = S.putStrLn
--    authenticate

ftpServer :: Monad m => Application m
ftpServer src snk = do
    (src', _) <- src $= C.lines $$+ return ()
    _ <- runFTP (defaultFTPState src' snk "") $ do
        reply "220" "Welcome to Haskell Ftp Server."
        commandLoop
    return ()
