{-# LANGUAGE TypeFamilies #-}
module Network.FTP.Class where

import qualified Prelude as P
import BasicPrelude

import Data.Conduit

class Monad m => FTPBackend m where
    type UserId

    ftplog        :: ByteString -> m ()

    authenticate  :: ByteString -> ByteString -> m Bool
    authenticated :: m Bool

    cwd           :: ByteString -> m ()
    pwd           :: m ByteString
    list          :: GSource m ByteString

    download      :: ByteString -> GSource m ByteString
    upload        :: ByteString -> GInfSink ByteString m
