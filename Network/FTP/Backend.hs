{-# LANGUAGE TypeFamilies #-}
module Network.FTP.Backend where

import qualified Prelude as P
import BasicPrelude

import Data.Conduit

class (Functor m, Monad m, MonadIO m) => FTPBackend m where
    type UserId m

    ftplog        :: ByteString -> m ()

    authenticate  :: ByteString -> ByteString -> m (Maybe (UserId m))
    authenticated :: m (Maybe (UserId m))

    cwd           :: ByteString -> m ()
    pwd           :: m ByteString
    list          :: GSource m ByteString

    download      :: ByteString -> GSource m ByteString
    upload        :: ByteString -> GInfSink ByteString m
