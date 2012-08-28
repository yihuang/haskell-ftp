{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.FTP.Backend where

import qualified Prelude as P
import BasicPrelude

import Control.Monad.Trans.Control
import Data.Conduit

class (Functor m, Monad m, MonadIO m, MonadBaseControl IO m) => FTPBackend m where
    type UserId m

    ftplog        :: ByteString -> m ()

    authenticate  :: ByteString -> ByteString -> m (Maybe (UserId m))
    authenticated :: m (Maybe (UserId m))

    cwd           :: ByteString -> m ()
    pwd           :: m ByteString
    list          :: ByteString -> GSource m ByteString

    remove        :: ByteString -> m ()
    download      :: ByteString -> GSource m ByteString
    upload        :: ByteString -> GInfSink ByteString m
