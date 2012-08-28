{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.FTP.Backend where

import qualified Prelude as P
import BasicPrelude

import Control.Monad.Trans.Control
import Data.Conduit

class (Functor m, Monad m, MonadIO m, MonadBaseControl IO m, Show (UserId m)) => FTPBackend m where
    type UserId m

    ftplog        :: ByteString -> m ()

    authenticate  :: ByteString -> ByteString -> m (Maybe (UserId m))
    authenticated :: m (Maybe (UserId m))

    cwd           :: FilePath -> m ()
    pwd           :: m FilePath
    list          :: FilePath -> Source m ByteString
    nlst          :: FilePath -> Source m ByteString
    mkd           :: FilePath -> m FilePath
    dele          :: FilePath -> m ()
    rmd           :: FilePath -> m ()
    rename        :: FilePath -> FilePath -> m ()

    download      :: FilePath -> Source m ByteString
    upload        :: FilePath -> Sink ByteString m ()
