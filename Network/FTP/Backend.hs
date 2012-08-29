{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.FTP.Backend where

{-|
 - Interface for backend.
 -}

import qualified Prelude as P
import BasicPrelude

import Control.Monad.Trans.Control
import Data.Conduit

class ( Functor m
      , Monad m
      , MonadIO m
      , MonadBaseControl IO m
      , Show (UserId m)
      ) => FTPBackend m where

    type UserId m

    ftplog        :: ByteString -> m ()               -- ^ logging.

    authenticate  :: ByteString
                  -> ByteString
                  -> m (Maybe (UserId m))             -- ^ login.
    authenticated :: m (Maybe (UserId m))             -- ^ checked current login status.

    cwd           :: FilePath -> m ()                 -- ^ change working directory.
    pwd           :: m FilePath                       -- ^ print working directory.
    list          :: FilePath -> Source m ByteString  -- ^ list directory content.
    nlst          :: FilePath -> Source m ByteString  -- ^ list names inside directory.
    mkd           :: FilePath -> m FilePath           -- ^ make directory.
    dele          :: FilePath -> m ()                 -- ^ remove file.
    rmd           :: FilePath -> m ()                 -- ^ remove directory.
    rename        :: FilePath -> FilePath -> m ()     -- ^ rename file.

    download      :: FilePath -> Source m ByteString  -- ^ fetch file.
    upload        :: FilePath -> Sink ByteString m () -- ^ store file.
