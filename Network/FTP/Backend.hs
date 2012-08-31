{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Network.FTP.Backend
  ( FTPBackend(..)
  ) where

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

    list          :: FilePath -> Source m ByteString  -- ^ list directory content.
    nlst          :: FilePath -> Source m ByteString  -- ^ list names inside directory.
    mkd           :: FilePath -> m ()                 -- ^ create directory.
    dele          :: FilePath -> m ()                 -- ^ remove file.
    rmd           :: FilePath -> m ()                 -- ^ remove directory.
    rename        :: FilePath -> FilePath -> m ()     -- ^ rename file.

    download      :: FilePath -> Source m ByteString  -- ^ fetch file.
    upload        :: FilePath -> Sink ByteString m () -- ^ store file.
