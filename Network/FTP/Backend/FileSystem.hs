{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , TypeFamilies
           , MultiParamTypeClasses
           #-}
module Network.FTP.Backend.FileSystem where

import BasicPrelude

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base

import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C

import Network.FTP.Backend

data FilesystemState = FilesystemState
  { user :: Maybe (UserId FilesystemBackend)
  , dir  :: ByteString
  }

defaultFilesystemState :: FilesystemState
defaultFilesystemState = FilesystemState Nothing ""

newtype FilesystemBackend a = FilesystemBackend { unFilesystemBackend :: StateT FilesystemState (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadUnsafeIO, MonadThrow, MonadResource
             )

instance MonadBase IO FilesystemBackend where
    liftBase = FilesystemBackend . liftBase

instance MonadBaseControl IO FilesystemBackend where
    newtype StM FilesystemBackend a = FilesystemBackendStM { unFilesystemBackendStM :: StM (StateT FilesystemState (ResourceT IO)) a }
    liftBaseWith f = FilesystemBackend . liftBaseWith $ \runInBase -> f $ liftM FilesystemBackendStM . runInBase . unFilesystemBackend
    restoreM = FilesystemBackend . restoreM . unFilesystemBackendStM

runFilesystemBackend :: FilesystemState -> FilesystemBackend a -> IO a
runFilesystemBackend st m = runResourceT $ evalStateT (unFilesystemBackend m) st

instance FTPBackend FilesystemBackend where
    type UserId FilesystemBackend = ByteString

    ftplog  = liftIO . S.putStrLn

    authenticate user pass = do
        when (user==pass) $
             FilesystemBackend (modify $ \st -> st{user=Just user})
        authenticated

    authenticated   = FilesystemBackend (gets user)

    cwd dir = FilesystemBackend (modify $ \st -> st{dir=dir})
    pwd     = FilesystemBackend (gets dir)

    list    = C.sourceList ["list"]

    download = C.sourceFile . S.unpack
    upload   = C.sinkFile . S.unpack
