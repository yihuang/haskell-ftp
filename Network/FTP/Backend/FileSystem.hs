{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , TypeFamilies
           , MultiParamTypeClasses
           , CPP
           #-}
module Network.FTP.Backend.FileSystem
  ( FSConf(..)
  , FSBackend(..)
  , runFSBackend
  ) where

{-|
 - Simple file system backend.
 -}

import qualified Prelude
import BasicPrelude
import System.Directory
import Filesystem.Path.CurrentOS

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base

import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Process as C

import Network.FTP.Utils (dropHeadingPathSeparator)
import Network.FTP.Backend (FTPBackend(..))

data FSConf = FSConf
  { fsBase :: FilePath
  }

newtype FSBackend a = FSBackend { unFSBackend :: ReaderT FSConf (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadUnsafeIO, MonadThrow, MonadResource
             )

instance MonadBase IO FSBackend where
    liftBase = FSBackend . liftBase

instance MonadBaseControl IO FSBackend where
    newtype StM FSBackend a = FSBackendStM { unFSBackendStM :: StM (ReaderT FSConf (ResourceT IO)) a }
    liftBaseWith f = FSBackend . liftBaseWith $ \runInBase -> f $ liftM FSBackendStM . runInBase . unFSBackend
    restoreM = FSBackend . restoreM . unFSBackendStM

runFSBackend :: FSConf -> FSBackend a -> IO a
runFSBackend st m = runResourceT $ runReaderT (unFSBackend m) st

makeAbsolute :: FilePath -> FSBackend FilePath
makeAbsolute path = do
    b <- FSBackend (asks fsBase)
    return $ b </> dropHeadingPathSeparator path

instance FTPBackend FSBackend where
    type UserId FSBackend = ByteString

    ftplog = liftIO . S.putStrLn

    authenticate user pass =
        if user==pass
          then return (Just user)
          else return Nothing

    list dir = do
        dir' <- lift (makeAbsolute dir)
        C.sourceCmd $ "ls -l " ++ encodeString dir'

    nlst dir = do
        dir'  <- lift (makeAbsolute dir)
        paths <- liftIO $ getDirectoryContents (encodeString dir')
        C.sourceList $ map (S.pack . (++"\n")) paths

    mkd dir =
        makeAbsolute dir >>= liftIO . createDirectory . encodeString

    dele name =
        makeAbsolute name >>= liftIO . removeFile . encodeString

    rename fname tname = do
        fname' <- makeAbsolute fname
        tname' <- makeAbsolute tname
        liftIO $ renameFile (encodeString fname') (encodeString tname')

    rmd dir = do
        dir' <- makeAbsolute dir
        liftIO $ removeDirectory (encodeString dir')

    download name =
        lift (makeAbsolute name) >>= C.sourceFile . encodeString

    upload   name =
        lift (makeAbsolute name) >>= C.sinkFile . encodeString
