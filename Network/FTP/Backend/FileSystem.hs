{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , TypeFamilies
           , MultiParamTypeClasses
           , CPP
           #-}
module Network.FTP.Backend.FileSystem where

{-|
 - Simple file system backend.
 -}

import qualified Prelude
import BasicPrelude
import qualified System.Posix.Directory as Dir
import qualified System.IO as IO
import System.Directory (getDirectoryContents, removeFile, removeDirectory, createDirectory, renameFile)
import Filesystem.Path.CurrentOS

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Base

import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Util as C
import qualified Data.Conduit.Process as C

import Network.FTP.Backend

data FSState = FSState
  { user :: Maybe (UserId FSBackend)
  , base :: FilePath
  }

defaultFSState :: FilePath -> FSState
defaultFSState base = FSState Nothing base

newtype FSBackend a = FSBackend { unFSBackend :: StateT FSState (ResourceT IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadUnsafeIO, MonadThrow, MonadResource
             )

instance MonadBase IO FSBackend where
    liftBase = FSBackend . liftBase

instance MonadBaseControl IO FSBackend where
    newtype StM FSBackend a = FSBackendStM { unFSBackendStM :: StM (StateT FSState (ResourceT IO)) a }
    liftBaseWith f = FSBackend . liftBaseWith $ \runInBase -> f $ liftM FSBackendStM . runInBase . unFSBackend
    restoreM = FSBackend . restoreM . unFSBackendStM

runFSBackend :: FSState -> FSBackend a -> IO a
runFSBackend st m = runResourceT $ evalStateT (unFSBackend m) st

dropHeadingPathSeparator :: FilePath -> FilePath
dropHeadingPathSeparator = decode . drop' . encode
  where
    drop' :: ByteString -> ByteString
    drop' s =
        case S.uncons s of
#ifdef mingw32_HOST_OS
            Just ('\\', s') -> s'
#else
            Just ('/',  s') -> s'
#endif
            _               -> s

makeAbsolute :: FilePath -> FSBackend FilePath
makeAbsolute path = do
    b <- FSBackend (gets base)
    return $ b </> dropHeadingPathSeparator path

instance FTPBackend FSBackend where
    type UserId FSBackend = ByteString

    ftplog = liftIO . S.putStrLn

    authenticate user pass =
        if (user==pass)
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
