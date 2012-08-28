{-# LANGUAGE GeneralizedNewtypeDeriving
           , OverloadedStrings
           , TypeFamilies
           , MultiParamTypeClasses
           , CPP
           #-}
module Network.FTP.Backend.FileSystem where

import BasicPrelude (lift, when, ByteString, Applicative, (<$>), liftM, intersperse)
import qualified System.Posix.Directory as Dir
import qualified System.IO as IO
import System.FilePath

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
  , dir  :: FilePath
  }

defaultFSState :: FilePath -> FSState
defaultFSState base = FSState Nothing base "/"

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
#ifdef mingw32_HOST_OS
dropHeadingPathSeparator ('\\':p) = p
#else
dropHeadingPathSeparator ('/':p)  = p
#endif
dropHeadingPathSeparator p = p

currentDirectory :: FSBackend FilePath
currentDirectory =
    FSBackend (gets dir)

makeAbsolute :: FilePath -> FSBackend FilePath
makeAbsolute path = do
    b <- FSBackend (gets base)
    d <- FSBackend (gets dir)
    return $ normalise $ b </> dropHeadingPathSeparator (d </> path)

instance FTPBackend FSBackend where
    type UserId FSBackend = ByteString

    ftplog  = liftIO . S.putStrLn

    authenticate user pass = do
        when (user==pass) $
             FSBackend (modify $ \st -> st{user=Just user})
        authenticated

    authenticated   = FSBackend (gets user)

    cwd ".." = FSBackend (modify $ \st -> st{dir = takeDirectory (dir st)})
    cwd d    = FSBackend (modify $ \st -> st{dir = dir st </> dropTrailingPathSeparator (S.unpack d)})

    pwd   = S.pack <$> currentDirectory

    list dir = do
        dir' <- lift (makeAbsolute (S.unpack dir))
        C.sourceCmd $ "ls -l " ++ dir'

    remove name = return ()

    download name =
        lift (makeAbsolute (S.unpack name)) >>= C.sourceFile

    upload   name =
        lift (makeAbsolute (S.unpack name)) >>= C.sinkFile
