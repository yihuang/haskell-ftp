{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Network.FTP.Monad where

import qualified Prelude as P
import BasicPrelude

import qualified Data.ByteString.Char8 as S
import Data.Conduit

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Control
import Control.Monad.Base

import Network.Socket (Socket, SockAddr)
import Network.Socket (SockAddr)

data DataChannel = NoChannel
                 | PasvChannel Socket
                 | PortChannel SockAddr
    deriving (Show)

data DataType = ASCII | Binary
    deriving (Show)

data FTPState m = FTPState
  { ftpSource   :: ResumableSource m ByteString
  , ftpSink     :: Sink ByteString m ()
  , ftpRemote   :: SockAddr
  , ftpLocal    :: SockAddr
  , ftpChannel  :: DataChannel
  , ftpDataType :: DataType
  , ftpRename   :: Maybe FilePath
  }

defaultFTPState :: ResumableSource m ByteString
                -> Sink ByteString m ()
                -> SockAddr
                -> SockAddr
                -> FTPState m
defaultFTPState src snk remote local =
    FTPState src
             snk
             remote
             local
             NoChannel
             ASCII
             Nothing

newtype FTP m a = FTP { unFTP :: StateT (FTPState m) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans FTP where
    lift m = FTP (lift m)

instance MonadBase IO m => MonadBase IO (FTP m) where
    liftBase = FTP . liftBase

instance MonadBaseControl IO m => MonadBaseControl IO (FTP m) where
    newtype StM (FTP m) a = FTPStM { unFTPStM :: StM (StateT (FTPState m) m) a }
    liftBaseWith f = FTP . liftBaseWith $ \runInBase -> f $ liftM FTPStM . runInBase . unFTP
    restoreM = FTP . restoreM . unFTPStM

runFTP :: FTPState m -> FTP m a -> m (a, FTPState m)
runFTP s m = runStateT (unFTP m) s

withClient :: Monad m
              => (ResumableSource m ByteString
              -> FTP m (ResumableSource m ByteString, a))
              -> FTP m a
withClient f = do
    st <- FTP get
    (src, r) <- f (ftpSource st)
    FTP $ put st{ ftpSource = src }
    return r

wait :: Monad m => Sink ByteString m b -> FTP m b
wait snk = withClient $ \src -> lift (src $$++ snk)

headOrFail :: Monad m => GSink a m a
headOrFail = await >>= maybe (fail "connection closed") return

getCommand :: Monad m => GSink ByteString m (ByteString, ByteString)
getCommand = do
    s' <- headOrFail
    let (cmd', arg) = S.span (\c -> c/=' ' && c/='\r') s'
        arg' = if S.length arg < 2
                 then S.empty
                 else S.tail (S.init arg) -- strip heading ' ' and trailing '\r'
    return (cmd', arg')

expect :: (Monad m) => ByteString -> GSink ByteString m ByteString
expect s = do
    (cmd, arg)  <- getCommand
    if s==cmd
      then return arg
      else fail $ concat ["expect ", P.show s, " but got ", P.show cmd, " ."]

send :: Monad m => ByteString -> FTP m ()
send s = do
    snk <- FTP $ gets ftpSink
    lift (yield s $$ snk)

reply :: Monad m => ByteString -> ByteString -> FTP m ()
reply code msg =
    send . S.concat $ build code (S.lines msg) ++ ["\r\n"]
  where
    build c []     = [c, "  "]
    build c [s]    = [c, " ", s]
    build c (s:ss) = [c, "-", s] ++ build c ss
