{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Network.FTP.Monad where

import qualified Prelude as P
import BasicPrelude
import Filesystem.Path.CurrentOS (parent)

import qualified Data.ByteString.Char8 as S
import qualified Data.CaseInsensitive as CI
import Data.Conduit

import Control.Monad.Trans.State (StateT, runStateT, gets, modify)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))

import Network.Socket (Socket, SockAddr)
import Network.FTP.Backend (UserId)

data DataChannel = NoChannel
                 | PasvChannel Socket
                 | PortChannel SockAddr
    deriving (Show)

data DataType = ASCII | Binary
    deriving (Show)

data FTPState m = FTPState
  { ftpSource   :: ResumableSource m ByteString -- ^ source for reading request
  , ftpSink     :: Sink ByteString m ()         -- ^ sink for sending respose
  , ftpRemote   :: SockAddr                     -- ^ client address
  , ftpLocal    :: SockAddr                     -- ^ local address
  , ftpChannel  :: DataChannel                  -- ^ ftp data channel
  , ftpDataType :: DataType                     -- ^ ftp data type
  , ftpRename   :: Maybe FilePath               -- ^ store from name during renaming.
  , ftpDir      :: FilePath                     -- ^ the ftp virtual directory.
  , ftpUser     :: Maybe (UserId m)             -- ^ current authenticated user.
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
             "/"
             Nothing

{-|
 - FTP is a monad transformer that only handles only ftp protocol, and leaves authentication and filesystem details to underlying monad.
 -}
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

{-|
 - Consume request source, store the modified ResumableSource into state monad.
 -}
consumeSource :: Monad m
              => (ResumableSource m ByteString -> FTP m (ResumableSource m ByteString, a))
              -> FTP m a
consumeSource f = do
    src <- FTP (gets ftpSource)
    (src', r) <- f src
    FTP $ modify $ \st -> st{ ftpSource = src' }
    return r

{-|
 - Run a `Sink` to parse request.
 - e.g. `wait getCommand :: FTP m (ByteString, ByteString)`
 -}
wait :: Monad m => Sink ByteString m b -> FTP m b
wait snk = consumeSource $ \src -> lift (src $$++ snk)

{-|
 - Get a line, fail when connection closed. The stream is assumed to be individual lines produced by upstream conduit.
 -}
headOrFail :: Monad m => GSink a m a
headOrFail = await >>= maybe (fail "connection closed") return

{-|
 - Parse a request command.
 -}
getCommand :: Monad m => GSink ByteString m (CI.CI ByteString, ByteString)
getCommand = do
    s' <- headOrFail
    let (cmd', arg) = S.span (\c -> c/=' ' && c/='\r') s'
        arg' = if S.length arg < 2
                 then S.empty
                 else S.tail (S.init arg) -- strip heading ' ' and trailing '\r'
    return (CI.mk cmd', arg')

{-|
 - Parse a command, if it's not the expected one, then fail.
 -}
expect :: (Monad m) => CI.CI ByteString -> GSink ByteString m ByteString
expect s = do
    (cmd, arg) <- getCommand
    if s==cmd
      then return arg
      else fail $ concat ["expect ", P.show s, " but got ", P.show cmd, " ."]

{-|
 - Send raw message to client
 -}
send :: Monad m => ByteString -> FTP m ()
send s = do
    snk <- FTP $ gets ftpSink
    lift (yield s $$ snk)

{-|
 - Send reply code and message to client, support multiline message.
 -}
reply :: Monad m => ByteString -> ByteString -> FTP m ()
reply code msg =
    send . S.concat $ build code (S.lines msg) ++ ["\r\n"]
  where
    build c []     = [c, "  "]
    build c [s]    = [c, " ", s]
    build c (s:ss) = [c, "-", s] ++ build c ss

{-|
 - Change working directory
 -}
cwd :: Monad m => FilePath -> FTP m ()
cwd ".." = FTP (modify $ \st -> st{ftpDir = parent (ftpDir st)})
cwd d    = FTP (modify $ \st -> st{ftpDir = ftpDir st </> d})

{-|
 - Print working directory
 -}
pwd :: Monad m => FTP m FilePath
pwd = FTP (gets ftpDir)

{-|
 - Make absolute path.
 -}
ftpAbsolute :: Monad m => FilePath -> FTP m FilePath
ftpAbsolute path = do
    dir <- FTP (gets ftpDir)
    return (dir </> path)
