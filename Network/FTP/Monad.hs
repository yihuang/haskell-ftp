{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Network.FTP.Monad where

import qualified Prelude as P
import BasicPrelude

import qualified Data.ByteString.Char8 as S
import Data.Conduit

import Control.Monad.Trans
import Control.Monad.Trans.State

import Network.Socket (SockAddr)

data Mode = Passive | Active
data DataType = ASCII | Binary

data FTPState m = FTPState
  { ftpSource   :: ResumableSource m ByteString
  , ftpSink     :: Sink ByteString m ()
  , ftpRemote   :: SockAddr
  , ftpLocal    :: SockAddr
  , ftpMode     :: Mode
  , ftpDataType :: DataType
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
             Passive
             ASCII

newtype FTP m a = FTP { unFTP :: StateT (FTPState m) m a }
    deriving (Functor, Monad, MonadIO)

instance MonadTrans FTP where
    lift m = FTP (lift m)

runFTP :: FTPState m -> FTP m a -> m (a, FTPState m)
runFTP s m = runStateT (unFTP m) s

withClient :: Monad m => (ResumableSource m ByteString -> FTP m (ResumableSource m ByteString, a)) -> FTP m a
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
