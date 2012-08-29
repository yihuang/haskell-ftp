{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}
module Network.FTP.Commands where

{-| Implement all ftp commands in FTP Monad.
 -}

import qualified Prelude as P
import BasicPrelude
import Filesystem.Path.CurrentOS (encode, decode)

import Control.Monad.Trans.State (get, gets, put, modify)
import Control.Exception (throw)
import qualified Control.Exception.Lifted as Lifted

import qualified Data.ByteString.Char8 as S
import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Data.Conduit ( ($$) )
import Data.Conduit.Network (Application, sinkSocket, sourceSocket)

import Network.FTP.Monad
import qualified Network.FTP.Socket as NS
import Network.FTP.Backend (FTPBackend(..))

type Command m = ByteString -> FTP m ()

data ApplicationQuit = ApplicationQuit
    deriving (Show, Typeable)

instance Exception ApplicationQuit

{-
 - check auth state before run command.
 -}
login_required :: FTPBackend m => Command m -> Command m
login_required cmd arg = do
    b <- isJust <$> FTP (gets ftpUser)
    if b
      then cmd arg
      else reply "530" "Command not possible in non-authenticated state."

cmd_user :: FTPBackend m => Command m
cmd_user name = do
    b <- isJust <$> FTP (gets ftpUser)
    if b
      then reply "530" "Command not possible in authenticated state."
      else do
        reply "331" "User name accepted; send password."
        pass <- wait (expect "PASS")
        muser <- lift (authenticate name pass)
        FTP $ modify $ \st -> st { ftpUser = muser }
        if isJust muser
           then reply "230" "login successful."
           else reply "530" "incorrect password."

cmd_cwd, cmd_cdup, cmd_pwd :: FTPBackend m => Command m

cmd_cwd dir = do
    cwd (decode dir)
    dir' <- pwd
    reply "250" $ "New directory now " ++ encode dir'

cmd_cdup _ = cmd_cwd ".."
cmd_pwd _ = do
    d <- pwd
    reply "257" $ "\"" ++ encode d ++ "\" is the current working directory."

cmd_type :: FTPBackend m => Command m
cmd_type tp = do
    let modType newtp = do
            st <- FTP get
            reply "200" $ S.pack $
                "Type changed from " ++ P.show (ftpDataType st) ++
                " to " ++ P.show newtp
            FTP $ put st{ ftpDataType = newtp }
    case tp of
        "I"     ->  modType Binary
        "L 8"   ->  modType Binary
        "A"     ->  modType ASCII
        "AN"    ->  modType ASCII
        "AT"    ->  modType ASCII
        _       ->  reply "504" $ "Type \"" ++ tp ++ "\" not supported."

cmd_pasv :: FTPBackend m => Command m
cmd_pasv _ = do
    local <- FTP (gets ftpLocal)
    sock <- liftIO $ NS.startPasvServer local
    FTP $ modify $ \st -> st { ftpChannel = PasvChannel sock }
    portstr <- liftIO $ NS.getSocketName sock >>= NS.toPortString
    reply "227" $ S.pack $ "Entering passive mode (" ++ portstr ++ ")"

cmd_port :: FTPBackend m => Command m
cmd_port port = do
    local <- FTP (gets ftpLocal)
    addr <- liftIO $ NS.fromPortString (S.unpack port)
    case validate local addr of
        Right _  -> doit addr
        Left err -> reply "501" err
  where
    validate (NS.SockAddrInet _ h1) (NS.SockAddrInet _ h2) =
                if h1==h2
                   then Right ()
                   else Left "Will only connect to same client as command channel."
    validate (NS.SockAddrInet _ _) _ = Left "Require IPv4 in specified address"
    validate _ _                     = Left "Require IPv4 on client"

    doit addr = do
        FTP $ modify $ \st -> st { ftpChannel = PortChannel addr }
        reply "200" $ S.pack $ "OK, later I will connect to " ++ P.show addr

runTransfer :: FTPBackend m => Application m -> FTP m ()
runTransfer app = do
    reply "150" "I'm going to open the data channel now."
    chan <- FTP (gets ftpChannel)
    FTP $ modify $ \st -> st{ ftpChannel = NoChannel }
    case chan of
        NoChannel -> fail "Can't connect when no data channel exists"

        PasvChannel sock ->
            Lifted.finally
              (do (csock, _) <- liftIO $ NS.accept sock
                  lift $ Lifted.finally
                           (app (sourceSocket csock) (sinkSocket csock))
                           (liftIO (NS.sClose csock))
              )
              (liftIO (NS.sClose sock))

        PortChannel addr -> do
            sock <- liftIO $ do
                proto <- NS.getProtocolNumber "tcp"
                sock <- NS.socket NS.AF_INET NS.Stream proto
                NS.connect sock addr
                return sock
            lift $ Lifted.finally
                     (app (sourceSocket sock) (sinkSocket sock))
                     (liftIO (NS.sClose sock))

    reply "226" "Closing data connection; transfer complete."

cmd_list :: FTPBackend m => Command m
cmd_list dir = do
    dir' <- ftpAbsolute (decode dir)
    runTransfer $ \_ snk -> list dir' $$ snk

cmd_nlst :: FTPBackend m => Command m
cmd_nlst dir = do
    dir' <- ftpAbsolute (decode dir)
    runTransfer $ \_ snk -> nlst dir' $$ snk

cmd_noop :: FTPBackend m => Command m
cmd_noop _ = reply "200" "OK"

cmd_dele :: FTPBackend m => Command m
cmd_dele "" = reply "501" "Filename required"
cmd_dele name = do
    ftpAbsolute (decode name) >>= lift . dele
    reply "250" $ "File " ++ name ++ " deleted."

cmd_retr :: FTPBackend m => Command m
cmd_retr "" = reply "501" "Filename required"
cmd_retr name = do
    name' <- ftpAbsolute (decode name)
    runTransfer $ \_ snk -> download name' $$ snk

cmd_stor :: FTPBackend m => Command m
cmd_stor ""   = reply "501" "Filename required"
cmd_stor name = do
    name' <- ftpAbsolute (decode name)
    runTransfer $ \src _ -> src $$ upload name'

cmd_syst :: FTPBackend m => Command m
cmd_syst _ = reply "215" "UNIX Type: L8"

cmd_mkd :: FTPBackend m => Command m
cmd_mkd ""  = reply "501" "Directory name required"
cmd_mkd dir = do
    dir' <- ftpAbsolute (decode dir)
    lift $ mkd dir'
    reply "257" $ "\"" ++ encode dir' ++ "\" created."

cmd_rnfr, cmd_rnto :: FTPBackend m => Command m
cmd_rnfr ""   = reply "501" "Filename required"
cmd_rnfr name = do
    FTP $ modify $ \st -> st { ftpRename = Just (decode name) }
    reply "350" $ "Noted rename from "++name++"; please send RNTO."

cmd_rnto ""   = reply "501" "Filename required"
cmd_rnto name = do
    mfromname <- FTP (gets ftpRename)
    case mfromname of
        Nothing ->
            reply "503" "RNFR required before RNTO"
        Just fromname -> do
            FTP $ modify $ \st -> st { ftpRename = Nothing }
            fname <- ftpAbsolute fromname
            tname <- ftpAbsolute (decode name)
            lift (rename fname tname)
            reply "250" $ "File "++encode fromname++" renamed to "++name

cmd_rmd :: FTPBackend m => Command m
cmd_rmd ""   = reply "501" "Filename required"
cmd_rmd dir = do
    ftpAbsolute (decode dir) >>= lift . rmd
    reply "250" $ "Directory " ++ dir ++ " removed."

cmd_stat :: FTPBackend m => Command m
cmd_stat _ = do
    st <- FTP get
    reply "211" $ S.pack $ P.unlines
         [" *** Sever statistics and information"
         ," *** Please type HELP for more details"
         ,""
         ,"Server Software     : haskell-ftp, https://github.com/yihuang/haskell-ftp"
         ,"Connected From      : ", P.show (ftpRemote st)
         ,"Connected To        : ", P.show (ftpLocal st)
         ,"Data Transfer Type  : ", P.show (ftpDataType st)
         ,"Auth Status         : ", P.show (ftpUser st)
         ,"End of status."
         ]

cmd_mode, cmd_stru :: FTPBackend m => Command m
cmd_mode m =
    case m of
        "S" -> reply "200" "Mode is Stream."
        _   -> reply "504" $ "Mode \"" ++ m ++ "\" not supported."

cmd_stru s =
    case s of
        "F" -> reply "200" "Structure is File."
        _   -> reply "504" $ "Structure \"" ++ s ++ "\" not supported."

commands :: FTPBackend m => [(ByteString, Command m)]
commands =
    [("USER", cmd_user)
    ,("PASS", const $ reply "530" "Out of sequence PASS command")
    ,("QUIT", const $ reply "221" "OK, Goodbye." >> throw ApplicationQuit)
    --,(Command "HELP" (help,             help_help))
    ,("CWD",  login_required cmd_cwd)
    ,("PWD",  login_required cmd_pwd)
    ,("CDUP", login_required cmd_cdup)
    ,("PASV", login_required cmd_pasv)
    ,("PORT", login_required cmd_port)
    ,("LIST", login_required cmd_list)
    ,("TYPE", login_required cmd_type)
    ,("MKD",  login_required cmd_mkd)
    ,("NOOP", login_required cmd_noop)
    ,("RNFR", login_required cmd_rnfr)
    ,("RNTO", login_required cmd_rnto)
    ,("DELE", login_required cmd_dele)
    ,("RMD",  login_required cmd_rmd)
    ,("RETR", login_required cmd_retr)
    ,("STOR", login_required cmd_stor)
    ,("STAT", login_required cmd_stat)
    ,("SYST", login_required cmd_syst)
    ,("NLST", login_required cmd_nlst)
    ,("MODE", login_required cmd_mode)
    ,("STRU", login_required cmd_stru)
    ]

commandLoop :: FTPBackend m => FTP m ()
commandLoop = do
    (cmd, arg) <- wait getCommand
    lift (ftplog $ cmd++" "++arg)
    case lookup cmd commands of
        Just cmd' -> do
            continue <- (cmd' arg >> return True)
                           `Lifted.catch` (\(_::ApplicationQuit) -> return False)
                           `Lifted.catch` (\(ex::SomeException) -> do
                                               reply "502" (S.pack $ P.show ex)
                                               return True
                                          )
            when continue commandLoop
        Nothing -> do
            reply "502" $ "Unrecognized command " ++ cmd
            commandLoop
