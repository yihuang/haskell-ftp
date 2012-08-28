{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Network.FTP.Commands where

import qualified Prelude as P
import BasicPrelude
import Filesystem.Path.CurrentOS

import Control.Monad.Trans.State (get, gets, put, modify)
import qualified Control.Exception.Lifted as Lifted

import Data.Maybe
import qualified Data.ByteString.Char8 as S
import Data.Conduit
import Data.Conduit.Network

import qualified Network.FTP.Socket as NS
import Network.FTP.Monad
import Network.FTP.Backend

type Command m = ByteString -> FTP m Bool

{-
 - check auth state before run command.
 -}
login_required :: FTPBackend m => Command m -> Command m
login_required cmd arg = do
    b <- isJust <$> lift authenticated
    if b
      then cmd arg
      else do
           reply "530" "Command not possible in non-authenticated state."
           return True

cmd_user :: FTPBackend m => Command m
cmd_user name = do
    b <- isJust <$> lift authenticated
    if b
    then reply "530" "Command not possible in authenticated state."
    else do
        reply "331" "User name accepted; send password."
        pass <- wait (expect "PASS")
        b' <- lift (isJust <$> authenticate name pass)
        if b' then reply "230" "login successful."
             else reply "530" "incorrect password."
    return True

cmd_cwd, cmd_cdup, cmd_pwd :: FTPBackend m => Command m
cmd_cwd dir = do
    lift (cwd (decode dir))
    dir' <- lift pwd
    reply "250" $ "New directory now " ++ encode dir'
    return True
cmd_cdup _ = cmd_cwd ".."
cmd_pwd _ = do
    d <- lift pwd
    reply "257" $ "\"" ++ encode d ++ "\" is the current working directory."
    return True

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
    return True

cmd_pasv :: FTPBackend m => Command m
cmd_pasv _ = do
    local <- FTP (gets ftpLocal)
    sock <- liftIO $ NS.startPasvServer local
    FTP $ modify $ \st -> st { ftpChannel = PasvChannel sock }
    portstr <- liftIO $ NS.getSocketName sock >>= NS.toPortString
    reply "227" $ S.pack $ "Entering passive mode (" ++ portstr ++ ")"
    return True

cmd_port :: FTPBackend m => Command m
cmd_port port = do
    local <- FTP (gets ftpLocal)
    addr <- liftIO $ NS.fromPortString (S.unpack port)
    case validate local addr of
        Right _  -> doit addr
        Left err -> reply "501" err
    return True
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

runTransfer :: FTPBackend m => Application m -> FTP m Bool
runTransfer app = do
    reply "150" "I'm going to open the data channel now."
    chan <- FTP (gets ftpChannel)
    case chan of
        NoChannel -> fail "Can't connect when no data channel exists"

        PasvChannel sock ->
            Lifted.finally
              (do (csock, _) <- liftIO $ NS.accept sock
                  lift $ Lifted.finally
                           (app (sourceSocket csock) (sinkSocket csock))
                           (liftIO (NS.sClose csock))
              )
              (do liftIO (NS.sClose sock)
                  FTP $ modify $ \st -> st{ ftpChannel = NoChannel }
              )

        PortChannel addr ->
            Lifted.finally
              (do sock <- liftIO $ do
                      proto <- NS.getProtocolNumber "tcp"
                      sock <- NS.socket NS.AF_INET NS.Stream proto
                      NS.connect sock addr
                      return sock
                  lift $ Lifted.finally
                           (app (sourceSocket sock) (sinkSocket sock))
                           (liftIO (NS.sClose sock))
              )
              (FTP $ modify $ \st -> st{ ftpChannel = NoChannel })

    reply "226" "Closing data connection; transfer complete."
    return True

cmd_list :: FTPBackend m => Command m
cmd_list dir =
    runTransfer $ \src snk -> list (decode dir) $$ snk

cmd_noop :: FTPBackend m => Command m
cmd_noop _ = reply "200" "OK" >> return True

cmd_dele :: FTPBackend m => Command m
cmd_dele "" = reply "501" "Filename required" >> return True
cmd_dele name = do
    lift (remove (decode name))
    reply "250" $ "File " ++ name ++ " deleted."
    return True

cmd_retr :: FTPBackend m => Command m
cmd_retr "" = reply "501" "Filename required" >> return True
cmd_retr name =
    runTransfer $ \src snk -> download (decode name) $$ snk

cmd_stor :: FTPBackend m => Command m
cmd_stor ""   = reply "501" "Filename required" >> return True
cmd_stor name =
    runTransfer $ \src snk -> src $$ upload (decode name)

cmd_syst :: FTPBackend m => Command m
cmd_syst _ = reply "215" "UNIX Type: L8" >> return True

commands :: FTPBackend m => [(ByteString, Command m)]
commands =
    [("USER", cmd_user)
    ,("PASS", const $ reply "530" "Out of sequence PASS command" >> return True)
    ,("QUIT", const $ reply "221" "OK, Goodbye." >> return False)
    --,(Command "HELP" (help,             help_help))
    ,("CWD",  login_required cmd_cwd)
    ,("PWD",  login_required cmd_pwd)
    ,("CDUP", login_required cmd_cdup)
    ,("PASV", login_required cmd_pasv)
    ,("PORT", login_required cmd_port)
    ,("LIST", login_required cmd_list)
    ,("TYPE", login_required cmd_type)
    --,("MKD",  login_required cmd_mkd)
    ,("NOOP", login_required cmd_noop)
    --,(Command "RNFR" (login_required cmd_rnfr,  help_rnfr))
    --,(Command "RNTO" (login_required cmd_rnto,  help_rnto))
    ,("DELE", login_required cmd_dele)
    --,(Command "RMD"  (login_required cmd_rmd,   help_rmd))
    --,(Command "MODE" (login_required cmd_mode,  help_mode))
    --,(Command "STRU" (login_required cmd_stru,  help_stru))
    ,("RETR", login_required cmd_retr)
    ,("STOR", login_required cmd_stor)
    --,(Command "STAT" (login_required cmd_stat,  help_stat))
    ,("SYST", login_required cmd_syst)
    --,(Command "NLST" (login_required cmd_nlst,  help_nlst))
    ]

commandLoop :: FTPBackend m => FTP m ()
commandLoop = do
    (cmd, arg) <- wait $ getCommand
    lift (ftplog $ cmd++" "++arg)
    case lookup cmd commands of
        Just cmd' -> do
            continue <- cmd' arg
            when continue commandLoop
        Nothing -> reply "502" $ "Unrecognized command " ++ cmd
