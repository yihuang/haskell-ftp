{-# LANGUAGE OverloadedStrings #-}
module Network.FTP.Commands where

import qualified Prelude
import BasicPrelude
import Control.Exception (bracketOnError)
import Control.Monad.Trans.State (gets)
import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Network.Socket as NS
import Network.BSD (getProtocolNumber)
import Network.FTP.Monad
import Network.FTP.Backend
import Network.FTP.Client.Parser (toPortString)

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

cmd_cwd, cmd_pwd :: FTPBackend m => Command m
cmd_cwd dir = do
    lift (cwd dir)
    return True
cmd_pwd _ = do
    d <- lift pwd
    reply "257" $ "\"" ++ d ++ "\" is the current working directory."
    return True

startPasvServer :: NS.SockAddr -> IO NS.Socket
startPasvServer (NS.SockAddrInet _ host) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
      (NS.socket NS.AF_INET NS.Stream proto)
      NS.sClose
      (\sock -> do
          NS.setSocketOption sock NS.ReuseAddr 0
          NS.bindSocket sock (NS.SockAddrInet NS.aNY_PORT host)
          NS.listen sock 1
          return sock
      )
startPasvServer _ = fail "Require IPv4 sockets"

cmd_pasv :: FTPBackend m => Command m
cmd_pasv _ = do
    local <- FTP (gets ftpLocal)
    sock <- liftIO $ startPasvServer local
    portstr <- liftIO $ NS.getSocketName sock >>= toPortString
    reply "227" $ S.pack $ "Entering passive mode (" ++ portstr ++ ")"
    return True

commands :: FTPBackend m => [(ByteString, Command m)]
commands =
    [("USER", cmd_user)
    ,("PASS", const $ reply "530" "Out of sequence PASS command" >> return True)
    ,("QUIT", const $ reply "221" "OK, Goodbye." >> return False)
    --,(Command "HELP" (help,             help_help))
    ,("CWD",  login_required cmd_cwd)
    ,("PWD",  login_required cmd_pwd)
    ,("PASV", login_required cmd_pasv)
    --,("LIST", login_required cmd_list)
    --,(Command "CDUP" (login_required cmd_cdup,  help_cdup))
    --,(Command "TYPE" (login_required cmd_type,  help_type))
    --,(Command "NOOP" (login_required cmd_noop,  help_noop))
    --,(Command "RNFR" (login_required cmd_rnfr,  help_rnfr))
    --,(Command "RNTO" (login_required cmd_rnto,  help_rnto))
    --,(Command "DELE" (login_required cmd_dele,  help_dele))
    --,(Command "RMD"  (login_required cmd_rmd,   help_rmd))
    --,(Command "MKD"  (login_required cmd_mkd,   help_mkd))
    --,(Command "MODE" (login_required cmd_mode,  help_mode))
    --,(Command "STRU" (login_required cmd_stru,  help_stru))
    --,(Command "PORT" (login_required cmd_port,  help_port))
    --,(Command "RETR" (login_required cmd_retr,  help_retr))
    --,(Command "STOR" (login_required cmd_stor,  help_stor))
    --,(Command "STAT" (login_required cmd_stat,  help_stat))
    --,(Command "SYST" (login_required cmd_syst,  help_syst))
    --,(Command "NLST" (login_required cmd_nlst,  help_nlst))
    ]

commandLoop :: FTPBackend m => FTP m ()
commandLoop = do
    (cmd, arg) <- wait $ getCommand
    case lookup cmd commands of
        Just cmd' -> do
            continue <- cmd' arg
            when continue commandLoop
        Nothing -> reply "502" $ "Unrecognized command " ++ cmd
