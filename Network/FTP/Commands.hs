{-# LANGUAGE OverloadedStrings #-}
module Network.FTP.Commands where

import qualified Prelude
import BasicPrelude
import Control.Monad.Trans.State
import qualified Data.CaseInsensitive as CI
import Network.FTP.Monad
import Network.FTP.Class

type Command m = ByteString -> FTP m ()

cmd_user :: Monad m => Command m
cmd_user name = do
    b <- FTP (gets authed)
    if b
      then reply "530" "Command not possible in authenticated state."
      else do
        reply "331" "User name accepted; send password."
        pass <- wait (expect "PASS")
        if pass==name
          then do
              FTP $ modify $ \st -> st { authed = True }
              reply "230" "login successful."
          else reply "530" "incorrect password."

login_required :: Monad m => Command m -> Command m
login_required cmd arg = do
    b <- FTP (gets authed)
    if b
      then cmd arg
      else reply "530" "Command not possible in non-authenticated state."

cmd_cwd dir = FTP (modify $ \st -> st{ directory = dir })

cmd_pwd _ = do
    d <- FTP (gets directory)
    reply "257" $ "\"" ++ d ++ "\" is the current working directory."

--cmd_pasv _ = do
--    lift list

commands :: Monad m => [(ByteString, Command m)]
commands =
    [("USER", cmd_user)
    ,("PASS", const $ reply "530" "Out of sequence PASS command")
    ,("QUIT", const $ reply "221" "OK, Goodbye." >> fail "connection closed.")
    --,(Command "HELP" (help,             help_help))
    ,("CWD",  login_required cmd_cwd)
    ,("PWD",  login_required cmd_pwd)
    --,("PASV", login_required cmd_pasv)
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

commandLoop :: Monad m => FTP m ()
commandLoop = do
    (cmd, arg) <- wait $ getCommand
    case lookup cmd commands of
        Just cmd' -> cmd' arg >> commandLoop
        Nothing  -> reply "502" $ "Unrecognized command " ++ cmd
