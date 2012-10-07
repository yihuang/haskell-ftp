{-# LANGUAGE ViewPatterns #-}
import System.Environment (getArgs)
import Filesystem.Path.CurrentOS (decodeString)
import Data.Conduit.Network (runTCPServer, serverSettings, HostPreference(HostAny))
import Network.FTP.Server (ftpServer)
import Network.FTP.Backend.FileSystem (runFSBackend, FSConf(FSConf))

main :: IO ()
main = do
    [read -> port, d] <- getArgs
    let serverConf = serverSettings port HostAny True
    runFSBackend (FSConf (decodeString d)) $
        runTCPServer serverConf ftpServer
